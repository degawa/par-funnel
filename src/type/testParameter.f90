module type_testParameter
    use, intrinsic :: iso_fortran_env
    use :: nml_util_format
    use :: nml_util_replace
    use :: nml_util_parse
    use :: nml_util_get
    implicit none
    private
    public :: new_test_parameter
    public :: replace_new_line_mark
    public :: replace_new_line_char
    public :: new_line_char
    public :: new_line_mark

    !>This user-defined type contains procedure arguments
    !>and expected values that the procedure should return,
    !>and the intended use is to describe multiple inputs within a unit test.
    !>
    !>All components are open to the public for type extension.
    type, public :: test_parameter_type
        character(:), public, allocatable :: arguments_namelist
            !! namelist of arguments to be passed to a procedure under test
        character(:), public, allocatable :: expected_namelist
            !! namelist of expected values returned from/updated by a procedure under test
        character(:), public, allocatable :: case_name
            !! name of a test case using the test parameter
    contains
        procedure, public, pass :: construct => construct_component_namelists
        !* constructs the `test_parameter_type` instance
        final :: finalize
        !* finalizes the `test_parameter_type` instance

        procedure, public, pass :: presented
        !* checks for the presence of an argument
        procedure, public, pass :: presence_index
        !* gets an index computed from the presence statuses of optional arguments,
        ! intended to be used in a `select case` construct
        procedure, public, pass :: presence_pattern
        !* gets the presence statuses of optional arguments in a string
        ! such as "PA" (P: present, A: absent),
        ! intended to be used in a `select case` construct

        procedure, private, pass :: get_int32
        procedure, private, pass :: get_int64
        procedure, private, pass :: get_real32
        procedure, private, pass :: get_real64
        procedure, private, pass :: get_logical
        procedure, private, pass :: get_char
        procedure, private, pass :: get_int32_rank1
        procedure, private, pass :: get_int64_rank1
        procedure, private, pass :: get_real32_rank1
        procedure, private, pass :: get_real64_rank1
        procedure, private, pass :: get_logical_rank1
        generic, public :: get => get_int32, get_int64, &
            get_real32, get_real64, &
            get_logical, get_char, &
            get_int32_rank1, get_int64_rank1, &
            get_real32_rank1, get_real64_rank1, &
            get_logical_rank1
        !* gets a value in the arguments or expected namelist by name,
        ! without requiring the user to declare a namelist

        procedure, public, pass :: arguments
        !* gets the arguments
        procedure, public, pass :: expected
        !* gets the expected results

        procedure, public, pass :: setup
        !* setups a test using the arguments<br>
        ! intended to be overridden by extended types
        procedure, public, pass :: teardown
        !* teardowns the test<br>
        ! intended to be overridden by extended types
    end type test_parameter_type

    interface new_test_parameter
        procedure :: construct_test_parameter
    end interface

    character(*), private, parameter :: arguments_group_name = "arguments"
        !! namelist group name for the arguments
    character(*), private, parameter :: expected_group_name = "expected"
        !! namelist group name for the expected results

    !>Presence indices for **two** optional arguments,
    !>returned by `presence_index` of `test_parameter_type`
    !>and `as_index` of `arguments_presence_type`.
    !>
    !>The letter `A` means absent and `P` means present,
    !>ordered as the arguments (the 1st argument first):
    !>`presence_PA` means that the 1st argument is present
    !>and the 2nd argument is absent.
    !>
    !>```Fortran
    !>select case (params(case)%presence_index("fmt", "less_digits"))
    !>case (presence_AA); str = int_to_str(input)
    !>case (presence_PA); str = int_to_str(input, fmt)
    !>case (presence_AP); str = int_to_str(input, less_digits=less_digits)
    !>case (presence_PP); str = int_to_str(input, fmt, less_digits)
    !>end select
    !>```
    !>
    !>@note
    !>The constants are provided only for two optional arguments
    !>because the number of combinations grows as 2^n
    !>as the number of arguments increases.
    !>For three or more arguments, consider using `presence_pattern`,
    !>which returns the presence statuses in a string such as `"PAP"`
    !>and requires no constants.
    !>@endnote
    integer(int32), public, parameter :: presence_AA = 0
        !! both the 1st and 2nd optional arguments are absent
    integer(int32), public, parameter :: presence_PA = 1
        !! the 1st optional argument is present and the 2nd is absent
    integer(int32), public, parameter :: presence_AP = 2
        !! the 1st optional argument is absent and the 2nd is present
    integer(int32), public, parameter :: presence_PP = 3
        !! both the 1st and 2nd optional arguments are present

contains
    !>returns a new `test_parameter_type` instance.
    pure function construct_test_parameter(arguments, expected) result(test_parameter)
        implicit none
        character(*), intent(in) :: arguments
            !! list of arguments to be passed to a procedure under test
        character(*), intent(in) :: expected
            !! list of expected values returned from/updated by a procedure under test
        type(test_parameter_type) :: test_parameter
            !! new `test_parameter_type` instance

        call test_parameter%construct(arguments, expected)
    end function construct_test_parameter

    !>constructs the `test_parameter_type` instance
    !>by converting the arguments and expected lists to namelists
    pure subroutine construct_component_namelists(this, arguments, expected)
        implicit none
        class(test_parameter_type), intent(inout) :: this
            !! passed dummy argument
        character(*), intent(in) :: arguments
            !! the list of arguments to be passed to a procedure under test
        character(*), intent(in) :: expected
            !! the list of expected values
            !! returned from/updated by a procedure under test

        this%arguments_namelist = namelist_start//arguments_group_name//" "// &
                                  normalize_assignments(arguments)//" "//namelist_end
        this%expected_namelist = namelist_start//expected_group_name//" "// &
                                 normalize_assignments(expected)//" "//namelist_end
    end subroutine construct_component_namelists

    !>finalize the `test_parameter_type` instance
    !>by deallocating components
    !>expected_namelist` and `expected_namelist`
    pure subroutine finalize(this)
        implicit none
        type(test_parameter_type), intent(inout) :: this
            !! passed dummy argument
        if (allocated(this%arguments_namelist)) deallocate (this%arguments_namelist)
        if (allocated(this%expected_namelist)) deallocate (this%expected_namelist)
    end subroutine finalize

    !>returns `.true.` if the `argument` is found
    !>in the component `arguments_namelist`
    !>and returns `.false.` otherwise.
    pure logical function presented(this, argument)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: argument
            !! the argument to be checked for its presence

        character(:), allocatable :: token

        call find_value_token(this%arguments_namelist, argument, token, presented)
    end function presented

    !>returns an index computed from the presence statuses
    !>of up to 8 optional arguments.
    !>
    !>The index is a bit pattern in that the bit `i-1` is set
    !>when the `i`-th argument is present in the arguments namelist:
    !>
    !>| presence of `(arg1, arg2)` | index |
    !>| :------------------------- | ----: |
    !>| `(absent , absent )`       |     0 |
    !>| `(present, absent )`       |     1 |
    !>| `(absent , present)`       |     2 |
    !>| `(present, present)`       |     3 |
    !>
    !>The intended use is to branch procedure calls with optional arguments
    !>in a `select case` construct instead of `if` statement chains:
    !>
    !>```Fortran
    !>select case (params(case)%presence_index("fmt", "less_digits"))
    !>case (presence_AA); act = int_to_str(input)
    !>case (presence_PA); act = int_to_str(input, fmt)
    !>case (presence_AP); act = int_to_str(input, less_digits=less_digits)
    !>case (presence_PP); act = int_to_str(input, fmt, less_digits)
    !>end select
    !>```
    !>
    !>The constants `presence_AA`, `presence_PA`, `presence_AP`, and `presence_PP`
    !>are provided for two optional arguments.
    !>For three or more arguments, consider using `presence_pattern`,
    !>which returns the presence statuses in a string such as `"PAP"`
    !>and requires no constants.
    pure function presence_index(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) result(idx)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: arg1
            !! the name of the 1st optional argument
        character(*), intent(in), optional :: arg2
            !! the name of the 2nd optional argument
        character(*), intent(in), optional :: arg3
            !! the name of the 3rd optional argument
        character(*), intent(in), optional :: arg4
            !! the name of the 4th optional argument
        character(*), intent(in), optional :: arg5
            !! the name of the 5th optional argument
        character(*), intent(in), optional :: arg6
            !! the name of the 6th optional argument
        character(*), intent(in), optional :: arg7
            !! the name of the 7th optional argument
        character(*), intent(in), optional :: arg8
            !! the name of the 8th optional argument
        integer(int32) :: idx
            !! the index computed from the presence statuses

        idx = 0
        if (this%presented(arg1)) idx = idx + 1
        if (present(arg2)) then
            if (this%presented(arg2)) idx = idx + 2
        end if
        if (present(arg3)) then
            if (this%presented(arg3)) idx = idx + 4
        end if
        if (present(arg4)) then
            if (this%presented(arg4)) idx = idx + 8
        end if
        if (present(arg5)) then
            if (this%presented(arg5)) idx = idx + 16
        end if
        if (present(arg6)) then
            if (this%presented(arg6)) idx = idx + 32
        end if
        if (present(arg7)) then
            if (this%presented(arg7)) idx = idx + 64
        end if
        if (present(arg8)) then
            if (this%presented(arg8)) idx = idx + 128
        end if
    end function presence_index

    !>returns the presence statuses of up to 8 optional arguments
    !>in a string, in that the `i`-th character is `P`
    !>when the `i`-th argument is present and `A` when absent:
    !>
    !>| presence of `(arg1, arg2)` | pattern |
    !>| :------------------------- | :------ |
    !>| `(absent , absent )`       | `"AA"`  |
    !>| `(present, absent )`       | `"PA"`  |
    !>| `(absent , present)`       | `"AP"`  |
    !>| `(present, present)`       | `"PP"`  |
    !>
    !>The intended use is to branch procedure calls with optional arguments
    !>in a `select case` construct.
    !>The case expressions are self-describing, requiring no constants,
    !>and the number of arguments scales without defining 2^n constants:
    !>
    !>```Fortran
    !>select case (params(case)%presence_pattern("fmt", "less_digits"))
    !>case ("AA"); str = int_to_str(input)
    !>case ("PA"); str = int_to_str(input, fmt)
    !>case ("AP"); str = int_to_str(input, less_digits=less_digits)
    !>case ("PP"); str = int_to_str(input, fmt, less_digits)
    !>end select
    !>```
    pure function presence_pattern(this, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) result(pattern)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: arg1
            !! the name of the 1st optional argument
        character(*), intent(in), optional :: arg2
            !! the name of the 2nd optional argument
        character(*), intent(in), optional :: arg3
            !! the name of the 3rd optional argument
        character(*), intent(in), optional :: arg4
            !! the name of the 4th optional argument
        character(*), intent(in), optional :: arg5
            !! the name of the 5th optional argument
        character(*), intent(in), optional :: arg6
            !! the name of the 6th optional argument
        character(*), intent(in), optional :: arg7
            !! the name of the 7th optional argument
        character(*), intent(in), optional :: arg8
            !! the name of the 8th optional argument
        character(:), allocatable :: pattern
            !! the presence statuses in a string (P: present, A: absent)

        pattern = a_or_p(this%presented(arg1))
        if (present(arg2)) pattern = pattern//a_or_p(this%presented(arg2))
        if (present(arg3)) pattern = pattern//a_or_p(this%presented(arg3))
        if (present(arg4)) pattern = pattern//a_or_p(this%presented(arg4))
        if (present(arg5)) pattern = pattern//a_or_p(this%presented(arg5))
        if (present(arg6)) pattern = pattern//a_or_p(this%presented(arg6))
        if (present(arg7)) pattern = pattern//a_or_p(this%presented(arg7))
        if (present(arg8)) pattern = pattern//a_or_p(this%presented(arg8))
    end function presence_pattern

    !>returns `"P"` when the presence status is `.true.`
    !>and `"A"` otherwise.
    pure function a_or_p(presented) result(letter)
        implicit none
        logical, intent(in) :: presented
            !! the presence status
        character(1) :: letter
            !! `"P"` for present or `"A"` for absent

        if (presented) then
            letter = "P"
        else
            letter = "A"
        end if
    end function a_or_p

    !>returns the arguments in a string,
    !>not including the namelist keywords
    pure function arguments(this) result(args)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(:), allocatable :: args
            !! the arguments not including the namelist keywords

        args = drop_namelist_keywords(this%arguments_namelist)
    end function arguments

    !>returns the expected results in a string,
    !>not including the namelist keywords
    pure function expected(this) result(expc)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(:), allocatable :: expc
            !! the expected results not including the namelist keywords

        expc = drop_namelist_keywords(this%expected_namelist)
    end function expected

    !>setups a test using the arguments and expected results.
    !>This procedure is intended to be overridden by extended types.
    subroutine setup(this)
        implicit none
        class(test_parameter_type), intent(inout) :: this
            !! passed-object dummy argument
    end subroutine setup

    !>teardowns the test.
    !>This procedure is intended to be overridden by extended types.
    subroutine teardown(this)
        implicit none
        class(test_parameter_type), intent(inout) :: this
            !! passed-object dummy argument
    end subroutine teardown

    !------------------------------------------------------------------!
    ! generic getters
    !
    ! The getters read a value by name from the arguments namelist and,
    ! if the name is not found there, from the expected namelist.
    ! Users do not need to declare namelists nor to execute read statements.
    !
    ! When the optional argument `found` is passed,
    ! it is set to the presence status of `name`
    ! and the value is left unchanged if `name` is not found,
    ! so that a default value assigned in advance is kept.
    ! When `found` is not passed and `name` is not found,
    ! the program is terminated by `error stop` with a message.
    ! A malformed value always terminates the program.
    !------------------------------------------------------------------!

    !>gets an `integer(int32)` value by name
    !>from the arguments or expected namelist.
    subroutine get_int32(this, name, value, found, from)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: name
            !! the name of the value to get
        integer(int32), intent(inout) :: value
            !! the value read from the namelist
        logical, intent(out), optional :: found
            !! the presence status of `name`
        character(*), intent(in), optional :: from
            !! the namelist to be searched:
            !! `"arguments"`, `"expected"`, or `"both"` (default)

        integer(int32) :: stat
        character(:), allocatable :: errmsg

        logical :: search_arguments, search_expected

        call search_scope_of(from, search_arguments, search_expected)

        stat = stat_not_found
        errmsg = ""
        if (search_arguments) then
            call read_value_from(this%arguments_namelist, name, value, stat, errmsg)
        end if
        if (stat == stat_not_found .and. search_expected) then
            call read_value_from(this%expected_namelist, name, value, stat, errmsg)
        end if
        call handle_get_status(name, stat, errmsg, found)
    end subroutine get_int32

    !>gets an `integer(int64)` value by name
    !>from the arguments or expected namelist.
    subroutine get_int64(this, name, value, found, from)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: name
            !! the name of the value to get
        integer(int64), intent(inout) :: value
            !! the value read from the namelist
        logical, intent(out), optional :: found
            !! the presence status of `name`
        character(*), intent(in), optional :: from
            !! the namelist to be searched:
            !! `"arguments"`, `"expected"`, or `"both"` (default)

        integer(int32) :: stat
        character(:), allocatable :: errmsg

        logical :: search_arguments, search_expected

        call search_scope_of(from, search_arguments, search_expected)

        stat = stat_not_found
        errmsg = ""
        if (search_arguments) then
            call read_value_from(this%arguments_namelist, name, value, stat, errmsg)
        end if
        if (stat == stat_not_found .and. search_expected) then
            call read_value_from(this%expected_namelist, name, value, stat, errmsg)
        end if
        call handle_get_status(name, stat, errmsg, found)
    end subroutine get_int64

    !>gets a `real(real32)` value by name
    !>from the arguments or expected namelist.
    subroutine get_real32(this, name, value, found, from)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: name
            !! the name of the value to get
        real(real32), intent(inout) :: value
            !! the value read from the namelist
        logical, intent(out), optional :: found
            !! the presence status of `name`
        character(*), intent(in), optional :: from
            !! the namelist to be searched:
            !! `"arguments"`, `"expected"`, or `"both"` (default)

        integer(int32) :: stat
        character(:), allocatable :: errmsg

        logical :: search_arguments, search_expected

        call search_scope_of(from, search_arguments, search_expected)

        stat = stat_not_found
        errmsg = ""
        if (search_arguments) then
            call read_value_from(this%arguments_namelist, name, value, stat, errmsg)
        end if
        if (stat == stat_not_found .and. search_expected) then
            call read_value_from(this%expected_namelist, name, value, stat, errmsg)
        end if
        call handle_get_status(name, stat, errmsg, found)
    end subroutine get_real32

    !>gets a `real(real64)` value by name
    !>from the arguments or expected namelist.
    subroutine get_real64(this, name, value, found, from)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: name
            !! the name of the value to get
        real(real64), intent(inout) :: value
            !! the value read from the namelist
        logical, intent(out), optional :: found
            !! the presence status of `name`
        character(*), intent(in), optional :: from
            !! the namelist to be searched:
            !! `"arguments"`, `"expected"`, or `"both"` (default)

        integer(int32) :: stat
        character(:), allocatable :: errmsg

        logical :: search_arguments, search_expected

        call search_scope_of(from, search_arguments, search_expected)

        stat = stat_not_found
        errmsg = ""
        if (search_arguments) then
            call read_value_from(this%arguments_namelist, name, value, stat, errmsg)
        end if
        if (stat == stat_not_found .and. search_expected) then
            call read_value_from(this%expected_namelist, name, value, stat, errmsg)
        end if
        call handle_get_status(name, stat, errmsg, found)
    end subroutine get_real64

    !>gets a `logical` value by name
    !>from the arguments or expected namelist.
    subroutine get_logical(this, name, value, found, from)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: name
            !! the name of the value to get
        logical, intent(inout) :: value
            !! the value read from the namelist
        logical, intent(out), optional :: found
            !! the presence status of `name`
        character(*), intent(in), optional :: from
            !! the namelist to be searched:
            !! `"arguments"`, `"expected"`, or `"both"` (default)

        integer(int32) :: stat
        character(:), allocatable :: errmsg

        logical :: search_arguments, search_expected

        call search_scope_of(from, search_arguments, search_expected)

        stat = stat_not_found
        errmsg = ""
        if (search_arguments) then
            call read_value_from(this%arguments_namelist, name, value, stat, errmsg)
        end if
        if (stat == stat_not_found .and. search_expected) then
            call read_value_from(this%expected_namelist, name, value, stat, errmsg)
        end if
        call handle_get_status(name, stat, errmsg, found)
    end subroutine get_logical

    !>gets a character value by name
    !>from the arguments or expected namelist.
    !>
    !>Trailing blanks are removed from the value.
    subroutine get_char(this, name, value, found, from)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: name
            !! the name of the value to get
        character(:), allocatable, intent(inout) :: value
            !! the value read from the namelist
        logical, intent(out), optional :: found
            !! the presence status of `name`
        character(*), intent(in), optional :: from
            !! the namelist to be searched:
            !! `"arguments"`, `"expected"`, or `"both"` (default)

        integer(int32) :: stat
        character(:), allocatable :: errmsg

        logical :: search_arguments, search_expected

        call search_scope_of(from, search_arguments, search_expected)

        stat = stat_not_found
        errmsg = ""
        if (search_arguments) then
            call read_value_from(this%arguments_namelist, name, value, stat, errmsg)
        end if
        if (stat == stat_not_found .and. search_expected) then
            call read_value_from(this%expected_namelist, name, value, stat, errmsg)
        end if
        call handle_get_status(name, stat, errmsg, found)
    end subroutine get_char

    !>gets an `integer(int32)` rank-1 array by name
    !>from the arguments or expected namelist.
    !>
    !>The array `value` must have the same size
    !>as the number of values written in the namelist.
    subroutine get_int32_rank1(this, name, value, found, from)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: name
            !! the name of the value to get
        integer(int32), intent(inout) :: value(:)
            !! the values read from the namelist
        logical, intent(out), optional :: found
            !! the presence status of `name`
        character(*), intent(in), optional :: from
            !! the namelist to be searched:
            !! `"arguments"`, `"expected"`, or `"both"` (default)

        integer(int32) :: stat
        character(:), allocatable :: errmsg

        logical :: search_arguments, search_expected

        call search_scope_of(from, search_arguments, search_expected)

        stat = stat_not_found
        errmsg = ""
        if (search_arguments) then
            call read_value_from(this%arguments_namelist, name, value, stat, errmsg)
        end if
        if (stat == stat_not_found .and. search_expected) then
            call read_value_from(this%expected_namelist, name, value, stat, errmsg)
        end if
        call handle_get_status(name, stat, errmsg, found)
    end subroutine get_int32_rank1

    !>gets an `integer(int64)` rank-1 array by name
    !>from the arguments or expected namelist.
    !>
    !>The array `value` must have the same size
    !>as the number of values written in the namelist.
    subroutine get_int64_rank1(this, name, value, found, from)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: name
            !! the name of the value to get
        integer(int64), intent(inout) :: value(:)
            !! the values read from the namelist
        logical, intent(out), optional :: found
            !! the presence status of `name`
        character(*), intent(in), optional :: from
            !! the namelist to be searched:
            !! `"arguments"`, `"expected"`, or `"both"` (default)

        integer(int32) :: stat
        character(:), allocatable :: errmsg

        logical :: search_arguments, search_expected

        call search_scope_of(from, search_arguments, search_expected)

        stat = stat_not_found
        errmsg = ""
        if (search_arguments) then
            call read_value_from(this%arguments_namelist, name, value, stat, errmsg)
        end if
        if (stat == stat_not_found .and. search_expected) then
            call read_value_from(this%expected_namelist, name, value, stat, errmsg)
        end if
        call handle_get_status(name, stat, errmsg, found)
    end subroutine get_int64_rank1

    !>gets a `real(real32)` rank-1 array by name
    !>from the arguments or expected namelist.
    !>
    !>The array `value` must have the same size
    !>as the number of values written in the namelist.
    subroutine get_real32_rank1(this, name, value, found, from)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: name
            !! the name of the value to get
        real(real32), intent(inout) :: value(:)
            !! the values read from the namelist
        logical, intent(out), optional :: found
            !! the presence status of `name`
        character(*), intent(in), optional :: from
            !! the namelist to be searched:
            !! `"arguments"`, `"expected"`, or `"both"` (default)

        integer(int32) :: stat
        character(:), allocatable :: errmsg

        logical :: search_arguments, search_expected

        call search_scope_of(from, search_arguments, search_expected)

        stat = stat_not_found
        errmsg = ""
        if (search_arguments) then
            call read_value_from(this%arguments_namelist, name, value, stat, errmsg)
        end if
        if (stat == stat_not_found .and. search_expected) then
            call read_value_from(this%expected_namelist, name, value, stat, errmsg)
        end if
        call handle_get_status(name, stat, errmsg, found)
    end subroutine get_real32_rank1

    !>gets a `real(real64)` rank-1 array by name
    !>from the arguments or expected namelist.
    !>
    !>The array `value` must have the same size
    !>as the number of values written in the namelist.
    subroutine get_real64_rank1(this, name, value, found, from)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: name
            !! the name of the value to get
        real(real64), intent(inout) :: value(:)
            !! the values read from the namelist
        logical, intent(out), optional :: found
            !! the presence status of `name`
        character(*), intent(in), optional :: from
            !! the namelist to be searched:
            !! `"arguments"`, `"expected"`, or `"both"` (default)

        integer(int32) :: stat
        character(:), allocatable :: errmsg

        logical :: search_arguments, search_expected

        call search_scope_of(from, search_arguments, search_expected)

        stat = stat_not_found
        errmsg = ""
        if (search_arguments) then
            call read_value_from(this%arguments_namelist, name, value, stat, errmsg)
        end if
        if (stat == stat_not_found .and. search_expected) then
            call read_value_from(this%expected_namelist, name, value, stat, errmsg)
        end if
        call handle_get_status(name, stat, errmsg, found)
    end subroutine get_real64_rank1

    !>gets a `logical` rank-1 array by name
    !>from the arguments or expected namelist.
    !>
    !>The array `value` must have the same size
    !>as the number of values written in the namelist.
    subroutine get_logical_rank1(this, name, value, found, from)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: name
            !! the name of the value to get
        logical, intent(inout) :: value(:)
            !! the values read from the namelist
        logical, intent(out), optional :: found
            !! the presence status of `name`
        character(*), intent(in), optional :: from
            !! the namelist to be searched:
            !! `"arguments"`, `"expected"`, or `"both"` (default)

        integer(int32) :: stat
        character(:), allocatable :: errmsg

        logical :: search_arguments, search_expected

        call search_scope_of(from, search_arguments, search_expected)

        stat = stat_not_found
        errmsg = ""
        if (search_arguments) then
            call read_value_from(this%arguments_namelist, name, value, stat, errmsg)
        end if
        if (stat == stat_not_found .and. search_expected) then
            call read_value_from(this%expected_namelist, name, value, stat, errmsg)
        end if
        call handle_get_status(name, stat, errmsg, found)
    end subroutine get_logical_rank1

    !>handles the status of a get operation.
    !>
    !>- sets `found` to the presence status of `name` if `found` is passed
    !>- terminates the program when `name` is not found and `found` is not passed
    !>- terminates the program when a value cannot be interpreted
    subroutine handle_get_status(name, stat, errmsg, found)
        implicit none
        character(*), intent(in) :: name
            !! the name of the value to get
        integer(int32), intent(in) :: stat
            !! the status of the read operation
        character(*), intent(in) :: errmsg
            !! the error message when `stat` is not `stat_ok`
        logical, intent(out), optional :: found
            !! the presence status of `name`

        select case (stat)
        case (stat_ok)
            if (present(found)) found = .true.

        case (stat_not_found)
            if (present(found)) then
                found = .false.
            else
                error stop 'par-funnel: variable "'//name// &
                    '" is not found in the searched namelist(s)'
            end if

        case default ! stat_read_error
            error stop errmsg
        end select
    end subroutine handle_get_status

    !>converts the optional `from` specifier of the getters
    !>into the flags of the namelists to be searched.
    !>
    !>Terminates the program when the specifier is invalid.
    subroutine search_scope_of(from, search_arguments, search_expected)
        implicit none
        character(*), intent(in), optional :: from
            !! the namelist to be searched:
            !! `"arguments"`, `"expected"`, or `"both"` (default)
        logical, intent(out) :: search_arguments
            !! `.true.` when the arguments namelist should be searched
        logical, intent(out) :: search_expected
            !! `.true.` when the expected namelist should be searched

        character(:), allocatable :: scope

        if (present(from)) then
            scope = from
        else
            scope = "both"
        end if

        select case (scope)
        case ("arguments")
            search_arguments = .true.
            search_expected = .false.
        case ("expected")
            search_arguments = .false.
            search_expected = .true.
        case ("both")
            search_arguments = .true.
            search_expected = .true.
        case default
            error stop 'par-funnel: invalid "from" specifier "'//scope// &
                '" (must be "arguments", "expected", or "both")'
        end select
    end subroutine search_scope_of
end module type_testParameter
