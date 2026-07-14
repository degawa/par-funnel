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
