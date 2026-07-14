module test_testParameter_unitTests_get
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: type_testParameter
    implicit none
    private
    public :: get_should_read_scalar_values_of_intrinsic_types
    public :: get_should_read_values_from_expected_namelist
    public :: get_should_read_rank1_arrays
    public :: get_should_set_found_false_and_keep_default_when_not_found
    public :: get_should_read_values_written_with_spaces_around_equals
    public :: get_should_read_quoted_and_unquoted_strings
    public :: get_should_search_only_the_namelist_specified_by_from

contains
    subroutine get_should_search_only_the_namelist_specified_by_from(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        logical :: val, found

        ! `less_digits` exists in both namelists with different values
        param = new_test_parameter(arguments="input=1000 less_digits=true", &
                                   expected="str='1000' less_digits=false")

        call param%get("less_digits", val)
        call check(error, val .eqv. .true., &
                   "get without from should find the value in the arguments namelist first")
        if (occurred(error)) return

        call param%get("less_digits", val, from="arguments")
        call check(error, val .eqv. .true., &
                   'get with from="arguments" should find the value in the arguments namelist')
        if (occurred(error)) return

        call param%get("less_digits", val, from="expected")
        call check(error, val .eqv. .false., &
                   'get with from="expected" should find the value in the expected namelist')
        if (occurred(error)) return

        call param%get("input", val, found=found, from="expected")
        call check(error, found .eqv. .false., &
                   'get with from="expected" should not find a name in the arguments namelist')
        if (occurred(error)) return
    end subroutine get_should_search_only_the_namelist_specified_by_from

    subroutine get_should_read_scalar_values_of_intrinsic_types(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        integer(int32) :: i32
        integer(int64) :: i64
        real(real32) :: r32
        real(real64) :: r64
        logical :: l
        character(:), allocatable :: str

        param = new_test_parameter(arguments="i32=42 i64=8589934592 r32=1.5 r64=2.5d0 flag=true", &
                                   expected="str='hello world'")

        call param%get("i32", i32)
        call check(error, i32 == 42, "i32: could not read integer(int32)")
        if (occurred(error)) return

        call param%get("i64", i64)
        call check(error, i64 == 8589934592_int64, "i64: could not read integer(int64)")
        if (occurred(error)) return

        call param%get("r32", r32)
        call check(error, r32 == 1.5_real32, "r32: could not read real(real32)")
        if (occurred(error)) return

        call param%get("r64", r64)
        call check(error, r64 == 2.5_real64, "r64: could not read real(real64)")
        if (occurred(error)) return

        call param%get("flag", l)
        call check(error, l .eqv. .true., "flag: could not read logical")
        if (occurred(error)) return

        call param%get("str", str)
        call check(error, str == "hello world", "str: could not read character")
        if (occurred(error)) return
    end subroutine get_should_read_scalar_values_of_intrinsic_types

    subroutine get_should_read_values_from_expected_namelist(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        integer(int32) :: input, output

        param = new_test_parameter(arguments="input=1", expected="output=2")

        call param%get("input", input)
        call param%get("output", output)

        call check(error, input == 1, "could not read a value from the arguments namelist")
        if (occurred(error)) return
        call check(error, output == 2, "could not read a value from the expected namelist")
        if (occurred(error)) return
    end subroutine get_should_read_values_from_expected_namelist

    subroutine get_should_read_rank1_arrays(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        integer(int32) :: arr(3)
        real(real64) :: rarr(3)
        logical :: larr(2)

        param = new_test_parameter(arguments="arr=1, 2, 3 rarr=1.0, 2*2.5 larr=true, false", &
                                   expected="dummy=0")

        call param%get("arr", arr)
        call check(error, all(arr == [1, 2, 3]), "arr: could not read integer array")
        if (occurred(error)) return

        call param%get("rarr", rarr)
        call check(error, all(rarr == [1.0_real64, 2.5_real64, 2.5_real64]), &
                   "rarr: could not read real array with a repeat count")
        if (occurred(error)) return

        call param%get("larr", larr)
        call check(error, (larr(1) .eqv. .true.) .and. (larr(2) .eqv. .false.), &
                   "larr: could not read logical array")
        if (occurred(error)) return
    end subroutine get_should_read_rank1_arrays

    subroutine get_should_set_found_false_and_keep_default_when_not_found(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        integer(int32) :: val
        logical :: found

        param = new_test_parameter(arguments="input=1", expected="output=2")

        val = 100 ! a default value
        call param%get("not_defined", val, found)

        call check(error, found .eqv. .false., "found should be false for an undefined name")
        if (occurred(error)) return
        call check(error, val == 100, "the default value should be kept when the name is not found")
        if (occurred(error)) return

        call param%get("input", val, found)
        call check(error, found .eqv. .true., "found should be true for a defined name")
        if (occurred(error)) return
        call check(error, val == 1, "the value should be read when the name is found")
        if (occurred(error)) return
    end subroutine get_should_set_found_false_and_keep_default_when_not_found

    subroutine get_should_read_values_written_with_spaces_around_equals(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        integer(int32) :: input, output
        character(:), allocatable :: fmt

        param = new_test_parameter(arguments="input = 1 fmt = '(i5) = ok'", &
                                   expected="output =2")

        call param%get("input", input)
        call param%get("output", output)
        call param%get("fmt", fmt)

        call check(error, input == 1, "could not read a value written as 'input = 1'")
        if (occurred(error)) return
        call check(error, output == 2, "could not read a value written as 'output =2'")
        if (occurred(error)) return
        call check(error, fmt == "(i5) = ok", "'=' in a quoted string should not be normalized")
        if (occurred(error)) return
    end subroutine get_should_read_values_written_with_spaces_around_equals

    subroutine get_should_read_quoted_and_unquoted_strings(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        character(:), allocatable :: quoted, unquoted

        param = new_test_parameter(arguments="quoted='(i4)' unquoted=(i4)", &
                                   expected="dummy=0")

        call param%get("quoted", quoted)
        call param%get("unquoted", unquoted)

        call check(error, quoted == "(i4)", "could not read a quoted string")
        if (occurred(error)) return
        call check(error, unquoted == "(i4)", "could not read an unquoted string")
        if (occurred(error)) return
    end subroutine get_should_read_quoted_and_unquoted_strings
end module test_testParameter_unitTests_get
