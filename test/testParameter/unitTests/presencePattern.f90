module test_testParameter_unitTests_presencePattern
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: type_testParameter
    implicit none
    private
    public :: presencePattern_should_return_string_of_presence_statuses

contains
    subroutine presencePattern_should_return_string_of_presence_statuses(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param

        param = new_test_parameter(arguments="input=1000 fmt='(I4)'", expected="str='1000'")

        call check(error, param%presence_pattern("fmt") == "P", &
                   'expected "P", but got '//param%presence_pattern("fmt"))
        if (occurred(error)) return

        call check(error, param%presence_pattern("fmt", "less_digits") == "PA", &
                   'expected "PA", but got '//param%presence_pattern("fmt", "less_digits"))
        if (occurred(error)) return

        call check(error, param%presence_pattern("less_digits", "fmt") == "AP", &
                   'expected "AP", but got '//param%presence_pattern("less_digits", "fmt"))
        if (occurred(error)) return

        call check(error, param%presence_pattern("less_digits", "fmt", "input") == "APP", &
                   'expected "APP", but got '//param%presence_pattern("less_digits", "fmt", "input"))
        if (occurred(error)) return
    end subroutine presencePattern_should_return_string_of_presence_statuses
end module test_testParameter_unitTests_presencePattern
