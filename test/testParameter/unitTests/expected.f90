module test_testParameter_unitTests_expected
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: strings_enclose
    use :: type_testParameter
    implicit none
    private
    public :: expected_should_return_string_without_namelist_keywords

contains
    subroutine expected_should_return_string_without_namelist_keywords(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        character(:), allocatable :: expected_expected
        character(:), allocatable :: actual_expected

        call setup(param, expected_expected)

        actual_expected = param%expected()

        call check(error, len(actual_expected) == len(expected_expected), &
                   "expected arguments namelist length "//to_string(len(expected_expected)) &
                   //", but got "//to_string(len(actual_expected)))
        if (occurred(error)) return

        call check(error, actual_expected == expected_expected, &
                   "expected "//enclose(expected_expected, '"')//", "// &
                   "but got "//enclose(actual_expected, '"'))

        call teardown()
    contains
        !
        subroutine setup(param, expected_expected)
            type(test_parameter_type), intent(inout) :: param
            character(:), allocatable, intent(inout) :: expected_expected

            expected_expected = "output1=2 output2=20"
            call param%construct("", "output1=2 output2=20")
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine expected_should_return_string_without_namelist_keywords
end module test_testParameter_unitTests_expected
