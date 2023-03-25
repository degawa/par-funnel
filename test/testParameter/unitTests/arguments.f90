module test_testParameter_unitTests_arguments
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: strings_enclose
    use :: type_testParameter
    implicit none
    private
    public :: arguments_should_return_string_without_namelist_keywords

contains
    subroutine arguments_should_return_string_without_namelist_keywords(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        character(:), allocatable :: expected_arguments
        character(:), allocatable :: actual_arguments

        call setup(param, expected_arguments)

        actual_arguments = param%arguments()

        call check(error, len(actual_arguments) == len(expected_arguments), &
                   "expected arguments namelist length "//to_string(len(expected_arguments)) &
                   //", but got "//to_string(len(actual_arguments)))
        if (occurred(error)) return

        call check(error, actual_arguments == expected_arguments, &
                   "expected "//enclose(expected_arguments, '"')//", "// &
                   "but got "//enclose(actual_arguments, '"'))

        call teardown()
    contains
        !
        subroutine setup(param, expected_arguments)
            type(test_parameter_type), intent(out) :: param
            character(:), allocatable, intent(out) :: expected_arguments

            expected_arguments = "input1=1 input2=10 input3=4"
            call param%construct("input1=1 input2=10 input3=4", "")
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine arguments_should_return_string_without_namelist_keywords
end module test_testParameter_unitTests_arguments
