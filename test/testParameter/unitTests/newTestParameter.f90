module test_testParameter_unitTests_newTestParameter
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: strings_enclose
    use :: type_testParameter
    implicit none
    private
    public :: newTestParam_should_return_test_parameter_type_instance
    public :: newTestParam_should_return_instance_that_having_namelists

contains
    subroutine newTestParam_should_return_test_parameter_type_instance(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param

        call check(error, same_type_as(new_test_parameter("", ""), param), &
                   "expected 'test_parameter_type' instance, but got not it")
    end subroutine newTestParam_should_return_test_parameter_type_instance

    subroutine newTestParam_should_return_instance_that_having_namelists(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        character(:), allocatable :: arguments, expected
        character(:), allocatable :: expected_arguments_namelist, &
                                     expected_expected_namelist

        call setup(param, arguments, expected, &
                   expected_arguments_namelist, expected_expected_namelist)

        param = new_test_parameter(arguments, expected)
        call check(error, param%arguments_namelist == expected_arguments_namelist, &
                   "expected "//enclose(expected_arguments_namelist, '"')// &
                   ", but got "//enclose(param%arguments_namelist, '"'))
        if (occurred(error)) return

        call check(error, param%expected_namelist == expected_expected_namelist, &
                   "expected "//enclose(expected_expected_namelist, '"')// &
                   ", but got "//enclose(param%expected_namelist, '"'))

        call teardown(arguments, expected, &
                      expected_arguments_namelist, expected_expected_namelist)
    contains
        !
        subroutine setup(param, arguments, expected, expected_arguments_namelist, expected_expected_namelist)
            type(test_parameter_type), intent(out) :: param
            character(:), allocatable, intent(out) :: arguments, expected
            character(:), allocatable, intent(out) :: expected_arguments_namelist, expected_expected_namelist

            if (allocated(param%arguments_namelist)) deallocate (param%arguments_namelist)
            if (allocated(param%expected_namelist)) deallocate (param%expected_namelist)

            arguments = "a=1 b=10 c=4"
            expected = "xyz=2 uvw=20"
            expected_arguments_namelist = "&arguments a=1 b=10 c=4 /"
            expected_expected_namelist = "&expected xyz=2 uvw=20 /"
        end subroutine setup
        !
        subroutine teardown(arguments, expected, expected_arguments_namelist, expected_expected_namelist)
            character(:), allocatable, intent(inout) :: arguments, expected
            character(:), allocatable, intent(inout) :: expected_arguments_namelist, expected_expected_namelist

            deallocate (arguments)
            deallocate (expected)
            deallocate (expected_arguments_namelist)
            deallocate (expected_expected_namelist)
        end subroutine teardown
    end subroutine newTestParam_should_return_instance_that_having_namelists
end module test_testParameter_unitTests_newTestParameter
