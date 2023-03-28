module test_testParameter_unitTests_construct
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: strings_enclose
    use :: type_testParameter
    implicit none
    private
    public :: construct_should_configure_namelists_for_arguments
    public :: construct_should_configure_namelists_for_expected_results

contains
    subroutine construct_should_configure_namelists_for_arguments(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        character(:), allocatable :: arguments, expected
        character(:), allocatable :: expected_arguments_namelist

        call setup(arguments, expected, expected_arguments_namelist)

        call param%construct(arguments, expected)

        call check(error, len(param%arguments_namelist) == len(expected_arguments_namelist), &
                   "expected arguments namelist length "//to_string(len(expected_arguments_namelist)) &
                   //", but got "//to_string(len(param%arguments_namelist)))
        if (occurred(error)) return

        call check(error, param%arguments_namelist == expected_arguments_namelist, &
                   "expected "//enclose(expected_arguments_namelist, '"')//", "// &
                   "but got "//enclose(param%arguments_namelist, '"'))

        call teardown(arguments, expected, expected_arguments_namelist)
    contains
        !
        subroutine setup(arguments, expected, expected_arguments_namelist)
            character(:), allocatable, intent(out) :: arguments, expected
            character(:), allocatable, intent(out) :: expected_arguments_namelist

            arguments = "input1=1 input2=10 input3=4"
            expected = ""
            expected_arguments_namelist = "&arguments input1=1 input2=10 input3=4 /"
        end subroutine setup
        !
        subroutine teardown(arguments, expected, expected_arguments_namelist)
            character(:), allocatable, intent(inout) :: arguments, expected
            character(:), allocatable, intent(inout) :: expected_arguments_namelist

            deallocate (arguments)
            deallocate (expected)
            deallocate (expected_arguments_namelist)
        end subroutine teardown
    end subroutine construct_should_configure_namelists_for_arguments

    subroutine construct_should_configure_namelists_for_expected_results(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        character(:), allocatable :: arguments, expected
        character(:), allocatable :: expected_expected_namelist

        call setup(arguments, expected, expected_expected_namelist)

        call param%construct(arguments, expected)

        call check(error, len(param%expected_namelist) == len(expected_expected_namelist), &
                   "expected expected namelist length "//to_string(len(expected_expected_namelist)) &
                   //", but got "//to_string(len(param%expected_namelist)))
        if (occurred(error)) return

        call check(error, param%expected_namelist == expected_expected_namelist, &
                   "expected "//enclose(expected_expected_namelist, '"')//", "// &
                   "but got "//enclose(param%expected_namelist, '"'))

        call teardown(arguments, expected, expected_expected_namelist)
    contains
        !
        subroutine setup(arguments, expected, expected_expected_namelist)
            character(:), allocatable, intent(out) :: arguments, expected
            character(:), allocatable, intent(out) :: expected_expected_namelist

            arguments = ""
            expected = "output1=2 output2=20"
            expected_expected_namelist = "&expected output1=2 output2=20 /"
        end subroutine setup
        !
        subroutine teardown(arguments, expected, expected_expected_namelist)
            character(:), allocatable, intent(inout) :: arguments, expected
            character(:), allocatable, intent(inout) :: expected_expected_namelist

            deallocate (arguments)
            deallocate (expected)
            deallocate (expected_expected_namelist)
        end subroutine teardown
    end subroutine construct_should_configure_namelists_for_expected_results
end module test_testParameter_unitTests_construct
