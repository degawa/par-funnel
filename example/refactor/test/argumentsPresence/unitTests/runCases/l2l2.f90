module refactor_test_argumentsPresentce_unitTests_eqvLL_runCases_l2l2
    use, intrinsic :: iso_fortran_env
    use :: testdrive_util, only:to_string
    use :: par_funnel
    implicit none
    private
    public :: run_test_cases

contains
    !>run all test cases
    subroutine run_test_cases(spec, results)
        implicit none
        type(parameterization_spec_type), intent(in) :: spec
        type(test_results_type), intent(inout) :: results

        logical :: lhs(2), rhs(2), is_equal

        character(:), allocatable :: case_name
        integer(int32) :: case
        logical :: actual

        do case = 1, results%get_number_of_test_cases()
            call setup(spec, case, case_name, lhs, rhs, is_equal)

            !v---
            actual = (lhs == rhs)

            call results%check_test(case, (actual .eqv. is_equal), &
                                    case_name//new_line(" ")// &
                                    "    expected : "//to_string(is_equal)//new_line(" ")// &
                                    "    actual   : "//to_string(actual))
            !^---

            call teardown(case_name, lhs, rhs, is_equal)
        end do
    end subroutine run_test_cases

    !>setup a test case
    subroutine setup(spec, case, case_name, lhs, rhs, is_equal)
        implicit none
        type(parameterization_spec_type), intent(in) :: spec
        integer(int32), intent(in) :: case
        character(:), allocatable, intent(out) :: case_name
        logical, intent(out) :: lhs(2), rhs(2), is_equal

        type(test_parameter_type) :: param

        namelist /arguments/ lhs, rhs
        namelist /expected/ is_equal

        param = spec%get_test_parameter_in(case)
        read (unit=param%expected_namelist, nml=expected)
        read (unit=param%arguments_namelist, nml=arguments)

        case_name = "eqv_logical_logical() should return "//param%expected()// &
                    " when compare "//param%arguments()
    end subroutine setup

    !>teardown a test case
    subroutine teardown(case_name, lhs, rhs, is_equal)
        implicit none
        character(:), allocatable, intent(inout) :: case_name
        logical, intent(inout) :: lhs(2), rhs(2), is_equal

        if (allocated(case_name)) deallocate (case_name)
        lhs(:) = .false.
        rhs(:) = .false.
        is_equal = .false.
    end subroutine teardown
end module refactor_test_argumentsPresentce_unitTests_eqvLL_runCases_l2l2
