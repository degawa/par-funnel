module test_FEATURE_unitTests_FUNCTION
    use, intrinsic :: iso_fortran_env
    use :: par_funnel
    use :: testdrive, only:error_type, check
    implicit none
    private
    public :: FUNCTION_parameterized_test

contains
    subroutine FUNCTION_parameterized_test(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        call runner(error, case_spec(), run_test_cases)
    contains
        function case_spec() result(spec)
            implicit none
            type(parameterization_spec_type) :: spec

            spec = new_parameterization_spec( &
                   ! add test parameter
                   )
        end function case_spec
        subroutine run_test_cases(spec, results)
            type(parameterization_spec_type), intent(in) :: spec
            type(test_results_type), intent(inout) :: results

            block
                character(:), allocatable :: case_name
                integer(int32) :: case

                do case = 1, results%get_number_of_test_cases()
                    call setup_case()

                    write (output_unit, '(12X, "- ",A)') case_name

                    !v---
                    ! execute function here
                    !^---

                    call results%check_test(case, cond, msg)

                    call teardown_case()
                end do
            end block
        end subroutine run_test_cases
    end subroutine FUNCTION_parameterized_test

    subroutine setup_case()
        implicit none

    end subroutine setup_case

    subroutine teardown_case()
        implicit none

    end subroutine teardown_case
end module test_FEATURE_unitTests_FUNCTION
