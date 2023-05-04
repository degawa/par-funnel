module test_FEATURE_unitTests_common
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: par_funnel
    implicit none
    private
    public :: runner

contains
    subroutine runner(error, spec, run_test_cases)
        implicit none
        interface
            subroutine test_case_runner(Ispec, Iresults)
                import parameterization_spec_type, test_results_type
                import error_type
                type(parameterization_spec_type), intent(in) :: Ispec
                type(test_results_type), intent(inout) :: Iresults
            end subroutine test_case_runner
        end interface

        type(error_type), allocatable, intent(out) :: error
        type(parameterization_spec_type), intent(in) :: spec
        procedure(test_case_runner) :: run_test_cases

        type(test_results_type) :: results

        results = new_test_results_for(spec)
        call run_test_cases(spec, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
    end subroutine runner
end module test_FEATURE_unitTests_common
