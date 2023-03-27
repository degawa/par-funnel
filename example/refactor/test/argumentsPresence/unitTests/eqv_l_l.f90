module refactor_test_argumentsPresence_unitTests_eqvLogicalLogical
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: par_funnel
    implicit none
    private
    public :: eqvLL_should_return_true_when_2_arrays_have_same_values
    public :: eqvLL_should_return_false_when_2_arrays_have_diffelent_values
    public :: eqvLL_should_return_false_when_2_arrays_have_different_shapes

contains
    subroutine eqvLL_should_return_true_when_2_arrays_have_same_values(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                   new_test_parameter(arguments="lhs(:)=true,true rhs(:)=true,true",     expected="is_equal=true") &
                 , new_test_parameter(arguments="lhs(:)=false,true rhs(:)=false,true",   expected="is_equal=true") &
                 , new_test_parameter(arguments="lhs(:)=true,false rhs(:)=true,false",   expected="is_equal=true") &
                 , new_test_parameter(arguments="lhs(:)=false,false rhs(:)=false,false", expected="is_equal=true") &
                 ] !&

        call run_test_cases_l2l2(params, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
    end subroutine eqvLL_should_return_true_when_2_arrays_have_same_values

    subroutine eqvLL_should_return_false_when_2_arrays_have_diffelent_values(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                 new_test_parameter(arguments="lhs(:)=true,true rhs(:)=false,true",    expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,true rhs(:)=true,false",    expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,true rhs(:)=false,false",   expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,true rhs(:)=true,true",    expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,true rhs(:)=true,false",   expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,true rhs(:)=false,false",  expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,false rhs(:)=true,true",    expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,false rhs(:)=false,true",   expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,false rhs(:)=false,false",  expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,false rhs(:)=true,true",   expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,false rhs(:)=false,true",  expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,false rhs(:)=true,false",  expected="is_equal=false") &
                 ] !&

        call run_test_cases_l2l2(params, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
    end subroutine eqvLL_should_return_false_when_2_arrays_have_diffelent_values

    subroutine run_test_cases_l2l2(params, results)
        implicit none
        type(test_parameter_type), intent(in) :: params(:)
        type(test_results_type), intent(inout) :: results

        logical :: lhs(2), rhs(2), is_equal
        namelist /arguments/ lhs, rhs
        namelist /expected/ is_equal

        character(:), allocatable :: case_name
        logical :: actual
        integer(int32) :: case

        call results%construct(params)

        do case = 1, results%get_number_of_test_cases()
            read (unit=params(case)%expected_namelist, nml=expected)
            read (unit=params(case)%arguments_namelist, nml=arguments)

            case_name = "eqv_logical_logical() should return "//params(case)%expected()// &
                        " when compare "//params(case)%arguments()

            actual = (lhs == rhs)

            call results%check_test(case, (actual .eqv. is_equal), &
                                    case_name//new_line(" ")// &
                                    "    expected : "//to_string(is_equal)//new_line(" ")// &
                                    "    actual   : "//to_string(actual))
        end do
    end subroutine run_test_cases_l2l2

    subroutine eqvLL_should_return_false_when_2_arrays_have_different_shapes(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                   new_test_parameter(arguments="lhs(:)=true,true rhs(:)=true,true,true",      expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,true rhs(:)=false,true,true",     expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,true rhs(:)=true,false,true",     expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,true rhs(:)=false,false,true",    expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,true rhs(:)=true,true,false",     expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,true rhs(:)=false,true,false",    expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,true rhs(:)=true,false,false",    expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,true rhs(:)=false,false,false",   expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,true rhs(:)=true,true,true",     expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,true rhs(:)=false,true,true",    expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,true rhs(:)=true,false,true",    expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,true rhs(:)=false,false,true",   expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,true rhs(:)=true,true,false",    expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,true rhs(:)=false,true,false",   expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,true rhs(:)=true,false,false",   expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,true rhs(:)=false,false,false",  expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,false rhs(:)=true,true,true",     expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,false rhs(:)=false,true,true",    expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,false rhs(:)=true,false,true",    expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,false rhs(:)=false,false,true",   expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,false rhs(:)=true,true,false",    expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,false rhs(:)=false,true,false",   expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,false rhs(:)=true,false,false",   expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=true,false rhs(:)=false,false,false",  expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,false rhs(:)=true,true,true",    expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,false rhs(:)=false,true,true",   expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,false rhs(:)=true,false,true",   expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,false rhs(:)=false,false,true",  expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,false rhs(:)=true,true,false",   expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,false rhs(:)=false,true,false",  expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,false rhs(:)=true,false,false",  expected="is_equal=false") &
                 , new_test_parameter(arguments="lhs(:)=false,false rhs(:)=false,false,false", expected="is_equal=false") &
                 ] !&

        call run_test_cases(params, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())

    contains
        subroutine run_test_cases(params, results)
            implicit none
            type(test_parameter_type), intent(in) :: params(:)
            type(test_results_type), intent(inout) :: results

            logical :: lhs(2), rhs(3), is_equal
            namelist /arguments/ lhs, rhs
            namelist /expected/ is_equal

            character(:), allocatable :: case_name
            logical :: actual
            integer(int32) :: case

            call results%construct(params)

            do case = 1, results%get_number_of_test_cases()
                read (unit=params(case)%expected_namelist, nml=expected)
                read (unit=params(case)%arguments_namelist, nml=arguments)

                case_name = "eqv_logical_logical() should return "//params(case)%expected()// &
                            " when compare "//params(case)%arguments()

                actual = (lhs == rhs)

                call results%check_test(case, (actual .eqv. is_equal), &
                                        case_name//new_line(" ")// &
                                        "    expected : "//to_string(is_equal)//new_line(" ")// &
                                        "    actual   : "//to_string(actual))
            end do
        end subroutine run_test_cases
    end subroutine eqvLL_should_return_false_when_2_arrays_have_different_shapes
end module refactor_test_argumentsPresence_unitTests_eqvLogicalLogical
