program example_typeExtension
    use :: par_funnel
    use :: doublifyParameterizationSpec
    use :: doublifyTestParameter
    use :: function_doublify
    implicit none

    type(doublify_test_parameter_type), allocatable :: params(:)
    type(doublify_parameterization_spec_type) :: spec
    type(test_results_type) :: results

    params = [ &
             new_doublify_test_parameter(arguments="input=1", expected="output=2") &
             , new_doublify_test_parameter(arguments="input=2", expected="output=4") &
             ]
    spec = new_doublify_parameterization_spec(params)

    results = spec%run_test_cases()

    if (.not. results%all_cases_successful()) &
        print *, results%get_summary_message()
end program example_typeExtension
