program test_testResultsParamSpecClass
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    use :: test_testResultsParamSpecClass_collection
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("par-funnel/testResults_with_extended_parameterization_spec_class", &
                                collect_testResultsParamSpecClass) &
                  ]
    call run_test(test_suites)
end program test_testResultsParamSpecClass
