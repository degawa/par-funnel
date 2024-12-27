program test_parameterizationSpecTestParamClass
    use :: test_parameterizationSpecTestParamClass_collection
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("par-funnel/parameterizationSpec_with_extended_test_parameter_class", &
                                collect_parameterizationSpecTestParamClass) &
                  ]
    call run_test(test_suites)
end program test_parameterizationSpecTestParamClass
