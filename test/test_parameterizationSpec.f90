program test_parameterizationSpec
    use :: test_parameterizationSpec_collection
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("par-funnel/parameterizationSpec", collect_parameterizationSpec) &
                  ]
    call run_test(test_suites)
end program test_parameterizationSpec
