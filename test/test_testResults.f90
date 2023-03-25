program test_testResults
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    use :: test_testResults_collection
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("par-funnel/testResults", collect_testResults) &
                  ]
    call run_test(test_suites)
end program test_testResults
