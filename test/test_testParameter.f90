program test_testParamter
    use :: test_testParameter_collection
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("par-funnel/testParameter", collect_testParameter) &
                  ]
    call run_test(test_suites)
end program test_testParamter
