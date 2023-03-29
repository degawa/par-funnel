program test_nmlUtil
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    use :: test_nmlUtil_collection_format
    use :: test_nmlUtil_collection_replace
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("nml/util/format", collect_nmlUtil_format) &
                  , new_testsuite("nml/util/replace", collect_nmlUtil_replace) &
                  ]
    call run_test(test_suites)
end program test_nmlUtil
