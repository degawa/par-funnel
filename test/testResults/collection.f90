module test_testResults_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_testResults_unitTests_construct
    use :: test_testResults_unitTests_checkTest
    use :: test_testResults_unitTests_getStatus
    use :: test_testResults_unitTests_getMessage
    implicit none
    private
    public :: collect_testResults

contains
    subroutine collect_testResults(test_suite)
        use, intrinsic :: iso_fortran_env
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("construct_params(), "// &
                                  "it should allocate the component test equal to the number of test parameters", &
                                  constructParams_should_allocate_test_equal_to_num_params) &
                     , new_unittest("construct_spec(), "// &
                                    "it should allocate the component test equal to the number of test parameters", &
                                    constructSpec_should_allocate_test_equal_to_num_params) &
                     , new_unittest("get_success_status_of(), "// &
                                    "it should return true if test case is successful", &
                                    getSuccessStatusOf_should_return_T_if_test_case_is_successful) &
                     , new_unittest("get_success_statuses(), "// &
                                    "it should return array having true where the test case is successful", &
                                    getSuccessStatuses_should_return_T_if_test_case_is_successful) &
                     , new_unittest("get_failure_statuses(), "// &
                                    "it should return array having true where the test case falied", &
                                    getFailureStatuses_should_return_T_if_test_case_failed) &
                     , new_unittest("get_number_of_failed_cases(), "// &
                                    "it should return number of failed test cases", &
                                    getNumOfFailedStat_should_return_num_of_failed_test_case) &
                     , new_unittest("all_cases_successful(), "// &
                                    "it should return true if all the test cases are successful", &
                                    allCasesSuccessful_should_return_T_if_all_cases_successful) &
                     , new_unittest("all_cases_successful(), "// &
                                    "it should return false if at least one test case failed", &
                                    allCasesSuccessful_should_return_F_if_not_all_cases_successful) &
                     , new_unittest("get_failure_message(), "// &
                                    "it should return failure message when the test case failed", &
                                    getFailureMsg_should_return_failure_msg_when_test_case_failed) &
                     , new_unittest("get_failure_message(), "// &
                                    "it should return message indicating that the test case is successful", &
                                    getFailureMsg_should_return_msg_that_the_test_case_successful) &
                     , new_unittest("get_failure_message(), "// &
                                    "it should return message indicating that the test case is not checked", &
                                    getFailureMsg_should_return_msg_that_the_test_case_not_checked) &
                     , new_unittest("get_summary_message(), "// &
                                    "it should return concatenated failure message of the failed test cases", &
                                    getSummaryMsg_should_return_failure_msgs_of_failed_cases) &
                     , new_unittest("append_failure_messages_to(), "// &
                                    "it should append all failure messages to the end of the input string.", &
                                    appendFailureMsgs_should_append_failure_msg_to_the_end_of_arg) &
                     ]
    end subroutine collect_testResults
end module test_testResults_collection
