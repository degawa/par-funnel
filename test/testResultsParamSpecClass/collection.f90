module test_testResultsParamSpecClass_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_testResultsParamSpecClass_unitTests_newTestResults
    implicit none
    private
    public :: collect_testResultsParamSpecClass

contains
    subroutine collect_testResultsParamSpecClass(test_suite)
        use, intrinsic :: iso_fortran_env
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("new_test_results_for(parameterization_spec), "// &
                                  "it should return a 'test_results_type' instance.", &
                                  newTestResSpec_should_return_test_results_type_instance) &
                     , new_unittest("new_test_results_for(test_parameters), "// &
                                    "it should return a 'test_results_type' instance with the same num of results as test cases.", &
                                    newTestResParam_should_return_instance_with_same_num_as_cases) &
                     , new_unittest("new_test_results_for(parameterization_spec), "// &
                                    "it should return a 'test_results_type' instance with the same num of results as test cases.", &
                                    newTestResSpec_should_return_instance_with_same_num_as_cases) &
                     ]
    end subroutine collect_testResultsParamSpecClass
end module test_testResultsParamSpecClass_collection
