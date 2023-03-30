module test_testParameter_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_testParameter_unitTests_construct
    use :: test_testParameter_unitTests_arguments
    use :: test_testParameter_unitTests_expected
    use :: test_testParameter_unitTests_presented
    use :: test_testParameter_unitTests_newTestParameter
    implicit none
    private
    public :: collect_testParameter

contains
    subroutine collect_testParameter(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("construct(), it should configure namelists for arguments.", &
                                  construct_should_configure_namelists_for_arguments) &
                     , new_unittest("construct(), it should configure namelists for expected results.", &
                                    construct_should_configure_namelists_for_expected_results) &
                     , new_unittest("arguments(), it should return string without namelist keywords.", &
                                    arguments_should_return_string_without_namelist_keywords) &
                     , new_unittest("expected(), it should return string without namelist keywords.", &
                                    expected_should_return_string_without_namelist_keywords) &
                     , new_unittest("presented(), it should return true when key is present in argument namelist.", &
                                    presented_should_return_T_when_key_is_present_in_namelist) &
                     , new_unittest("presented(), it should return false when key is not present in argument namelist.", &
                                    presented_should_return_F_when_key_is_not_present_in_namelist) &
                     , new_unittest("new_test_parameter(), "// &
                                    "it should return a 'test_parameter_type' instance.", &
                                    newTestParam_should_return_test_parameter_type_instance) &
                     , new_unittest("new_test_parameter(), "// &
                                    "it should return a 'test_parameter_type' instance having configured namelists.", &
                                    newTestParam_should_return_instance_that_having_namelists) &
                     ]
    end subroutine collect_testParameter
end module test_testParameter_collection
