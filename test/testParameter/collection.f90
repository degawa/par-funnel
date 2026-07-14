module test_testParameter_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_testParameter_unitTests_construct
    use :: test_testParameter_unitTests_arguments
    use :: test_testParameter_unitTests_expected
    use :: test_testParameter_unitTests_presented
    use :: test_testParameter_unitTests_newTestParameter
    use :: test_testParameter_unitTests_get
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
                     , new_unittest("get(), it should read scalar values of intrinsic types.", &
                                    get_should_read_scalar_values_of_intrinsic_types) &
                     , new_unittest("get(), it should read values from the expected namelist.", &
                                    get_should_read_values_from_expected_namelist) &
                     , new_unittest("get(), it should read rank-1 arrays.", &
                                    get_should_read_rank1_arrays) &
                     , new_unittest("get(), it should set found to false and keep the default value "// &
                                    "when the name is not found.", &
                                    get_should_set_found_false_and_keep_default_when_not_found) &
                     , new_unittest("get(), it should read values written with spaces around '='.", &
                                    get_should_read_values_written_with_spaces_around_equals) &
                     , new_unittest("get(), it should read quoted and unquoted strings.", &
                                    get_should_read_quoted_and_unquoted_strings) &
                     ]
    end subroutine collect_testParameter
end module test_testParameter_collection
