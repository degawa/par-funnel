module test_parameterizationSpec_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_ParameterizationSpec_unitTests_newParameterizationSpec
    use :: test_parameterizationSpec_unitTests_get
    implicit none
    private
    public :: collect_parameterizationSpec

contains
    subroutine collect_parameterizationSpec(test_suite)
        use, intrinsic :: iso_fortran_env
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("new_parameterization_spec(), "// &
                                  "it should return a 'parameterization_spec_type' instance.", &
                                  newParaSpec_should_return_parameterization_spec_type_instance) &
                     , new_unittest("new_parameterization_spec(), "// &
                                    "it should replace new-line in arguments and expected namelists with new-line mark"// &
                                    " if `replace_new_line=.true.`.", &
                                    newParaSpec_should_replace_new_line_if_replace_new_line_T) &
                     , new_unittest("new_parameterization_spec(), "// &
                                    "it should not replace new-line in arguments and expected namelists with new-line mark"// &
                                    " if `replace_new_line=.false.`.", &
                                    newParaSpec_should_not_replace_new_line_if_replace_new_line_F) &
                     , new_unittest("new_parameterization_spec(), "// &
                                    "it should not replace new-line in arguments and expected namelists with new-line mark"// &
                                    " if `replace_new_line` not present.", &
                                    newParaSpec_should_not_replace_new_line_if_arg_not_present) &
                     , new_unittest("get_number_of_test_cases(), "// &
                                    "it should return 1 when the instance managed 1 test parameter.", &
                                    getNumTestCases_should_return_1_when_manage_1_test_parameter) &
                     , new_unittest("get_number_of_test_cases(), "// &
                                    "it should return 0 when the instance is not initialized.", &
                                    getNumTestCases_should_return_0_when_spec_is_not_initialized) &
                     , new_unittest("get_number_of_optional_arguments(), "// &
                                    "it should return 1 when the instance managed 1 optional argument.", &
                                    getNumOptArgs_should_return_1_when_manage_1_optional_argument) &
                     , new_unittest("get_number_of_optional_arguments(), "// &
                                    "it should return 0 when the instance is not initialized.", &
                                    getNumOptArgs_should_return_0_when_spec_is_not_initialized) &
                     , new_unittest("get_test_parameter_in(), "// &
                                    "it should return the test parameter instance in the specified case.", &
                                    getTestParam_should_return_test_parameter_in_specified_case) &
                     , new_unittest("get_test_parameter_in(), "// &
                                    "it should return an uninitialized instance if the case is out-of-range.", &
                                    getTestParam_should_return_uninit_inst_if_case_is_OOR) &
                     , new_unittest("get_optional_arguments_presence_in(), "// &
                                    "it should return arguments presence status in the argument namelist in the specific case.", &
                                    getOptArgPres_should_return_test_parameter_in_specified_case) &
                     , new_unittest("get_optional_arguments_presence_in(), "// &
                                    "it should return an uninitialized instance if the case is out-of-range.", &
                                    getOptArgPres_should_return_uninit_inst_if_case_is_OOR) &
                     ]
    end subroutine collect_parameterizationSpec
end module test_parameterizationSpec_collection
