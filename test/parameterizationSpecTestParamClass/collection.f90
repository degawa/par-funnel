module test_parameterizationSpecTestParamClass_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_parameterizationSpecTestParamClass_unitTests_get
    implicit none
    private
    public :: collect_parameterizationSpecTestParamClass

contains
    subroutine collect_parameterizationSpecTestParamClass(test_suite)
        use, intrinsic :: iso_fortran_env
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("get_number_of_test_cases(), "// &
                                  "it should return 1 when the instance managed 1 test parameter.", &
                                  getNumTestCases_should_return_1_when_manage_1_test_parameter) &
                     , new_unittest("get_number_of_test_cases(), "// &
                                    "it should return 2 when the instance managed 2 test parameter.", &
                                    getNumTestCases_should_return_2_when_manage_2_test_parameter) &
                     , new_unittest("get_number_of_optional_arguments(), "// &
                                    "it should return 1 when the instance managed 1 optional argument.", &
                                    getNumOptArgs_should_return_1_when_manage_1_optional_argument) &
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
    end subroutine collect_parameterizationSpecTestParamClass
end module test_parameterizationSpecTestParamClass_collection
