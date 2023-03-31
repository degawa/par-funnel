module test_argumentsPresence_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_argumentsPresence_unitTests_eqvLogicalLogical
    use :: test_argumentsPresence_unitTests_eqvLogical
    use :: test_argumentsPresence_unitTests_eqvType
    use :: test_argumentsPresence_unitTests_eqvLogicalType
    use :: test_argumentsPresence_unitTests_newArgumentsPresence
    use :: test_argumentsPresence_unitTests_get
    use :: test_argumentsPresence_unitTests_asString
    implicit none
    private
    public :: collect_argumentsPresence

contains
    subroutine collect_argumentsPresence(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        type(unittest_type), allocatable :: test_suite_ll(:)
        type(unittest_type), allocatable :: test_suite_tl(:)
        type(unittest_type), allocatable :: test_suite_tt(:)
        type(unittest_type), allocatable :: test_suite_lt(:)
        type(unittest_type), allocatable :: test_suite_get(:)
        type(unittest_type), allocatable :: test_suite_asStr(:)

        test_suite_ll = [ &
                        new_unittest("eqv_logical_logical(), it should return true "// &
                                     "when compare [.true., .true.] with [.true., .true.].", &
                                     eqvLL_should_return_T_when_compare_TT_TT) &
                        , new_unittest("eqv_logical_logical(), it should return false "// &
                                       "when compare [.true., .true.] with other than [.true., .true.].", &
                                       eqvLL_should_return_F_when_compare_TT_w_other_than_TT) &
                        , new_unittest("eqv_logical_logical(), it should return true "// &
                                       "when compare [.false., .true.] with [.false., .true.].", &
                                       eqvLL_should_return_T_when_compare_FT_FT) &
                        , new_unittest("eqv_logical_logical(), it should return false "// &
                                       "when compare [.false., .true.] with other than [.false., .true.].", &
                                       eqvLL_should_return_F_when_compare_FT_w_other_than_FT) &
                        , new_unittest("eqv_logical_logical(), it should return true "// &
                                       "when compare [.true., .false.] with [.true., .false.].", &
                                       eqvLL_should_return_T_when_compare_TF_TF) &
                        , new_unittest("eqv_logical_logical(), it should return false "// &
                                       "when compare [.true., .false.] with other than [.true., .false.].", &
                                       eqvLL_should_return_F_when_compare_TF_w_other_than_TF) &
                        , new_unittest("eqv_logical_logical(), it should return true "// &
                                       "when compare [.false., .false.] with [.false., .false.].", &
                                       eqvLL_should_return_T_when_compare_FF_FF) &
                        , new_unittest("eqv_logical_logical(), it should return false "// &
                                       "when compare [.false., .false.] with other than [.false., .false.].", &
                                       eqvLL_should_return_F_when_compare_FF_w_other_than_FF) &
                        , new_unittest("eqv_logical_logical(), it should return true "// &
                                       "when compare multidimensional arrays of the same value", &
                                       eqvLL_should_return_T_when_compare_ndarray_of_the_same_value) &
                        , new_unittest("eqv_logical_logical(), it should return true "// &
                                       "when compare zero-size arrays", &
                                       eqvLL_should_return_T_when_compare_zero_size_arrays) &
                        ]

        test_suite_tl = [ &
                        new_unittest("eqv_logical(), it should return true "// &
                                     "when compare [.true., .true.] with [.true., .true.].", &
                                     eqvTL_should_return_T_when_compare_TT_TT) &
                        , new_unittest("eqv_logical(), it should return false "// &
                                       "when compare [.true., .true.] with other than [.true., .true.].", &
                                       eqvTL_should_return_F_when_compare_TT_w_other_than_TT) &
                        , new_unittest("eqv_logical(), it should return true "// &
                                       "when compare [.false., .true.] with [.false., .true.].", &
                                       eqvTL_should_return_T_when_compare_FT_FT) &
                        , new_unittest("eqv_logical(), it should return false "// &
                                       "when compare [.false., .true.] with other than [.false., .true.].", &
                                       eqvTL_should_return_F_when_compare_FT_w_other_than_FT) &
                        , new_unittest("eqv_logical(), it should return true "// &
                                       "when compare [.true., .false.] with [.true., .false.].", &
                                       eqvTL_should_return_T_when_compare_TF_TF) &
                        , new_unittest("eqv_logical(), it should return false "// &
                                       "when compare [.true., .false.] with other than [.true., .false.].", &
                                       eqvTL_should_return_F_when_compare_TF_w_other_than_TF) &
                        , new_unittest("eqv_logical(), it should return true "// &
                                       "when compare [.false., .false.] with [.false., .false.].", &
                                       eqvTL_should_return_T_when_compare_FF_FF) &
                        , new_unittest("eqv_logical(), it should return false "// &
                                       "when compare [.false., .false.] with other than [.false., .false.].", &
                                       eqvTL_should_return_F_when_compare_FF_w_other_than_FF) &
                        , new_unittest("eqv_logical(), it should return true "// &
                                       "when compare with multidimensional arrays of the same value", &
                                       eqvTL_should_return_T_when_compare_w_ndarray_of_the_same_value) &
                        , new_unittest("eqv_logical(), it should return true "// &
                                       "when compare with zero-size array", &
                                       eqvTL_should_return_T_when_compare_w_zero_size_array) &
                        ]

        test_suite_lt = [ &
                        new_unittest("eqv_logical_type(), it should return true "// &
                                     "when compare [.true., .true.] with [.true., .true.].", &
                                     eqvLT_should_return_T_when_compare_TT_TT) &
                        , new_unittest("eqv_logical_type(), it should return false "// &
                                       "when compare [.true., .true.] with other than [.true., .true.].", &
                                       eqvLT_should_return_F_when_compare_TT_w_other_than_TT) &
                        , new_unittest("eqv_logical_type(), it should return true "// &
                                       "when compare [.false., .true.] with [.false., .true.].", &
                                       eqvLT_should_return_T_when_compare_FT_FT) &
                        , new_unittest("eqv_logical_type(), it should return false "// &
                                       "when compare [.false., .true.] with other than [.false., .true.].", &
                                       eqvLT_should_return_F_when_compare_FT_w_other_than_FT) &
                        , new_unittest("eqv_logical_type(), it should return true "// &
                                       "when compare [.true., .false.] with [.true., .false.].", &
                                       eqvLT_should_return_T_when_compare_TF_TF) &
                        , new_unittest("eqv_logical_type(), it should return false "// &
                                       "when compare [.true., .false.] with other than [.true., .false.].", &
                                       eqvLT_should_return_F_when_compare_TF_w_other_than_TF) &
                        , new_unittest("eqv_logical_type(), it should return true "// &
                                       "when compare [.false., .false.] with [.false., .false.].", &
                                       eqvLT_should_return_T_when_compare_FF_FF) &
                        , new_unittest("eqv_logical_type(), it should return false "// &
                                       "when compare [.false., .false.] with other than [.false., .false.].", &
                                       eqvLT_should_return_F_when_compare_FF_w_other_than_FF) &
                        , new_unittest("eqv_logical_type(), it should return true "// &
                                       "when compare with multidimensional arrays of the same value", &
                                       eqvLT_should_return_T_when_compare_w_ndarray_of_the_same_value) &
                        , new_unittest("eqv_logical_type(), it should return true "// &
                                       "when compare with zero-size array", &
                                       eqvLT_should_return_T_when_compare_w_zero_size_array) &
                        ]

        test_suite_tt = [ &
                        new_unittest("eqv_type(), it should return true "// &
                                     "when compare [.true., .true.] with [.true., .true.].", &
                                     eqvTT_should_return_T_when_compare_TT_TT) &
                        , new_unittest("eqv_type(), it should return false "// &
                                       "when compare [.true., .true.] with other than [.true., .true.].", &
                                       eqvTT_should_return_F_when_compare_TT_w_other_than_TT) &
                        , new_unittest("eqv_type(), it should return true "// &
                                       "when compare [.false., .true.] with [.false., .true.].", &
                                       eqvTT_should_return_T_when_compare_FT_FT) &
                        , new_unittest("eqv_type(), it should return false "// &
                                       "when compare [.false., .true.] with other than [.false., .true.].", &
                                       eqvTT_should_return_F_when_compare_FT_w_other_than_FT) &
                        , new_unittest("eqv_type(), it should return true "// &
                                       "when compare [.true., .false.] with [.true., .false.].", &
                                       eqvTT_should_return_T_when_compare_TF_TF) &
                        , new_unittest("eqv_type(), it should return false "// &
                                       "when compare [.true., .false.] with other than [.true., .false.].", &
                                       eqvTT_should_return_F_when_compare_TF_w_other_than_TF) &
                        , new_unittest("eqv_type(), it should return true "// &
                                       "when compare [.false., .false.] with [.false., .false.].", &
                                       eqvTT_should_return_T_when_compare_FF_FF) &
                        , new_unittest("eqv_type(), it should return false "// &
                                       "when compare [.false., .false.] with other than [.false., .false.].", &
                                       eqvTT_should_return_F_when_compare_FF_w_other_than_FF) &
                        , new_unittest("eqv_type(), it should return true "// &
                                       "when compare with `argumentPresence_type` instance having the same value", &
                                       eqvTL_should_return_T_when_compare_w_ndarray_of_the_same_value) &
                        , new_unittest("eqv_type(), it should return true "// &
                                       "when compare `argumentPresence_type` instances having zero-size statuses", &
                                       eqvTT_should_return_T_when_compare_zero_size_statuses) &
                        ]

        test_suite_get = [ &
                         new_unittest("get_number_of_presence_statuses(), "// &
                                      "it should return 1 when the instance managed 1 argument presence status.", &
                                      getNumPresStat_should_return_1_when_manage_1_presence_status) &
                         , new_unittest("get_number_of_presence_statuses(), "// &
                                        "it should return 3 when the instance managed 3 argument presence status.", &
                                        getNumPresStat_should_return_3_when_manage_3_presence_status) &
                         , new_unittest("get_number_of_presence_statuses(), "// &
                                        "it should return 0 when the instance is not initialized.", &
                                        getNumPresStat_should_return_0_when_instance_is_not_initialized) &
                         ]

        test_suite_asStr = [ &
                           new_unittest("as_string(), "// &
                                        "it should return [ T F T ] when the instance has [.true., .false., .true.].", &
                                        asString_should_return_TFT_when_arg_pres_has_TFT) &
                           , new_unittest("as_string(), "// &
                                          "it should return [ ] when the instance is not intialized.", &
                                          asString_should_return_blank_when_arg_pres_is_not_initialized) &
                           , new_unittest("to_string(), "// &
                                          "it should return [ F T F ] when the instance has [.false., .true., .false.].", &
                                          toString_should_return_FTF_when_arg_pres_has_FTF) &
                           , new_unittest("to_string(), "// &
                                          "it should return [ ] when the instance is not intialized.", &
                                          toString_should_return_blank_when_arg_pres_is_not_initialized) &
                           ]

        test_suite = [test_suite_ll, test_suite_tl, test_suite_lt, test_suite_tt &
                      , new_unittest("argumets_presence(), "// &
                                     "it should return a 'arguments_presence_type' instance.", &
                                     argsPres_should_return_arguments_presence_type_instance) &
                      , new_unittest("argumets_presence(), "// &
                                     "it should return a 'arguments_presence_type' instance having presence statuses.", &
                                     argsPres_should_return_instance_that_having_presence_status) &
                      , test_suite_get, test_suite_asStr &
                      ]
    end subroutine collect_argumentsPresence
end module test_argumentsPresence_collection
