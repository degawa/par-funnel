module test_nmlUtil_collection_format
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_nmlUtil_unitTests_format_getValueOf
    use :: test_nmlUtil_unitTests_format_dropNamelistKeywords
    implicit none
    private
    public :: collect_nmlUtil_format

contains
    subroutine collect_nmlUtil_format(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("get_value_of(), it should return zero-length string when key is not present in a namelist.", &
                                  getValueOf_should_return_0_len_str_when_key_is_not_present) &
                     , new_unittest("get_value_of(), it should return value in string when key is present in a namelist.", &
                                    getValueOf_should_return_value_described_in_namelist) &
                     , new_unittest("drop_namelist_keyword(), it should return string without namelist keywords.", &
                                    dropNmlKeys_should_return_string_without_namelist_keywords) &
                     ]
    end subroutine collect_nmlUtil_format
end module test_nmlUtil_collection_format
