module test_nmlUtil_collection_replace
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_nmlUtil_unitTests_replace_newLineMark
    use :: test_nmlUtil_unitTests_replace_newLineChar
    implicit none
    private
    public :: collect_nmlUtil_replace

contains
    subroutine collect_nmlUtil_replace(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("replace_new_line_mark(), "// &
                                  "it should return string that replaces new-line mark with new-line character.", &
                                  replNLMark_should_return_string_that_replaces_mark_with_char) &
                     , new_unittest("replace_new_line_char(), "// &
                                    "it should return string that replaces new-line character with new-line mark.", &
                                    replNLChar_should_return_string_that_replaces_char_with_mark) &
                     ]
    end subroutine collect_nmlUtil_replace
end module test_nmlUtil_collection_replace
