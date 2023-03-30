module test_nmlUtil_collection_format
    use :: testdrive, only:new_unittest, unittest_type
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
                     new_unittest("drop_namelist_keyword(), it should return string without namelist keywords.", &
                                  dropNmlKeys_should_return_string_without_namelist_keywords) &
                     ]
    end subroutine collect_nmlUtil_format
end module test_nmlUtil_collection_format
