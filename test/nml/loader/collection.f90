module test_nml_loader_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_nml_loader_unitTests_loadTestParameters
    implicit none
    private
    public :: collect_nml_loader

contains
    subroutine collect_nml_loader(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("load_test_parameters(), it should read test cases from a namelist file.", &
                                  loadTestParameters_should_read_test_cases_from_namelist_file) &
                     ]
    end subroutine collect_nml_loader
end module test_nml_loader_collection
