module refactor_test_argumentsPresence_collection
    use :: testdrive, only:new_unittest, unittest_type
    use :: refactor_test_argumentsPresence_unitTests_eqvLogicalLogical
    implicit none
    private
    public :: collect_argumentsPresence

contains
    subroutine collect_argumentsPresence(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        type(unittest_type), allocatable :: test_suite_ll(:)

        test_suite_ll = [ &
                        new_unittest("eqv_logical_logical(), parameterized test for comparision of size-2 arrays.", &
                                     eqvLL_parameterized_compare_size_2_arrays) &
                        , new_unittest("eqv_logical_logical(), parameterized test for comparision of arrays with size-2 and 3.", &
                                       eqvLL_parameterized_compare_btw_arrays_w_size_2_and_3) &
                        ]

        test_suite = [test_suite_ll]
    end subroutine collect_argumentsPresence
end module refactor_test_argumentsPresence_collection
