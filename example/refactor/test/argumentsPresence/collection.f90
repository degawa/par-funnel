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
                        new_unittest("eqv_logical_logical(), it should return true when 2 arrays have the same values.", &
                                     eqvLL_should_return_true_when_2_arrays_have_same_values) &
                        , new_unittest("eqv_logical_logical(), it should return false when 2 arrays have diffelent values.", &
                                       eqvLL_should_return_false_when_2_arrays_have_diffelent_values) &
                        , new_unittest("eqv_logical_logical(), it should return false when 2 arrays have diffelent shapes.", &
                                       eqvLL_should_return_false_when_2_arrays_have_different_shapes) &
                        ]

        test_suite = [test_suite_ll]
    end subroutine collect_argumentsPresence
end module refactor_test_argumentsPresence_collection
