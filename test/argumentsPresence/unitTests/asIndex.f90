module test_argumentsPresence_unitTests_asIndex
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: type_argumentsPresence
    implicit none
    private
    public :: asIndex_should_return_bit_pattern_of_presence_statuses

contains
    subroutine asIndex_should_return_bit_pattern_of_presence_statuses(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: arg_pres

        arg_pres = arguments_presence([.false., .false.])
        call check(error, arg_pres%as_index() == 0, "[F F] should be 0")
        if (occurred(error)) return

        arg_pres = arguments_presence([.true., .false.])
        call check(error, arg_pres%as_index() == 1, "[T F] should be 1")
        if (occurred(error)) return

        arg_pres = arguments_presence([.false., .true.])
        call check(error, arg_pres%as_index() == 2, "[F T] should be 2")
        if (occurred(error)) return

        arg_pres = arguments_presence([.true., .true., .true.])
        call check(error, arg_pres%as_index() == 7, "[T T T] should be 7")
        if (occurred(error)) return
    end subroutine asIndex_should_return_bit_pattern_of_presence_statuses
end module test_argumentsPresence_unitTests_asIndex
