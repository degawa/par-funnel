module test_argumentsPresence_unitTests_get
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: type_argumentsPresence
    implicit none
    private
    public :: getNumPresStat_should_return_1_when_manage_1_presence_status
    public :: getNumPresStat_should_return_3_when_manage_3_presence_status
    public :: getNumPresStat_should_return_0_when_instance_is_not_initialized

contains
    subroutine getNumPresStat_should_return_1_when_manage_1_presence_status(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        integer(int32) :: num_presence_stat

        pres = arguments_presence([.true.])

        num_presence_stat = pres%get_number_of_presence_statuses()

        call check(error, num_presence_stat == 1, &
                   "expected "//to_string(1)// &
                   ", but got "//to_string(num_presence_stat))
    end subroutine getNumPresStat_should_return_1_when_manage_1_presence_status

    subroutine getNumPresStat_should_return_3_when_manage_3_presence_status(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        integer(int32) :: num_presence_stat

        pres = arguments_presence([.true., .false., .true.])

        num_presence_stat = pres%get_number_of_presence_statuses()

        call check(error, num_presence_stat == 3, &
                   "expected "//to_string(3)// &
                   ", but got "//to_string(num_presence_stat))
    end subroutine getNumPresStat_should_return_3_when_manage_3_presence_status

    subroutine getNumPresStat_should_return_0_when_instance_is_not_initialized(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        integer(int32) :: num_presence_stat

        num_presence_stat = pres%get_number_of_presence_statuses()

        call check(error, num_presence_stat == 0, &
                   "expected "//to_string(0)// &
                   ", but got "//to_string(num_presence_stat))
    end subroutine getNumPresStat_should_return_0_when_instance_is_not_initialized
end module test_argumentsPresence_unitTests_get
