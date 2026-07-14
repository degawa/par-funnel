module test_argumentsPresence_unitTests_asPattern
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: type_argumentsPresence
    implicit none
    private
    public :: asPattern_should_return_string_of_presence_statuses

contains
    subroutine asPattern_should_return_string_of_presence_statuses(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: arg_pres

        arg_pres = arguments_presence([.true., .false.])
        call check(error, arg_pres%as_pattern() == "PA", &
                   '[T F] should be "PA", but got '//arg_pres%as_pattern())
        if (occurred(error)) return

        arg_pres = arguments_presence([.false., .true., .true.])
        call check(error, arg_pres%as_pattern() == "APP", &
                   '[F T T] should be "APP", but got '//arg_pres%as_pattern())
        if (occurred(error)) return
    end subroutine asPattern_should_return_string_of_presence_statuses
end module test_argumentsPresence_unitTests_asPattern
