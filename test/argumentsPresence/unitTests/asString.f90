module test_argumentsPresence_unitTests_asString
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: type_argumentsPresence
    implicit none
    private
    public :: asString_should_return_TFT_when_arg_pres_has_TFT
    public :: asString_should_return_blank_when_arg_pres_is_not_initialized
    public :: toString_should_return_FTF_when_arg_pres_has_FTF
    public :: toString_should_return_blank_when_arg_pres_is_not_initialized

contains
    subroutine asString_should_return_TFT_when_arg_pres_has_TFT(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        character(:), allocatable :: str

        pres = arguments_presence([.true., .false., .true.])

        str = pres%as_string()

        call check(error, str == "[ T F T ]", &
                   "expected [ T F T ], but got "//str)
    end subroutine asString_should_return_TFT_when_arg_pres_has_TFT

    subroutine asString_should_return_blank_when_arg_pres_is_not_initialized(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        character(:), allocatable :: str

        str = pres%as_string()

        call check(error, str == "[ ]", &
                   "expected [ ], but got "//str)
    end subroutine asString_should_return_blank_when_arg_pres_is_not_initialized

    subroutine toString_should_return_FTF_when_arg_pres_has_FTF(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        character(:), allocatable :: str

        pres = arguments_presence([.false., .true., .false.])

        str = to_string(pres)

        call check(error, str == "[ F T F ]", &
                   "expected [ F T F ], but got "//str)
    end subroutine toString_should_return_FTF_when_arg_pres_has_FTF

    subroutine toString_should_return_blank_when_arg_pres_is_not_initialized(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        character(:), allocatable :: str

        str = to_string(pres)

        call check(error, str == "[ ]", &
                   "expected [ ], but got "//str)
    end subroutine toString_should_return_blank_when_arg_pres_is_not_initialized
end module test_argumentsPresence_unitTests_asString
