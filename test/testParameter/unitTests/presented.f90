module type_string_type_presented
    implicit none
    private

    type, public :: string_type
        character(:), allocatable :: val
    end type string_type
end module type_string_type_presented

module test_testParameter_unitTests_presented
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: strings_enclose
    use :: type_string_type_presented
    use :: type_testParameter
    implicit none
    private
    public :: presented_should_return_T_when_key_is_present_in_namelist
    public :: presented_should_return_F_when_key_is_not_present_in_namelist

contains
    subroutine presented_should_return_T_when_key_is_present_in_namelist(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        type(string_type), allocatable :: key(:)
        logical, allocatable :: presented(:)

        call setup(param, key, presented)

        block
            integer(int32) :: case
            do case = 1, size(presented)
                call check(error, param%presented(key(case)%val) .eqv. presented(case), &
                           "expected "//to_string(presented(case)) &
                           //", but got "//to_string(param%presented(key(case)%val)))
                if (occurred(error)) return
            end do
        end block

        call teardown()
    contains
        !
        subroutine setup(param, key, presented)
            type(test_parameter_type), intent(inout) :: param
            type(string_type), allocatable, intent(inout) :: key(:)
            logical, allocatable, intent(inout) :: presented(:)

            call param%construct("input1=1 input2=10 input3=4 input4=5", "")
            key = [string_type("input4"), string_type("input1"), string_type("input2"), string_type("input3")]
            presented = [.true., .true., .true., .true.]
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine presented_should_return_T_when_key_is_present_in_namelist

    subroutine presented_should_return_F_when_key_is_not_present_in_namelist(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        type(string_type), allocatable :: key(:)
        logical, allocatable :: not_presented(:)

        call setup(param, key, not_presented)

        block
            integer(int32) :: case
            do case = 1, size(not_presented)
                call check(error, param%presented(key(case)%val) .eqv. not_presented(case), &
                           "expected "//to_string(not_presented(case)) &
                           //", but got "//to_string(param%presented(key(case)%val)))
                if (occurred(error)) return
            end do
        end block

        call teardown()
    contains
        !
        subroutine setup(param, key, not_presented)
            type(test_parameter_type), intent(inout) :: param
            type(string_type), allocatable, intent(inout) :: key(:)
            logical, allocatable, intent(inout) :: not_presented(:)

            call param%construct("input1=1 input2=10 input3=4 input4=5", "")
            key = [string_type("input5"), string_type("input6"), string_type("input7"), string_type("input8")]
            not_presented = [.false., .false., .false., .false.]
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine presented_should_return_F_when_key_is_not_present_in_namelist
end module test_testParameter_unitTests_presented
