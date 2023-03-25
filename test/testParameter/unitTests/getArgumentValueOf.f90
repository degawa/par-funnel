module type_string_type_getArgumentValueOf
    implicit none
    private

    type, public :: string_type
        character(:), allocatable :: val
    end type string_type
end module type_string_type_getArgumentValueOf

module test_testParameter_unitTests_getArgumentValueOf
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: strings_enclose
    use :: type_string_type_getArgumentValueOf
    use :: type_testParameter
    implicit none
    private
    public :: getArgValOf_should_not_return_value_when_key_is_not_present
    public :: getArgValOf_should_return_value_described_in_namelist

contains
    subroutine getArgValOf_should_not_return_value_when_key_is_not_present(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        type(string_type), allocatable :: key(:), expected(:)
        character(:), allocatable :: key_val, exp_val

        call setup(param, key, expected)

        block
            integer(int32) :: case
            do case = 1, size(key)
                key_val = key(case)%val
                exp_val = expected(case)%val
                call check(error, len(param%get_argument_value_of(key_val)) == len(exp_val), &
                           "expected length "//to_string(len(exp_val)) &
                           //", but got "//to_string(len(param%get_argument_value_of(key_val))))
                if (occurred(error)) return

                call check(error, param%get_argument_value_of(key_val) == exp_val, &
                           "expected "//exp_val//", "// &
                           "but got "//param%get_argument_value_of(key_val))
            end do
        end block

        call teardown()
    contains
        !
        subroutine setup(param, key, expected)
            type(test_parameter_type), intent(inout) :: param
            type(string_type), allocatable, intent(inout) :: key(:)
            type(string_type), allocatable, intent(inout) :: expected(:)

            call param%construct("input=5", "")
            key = [string_type("value")]
            expected = [string_type("")]
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine getArgValOf_should_not_return_value_when_key_is_not_present

    subroutine getArgValOf_should_return_value_described_in_namelist(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        integer(int32), allocatable :: expected(:)
        integer(int32) :: input1, value, input2
        type(string_type), allocatable :: key(:)
        character(:), allocatable :: key_val
        integer(int32) :: exp_val

        namelist /arguments/ input1, value, input2

        call setup(param, key, expected)

        block
            integer(int32) :: case
            do case = 1, size(key)
                key_val = key(case)%val
                exp_val = expected(case)
                call check(error, len(param%get_argument_value_of(key_val)) == len(to_string(exp_val)), &
                           "expected length "//to_string(len(to_string(exp_val))) &
                           //", but got "//to_string(len(param%get_argument_value_of(key_val))))
                if (occurred(error)) return

                call check(error, param%get_argument_value_of(key_val) == to_string(exp_val), &
                           "expected "//to_string(exp_val)//", "// &
                           "but got "//param%get_argument_value_of(key_val))
            end do
        end block

        call teardown()
    contains
        !
        subroutine setup(param, key, expected)
            type(test_parameter_type), intent(inout) :: param
            type(string_type), allocatable, intent(inout) :: key(:)
            integer(int32), allocatable, intent(inout) :: expected(:)

            call param%construct("input1=3 value=5 input2=10", "")
            read (unit=param%arguments_namelist, nml=arguments)

            key = [string_type("input1"), string_type("value"), string_type("input2")]
            expected = [input1, value, input2]
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine getArgValOf_should_return_value_described_in_namelist
end module test_testParameter_unitTests_getArgumentValueOf
