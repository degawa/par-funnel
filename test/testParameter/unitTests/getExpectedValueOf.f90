module type_string_type_getExpectedValueOf
    implicit none
    private

    type, public :: string_type
        character(:), allocatable :: val
    end type string_type
end module type_string_type_getExpectedValueOf

module test_testParameter_unitTests_getExpectedValueOf
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: strings_enclose
    use :: type_string_type_getExpectedValueOf
    use :: type_testParameter
    implicit none
    private
    public :: getExpValOf_should_not_return_value_when_key_is_not_present
    public :: getExpValOf_should_return_value_described_in_namelist

contains
    subroutine getExpValOf_should_not_return_value_when_key_is_not_present(error)
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
                call check(error, len(param%get_expected_value_of(key_val)) == len(exp_val), &
                           "expected length "//to_string(len(exp_val)) &
                           //", but got "//to_string(len(param%get_expected_value_of(key_val))))
                if (occurred(error)) return

                call check(error, param%get_expected_value_of(key_val) == exp_val, &
                           "expected "//exp_val//", "// &
                           "but got "//param%get_expected_value_of(key_val))
            end do
        end block

        call teardown()
    contains
        !
        subroutine setup(param, key, expected)
            type(test_parameter_type), intent(out) :: param
            type(string_type), allocatable, intent(out) :: key(:)
            type(string_type), allocatable, intent(out) :: expected(:)

            call param%construct(arguments="", expected="output=5")
            key = [string_type("value")]
            expected = [string_type("")]
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine getExpValOf_should_not_return_value_when_key_is_not_present

    subroutine getExpValOf_should_return_value_described_in_namelist(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type) :: param
        integer(int32), allocatable :: expected_int(:)
        integer(int32) :: output1, value, output2
        type(string_type), allocatable :: key(:)
        character(:), allocatable :: key_val
        integer(int32) :: exp_val

        namelist /expected/ output1, value, output2

        call setup(param, key, expected_int)

        block
            integer(int32) :: case
            do case = 1, size(key)
                key_val = key(case)%val
                exp_val = expected_int(case)
                call check(error, len(param%get_expected_value_of(key_val)) == len(to_string(exp_val)), &
                           "expected length "//to_string(len(to_string(exp_val))) &
                           //", but got "//to_string(len(param%get_expected_value_of(key_val))))
                if (occurred(error)) return

                call check(error, param%get_expected_value_of(key_val) == to_string(exp_val), &
                           "expected "//to_string(exp_val)//", "// &
                           "but got "//param%get_expected_value_of(key_val))
            end do
        end block

        call teardown()
    contains
        !
        subroutine setup(param, key, expected_int)
            type(test_parameter_type), intent(out) :: param
            type(string_type), allocatable, intent(out) :: key(:)
            integer(int32), allocatable, intent(out) :: expected_int(:)

            call param%construct("", "output1=3 value=5 output2=10")
            read (unit=param%expected_namelist, nml=expected)

            key = [string_type("value"), string_type("output2"), string_type("output1")]
            expected_int = [value, output2, output1]
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine getExpValOf_should_return_value_described_in_namelist
end module test_testParameter_unitTests_getExpectedValueOf
