module test_nmlUtil_unitTests_format_getValueOf
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: strings_enclose
    use :: nml_util_format
    implicit none
    private
    public :: getValueOf_should_return_0_len_str_when_key_is_not_present
    public :: getValueOf_should_return_value_described_in_namelist

contains
    subroutine getValueOf_should_return_0_len_str_when_key_is_not_present(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: str_namelist, key, expected, actual

        call setup(str_namelist, key, expected)

        block
            actual = get_value_of(key, str_namelist)
            call check(error, len(actual) == len(expected), &
                       "expected length "//to_string(len(expected)) &
                       //", but got "//to_string(len(actual)))
            if (occurred(error)) return

            call check(error, actual == expected, &
                       "expected "//expected//", "// &
                       "but got "//actual)
        end block

        call teardown(str_namelist, key, expected, actual)
    contains
        !
        subroutine setup(str_namelist, key, expected)
            character(:), allocatable, intent(out) :: str_namelist, key, expected

            str_namelist = "&namelist_group input=5 /"
            key = "value"
            expected = ""
        end subroutine setup
        !
        subroutine teardown(str_namelist, key, expected, actual)
            character(:), allocatable, intent(inout) :: str_namelist, key, expected, actual
            deallocate (str_namelist)
            deallocate (key)
            deallocate (expected)
            deallocate (actual)
        end subroutine teardown
    end subroutine getValueOf_should_return_0_len_str_when_key_is_not_present

    subroutine getValueOf_should_return_value_described_in_namelist(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(:), allocatable :: str_namelist, key, expected, actual

        call setup(str_namelist, key, expected)

        block
            actual = get_value_of(key, str_namelist)
            call check(error, len(actual) == len(expected), &
                       "expected length "//to_string(len(expected)) &
                       //", but got "//to_string(len(actual)))
            if (occurred(error)) return

            call check(error, actual == expected, &
                       "expected "//expected//", "// &
                       "but got "//actual)
        end block

        call teardown(str_namelist, key, expected, actual)
    contains
        !
        subroutine setup(str_namelist, key, expected)
            character(:), allocatable, intent(out) :: str_namelist, key, expected

            str_namelist = "&namelist_group value=5 /"
            key = "value"
            expected = "5"
        end subroutine setup
        !
        subroutine teardown(str_namelist, key, expected, actual)
            character(:), allocatable, intent(inout) :: str_namelist, key, expected, actual
            deallocate (str_namelist)
            deallocate (key)
            deallocate (expected)
            deallocate (actual)
        end subroutine teardown
    end subroutine getValueOf_should_return_value_described_in_namelist
end module test_nmlUtil_unitTests_format_getValueOf
