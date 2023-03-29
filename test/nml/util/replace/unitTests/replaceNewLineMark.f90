module test_nmlUtil_unitTests_replace_newLineMark
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: strings_enclose
    use :: nml_util_replace
    implicit none
    private
    public :: replNLMark_should_return_string_that_replaces_mark_with_char

contains
    subroutine replNLMark_should_return_string_that_replaces_mark_with_char(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        character(:), allocatable :: str_namelist, expected_namelist, actual_namelist

        call setup(str_namelist, expected_namelist)

        actual_namelist = replace_new_line_mark(str_namelist)

        call check(error, len(actual_namelist) == len(expected_namelist), &
                   "expected namelist length "//to_string(len(expected_namelist)) &
                   //", but got "//to_string(len(actual_namelist)))
        if (occurred(error)) return

        call check(error, actual_namelist == expected_namelist, &
                   "expected "//enclose(expected_namelist, '"')//", "// &
                   "but got "//enclose(actual_namelist, '"'))

        call teardown(str_namelist, expected_namelist, actual_namelist)
    contains
        !
        subroutine setup(str_namelist, expected_namelist)
            character(:), allocatable, intent(out) :: str_namelist, expected_namelist
            str_namelist = "&namelist_group_name input1=1 \ninput2=10 \ninput3=4 /"
            expected_namelist = "&namelist_group_name input1=1 "//new_line(" ")// &
                                "input2=10 "//new_line(" ")// &
                                "input3=4 /"
        end subroutine setup
        !
        subroutine teardown(str_namelist, expected_namelist, actual_namelist)
            character(:), allocatable, intent(inout) :: str_namelist, expected_namelist, actual_namelist
            deallocate (str_namelist)
            deallocate (expected_namelist)
            deallocate (actual_namelist)
        end subroutine teardown
    end subroutine replNLMark_should_return_string_that_replaces_mark_with_char
end module test_nmlUtil_unitTests_replace_newLineMark
