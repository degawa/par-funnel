module test_nmlUtil_unitTests_format_dropNamelistKeywords
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: strings_enclose
    use :: nml_util_format
    implicit none
    private
    public :: dropNmlKeys_should_return_string_without_namelist_keywords

contains
    subroutine dropNmlKeys_should_return_string_without_namelist_keywords(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        character(:), allocatable :: str_namelist, expected_arguments, actual_arguments

        call setup(str_namelist, expected_arguments)

        actual_arguments = drop_namelist_keywords(str_namelist)

        call check(error, len(actual_arguments) == len(expected_arguments), &
                   "expected length "//to_string(len(expected_arguments)) &
                   //", but got "//to_string(len(actual_arguments)))
        if (occurred(error)) return

        call check(error, actual_arguments == expected_arguments, &
                   "expected "//enclose(expected_arguments, '"')//", "// &
                   "but got "//enclose(actual_arguments, '"'))

        call teardown(str_namelist, expected_arguments, actual_arguments)
    contains
        !
        subroutine setup(str_namelist, expected_arguments)
            character(:), allocatable, intent(out) :: str_namelist, expected_arguments
            expected_arguments = "input1=1 input2=10 input3=4"
            str_namelist = "&namelist_group_name input1=1 input2=10 input3=4 /"
        end subroutine setup
        !
        subroutine teardown(str_namelist, expected_arguments, actual_arguments)
            character(:), allocatable, intent(inout) :: str_namelist, expected_arguments, actual_arguments
            deallocate (str_namelist)
            deallocate (expected_arguments)
            deallocate (actual_arguments)
        end subroutine teardown
    end subroutine dropNmlKeys_should_return_string_without_namelist_keywords
end module test_nmlUtil_unitTests_format_dropNamelistKeywords
