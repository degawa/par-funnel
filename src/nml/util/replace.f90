module nml_util_replace
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: replace_new_line_mark

    character(*), public, parameter :: new_line_mark = "\n"
        !! library-specific new-line mark to be replaced with new-line character
    character(*), public, parameter :: new_line_char = new_line(" ")
        !! new-line character

contains
    !>returns a string that replaces all new line mark
    !>in `string` with new line character.
    pure function replace_new_line_mark(string) result(replaced)
        implicit none
        character(*), intent(in) :: string
            !! input string
        character(:), allocatable :: replaced
            !! new string

        replaced = replace_all(string, it=new_line_mark, with=new_line_char)
    end function replace_new_line_mark

    !>returns a string that replaces all substrings `it`
    !>in `string` with `with`.
    pure function replace_all(string, it, with) result(replaced)
        implicit none
        character(*), intent(in) :: string
            !! input string
        character(*), intent(in) :: it
            !! the substring to be replaced
        character(*), intent(in) :: with
            !! the replacement
        character(:), allocatable :: replaced
            !! new string

        integer(int32) :: len_it, len_with, idx_it
        len_it = len(it)
        len_with = len(with)

        replaced = string

        idx_it = index(replaced, it, back=.false.)
        do while (idx_it > 0)
            replaced = replaced(1:idx_it - 1)//with//replaced(idx_it + len_it:)
            idx_it = index(replaced, it, back=.false.)
        end do
    end function replace_all
end module nml_util_replace
