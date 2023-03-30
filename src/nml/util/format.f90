module nml_util_format
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: drop_namelist_keywords

    character(*), public, parameter :: namelist_start = "&"
        !! The character placed at the beginning of a namelist
    character(*), public, parameter :: namelist_end = "/"
        !! The character placed at the end of a namelist

contains
    !>returns the string, not including namelist keywords.
    pure function drop_namelist_keywords(str_namelist) result(str)
        implicit none
        character(*), intent(in) :: str_namelist
            !! a namelist
        character(:), allocatable :: str
            !! the string not including namelist keywords

        integer(int32) :: len_header, len_tailer, len_namelist

        !arguments='&groupname input1=1 input2=40 /'
        !           1234567890^
        len_header = index(str_namelist, " ", back=.false.)

        !arguments='&groupname input1=1 input2=40 /'
        !                                        1^
        len_tailer = len(" "//namelist_end)

        !arguments='&groupname input1=1 input2=40 /'
        !           123456789012345678901234567890^
        len_namelist = len(str_namelist)

        !            len_header                  len_tailer
        !           ----------v                  -v
        !arguments='&groupname input1=1 input2=40 /'
        !         len_header+1 ^                ^ len_namelist-len_tailer
        str = str_namelist(len_header+1:len_namelist-len_tailer) !&
    end function drop_namelist_keywords
end module nml_util_format
