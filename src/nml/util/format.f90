module nml_util_format
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: get_value_of
    public :: drop_namelist_keywords

    character(*), public, parameter :: namelist_start = "&"
        !! The character placed at the beginning of a namelist
    character(*), public, parameter :: namelist_end = "/"
        !! The character placed at the end of a namelist

contains
    !>returns the key's value retrieved from `"key=val"` format
    !>written in the `string`.
    pure function get_value_of(key, str_namelist) result(val)
        implicit none
        character(*), intent(in) :: key
            !! a key of the arguments or expected resutls
        character(*), intent(in) :: str_namelist
            !! a namelist
        character(:), allocatable :: val
            !! the key's value written in the namelist

        integer(int32) :: key_pos, val_head_pos, val_len

        ! getting value of input2
        !
        !arguments='&groupname input1=1 input2=40 /'
        !           1234567890123456789^
        key_pos = index(str_namelist, " "//key//"=")

        ! if key does not exist in `str_namelist`
        if (key_pos == 0) then
            val = ""
            return
        end if

        !                      key_pos v
        !arguments='&groupname input1=1 input2=40 /'
        !                               -------^
        !                               len()+1
        val_head_pos = key_pos + len(key//"=") + 1

        !                         val_head_pos v
        !arguments='&groupname input1=1 input2=40 /'
        !                                      --^
        !                                     val_len
        val_len = index(str_namelist(val_head_pos:), " ") - 1

        !                         val_head_pos v
        !arguments='&groupname input1=1 input2=40 /'
        !                                      -^
        !                                      12
        val = str_namelist(val_head_pos:val_head_pos + val_len - 1)
    end function get_value_of

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
