module nml_util_get
    use, intrinsic :: iso_fortran_env
    use :: nml_util_parse
    implicit none
    private
    public :: read_value_from

    integer(int32), public, parameter :: stat_ok = 0
        !! status value meaning the value is successfully read
    integer(int32), public, parameter :: stat_not_found = 1
        !! status value meaning the variable is not found in the namelist
    integer(int32), public, parameter :: stat_read_error = 2
        !! status value meaning the value token cannot be interpreted

    integer(int32), private, parameter :: max_string_len = 1024
        !! the maximum length of a character value

    !>Reads the value of a variable from a namelist string
    !>without requiring the user to declare a namelist.
    !>
    !>Supported types are
    !>`integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)`,
    !>`logical`, and `character(:), allocatable` for scalars, and
    !>rank-1 arrays of the numeric and logical types.
    interface read_value_from
        procedure :: read_int32_from
        procedure :: read_int64_from
        procedure :: read_real32_from
        procedure :: read_real64_from
        procedure :: read_logical_from
        procedure :: read_char_from
        procedure :: read_int32_rank1_from
        procedure :: read_int64_rank1_from
        procedure :: read_real32_rank1_from
        procedure :: read_real64_rank1_from
        procedure :: read_logical_rank1_from
    end interface

contains
    !>reads an `integer(int32)` value assigned to `name` from `namelist_str`.
    subroutine read_int32_from(namelist_str, name, v, stat, errmsg)
        implicit none
        character(*), intent(in) :: namelist_str
            !! a namelist string
        character(*), intent(in) :: name
            !! the variable name
        integer(int32), intent(inout) :: v
            !! the value read from the namelist
        integer(int32), intent(out) :: stat
            !! the status of the read operation
        character(:), allocatable, intent(out) :: errmsg
            !! the error message when `stat` is not `stat_ok`

        namelist /par_funnel_value/ v
        character(:), allocatable :: token, buf
        logical :: found
        integer(int32) :: io_stat
        character(256) :: io_msg

        call find_value_token(namelist_str, name, token, found)
        if (.not. found) then
            call report_not_found(name, namelist_str, stat, errmsg)
            return
        end if

        buf = "&par_funnel_value v="//token//" /"
        read (buf, nml=par_funnel_value, iostat=io_stat, iomsg=io_msg)
        call report_read_status(io_stat, io_msg, name, token, stat, errmsg)
    end subroutine read_int32_from

    !>reads an `integer(int64)` value assigned to `name` from `namelist_str`.
    subroutine read_int64_from(namelist_str, name, v, stat, errmsg)
        implicit none
        character(*), intent(in) :: namelist_str
            !! a namelist string
        character(*), intent(in) :: name
            !! the variable name
        integer(int64), intent(inout) :: v
            !! the value read from the namelist
        integer(int32), intent(out) :: stat
            !! the status of the read operation
        character(:), allocatable, intent(out) :: errmsg
            !! the error message when `stat` is not `stat_ok`

        namelist /par_funnel_value/ v
        character(:), allocatable :: token, buf
        logical :: found
        integer(int32) :: io_stat
        character(256) :: io_msg

        call find_value_token(namelist_str, name, token, found)
        if (.not. found) then
            call report_not_found(name, namelist_str, stat, errmsg)
            return
        end if

        buf = "&par_funnel_value v="//token//" /"
        read (buf, nml=par_funnel_value, iostat=io_stat, iomsg=io_msg)
        call report_read_status(io_stat, io_msg, name, token, stat, errmsg)
    end subroutine read_int64_from

    !>reads a `real(real32)` value assigned to `name` from `namelist_str`.
    subroutine read_real32_from(namelist_str, name, v, stat, errmsg)
        implicit none
        character(*), intent(in) :: namelist_str
            !! a namelist string
        character(*), intent(in) :: name
            !! the variable name
        real(real32), intent(inout) :: v
            !! the value read from the namelist
        integer(int32), intent(out) :: stat
            !! the status of the read operation
        character(:), allocatable, intent(out) :: errmsg
            !! the error message when `stat` is not `stat_ok`

        namelist /par_funnel_value/ v
        character(:), allocatable :: token, buf
        logical :: found
        integer(int32) :: io_stat
        character(256) :: io_msg

        call find_value_token(namelist_str, name, token, found)
        if (.not. found) then
            call report_not_found(name, namelist_str, stat, errmsg)
            return
        end if

        buf = "&par_funnel_value v="//token//" /"
        read (buf, nml=par_funnel_value, iostat=io_stat, iomsg=io_msg)
        call report_read_status(io_stat, io_msg, name, token, stat, errmsg)
    end subroutine read_real32_from

    !>reads a `real(real64)` value assigned to `name` from `namelist_str`.
    subroutine read_real64_from(namelist_str, name, v, stat, errmsg)
        implicit none
        character(*), intent(in) :: namelist_str
            !! a namelist string
        character(*), intent(in) :: name
            !! the variable name
        real(real64), intent(inout) :: v
            !! the value read from the namelist
        integer(int32), intent(out) :: stat
            !! the status of the read operation
        character(:), allocatable, intent(out) :: errmsg
            !! the error message when `stat` is not `stat_ok`

        namelist /par_funnel_value/ v
        character(:), allocatable :: token, buf
        logical :: found
        integer(int32) :: io_stat
        character(256) :: io_msg

        call find_value_token(namelist_str, name, token, found)
        if (.not. found) then
            call report_not_found(name, namelist_str, stat, errmsg)
            return
        end if

        buf = "&par_funnel_value v="//token//" /"
        read (buf, nml=par_funnel_value, iostat=io_stat, iomsg=io_msg)
        call report_read_status(io_stat, io_msg, name, token, stat, errmsg)
    end subroutine read_real64_from

    !>reads a `logical` value assigned to `name` from `namelist_str`.
    subroutine read_logical_from(namelist_str, name, v, stat, errmsg)
        implicit none
        character(*), intent(in) :: namelist_str
            !! a namelist string
        character(*), intent(in) :: name
            !! the variable name
        logical, intent(inout) :: v
            !! the value read from the namelist
        integer(int32), intent(out) :: stat
            !! the status of the read operation
        character(:), allocatable, intent(out) :: errmsg
            !! the error message when `stat` is not `stat_ok`

        namelist /par_funnel_value/ v
        character(:), allocatable :: token, buf
        logical :: found
        integer(int32) :: io_stat
        character(256) :: io_msg

        call find_value_token(namelist_str, name, token, found)
        if (.not. found) then
            call report_not_found(name, namelist_str, stat, errmsg)
            return
        end if

        buf = "&par_funnel_value v="//token//" /"
        read (buf, nml=par_funnel_value, iostat=io_stat, iomsg=io_msg)
        call report_read_status(io_stat, io_msg, name, token, stat, errmsg)
    end subroutine read_logical_from

    !>reads a character value assigned to `name` from `namelist_str`.
    !>
    !>The value token is automatically enclosed in quotes if it is not quoted,
    !>so that users can write `fmt=(i5)` as well as `fmt='(i5)'`.
    !>Trailing blanks are removed from the result.
    subroutine read_char_from(namelist_str, name, v, stat, errmsg)
        implicit none
        character(*), intent(in) :: namelist_str
            !! a namelist string
        character(*), intent(in) :: name
            !! the variable name
        character(:), allocatable, intent(inout) :: v
            !! the value read from the namelist
        integer(int32), intent(out) :: stat
            !! the status of the read operation
        character(:), allocatable, intent(out) :: errmsg
            !! the error message when `stat` is not `stat_ok`

        character(max_string_len) :: buffer
        namelist /par_funnel_value/ buffer
        character(:), allocatable :: token, buf
        logical :: found
        integer(int32) :: io_stat
        character(256) :: io_msg

        call find_value_token(namelist_str, name, token, found)
        if (.not. found) then
            call report_not_found(name, namelist_str, stat, errmsg)
            return
        end if

        token = quote_if_unquoted(token)

        buf = "&par_funnel_value buffer="//token//" /"
        read (buf, nml=par_funnel_value, iostat=io_stat, iomsg=io_msg)
        call report_read_status(io_stat, io_msg, name, token, stat, errmsg)
        if (stat == stat_ok) v = trim(buffer)
    end subroutine read_char_from

    !>reads an `integer(int32)` rank-1 array assigned to `name` from `namelist_str`.
    subroutine read_int32_rank1_from(namelist_str, name, v, stat, errmsg)
        implicit none
        character(*), intent(in) :: namelist_str
            !! a namelist string
        character(*), intent(in) :: name
            !! the variable name
        integer(int32), intent(inout) :: v(:)
            !! the values read from the namelist
        integer(int32), intent(out) :: stat
            !! the status of the read operation
        character(:), allocatable, intent(out) :: errmsg
            !! the error message when `stat` is not `stat_ok`

        namelist /par_funnel_value/ v
        character(:), allocatable :: token, buf
        logical :: found
        integer(int32) :: io_stat
        character(256) :: io_msg

        call find_value_token(namelist_str, name, token, found)
        if (.not. found) then
            call report_not_found(name, namelist_str, stat, errmsg)
            return
        end if

        buf = "&par_funnel_value v="//token//" /"
        read (buf, nml=par_funnel_value, iostat=io_stat, iomsg=io_msg)
        call report_read_status(io_stat, io_msg, name, token, stat, errmsg)
    end subroutine read_int32_rank1_from

    !>reads an `integer(int64)` rank-1 array assigned to `name` from `namelist_str`.
    subroutine read_int64_rank1_from(namelist_str, name, v, stat, errmsg)
        implicit none
        character(*), intent(in) :: namelist_str
            !! a namelist string
        character(*), intent(in) :: name
            !! the variable name
        integer(int64), intent(inout) :: v(:)
            !! the values read from the namelist
        integer(int32), intent(out) :: stat
            !! the status of the read operation
        character(:), allocatable, intent(out) :: errmsg
            !! the error message when `stat` is not `stat_ok`

        namelist /par_funnel_value/ v
        character(:), allocatable :: token, buf
        logical :: found
        integer(int32) :: io_stat
        character(256) :: io_msg

        call find_value_token(namelist_str, name, token, found)
        if (.not. found) then
            call report_not_found(name, namelist_str, stat, errmsg)
            return
        end if

        buf = "&par_funnel_value v="//token//" /"
        read (buf, nml=par_funnel_value, iostat=io_stat, iomsg=io_msg)
        call report_read_status(io_stat, io_msg, name, token, stat, errmsg)
    end subroutine read_int64_rank1_from

    !>reads a `real(real32)` rank-1 array assigned to `name` from `namelist_str`.
    subroutine read_real32_rank1_from(namelist_str, name, v, stat, errmsg)
        implicit none
        character(*), intent(in) :: namelist_str
            !! a namelist string
        character(*), intent(in) :: name
            !! the variable name
        real(real32), intent(inout) :: v(:)
            !! the values read from the namelist
        integer(int32), intent(out) :: stat
            !! the status of the read operation
        character(:), allocatable, intent(out) :: errmsg
            !! the error message when `stat` is not `stat_ok`

        namelist /par_funnel_value/ v
        character(:), allocatable :: token, buf
        logical :: found
        integer(int32) :: io_stat
        character(256) :: io_msg

        call find_value_token(namelist_str, name, token, found)
        if (.not. found) then
            call report_not_found(name, namelist_str, stat, errmsg)
            return
        end if

        buf = "&par_funnel_value v="//token//" /"
        read (buf, nml=par_funnel_value, iostat=io_stat, iomsg=io_msg)
        call report_read_status(io_stat, io_msg, name, token, stat, errmsg)
    end subroutine read_real32_rank1_from

    !>reads a `real(real64)` rank-1 array assigned to `name` from `namelist_str`.
    subroutine read_real64_rank1_from(namelist_str, name, v, stat, errmsg)
        implicit none
        character(*), intent(in) :: namelist_str
            !! a namelist string
        character(*), intent(in) :: name
            !! the variable name
        real(real64), intent(inout) :: v(:)
            !! the values read from the namelist
        integer(int32), intent(out) :: stat
            !! the status of the read operation
        character(:), allocatable, intent(out) :: errmsg
            !! the error message when `stat` is not `stat_ok`

        namelist /par_funnel_value/ v
        character(:), allocatable :: token, buf
        logical :: found
        integer(int32) :: io_stat
        character(256) :: io_msg

        call find_value_token(namelist_str, name, token, found)
        if (.not. found) then
            call report_not_found(name, namelist_str, stat, errmsg)
            return
        end if

        buf = "&par_funnel_value v="//token//" /"
        read (buf, nml=par_funnel_value, iostat=io_stat, iomsg=io_msg)
        call report_read_status(io_stat, io_msg, name, token, stat, errmsg)
    end subroutine read_real64_rank1_from

    !>reads a `logical` rank-1 array assigned to `name` from `namelist_str`.
    subroutine read_logical_rank1_from(namelist_str, name, v, stat, errmsg)
        implicit none
        character(*), intent(in) :: namelist_str
            !! a namelist string
        character(*), intent(in) :: name
            !! the variable name
        logical, intent(inout) :: v(:)
            !! the values read from the namelist
        integer(int32), intent(out) :: stat
            !! the status of the read operation
        character(:), allocatable, intent(out) :: errmsg
            !! the error message when `stat` is not `stat_ok`

        namelist /par_funnel_value/ v
        character(:), allocatable :: token, buf
        logical :: found
        integer(int32) :: io_stat
        character(256) :: io_msg

        call find_value_token(namelist_str, name, token, found)
        if (.not. found) then
            call report_not_found(name, namelist_str, stat, errmsg)
            return
        end if

        buf = "&par_funnel_value v="//token//" /"
        read (buf, nml=par_funnel_value, iostat=io_stat, iomsg=io_msg)
        call report_read_status(io_stat, io_msg, name, token, stat, errmsg)
    end subroutine read_logical_rank1_from

    !------------------------------------------------------------------!
    !>sets `stat` and `errmsg` to report that
    !>the variable `name` is not found in the namelist.
    pure subroutine report_not_found(name, namelist_str, stat, errmsg)
        implicit none
        character(*), intent(in) :: name
            !! the variable name that is not found
        character(*), intent(in) :: namelist_str
            !! the namelist string searched
        integer(int32), intent(out) :: stat
            !! the status of the read operation
        character(:), allocatable, intent(out) :: errmsg
            !! the error message

        stat = stat_not_found
        errmsg = 'par-funnel: variable "'//name//'" is not found in '//namelist_str
    end subroutine report_not_found

    !>converts the iostat/iomsg of a namelist read
    !>into the status and error message of this module.
    pure subroutine report_read_status(io_stat, io_msg, name, token, stat, errmsg)
        implicit none
        integer(int32), intent(in) :: io_stat
            !! the iostat value of the namelist read
        character(*), intent(in) :: io_msg
            !! the iomsg value of the namelist read
        character(*), intent(in) :: name
            !! the variable name
        character(*), intent(in) :: token
            !! the value token
        integer(int32), intent(out) :: stat
            !! the status of the read operation
        character(:), allocatable, intent(out) :: errmsg
            !! the error message

        if (io_stat == 0) then
            stat = stat_ok
            errmsg = ""
        else
            stat = stat_read_error
            errmsg = 'par-funnel: could not interpret "'//token//'" as the value of "' &
                     //name//'" ('//trim(io_msg)//')'
        end if
    end subroutine report_read_status

    !>returns the token enclosed in single quotes if it is not quoted.
    !>Single quotes contained in the token are doubled when enclosing.
    pure function quote_if_unquoted(token) result(quoted)
        implicit none
        character(*), intent(in) :: token
            !! the value token
        character(:), allocatable :: quoted
            !! the quoted token

        integer(int32) :: i

        if (len(token) > 0) then
            if (token(1:1) == "'" .or. token(1:1) == '"') then
                quoted = token
                return
            end if
        end if

        quoted = "'"
        do i = 1, len(token)
            if (token(i:i) == "'") then
                quoted = quoted//"''"
            else
                quoted = quoted//token(i:i)
            end if
        end do
        quoted = quoted//"'"
    end function quote_if_unquoted
end module nml_util_get
