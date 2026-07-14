module nml_util_parse
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: find_value_token
    public :: normalize_assignments

contains
    !>searches a namelist string, e.g. `"&arguments input=1 arr=1, 2, 3 /"`,
    !>for the variable `name` and returns the value token assigned to it.
    !>
    !>The search is aware of quoted strings (both `'` and `"`),
    !>is case-insensitive as the namelist feature is,
    !>and a value token may contain blanks and commas,
    !>for example `"1, 2, 3"` for an array.
    !>
    !>`found` is set to `.false.` and `token` to an empty string
    !>if `name` is not found.
    pure subroutine find_value_token(namelist_str, name, token, found)
        implicit none
        character(*), intent(in) :: namelist_str
            !! a namelist string, including the group name and `/`
        character(*), intent(in) :: name
            !! the variable name to search for
        character(:), allocatable, intent(out) :: token
            !! the value token assigned to `name`
        logical, intent(out) :: found
            !! `.true.` if `name` is found in `namelist_str`

        integer(int32), allocatable :: key_begin(:), key_end(:), val_begin(:)
        integer(int32) :: num_keys, key, val_end

        call locate_keys(namelist_str, key_begin, key_end, val_begin, num_keys)

        token = ""
        found = .false.
        do key = 1, num_keys
            if (equals_ignoring_case(namelist_str(key_begin(key):key_end(key)), name)) then
                if (key < num_keys) then
                    val_end = key_begin(key + 1) - 1
                else
                    val_end = end_of_body(namelist_str)
                end if
                token = trim(adjustl(namelist_str(val_begin(key):val_end)))
                found = .true.
                return
            end if
        end do
    end subroutine find_value_token

    !>returns a string in that blanks on both sides of `=` are removed.
    !>`=` inside quoted strings are not affected.
    !>
    !>This allows users to write namelist bodies
    !>in a natural style like `"input = 1"`.
    pure function normalize_assignments(str) result(normalized)
        implicit none
        character(*), intent(in) :: str
            !! input string, a namelist body like `"input = 1 fmt = '(i5)'"`
        character(:), allocatable :: normalized
            !! the string with blanks around `=` removed

        character :: quote, c
        logical :: in_quote
        integer(int32) :: i, len_str

        len_str = len(str)
        normalized = ""
        in_quote = .false.
        quote = " "

        i = 1
        do while (i <= len_str)
            c = str(i:i)

            if (in_quote) then
                normalized = normalized//c
                if (c == quote) in_quote = .false.
                i = i + 1
                cycle
            end if

            select case (c)
            case ("'", '"')
                in_quote = .true.
                quote = c
                normalized = normalized//c
            case ("=")
                ! drop blanks already collected before `=`
                normalized = trim(normalized)//c
                ! skip blanks after `=`
                do while (i + 1 <= len_str)
                    if (str(i + 1:i + 1) /= " ") exit
                    i = i + 1
                end do
            case default
                normalized = normalized//c
            end select
            i = i + 1
        end do
    end function normalize_assignments

    !------------------------------------------------------------------!
    !>locates all `key=` occurrences outside quoted strings
    !>and returns the begin/end positions of each key
    !>and the begin position of each value.
    pure subroutine locate_keys(str, key_begin, key_end, val_begin, num_keys)
        implicit none
        character(*), intent(in) :: str
            !! a namelist string
        integer(int32), allocatable, intent(out) :: key_begin(:)
            !! begin positions of keys
        integer(int32), allocatable, intent(out) :: key_end(:)
            !! end positions of keys
        integer(int32), allocatable, intent(out) :: val_begin(:)
            !! begin positions of values
        integer(int32), intent(out) :: num_keys
            !! the number of keys found

        character :: quote, c
        logical :: in_quote
        integer(int32) :: i, len_str, begin_of_key

        len_str = len(str)
        allocate (key_begin(0), key_end(0), val_begin(0))
        num_keys = 0
        in_quote = .false.
        quote = " "

        do i = 1, len_str
            c = str(i:i)

            if (in_quote) then
                if (c == quote) in_quote = .false.
                cycle
            end if

            select case (c)
            case ("'", '"')
                in_quote = .true.
                quote = c
            case ("=")
                begin_of_key = begin_of_identifier(str, ends_at=i - 1)
                if (begin_of_key > 0) then
                    key_begin = [key_begin, begin_of_key]
                    key_end = [key_end, i - 1]
                    val_begin = [val_begin, i + 1]
                    num_keys = num_keys + 1
                end if
            end select
        end do
    end subroutine locate_keys

    !>returns the begin position of the identifier that ends at `ends_at`
    !>by scanning backward over identifier characters
    !>(letters, digits, and underscores).
    !>Returns 0 if there is no identifier.
    pure function begin_of_identifier(str, ends_at) result(begin_pos)
        implicit none
        character(*), intent(in) :: str
            !! the string to be scanned
        integer(int32), intent(in) :: ends_at
            !! the end position of the identifier
        integer(int32) :: begin_pos
            !! the begin position of the identifier, 0 if not found

        integer(int32) :: i

        begin_pos = 0
        do i = ends_at, 1, -1
            if (is_identifier_char(str(i:i))) then
                begin_pos = i
            else
                exit
            end if
        end do
    end function begin_of_identifier

    !>returns `.true.` if `c` is a letter, a digit, or an underscore.
    pure logical function is_identifier_char(c)
        implicit none
        character, intent(in) :: c
            !! the character to be checked

        is_identifier_char = (("a" <= c .and. c <= "z") .or. &
                              ("A" <= c .and. c <= "Z") .or. &
                              ("0" <= c .and. c <= "9") .or. &
                              (c == "_"))
    end function is_identifier_char

    !>returns the position of the last character of the namelist body,
    !>that is the position just before the trailing `/`.
    !>Returns `len(str)` if no trailing `/` is found.
    pure function end_of_body(str) result(pos)
        implicit none
        character(*), intent(in) :: str
            !! a namelist string
        integer(int32) :: pos
            !! the end position of the namelist body

        integer(int32) :: slash_pos

        slash_pos = index(str, "/", back=.true.)
        if (slash_pos > 0) then
            pos = slash_pos - 1
        else
            pos = len(str)
        end if
    end function end_of_body

    !>returns `.true.` if two strings are equal ignoring cases.
    pure logical function equals_ignoring_case(lhs, rhs)
        implicit none
        character(*), intent(in) :: lhs
            !! a string to be compared
        character(*), intent(in) :: rhs
            !! a string to be compared

        integer(int32) :: i

        equals_ignoring_case = .false.
        if (len_trim(lhs) /= len_trim(rhs)) return

        do i = 1, len_trim(lhs)
            if (to_lower(lhs(i:i)) /= to_lower(rhs(i:i))) return
        end do
        equals_ignoring_case = .true.
    end function equals_ignoring_case

    !>returns the lowercase character of `c` if `c` is an uppercase letter
    !>and returns `c` itself otherwise.
    pure function to_lower(c) result(lower)
        implicit none
        character, intent(in) :: c
            !! the character to be converted
        character :: lower
            !! the converted character

        if ("A" <= c .and. c <= "Z") then
            lower = achar(iachar(c) + (iachar("a") - iachar("A")))
        else
            lower = c
        end if
    end function to_lower
end module nml_util_parse
