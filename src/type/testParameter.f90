module type_testParameter
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: new_test_parameter

    !>This type contains a procedure arguments
    !>and expected values that the procedure should return.
    !>
    !>intended use is to describe multiple inputs within a unit test.
    type, public :: test_parameter_type
        character(:), public, allocatable :: arguments_namelist
            !! namelist of parameter to be passed to a procedure under test
        character(:), public, allocatable :: expected_namelist
            !! namelist of expected value returned from/updated by a procedure under test
    contains
        procedure, public, pass :: construct => construct_component_namelists
        !* constructs the `test_parameter_type` instance

        procedure, public, pass :: presented
        !* checking for the presence of an argument
        procedure, public, pass :: get_argument_value_of
        !* get value of an argument
        procedure, public, pass :: get_expected_value_of
        !* get value of an expected result
        procedure, public, pass :: arguments
        !* get arguments
        procedure, public, pass :: expected
        !* get expected results
    end type test_parameter_type

    interface new_test_parameter
        procedure :: construct_test_parameter
    end interface

    character(*), private, parameter :: arguments_group_name = "arguments"
        !! namelist group name for arguments
    character(*), private, parameter :: expected_group_name = "expected"
        !! namelist group name for expected results

    character(*), private, parameter :: namelist_start = "&"
        !! The chacater placed at the beginning of a namelist
    character(*), private, parameter :: namelist_end = "/"
        !! The chacater placed at the end of a namelist

contains
    !>returns a new `test_parameter_type` instance.
    pure function construct_test_parameter(arguments, expected) result(test_parameter)
        implicit none
        character(*), intent(in) :: arguments
            !! list of arguments to be passed to a procedure under test
        character(*), intent(in) :: expected
            !! list of expected value returned from/updated by a procedure under test
        type(test_parameter_type) :: test_parameter
            !! new `test_parameter_type` instance

        call test_parameter%construct(arguments, expected)
    end function construct_test_parameter

    !>constructs the `test_parameter_type` instance
    !>with converting the arguments and expected lists to namelists
    pure subroutine construct_component_namelists(this, arguments, expected)
        implicit none
        class(test_parameter_type), intent(inout) :: this
            !! passed dummy argument
        character(*), intent(in) :: arguments
            !! list of arguments to be passed to a procedure under test
        character(*), intent(in) :: expected
            !! list of expected value returned from/updated by a procedure under test

        this%arguments_namelist = namelist_start//arguments_group_name//" "//arguments//" "//namelist_end
        this%expected_namelist = namelist_start//expected_group_name//" "//expected//" "//namelist_end
    end subroutine construct_component_namelists

    !>returns `.true.` if `argument` is found
    !>in the component `arguments_namelist`
    !>and returns `.false.` otherwise.
    pure logical function presented(this, argument)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: argument
            !! the argument to be checked of its presence

        presented = (index(this%arguments_namelist, " "//argument//"=") > 0)
    end function presented

    !>returns the value of key retrieved from `" key=val "` format
    !>written in the component `arguments`.
    pure function get_argument_value_of(this, argument) result(val)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: argument
            !! the key of argument to find in the namelist of arguments
        character(:), allocatable :: val
            !! value of the argument

        if (.not. this%presented(argument)) then
            val = ""
            return
        end if

        val = get_value_of(argument, this%arguments_namelist)
    end function get_argument_value_of

    !>returns the value of key retrieved from `" key=val "` format
    !>written in the component `expected`.
    pure function get_expected_value_of(this, expected) result(val)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: expected
            !! the key of expected result to find in the namelist of expected
        character(:), allocatable :: val
            !! value of the expected result

        val = get_value_of(expected, this%expected_namelist)
    end function get_expected_value_of

    !>returns the arguments in string
    !>not including the namelist keywords
    pure function arguments(this) result(args)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(:), allocatable :: args
            !! arguments

        args = drop_namelist_keywords(this%arguments_namelist, arguments_group_name)
    end function arguments

    !>returns the expected results in string
    !>not including the namelist keywords
    pure function expected(this) result(expc)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(:), allocatable :: expc
            !! expected results

        expc = drop_namelist_keywords(this%expected_namelist, expected_group_name)
    end function expected

    !------------------------------------------------------------------!
    !>returns the value of key retrieved from `" key=val "` format
    !>written in `string`.
    pure function get_value_of(key, string) result(val)
        implicit none
        character(*), intent(in) :: key
        character(*), intent(in) :: string
        character(:), allocatable :: val

        integer(int32) :: key_pos, val_head_pos, val_len

        ! getting value of input2
        !
        !arguments='&groupname input1=1 input2=40 /'
        !           1234567890123456789^
        key_pos = index(string, " "//key//"=")

        !                      key_pos v
        !arguments='&groupname input1=1 input2=40 /'
        !                               -------^
        !                               len()+1
        val_head_pos = key_pos + len(key//"=") + 1

        !                         val_head_pos v
        !arguments='&groupname input1=1 input2=40 /'
        !                                      --^
        !                                     val_len
        val_len = index(string(val_head_pos:), " ") - 1

        !                         val_head_pos v
        !arguments='&groupname input1=1 input2=40 /'
        !                                      -^
        !                                      12
        val = string(val_head_pos:val_head_pos + val_len - 1)
    end function get_value_of

    !>returns the string not including namelist keywords.
    pure function drop_namelist_keywords(str_namelist, group_name) result(str)
        implicit none
        character(*), intent(in) :: str_namelist
            !! namelist in string
        character(*), intent(in) :: group_name
            !! namelist group name
        character(:), allocatable :: str
            !! string not including namelist keywords

        integer(int32) :: len_header, len_tailer, len_namelist

        !arguments='&groupname input1=1 input2=40 /'
        !           1234567890^
        len_header = len(namelist_start//group_name//" ")

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
end module type_testParameter
