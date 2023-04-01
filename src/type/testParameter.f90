module type_testParameter
    use, intrinsic :: iso_fortran_env
    use :: nml_util_format
    use :: nml_util_replace
    implicit none
    private
    public :: new_test_parameter
    public :: replace_new_line_mark
    public :: replace_new_line_char
    public :: new_line_char
    public :: new_line_mark

    !>This user-defined type contains procedure arguments
    !>and expected values that the procedure should return,
    !>and the intended use is to describe multiple inputs within a unit test.
    type, public :: test_parameter_type
        character(:), public, allocatable :: arguments_namelist
            !! namelist of arguments to be passed to a procedure under test
        character(:), public, allocatable :: expected_namelist
            !! namelist of expected values returned from/updated by a procedure under test
    contains
        procedure, public, pass :: construct => construct_component_namelists
        !* constructs the `test_parameter_type` instance
        final :: finalize
        !* finalizes the `test_parameter_type` instance

        procedure, public, pass :: presented
        !* checks for the presence of an argument
        !* gets the value of an expected result
        procedure, public, pass :: arguments
        !* gets the arguments
        procedure, public, pass :: expected
        !* gets the expected results
    end type test_parameter_type

    interface new_test_parameter
        procedure :: construct_test_parameter
    end interface

    character(*), private, parameter :: arguments_group_name = "arguments"
        !! namelist group name for the arguments
    character(*), private, parameter :: expected_group_name = "expected"
        !! namelist group name for the expected results

contains
    !>returns a new `test_parameter_type` instance.
    pure function construct_test_parameter(arguments, expected) result(test_parameter)
        implicit none
        character(*), intent(in) :: arguments
            !! list of arguments to be passed to a procedure under test
        character(*), intent(in) :: expected
            !! list of expected values returned from/updated by a procedure under test
        type(test_parameter_type) :: test_parameter
            !! new `test_parameter_type` instance

        call test_parameter%construct(arguments, expected)
    end function construct_test_parameter

    !>constructs the `test_parameter_type` instance
    !>by converting the arguments and expected lists to namelists
    pure subroutine construct_component_namelists(this, arguments, expected)
        implicit none
        class(test_parameter_type), intent(inout) :: this
            !! passed dummy argument
        character(*), intent(in) :: arguments
            !! the list of arguments to be passed to a procedure under test
        character(*), intent(in) :: expected
            !! the list of expected values
            !! returned from/updated by a procedure under test

        this%arguments_namelist = namelist_start//arguments_group_name//" "//arguments//" "//namelist_end
        this%expected_namelist = namelist_start//expected_group_name//" "//expected//" "//namelist_end
    end subroutine construct_component_namelists

    !>finalize the `test_parameter_type` instance
    !>by deallocating components
    !>expected_namelist` and `expected_namelist`
    pure subroutine finalize(this)
        implicit none
        type(test_parameter_type), intent(inout) :: this
            !! passed dummy argument
        if (allocated(this%arguments_namelist)) deallocate (this%arguments_namelist)
        if (allocated(this%expected_namelist)) deallocate (this%expected_namelist)
    end subroutine finalize

    !>returns `.true.` if the `argument` is found
    !>in the component `arguments_namelist`
    !>and returns `.false.` otherwise.
    pure logical function presented(this, argument)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(*), intent(in) :: argument
            !! the argument to be checked for its presence

        presented = (index(this%arguments_namelist, " "//argument//"=") > 0)
    end function presented

    !>returns the arguments in a string,
    !>not including the namelist keywords
    pure function arguments(this) result(args)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(:), allocatable :: args
            !! the arguments not including the namelist keywords

        args = drop_namelist_keywords(this%arguments_namelist)
    end function arguments

    !>returns the expected results in a string,
    !>not including the namelist keywords
    pure function expected(this) result(expc)
        implicit none
        class(test_parameter_type), intent(in) :: this
            !! passed dummy argument
        character(:), allocatable :: expc
            !! the expected results not including the namelist keywords

        expc = drop_namelist_keywords(this%expected_namelist)
    end function expected
end module type_testParameter
