module doublifyTestParameter
    use, intrinsic :: iso_fortran_env
    use :: par_funnel
    implicit none
    private
    public :: new_doublify_test_parameter

    type, public, extends(test_parameter_type) :: doublify_test_parameter_type
        integer(int32), public :: input
        integer(int32), public :: output
    contains
        procedure, public, pass :: setup
        procedure, public, pass :: teardown
        final :: finalize

        procedure, public, pass :: failure_message
    end type doublify_test_parameter_type

    type(doublify_test_parameter_type), public :: doublify_test_parameter_type_mold

    interface new_doublify_test_parameter
        procedure :: construct_doublify_test_parameter
    end interface
contains
    !>returns a new `test_parameter_type` instance.
    pure function construct_doublify_test_parameter(arguments, expected) result(test_parameter)
        implicit none
        character(*), intent(in) :: arguments
            !! list of arguments to be passed to a procedure under test
        character(*), intent(in) :: expected
            !! list of expected values returned from/updated by a procedure under test
        type(doublify_test_parameter_type) :: test_parameter
            !! new `test_parameter_type` instance

        call test_parameter%construct(arguments, expected)
    end function construct_doublify_test_parameter

    subroutine setup(this)
        implicit none
        class(doublify_test_parameter_type), intent(inout) :: this

        integer(int32) :: input, output
        namelist /arguments/ input
        namelist /expected/ output

        ! arguments for the procedure under test
        read (unit=this%arguments_namelist, nml=arguments)
        this%input = input

        ! expected results
        read (unit=this%expected_namelist, nml=expected)
        this%output = output

        this%case_name = "doublify(input) should return "//this%expected()// &
                         " when input "//this%arguments()
    end subroutine setup

    subroutine teardown(this)
        implicit none
        class(doublify_test_parameter_type), intent(inout) :: this
        this%input = 0
        this%output = 0
    end subroutine teardown

    !>finalize the `doublify_test_parameter_type` instance
    !>by deallocating components
    !>expected_namelist` and `expected_namelist`
    pure subroutine finalize(this)
        implicit none
        type(doublify_test_parameter_type), intent(inout) :: this
            !! passed-object dummy argument
        this%input = 0
        this%output = 0
        this%case_name = ""
        if (allocated(this%arguments_namelist)) deallocate (this%arguments_namelist)
        if (allocated(this%expected_namelist)) deallocate (this%expected_namelist)
    end subroutine finalize

    function failure_message(this, actual) result(msg)
        use :: testdrive, only:to_string
        implicit none
        class(doublify_test_parameter_type), intent(in) :: this
        integer(int32), intent(in) :: actual
        character(:), allocatable :: msg

        msg = this%case_name//new_line_char// &
              "    expected : "//to_string(this%output)//new_line_char// &
              "    actual   : "//to_string(actual)
    end function failure_message
end module doublifyTestParameter
