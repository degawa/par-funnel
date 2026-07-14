module doublifyTestParameter
    use, intrinsic :: iso_fortran_env
    use :: par_funnel
    implicit none
    private
    public :: new_doublify_test_parameter

    !>An extension of `test_parameter_type` that holds
    !>the arguments and expected results as components.
    !>`setup` is overridden to read the values from the namelists.
    type, public, extends(test_parameter_type) :: doublify_test_parameter_type
        integer(int32), public :: input
        integer(int32), public :: output
    contains
        procedure, public, pass :: setup
        procedure, public, pass :: teardown
        final :: finalize
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

    !>reads the arguments and expected results into the components
    !>using the simplified API `get`.
    subroutine setup(this)
        implicit none
        class(doublify_test_parameter_type), intent(inout) :: this

        call this%get("input", this%input)
        call this%get("output", this%output)

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
    !>`arguments_namelist` and `expected_namelist`
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
end module doublifyTestParameter
