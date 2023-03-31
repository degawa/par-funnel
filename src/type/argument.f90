module type_argument
    implicit none
    private
    public :: argument

    !>This user-defined type contains argument name,
    !>and the intended use is to specify additional information
    !>for arguments appeared in the list of test parameter.
    type, public :: argument_type
        character(:), public, allocatable :: name
            !! argument's name
    contains
        procedure, public, pass :: construct
        !* constructs the `argument_type` instance
        final :: finalize
        !* finalizes the `argument_type` instance
    end type argument_type

    !>the interface to simplify the instance creation
    !>by allowing to write `[argument("stat"), ...]`
    !>instead of `[argument_type("stat"), ...]`
    interface argument
        procedure :: construct_argument_type
    end interface
contains
    !>returns a new `argument_type` instance.
    function construct_argument_type(name) result(new_argument)
        implicit none
        character(*), intent(in) :: name
            !! argument's name
        type(argument_type) :: new_argument
            !! a new `argument_type` instance

        call new_argument%construct(name)
    end function construct_argument_type

    !>constructs the `parameterization_spec_type` instance
    !>based on an argument's name.
    subroutine construct(this, name)
        implicit none
        class(argument_type), intent(inout) :: this
            !! passed dummy argument
        character(*), intent(in) :: name
            !! argument's name

        this%name = name
    end subroutine construct

    !>finalize the `argument_type` instance
    !>by deallocating the component `name`.
    subroutine finalize(this)
        implicit none
        type(argument_type), intent(inout) :: this
            !! passed dummy argument
        if (allocated(this%name)) deallocate (this%name)
    end subroutine finalize
end module type_argument
