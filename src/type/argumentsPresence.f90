module type_argumentsPresence
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: arguments_presence
    public :: operator(.has.)
    public :: operator(==)

    !>This user-defined type contains the presence status of arguments
    !>in the arguments namelist,
    !>and the intended use is to check the presence status of arguments
    !>and branch procedure calls within a unit test.
    type, public :: arguments_presence_type
        logical, private, allocatable :: presented(:)
            !! presence status of optional arguments in the namelist
    contains
        procedure, public, pass :: construct
        !* constructs the `arguments_presence_type` instance
        final :: finalize
        !* finalizes the `arguments_presence_type` instance

        procedure, public, pass :: get_number_of_presence_statuses
        !* gets the number of statuses.

        procedure, public, pass :: eqv_type
        !* gets the equality between a `arguments_presence_type` and a `arguments_presence_type`
        procedure, public, pass :: eqv_logical
        !* gets the equality between a `arguments_presence_type` and a logical array
        procedure, public, pass(rhs) :: eqv_logical_type
        !* gets the equality between a logical array and a `arguments_presence_type`
    end type arguments_presence_type

    interface arguments_presence
        procedure :: construct_arguments_presence_type
    end interface

    !>overloading `==` operator
    !>
    !>@note to work with ifort, `generic :: operator(==)=>eqv_type` is not used.
    interface operator(==)
        procedure :: eqv_type
        procedure :: eqv_logical_logical
        procedure :: eqv_logical_type
    end interface

    !>declare `.has.` operator
    !>
    !>@note to work with ifort, `generic :: operator(.has.)=>eqv_logical` is not used.
    interface operator(.has.)
        procedure :: eqv_logical
    end interface

contains
    !>returns a new `arguments_presence_type` instance.
    pure function construct_arguments_presence_type(presented) result(new_present_arguments)
        implicit none
        logical, intent(in) :: presented(:)
            !! presence status of optional arguments written in a namelist
        type(arguments_presence_type) :: new_present_arguments
            !! new `arguments_presence_type` instance

        call new_present_arguments%construct(presented)
    end function construct_arguments_presence_type

    !>constructs the `arguments_presence_type` instance
    !>based on the presence statuses of optional arguments in the namelist
    pure subroutine construct(this, presented)
        implicit none
        class(arguments_presence_type), intent(inout) :: this
            !! passed dummy argument
        logical, intent(in) :: presented(:)
            !! presence statuses of optional arguments

        allocate (this%presented, source=presented)
    end subroutine construct

    !>finalize the `arguments_presence_type` instance
    !>by deallocating `presented` component.
    pure subroutine finalize(this)
        implicit none
        type(arguments_presence_type), intent(inout) :: this
            !! passed dummy argument
        if (allocated(this%presented)) deallocate (this%presented)
    end subroutine finalize

    !>returns the number of presence statuses managed by the instance.
    pure function get_number_of_presence_statuses(this) result(num)
        implicit none
        class(arguments_presence_type), intent(in) :: this
            !! passed dummy argument
        integer(int32) :: num
            !! the number of statuses

        if (allocated(this%presented)) then
            num = size(this%presented)
        else
            num = 0
        end if
    end function get_number_of_presence_statuses

    !>returns `.true.` if all the values of component `presented`
    !>are equivalent and `.false.` otherwise.
    !>
    !>This function is referenced via the overloaded operator `==`
    pure logical function eqv_type(lhs, rhs)
        implicit none
        class(arguments_presence_type), intent(in) :: lhs
            !! passed dummy argument<br>
            !! the left-hand side of the operator
        class(arguments_presence_type), intent(in) :: rhs
            !! the right-hand side of the operator

        if (are_different_shape(lhs%presented, rhs%presented)) then
            eqv_type = .false.
            return
        end if

        eqv_type = all(lhs%presented .eqv. rhs%presented)
    end function eqv_type

    !>returns `.true.` if all the values of components `presented`
    !>and logical array are equivalent and `.false.` otherwise.
    !>
    !>This function is referenced via the overloaded operator `.has.`
    pure logical function eqv_logical(lhs, rhs)
        implicit none
        class(arguments_presence_type), intent(in) :: lhs
            !! passed dummy argument<br>
            !! the left-hand side of the operator
        logical, intent(in) :: rhs(:)
            !! the right-hand side of the operator

        if (are_different_shape(lhs%presented, rhs)) then
            eqv_logical = .false.
            return
        end if

        eqv_logical = all(lhs%presented .eqv. rhs)
    end function eqv_logical

    !>This function is referenced via the overloaded operator `.has.`
    pure logical function eqv_logical_type(lhs, rhs)
        implicit none
        logical, intent(in) :: lhs(:)
            !! the right-hand side of the operator
        class(arguments_presence_type), intent(in) :: rhs
            !! passed dummy argument<br>
            !! the left-hand side of the operator

        eqv_logical_type = (rhs.has.lhs)
    end function eqv_logical_type

    !>returns `.true.` if two logical arrays are equivalent
    !>and `.false.` otherwise.
    !>
    !>This function is referenced via the overloaded operator `==`
    pure logical function eqv_logical_logical(lhs, rhs)
        implicit none
        logical, intent(in) :: lhs(:)
            !! the left-hand side of the operator
        logical, intent(in) :: rhs(:)
            !! the right-hand side of the operator

        if (are_different_shape(lhs, rhs)) then
            eqv_logical_logical = .false.
            return
        end if

        eqv_logical_logical = all(lhs .eqv. rhs)
    end function eqv_logical_logical

    !------------------------------------------------------------------!
    !>returns `.true.` if `a` and `b` have different shapes.
    pure logical function are_different_shape(a, b)
        implicit none
        logical, intent(in) :: a(:)
        logical, intent(in) :: b(:)

        are_different_shape = any(shape(a) /= shape(b))
    end function are_different_shape
end module type_argumentsPresence
