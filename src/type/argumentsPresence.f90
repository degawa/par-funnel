module type_argumentsPresence
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: arguments_presence
    public :: operator(.has.)
    public :: operator(==)
    public :: to_string

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

        procedure, public, pass :: as_string
        !* gets the presentce statuses in a string

        procedure, public, pass :: as_index
        !* gets the presence statuses as an index (bit pattern),
        ! intended to be used in a `select case` construct

        procedure, public, pass :: as_pattern
        !* gets the presence statuses in a string such as "PA"
        ! (P: present, A: absent),
        ! intended to be used in a `select case` construct

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

    interface to_string
        procedure :: to_string_arg_pres
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

    !>returns the arguments presence statuses in string.
    !>For example,
    !>
    !>- `"[ T T F ]"` when an `arguments_presence_type` instance has [.true. .true. .false.]
    !>- `"[ ]"` when an `arguments_presence_type` instance is not initialized
    !>
    pure function as_string(this) result(str)
        implicit none
        class(arguments_presence_type), intent(in) :: this
            !! passed dummy argument
        character(:), allocatable :: str
            !! presence statues in string

        integer(int32) :: arg

        str = "["
        do arg = 1, this%get_number_of_presence_statuses()
            if (this%presented(arg)) then
                str = str//" T"
            else
                str = str//" F"
            end if
        end do
        str = str//" ]"
    end function as_string

    !>returns the presence statuses as an index (bit pattern)
    !>in that the bit `i-1` is set when the `i`-th status is `.true.`:
    !>
    !>| `presented`          | index |
    !>| :------------------- | ----: |
    !>| `[.false., .false.]` |     0 |
    !>| `[.true. , .false.]` |     1 |
    !>| `[.false., .true. ]` |     2 |
    !>| `[.true. , .true. ]` |     3 |
    !>
    !>The intended use is to branch procedure calls with optional arguments
    !>in a `select case` construct instead of `if` statement chains.
    !>Returns 0 when the instance is not initialized.
    pure function as_index(this) result(idx)
        implicit none
        class(arguments_presence_type), intent(in) :: this
            !! passed dummy argument
        integer(int32) :: idx
            !! the presence statuses as a bit pattern

        integer(int32) :: i

        idx = 0
        do i = 1, this%get_number_of_presence_statuses()
            if (this%presented(i)) idx = idx + 2**(i - 1)
        end do
    end function as_index

    !>returns the presence statuses in a string,
    !>in that the `i`-th character is `P` when the `i`-th status is `.true.`
    !>and `A` when `.false.`:
    !>
    !>| `presented`          | pattern |
    !>| :------------------- | :------ |
    !>| `[.false., .false.]` | `"AA"`  |
    !>| `[.true. , .false.]` | `"PA"`  |
    !>| `[.false., .true. ]` | `"AP"`  |
    !>| `[.true. , .true. ]` | `"PP"`  |
    !>
    !>The intended use is to branch procedure calls with optional arguments
    !>in a `select case` construct.
    !>The case expressions are self-describing, requiring no constants,
    !>and the number of arguments scales without defining 2^n constants.
    !>Returns an empty string when the instance is not initialized.
    pure function as_pattern(this) result(pattern)
        implicit none
        class(arguments_presence_type), intent(in) :: this
            !! passed dummy argument
        character(:), allocatable :: pattern
            !! the presence statuses in a string (P: present, A: absent)

        integer(int32) :: i

        pattern = ""
        do i = 1, this%get_number_of_presence_statuses()
            if (this%presented(i)) then
                pattern = pattern//"P"
            else
                pattern = pattern//"A"
            end if
        end do
    end function as_pattern

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

    !------------------------------------------------------------------!
    pure function to_string_arg_pres(x) result(str)
        implicit none
        type(arguments_presence_type), intent(in) :: x
            !! `arguments_presence_type` instance
        character(:), allocatable :: str

        str = x%as_string()
    end function to_string_arg_pres
end module type_argumentsPresence
