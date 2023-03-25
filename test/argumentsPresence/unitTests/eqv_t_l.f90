module test_argumentsPresence_unitTests_eqvLogical
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: strings_enclose
    use :: type_argumentsPresence
    implicit none
    private
    public :: eqvTL_should_return_T_when_compare_TT_TT
    public :: eqvTL_should_return_F_when_compare_TT_w_other_than_TT
    public :: eqvTL_should_return_T_when_compare_FT_FT
    public :: eqvTL_should_return_F_when_compare_FT_w_other_than_FT
    public :: eqvTL_should_return_T_when_compare_TF_TF
    public :: eqvTL_should_return_F_when_compare_TF_w_other_than_TF
    public :: eqvTL_should_return_T_when_compare_FF_FF
    public :: eqvTL_should_return_F_when_compare_FF_w_other_than_FF
    public :: eqvTL_should_return_T_when_compare_w_ndarray_of_the_same_value
    public :: eqvTL_should_return_T_when_compare_w_zero_size_array

contains
    subroutine eqvTL_should_return_T_when_compare_TT_TT(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        logical, allocatable :: target(:)
        logical :: expected, actual

        call setup(pres, target, expected)

        actual = (pres.has.target)
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))

        call teardown(target)
    contains
        !
        subroutine setup(pres, target, expected)
            type(arguments_presence_type), intent(out) :: pres
            logical, intent(out) :: expected
            logical, allocatable, intent(out) :: target(:)

            call pres%construct([.true., .true.])
            target = [.true., .true.]
            expected = .true.
        end subroutine setup
        !
        subroutine teardown(target)
            logical, allocatable, intent(inout) :: target(:)
            deallocate (target)
        end subroutine teardown
    end subroutine eqvTL_should_return_T_when_compare_TT_TT

    subroutine eqvTL_should_return_F_when_compare_TT_w_other_than_TT(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        logical :: expected, actual

        call setup(pres, expected)

        !---
        actual = (pres.has. [.false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        !---
        actual = (pres.has. [.true., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.true., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        !---
        actual = (pres.has. [.true., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.true., .false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        call teardown()
    contains
        !
        subroutine setup(pres, expected)
            type(arguments_presence_type), intent(out) :: pres
            logical, intent(out) :: expected

            call pres%construct([.true., .true.])
            expected = .false.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvTL_should_return_F_when_compare_TT_w_other_than_TT

    subroutine eqvTL_should_return_T_when_compare_FT_FT(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        logical, allocatable :: target(:)
        logical :: expected, actual

        call setup(pres, target, expected)

        actual = (pres.has.target)
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))

        call teardown(target)
    contains
        !
        subroutine setup(pres, target, expected)
            type(arguments_presence_type), intent(out) :: pres
            logical, intent(out) :: expected
            logical, allocatable, intent(out) :: target(:)

            call pres%construct([.false., .true.])
            target = [.false., .true.]
            expected = .true.
        end subroutine setup
        !
        subroutine teardown(target)
            logical, allocatable, intent(inout) :: target(:)
            deallocate (target)
        end subroutine teardown
    end subroutine eqvTL_should_return_T_when_compare_FT_FT

    subroutine eqvTL_should_return_F_when_compare_FT_w_other_than_FT(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        logical :: expected, actual

        call setup(pres, expected)

        !---
        actual = (pres.has. [.true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        !---
        actual = (pres.has. [.false., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.true., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.true., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        !---
        actual = (pres.has. [.false., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.true., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.true., .false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        call teardown()
    contains
        !
        subroutine setup(pres, expected)
            type(arguments_presence_type), intent(out) :: pres
            logical, intent(out) :: expected

            call pres%construct([.false., .true.])
            expected = .false.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvTL_should_return_F_when_compare_FT_w_other_than_FT

    subroutine eqvTL_should_return_T_when_compare_TF_TF(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        logical, allocatable :: target(:)
        logical :: expected, actual

        call setup(pres, target, expected)

        actual = (pres.has.target)
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))

        call teardown(target)
    contains
        !
        subroutine setup(pres, target, expected)
            type(arguments_presence_type), intent(out) :: pres
            logical, intent(out) :: expected
            logical, allocatable, intent(out) :: target(:)

            call pres%construct([.true., .false.])
            target = [.true., .false.]
            expected = .true.
        end subroutine setup
        !
        subroutine teardown(target)
            logical, allocatable, intent(inout) :: target(:)
            deallocate (target)
        end subroutine teardown
    end subroutine eqvTL_should_return_T_when_compare_TF_TF

    subroutine eqvTL_should_return_F_when_compare_TF_w_other_than_TF(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        logical :: expected, actual

        call setup(pres, expected)

        !---
        actual = (pres.has. [.true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        !---
        actual = (pres.has. [.true., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.true., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        !---
        actual = (pres.has. [.true., .false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.true., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        call teardown()
    contains
        !
        subroutine setup(pres, expected)
            type(arguments_presence_type), intent(out) :: pres
            logical, intent(out) :: expected

            call pres%construct([.true., .false.])
            expected = .false.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvTL_should_return_F_when_compare_TF_w_other_than_TF

    subroutine eqvTL_should_return_T_when_compare_FF_FF(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        logical, allocatable :: target(:)
        logical :: expected, actual

        call setup(pres, target, expected)

        actual = (pres.has.target)
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))

        call teardown(target)
    contains
        !
        subroutine setup(pres, target, expected)
            type(arguments_presence_type), intent(out) :: pres
            logical, intent(out) :: expected
            logical, allocatable, intent(out) :: target(:)

            call pres%construct([.false., .false.])
            target = [.false., .false.]
            expected = .true.
        end subroutine setup
        !
        subroutine teardown(target)
            logical, allocatable, intent(inout) :: target(:)
            deallocate (target)
        end subroutine teardown
    end subroutine eqvTL_should_return_T_when_compare_FF_FF

    subroutine eqvTL_should_return_F_when_compare_FF_w_other_than_FF(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        logical :: expected, actual

        call setup(pres, expected)

        !---
        actual = (pres.has. [.true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        !---
        actual = (pres.has. [.false., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.true., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.true., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        !---
        actual = (pres.has. [.false., .false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.true., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.false., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres.has. [.true., .false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        call teardown()
    contains
        !
        subroutine setup(pres, expected)
            type(arguments_presence_type), intent(out) :: pres
            logical, intent(out) :: expected

            call pres%construct([.false., .false.])
            expected = .false.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvTL_should_return_F_when_compare_FF_w_other_than_FF

    subroutine eqvTL_should_return_T_when_compare_w_ndarray_of_the_same_value(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres(7)
        logical :: expected, actual

        call setup(pres, expected)

        actual = (pres(1) .has. &
                  [.true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "//"but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres(2) .has. &
                  [.true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "//"but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres(3) .has. &
                  [.true., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "//"but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres(4) .has. &
                  [.true., .true., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "//"but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres(5) .has. &
                  [.false., .true., .true., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "//"but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres(6) .has. &
                  [.false., .true., .true., .true., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "//"but got "//to_string(actual))
        if (occurred(error)) return

        actual = (pres(7) .has. &
                  [.false., .true., .true., .true., .true., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "//"but got "//to_string(actual))
        if (occurred(error)) return

        call teardown()
    contains
        !
        subroutine setup(pres, expected)
            type(arguments_presence_type), intent(out) :: pres(:)
            logical, intent(out) :: expected

            call pres(1)%construct([.true.])
            call pres(2)%construct([.true., .false.])
            call pres(3)%construct([.true., .false., .true.])
            call pres(4)%construct([.true., .true., .true., .false.])
            call pres(5)%construct([.false., .true., .true., .true., .true.])
            call pres(6)%construct([.false., .true., .true., .true., .true., .false.])
            call pres(7)%construct([.false., .true., .true., .true., .true., .true., .false.])
            expected = .true.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvTL_should_return_T_when_compare_w_ndarray_of_the_same_value

    subroutine eqvTL_should_return_T_when_compare_w_zero_size_array(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        logical, allocatable :: target(:)
        logical :: expected, actual

        call setup(pres, target, expected)

        ! gfortran rises error when expr `[logical ::] == array2` is used.
        actual = (pres.has.target)
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))

        call teardown()
    contains
        !
        subroutine setup(pres, target, expected)
            type(arguments_presence_type), intent(out) :: pres
            logical, allocatable, intent(out) :: target(:)
            logical, intent(out) :: expected

            call pres%construct([logical ::])
            target = [logical ::]
            expected = .true.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvTL_should_return_T_when_compare_w_zero_size_array
end module test_argumentsPresence_unitTests_eqvLogical
