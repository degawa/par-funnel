module test_argumentsPresence_unitTests_eqvLogicalLogical
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: strings_enclose
    use :: type_argumentsPresence
    implicit none
    private
    public :: eqvLL_should_return_T_when_compare_TT_TT
    public :: eqvLL_should_return_F_when_compare_TT_w_other_than_TT
    public :: eqvLL_should_return_T_when_compare_FT_FT
    public :: eqvLL_should_return_F_when_compare_FT_w_other_than_FT
    public :: eqvLL_should_return_T_when_compare_TF_TF
    public :: eqvLL_should_return_F_when_compare_TF_w_other_than_TF
    public :: eqvLL_should_return_T_when_compare_FF_FF
    public :: eqvLL_should_return_F_when_compare_FF_w_other_than_FF
    public :: eqvLL_should_return_T_when_compare_ndarray_of_the_same_value
    public :: eqvLL_should_return_T_when_compare_zero_size_arrays

contains
    subroutine eqvLL_should_return_T_when_compare_TT_TT(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical, allocatable :: target(:)
        logical :: expected, actual

        call setup(target, expected)

        actual = ([.true., .true.] == target)
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))

        call teardown(target)
    contains
        !
        subroutine setup(target, expected)
            logical, intent(out) :: expected
            logical, allocatable, intent(out) :: target(:)

            target = [.true., .true.]
            expected = .true.
        end subroutine setup
        !
        subroutine teardown(target)
            logical, allocatable, intent(inout) :: target(:)
            deallocate (target)
        end subroutine teardown
    end subroutine eqvLL_should_return_T_when_compare_TT_TT

    subroutine eqvLL_should_return_F_when_compare_TT_w_other_than_TT(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: expected, actual

        call setup(expected)

        !---
        actual = ([.true., .true.] == [.false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .true.] == [.true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .true.] == [.false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        !---
        actual = ([.true., .true.] == [.true., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .true.] == [.false., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .true.] == [.true., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .true.] == [.false., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        !---
        actual = ([.true., .true.] == [.true., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .true.] == [.false., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .true.] == [.true., .false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .true.] == [.false., .false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        call teardown()
    contains
        !
        subroutine setup(expected)
            logical, intent(out) :: expected

            expected = .false.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvLL_should_return_F_when_compare_TT_w_other_than_TT

    subroutine eqvLL_should_return_T_when_compare_FT_FT(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical, allocatable :: target(:)
        logical :: expected, actual

        call setup(target, expected)

        actual = ([.false., .true.] == target)
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))

        call teardown(target)
    contains
        !
        subroutine setup(target, expected)
            logical, intent(out) :: expected
            logical, allocatable, intent(out) :: target(:)

            target = [.false., .true.]
            expected = .true.
        end subroutine setup
        !
        subroutine teardown(target)
            logical, allocatable, intent(inout) :: target(:)
            deallocate (target)
        end subroutine teardown
    end subroutine eqvLL_should_return_T_when_compare_FT_FT

    subroutine eqvLL_should_return_F_when_compare_FT_w_other_than_FT(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: expected, actual

        call setup(expected)

        !---
        actual = ([.false., .true.] == [.false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .true.] == [.true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .true.] == [.true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        !---
        actual = ([.false., .true.] == [.false., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .true.] == [.false., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .true.] == [.true., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .true.] == [.true., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        !---
        actual = ([.false., .true.] == [.false., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .true.] == [.false., .false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .true.] == [.true., .false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .true.] == [.true., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        call teardown()
    contains
        !
        subroutine setup(expected)
            logical, intent(out) :: expected

            expected = .false.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvLL_should_return_F_when_compare_FT_w_other_than_FT

    subroutine eqvLL_should_return_T_when_compare_TF_TF(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical, allocatable :: target(:)
        logical :: expected, actual

        call setup(target, expected)

        actual = ([.true., .false.] == target)
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))

        call teardown(target)
    contains
        !
        subroutine setup(target, expected)
            logical, intent(out) :: expected
            logical, allocatable, intent(out) :: target(:)

            target = [.true., .false.]
            expected = .true.
        end subroutine setup
        !
        subroutine teardown(target)
            logical, allocatable, intent(inout) :: target(:)
            deallocate (target)
        end subroutine teardown
    end subroutine eqvLL_should_return_T_when_compare_TF_TF

    subroutine eqvLL_should_return_F_when_compare_TF_w_other_than_TF(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: expected, actual

        call setup(expected)

        !---
        actual = ([.true., .false.] == [.false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .false.] == [.true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .false.] == [.false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        !---
        actual = ([.true., .false.] == [.true., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .false.] == [.false., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .false.] == [.true., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .false.] == [.false., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        !---
        actual = ([.true., .false.] == [.true., .false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .false.] == [.false., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .false.] == [.true., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .false.] == [.false., .false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        call teardown()
    contains
        !
        subroutine setup(expected)
            logical, intent(out) :: expected

            expected = .false.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvLL_should_return_F_when_compare_TF_w_other_than_TF

    subroutine eqvLL_should_return_T_when_compare_FF_FF(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical, allocatable :: target(:)
        logical :: expected, actual

        call setup(target, expected)

        actual = ([.false., .false.] == target)
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))

        call teardown(target)
    contains
        !
        subroutine setup(target, expected)
            logical, intent(out) :: expected
            logical, allocatable, intent(out) :: target(:)

            target = [.false., .false.]
            expected = .true.
        end subroutine setup
        !
        subroutine teardown(target)
            logical, allocatable, intent(inout) :: target(:)
            deallocate (target)
        end subroutine teardown
    end subroutine eqvLL_should_return_T_when_compare_FF_FF

    subroutine eqvLL_should_return_F_when_compare_FF_w_other_than_FF(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: expected, actual

        call setup(expected)

        !---
        actual = ([.false., .false.] == [.false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .false.] == [.true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .false.] == [.false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        !---
        actual = ([.false., .false.] == [.false., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .false.] == [.false., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .false.] == [.true., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .false.] == [.false., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        !---
        actual = ([.false., .false.] == [.false., .false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .false.] == [.false., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .false.] == [.true., .false., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .false.] == [.false., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))
        if (occurred(error)) return

        call teardown()
    contains
        !
        subroutine setup(expected)
            logical, intent(out) :: expected

            expected = .false.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvLL_should_return_F_when_compare_FF_w_other_than_FF

    subroutine eqvLL_should_return_T_when_compare_ndarray_of_the_same_value(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: expected, actual

        call setup(expected)

        actual = ([.true.] == &
                  [.true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "//"but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .false.] == &
                  [.true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "//"but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .false., .true.] == &
                  [.true., .false., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "//"but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.true., .true., .true., .false.] == &
                  [.true., .true., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "//"but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .true., .true., .true., .true.] == &
                  [.false., .true., .true., .true., .true.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "//"but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .true., .true., .true., .true., .false.] == &
                  [.false., .true., .true., .true., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "//"but got "//to_string(actual))
        if (occurred(error)) return

        actual = ([.false., .true., .true., .true., .true., .true., .false.] == &
                  [.false., .true., .true., .true., .true., .true., .false.])
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "//"but got "//to_string(actual))
        if (occurred(error)) return

        call teardown()
    contains
        !
        subroutine setup(expected)
            logical, intent(out) :: expected

            expected = .true.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvLL_should_return_T_when_compare_ndarray_of_the_same_value

    subroutine eqvLL_should_return_T_when_compare_zero_size_arrays(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical, allocatable :: array1(:), array2(:)
        logical :: expected, actual

        call setup(array1, array2, expected)

        ! gfortran rises error when expr `[logical ::] == array2` is used.
        actual = (array1 == array2)
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))

        call teardown()
    contains
        !
        subroutine setup(array1, array2, expected)
            logical, allocatable, intent(out) :: array1(:), array2(:)
            logical, intent(out) :: expected

            array1 = [logical ::]
            array2 = [logical ::]
            expected = .true.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvLL_should_return_T_when_compare_zero_size_arrays
end module test_argumentsPresence_unitTests_eqvLogicalLogical
