module test_argumentsPresence_unitTests_eqvType
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: strings_enclose
    use :: type_argumentsPresence
    implicit none
    private
    public :: eqvTT_should_return_T_when_compare_TT_TT
    public :: eqvTT_should_return_F_when_compare_TT_w_other_than_TT
    public :: eqvTT_should_return_T_when_compare_FT_FT
    public :: eqvTT_should_return_F_when_compare_FT_w_other_than_FT
    public :: eqvTT_should_return_T_when_compare_TF_TF
    public :: eqvTT_should_return_F_when_compare_TF_w_other_than_TF
    public :: eqvTT_should_return_T_when_compare_FF_FF
    public :: eqvTT_should_return_F_when_compare_FF_w_other_than_FF
    public :: eqvTT_should_return_T_when_compare_w_ndarray_of_the_same_value
    public :: eqvTT_should_return_T_when_compare_zero_size_statuses

contains
    subroutine eqvTT_should_return_T_when_compare_TT_TT(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        type(arguments_presence_type) :: target
        logical :: expected, actual

        call setup(pres, target, expected)

        actual = (pres == target)
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))

        call teardown()
    contains
        !
        subroutine setup(pres, target, expected)
            type(arguments_presence_type), intent(out) :: pres
            type(arguments_presence_type), intent(out) :: target
            logical, intent(out) :: expected

            call pres%construct([.true., .true.])
            call target%construct([.true., .true.])
            expected = .true.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvTT_should_return_T_when_compare_TT_TT

    subroutine eqvTT_should_return_F_when_compare_TT_w_other_than_TT(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        type(arguments_presence_type) :: target(11)
        logical :: expected, actual
        integer(int32) :: case

        call setup(pres, target, expected)

        !---
        do case = 1, size(target)
            actual = (pres == target(case))
            call check(error, actual .eqv. expected, &
                       "expected "//to_string(expected)//", "// &
                       "but got "//to_string(actual))
            if (occurred(error)) return
        end do

        call teardown()
    contains
        !
        subroutine setup(pres, target, expected)
            type(arguments_presence_type), intent(out) :: pres
            type(arguments_presence_type), intent(out) :: target(11)
            logical, intent(out) :: expected

            call pres%construct([.true., .true.])
            call target(1)%construct([.false., .true.])
            call target(2)%construct([.true., .false.])
            call target(3)%construct([.false., .false.])
            call target(4)%construct([.true., .true., .true.])
            call target(5)%construct([.false., .true., .true.])
            call target(6)%construct([.true., .false., .true.])
            call target(7)%construct([.false., .false., .true.])
            call target(8)%construct([.true., .true., .false.])
            call target(9)%construct([.false., .true., .false.])
            call target(10)%construct([.true., .false., .false.])
            call target(11)%construct([.false., .false., .false.])
            expected = .false.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvTT_should_return_F_when_compare_TT_w_other_than_TT

    subroutine eqvTT_should_return_T_when_compare_FT_FT(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        type(arguments_presence_type) :: target
        logical :: expected, actual

        call setup(pres, target, expected)

        actual = (pres == target)
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))

        call teardown()
    contains
        !
        subroutine setup(pres, target, expected)
            type(arguments_presence_type), intent(out) :: pres
            type(arguments_presence_type), intent(out) :: target
            logical, intent(out) :: expected

            call pres%construct([.false., .true.])
            call target%construct([.false., .true.])
            expected = .true.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvTT_should_return_T_when_compare_FT_FT

    subroutine eqvTT_should_return_F_when_compare_FT_w_other_than_FT(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        type(arguments_presence_type) :: target(11)
        logical :: expected, actual
        integer(int32) :: case

        call setup(pres, target, expected)

        !---
        do case = 1, size(target)
            actual = (pres == target(case))
            call check(error, actual .eqv. expected, &
                       "expected "//to_string(expected)//", "// &
                       "but got "//to_string(actual))
            if (occurred(error)) return
        end do

        call teardown()
    contains
        !
        subroutine setup(pres, target, expected)
            type(arguments_presence_type), intent(out) :: pres
            type(arguments_presence_type), intent(out) :: target(11)
            logical, intent(out) :: expected

            call pres%construct([.false., .true.])
            call target(1)%construct([.true., .true.])
            call target(2)%construct([.true., .false.])
            call target(3)%construct([.false., .false.])
            call target(4)%construct([.false., .true., .true.])
            call target(5)%construct([.true., .true., .true.])
            call target(6)%construct([.true., .false., .true.])
            call target(7)%construct([.false., .false., .true.])
            call target(8)%construct([.false., .true., .false.])
            call target(9)%construct([.true., .true., .false.])
            call target(10)%construct([.true., .false., .false.])
            call target(11)%construct([.false., .false., .false.])
            expected = .false.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvTT_should_return_F_when_compare_FT_w_other_than_FT

    subroutine eqvTT_should_return_T_when_compare_TF_TF(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        type(arguments_presence_type) :: target
        logical :: expected, actual

        call setup(pres, target, expected)

        actual = (pres == target)
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))

        call teardown()
    contains
        !
        subroutine setup(pres, target, expected)
            type(arguments_presence_type), intent(out) :: pres
            type(arguments_presence_type), intent(out) :: target
            logical, intent(out) :: expected

            call pres%construct([.true., .false.])
            call target%construct([.true., .false.])
            expected = .true.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvTT_should_return_T_when_compare_TF_TF

    subroutine eqvTT_should_return_F_when_compare_TF_w_other_than_TF(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        type(arguments_presence_type) :: target(11)
        logical :: expected, actual
        integer(int32) :: case

        call setup(pres, target, expected)

        !---
        do case = 1, size(target)
            actual = (pres == target(case))
            call check(error, actual .eqv. expected, &
                       "expected "//to_string(expected)//", "// &
                       "but got "//to_string(actual))
            if (occurred(error)) return
        end do

        call teardown()
    contains
        !
        subroutine setup(pres, target, expected)
            type(arguments_presence_type), intent(out) :: pres
            type(arguments_presence_type), intent(out) :: target(11)
            logical, intent(out) :: expected

            call pres%construct([.true., .false.])
            call target(1)%construct([.true., .true.])
            call target(2)%construct([.false., .true.])
            call target(3)%construct([.false., .false.])
            call target(4)%construct([.true., .true., .true.])
            call target(5)%construct([.false., .true., .true.])
            call target(6)%construct([.true., .false., .true.])
            call target(7)%construct([.false., .false., .true.])
            call target(8)%construct([.true., .true., .false.])
            call target(9)%construct([.false., .true., .false.])
            call target(10)%construct([.true., .false., .false.])
            call target(11)%construct([.false., .false., .false.])
            expected = .false.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvTT_should_return_F_when_compare_TF_w_other_than_TF

    subroutine eqvTT_should_return_T_when_compare_FF_FF(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        type(arguments_presence_type) :: target
        logical :: expected, actual

        call setup(pres, target, expected)

        actual = (pres == target)
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))

        call teardown()
    contains
        !
        subroutine setup(pres, target, expected)
            type(arguments_presence_type), intent(out) :: pres
            type(arguments_presence_type), intent(out) :: target
            logical, intent(out) :: expected

            call pres%construct([.false., .false.])
            call target%construct([.false., .false.])
            expected = .true.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvTT_should_return_T_when_compare_FF_FF

    subroutine eqvTT_should_return_F_when_compare_FF_w_other_than_FF(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        type(arguments_presence_type) :: target(11)
        logical :: expected, actual
        integer(int32) :: case

        call setup(pres, target, expected)

        !---
        do case = 1, size(target)
            actual = (pres == target(case))
            call check(error, actual .eqv. expected, &
                       "expected "//to_string(expected)//", "// &
                       "but got "//to_string(actual))
            if (occurred(error)) return
        end do

        call teardown()
    contains
        !
        subroutine setup(pres, target, expected)
            type(arguments_presence_type), intent(out) :: pres
            type(arguments_presence_type), intent(out) :: target(11)
            logical, intent(out) :: expected

            call pres%construct([.false., .false.])
            call target(1)%construct([.true., .true.])
            call target(2)%construct([.false., .true.])
            call target(3)%construct([.true., .false.])
            call target(4)%construct([.true., .true., .true.])
            call target(5)%construct([.false., .true., .true.])
            call target(6)%construct([.true., .false., .true.])
            call target(7)%construct([.false., .false., .true.])
            call target(8)%construct([.true., .true., .false.])
            call target(9)%construct([.false., .true., .false.])
            call target(10)%construct([.true., .false., .false.])
            call target(11)%construct([.false., .false., .false.])
            expected = .false.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvTT_should_return_F_when_compare_FF_w_other_than_FF

    subroutine eqvTT_should_return_T_when_compare_w_ndarray_of_the_same_value(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres(15), target(15)
        logical :: expected, actual
        integer(int32) :: case

        call setup(pres, target, expected)

        do case = 1, size(pres)
            actual = (pres(case) == target(case))
            call check(error, actual .eqv. expected, &
                       "expected "//to_string(expected)//", "//"but got "//to_string(actual))
            if (occurred(error)) return
        end do

        call teardown()
    contains
        !
        subroutine setup(pres, target, expected)
            type(arguments_presence_type), intent(out) :: pres(:), target(:)
            logical, intent(out) :: expected
            logical, allocatable :: literal(:)
            integer(int32) :: case

            literal = [logical ::]
            do case = 1, size(pres)
                literal = [literal, (mod(case, 2) == 0)]
                call pres(case)%construct(literal)
                call target(case)%construct(literal)
            end do
            expected = .true.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvTT_should_return_T_when_compare_w_ndarray_of_the_same_value

    subroutine eqvTT_should_return_T_when_compare_zero_size_statuses(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        type(arguments_presence_type) :: target
        logical :: expected, actual

        call setup(pres, target, expected)

        actual = (pres == target)
        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)//", "// &
                   "but got "//to_string(actual))

        call teardown()
    contains
        !
        subroutine setup(pres, target, expected)
            type(arguments_presence_type), intent(out) :: pres
            type(arguments_presence_type), intent(out) :: target
            logical, intent(out) :: expected

            call pres%construct([logical ::])
            call target%construct([logical ::])
            expected = .true.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine eqvTT_should_return_T_when_compare_zero_size_statuses
end module test_argumentsPresence_unitTests_eqvType
