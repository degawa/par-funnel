module test_argumentsPresence_unitTests_newArgumentsPresence
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: type_argumentsPresence
    implicit none
    private
    public :: argsPres_should_return_arguments_presence_type_instance
    public :: argsPres_should_return_instance_that_having_presence_status

contains
    subroutine argsPres_should_return_arguments_presence_type_instance(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres

        call check(error, same_type_as(arguments_presence([logical ::]), pres), &
                   "expected 'arguments_presence_type' instance, but got not it")
    end subroutine argsPres_should_return_arguments_presence_type_instance

    subroutine argsPres_should_return_instance_that_having_presence_status(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(arguments_presence_type) :: pres
        type(arguments_presence_type) :: expected
        logical, allocatable :: source(:)
        logical :: actual

        call setup(15, source, expected)

        pres = arguments_presence(source)
        actual = (pres == expected)
        call check(error, actual, &
                   "expected "//to_string(.true.)//", "// &
                   "but got "//to_string(actual))

        call teardown()
    contains
        !
        subroutine setup(dim, source, expected)
            integer(int32), intent(in) :: dim
            logical, allocatable, intent(out) :: source(:)
            type(arguments_presence_type), intent(out) :: expected

            real(real32), allocatable :: rand(:)

            allocate (source(dim))
            allocate (rand(dim))
            call random_number(rand)

            source = (rand > 0.5)
            call expected%construct(source)
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine argsPres_should_return_instance_that_having_presence_status
end module test_argumentsPresence_unitTests_newArgumentsPresence
