
module test_testResultsParamSpecClass_unitTests_extendedTestParam
    use, intrinsic :: iso_fortran_env
    use :: type_testParameter
    implicit none
    private
    public :: new_extended_test_parameter

    type, public, extends(test_parameter_type) :: extended_test_parameter_type
        integer(int32) :: arg
        integer(int32) :: retval
    contains
        procedure, public, pass :: setup
        procedure, public, pass :: teardown
    end type extended_test_parameter_type

    interface new_extended_test_parameter
        procedure :: construct_ext_test_param
    end interface
contains
    function construct_ext_test_param(arguments, expected) result(test_parameter)
        implicit none
        character(*), intent(in) :: arguments
        character(*), intent(in) :: expected
        type(extended_test_parameter_type) :: test_parameter

        call test_parameter%construct(arguments, expected)
    end function construct_ext_test_param

    subroutine setup(this)
        implicit none
        class(extended_test_parameter_type), intent(inout) :: this

        this%arg = 1
        this%retval = 2
    end subroutine setup

    subroutine teardown(this)
        implicit none
        class(extended_test_parameter_type), intent(inout) :: this

        this%arg = 0
        this%retval = 0
    end subroutine teardown
end module test_testResultsParamSpecClass_unitTests_extendedTestParam
