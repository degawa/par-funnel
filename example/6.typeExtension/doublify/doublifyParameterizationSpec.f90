module doublifyParameterizationSpec
    use, intrinsic :: iso_fortran_env
    use :: par_funnel
    use :: doublifyTestParameter
    implicit none
    private
    public :: new_doublify_parameterization_spec
    public :: new_doublify_test_parameter

    type, public, extends(parameterization_spec_type) :: doublify_parameterization_spec_type
    contains
        final :: finalize
        procedure, public, pass :: run_test_cases
    end type doublify_parameterization_spec_type

    interface new_doublify_parameterization_spec
        procedure :: construct_parameterization_spec_type
    end interface
contains
    pure function construct_parameterization_spec_type(parameter_cases, &
                                                       optional_args, &
                                                       replace_new_line) result(new_spec)
        implicit none
        class(test_parameter_type), intent(in) :: parameter_cases(:)
            !! list of test parameters
        type(argument_type), intent(in), optional :: optional_args(:)
            !! list of arguments name having the optional attribute
        logical, intent(in), optional :: replace_new_line
            !! if `.true.`, replace new-line char (`new_line()`) in namelists
            !! with new-line mark ("\n")
        type(doublify_parameterization_spec_type) :: new_spec
            !! new `doublify_parameterization_spec_type` instance

        call new_spec%construct(parameter_cases, optional_args, replace_new_line)
    end function construct_parameterization_spec_type

    !>finalize the `parameterization_spec_type` instance
    !>by deallocating `test_parameters` and `optional_args`.
    pure subroutine finalize(this)
        implicit none
        type(doublify_parameterization_spec_type), intent(inout) :: this
            !! passed dummy argument
        call this%destruct()
    end subroutine finalize

    function run_test_cases(this) result(results)
        use :: function_doublify
        implicit none
        class(doublify_parameterization_spec_type), intent(inout) :: this

        type(test_results_type) :: results

        integer(int32) :: case
        class(test_parameter_type), allocatable :: param

        integer(int32) :: actual

        results = new_test_results_for(this)

        do case = 1, this%get_number_of_test_cases()
            ! param = this%get_test_parameter_in(case) ! assignment automatically reallocates param
            allocate (param, source=this%get_test_parameter_in(case))

            select type (param); type is (doublify_test_parameter_type)
                call param%setup()

                ! run the procedure under test with a parameter
                ! and check the result
                actual = doublify(param%input)
                call results%check_test(case, actual == param%output, param%failure_message(actual=actual))

                call param%teardown()
            end select

            deallocate (param)
        end do
    end function run_test_cases
end module doublifyParameterizationSpec
