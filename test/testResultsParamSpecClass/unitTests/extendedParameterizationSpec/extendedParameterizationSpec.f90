module test_testResultsParamSpecClass_unitTests_extendedParamSpec
    use, intrinsic :: iso_fortran_env
    use :: type_parameterizationSpec
    use :: type_argument
    use :: type_testParameter
    implicit none
    private
    public :: new_extended_parameterization_spec

    type, public, extends(parameterization_spec_type) :: extended_parameterization_spec_type
    end type extended_parameterization_spec_type

    interface new_extended_parameterization_spec
        procedure :: construct_extended_parameterization_spec_type
    end interface
contains
    pure function construct_extended_parameterization_spec_type(parameter_cases, &
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
        type(extended_parameterization_spec_type) :: new_spec
            !! new `parameterization_spec_type` instance

        call new_spec%construct(parameter_cases, optional_args, replace_new_line)
    end function construct_extended_parameterization_spec_type
end module test_testResultsParamSpecClass_unitTests_extendedParamSpec
