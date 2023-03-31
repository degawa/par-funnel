module test_ParameterizationSpec_unitTests_newParameterizationSpec
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: strings_enclose
    use :: type_testParameter
    use :: type_parameterizationSpec
    implicit none
    private
    public :: newParaSpec_should_return_parameterization_spec_type_instance

contains
    subroutine newParaSpec_should_return_parameterization_spec_type_instance(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec

        call check(error, &
                   same_type_as(new_parameterization_spec( &
                                parameter_cases=[new_test_parameter("", "")]), &
                                spec), &
                   "expected 'parameterization_spec_type' instance, but got not it")
    end subroutine newParaSpec_should_return_parameterization_spec_type_instance
end module test_ParameterizationSpec_unitTests_newParameterizationSpec
