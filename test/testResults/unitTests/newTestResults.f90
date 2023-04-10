module test_testResults_unitTests_newTestResults
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string
    use :: type_testParameter
    use :: type_parameterizationSpec
    use :: type_testResults
    implicit none
    private
    public :: newTestResParam_should_return_test_results_type_instance
    public :: newTestResSpec_should_return_test_results_type_instance
    public :: newTestResParam_should_return_instance_with_same_num_as_cases
    public :: newTestResSpec_should_return_instance_with_same_num_as_cases

contains
    subroutine newTestResParam_should_return_test_results_type_instance(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_results_type) :: results

        call check(error, &
                   same_type_as(new_test_results_for([new_test_parameter("", "")]), &
                                results), &
                   "expected 'test_results_type' instance, but got not it")
    end subroutine newTestResParam_should_return_test_results_type_instance

    subroutine newTestResSpec_should_return_test_results_type_instance(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_results_type) :: results

        call check(error, &
                   same_type_as(new_test_results_for(new_parameterization_spec( &
                                                     [new_test_parameter("", "")])), &
                                results), &
                   "expected 'test_results_type' instance, but got not it")
    end subroutine newTestResSpec_should_return_test_results_type_instance

    subroutine newTestResParam_should_return_instance_with_same_num_as_cases(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_results_type) :: results
        type(test_parameter_type), allocatable :: params(:)
        integer(int32) :: num_cases, num_exp

        params = [new_test_parameter("", ""), &
                  new_test_parameter("", ""), &
                  new_test_parameter("", "")]

        results = new_test_results_for(params)

        num_cases = results%get_number_of_test_cases()
        num_exp = size(params)
        call check(error, &
                   num_cases == num_exp, &
                   "expected "//to_string(num_exp)//", but got "//to_string(num_cases))
    end subroutine newTestResParam_should_return_instance_with_same_num_as_cases

    subroutine newTestResSpec_should_return_instance_with_same_num_as_cases(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_results_type) :: results
        type(parameterization_spec_type) :: spec
        integer(int32) :: num_cases, num_exp

        spec = new_parameterization_spec([new_test_parameter("", ""), &
                                          new_test_parameter("", ""), &
                                          new_test_parameter("", "")])

        results = new_test_results_for(spec)

        num_cases = results%get_number_of_test_cases()
        num_exp = spec%get_number_of_test_cases()
        call check(error, &
                   num_cases == num_exp, &
                   "expected "//to_string(num_exp)//", but got "//to_string(num_cases))
    end subroutine newTestResSpec_should_return_instance_with_same_num_as_cases
end module test_testResults_unitTests_newTestResults
