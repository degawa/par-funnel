module test_testResults_unitTests_construct
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: type_testParameter
    use :: type_parameterizationSpec
    use :: type_testResults
    implicit none
    private
    public :: constructParams_should_allocate_test_equal_to_num_params
    public :: constructSpec_should_allocate_test_equal_to_num_params

contains
    subroutine constructParams_should_allocate_test_equal_to_num_params(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results
        integer(int32) :: expected_num_test
        integer(int32) :: actual

        call setup(12, params, expected_num_test)

        call results%construct(params)
        actual = results%get_number_of_test_cases()

        call check(error, actual == expected_num_test, &
                   "expected "//to_string(expected_num_test)// &
                   ", but got "//to_string(actual))

        call teardown(params)
    contains
        !
        subroutine setup(num_param, params, expected)
            integer(int32), intent(in) :: num_param
            type(test_parameter_type), allocatable, intent(out) :: params(:)
            integer(int32), intent(out) :: expected

            allocate (params(num_param))
            expected = size(params)
        end subroutine setup
        !
        subroutine teardown(params)
            type(test_parameter_type), allocatable, intent(inout) :: params(:)
            deallocate (params)
        end subroutine teardown
    end subroutine constructParams_should_allocate_test_equal_to_num_params

    subroutine constructSpec_should_allocate_test_equal_to_num_params(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec
        type(test_results_type) :: results

        integer(int32) :: expected_num_test
        integer(int32) :: actual

        call setup(9, spec, expected_num_test)

        call results%construct(spec)
        actual = results%get_number_of_test_cases()

        call check(error, actual == expected_num_test, &
                   "expected "//to_string(expected_num_test)// &
                   ", but got "//to_string(actual))

        call teardown(spec)
    contains
        !
        subroutine setup(num_param, spec, expected)
            integer(int32), intent(in) :: num_param
            type(parameterization_spec_type), intent(out) :: spec
            integer(int32), intent(out) :: expected

            type(test_parameter_type), allocatable :: params(:)

            allocate (params(num_param))
            spec = new_parameterization_spec(params)
            expected = size(params)

            deallocate (params)
        end subroutine setup
        !
        subroutine teardown(spec)
            type(parameterization_spec_type), intent(inout) :: spec
            call spec%destruct()
        end subroutine teardown
    end subroutine constructSpec_should_allocate_test_equal_to_num_params
end module test_testResults_unitTests_construct
