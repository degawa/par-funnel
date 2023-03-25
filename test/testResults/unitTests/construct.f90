module test_testResults_unitTests_construct
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: type_testParameter
    use :: type_testResults
    implicit none
    private
    public :: construct_should_allocate_test_equal_to_num_params

contains
    subroutine construct_should_allocate_test_equal_to_num_params(error)
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

        call teardown
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
        subroutine teardown()
        end subroutine teardown
    end subroutine construct_should_allocate_test_equal_to_num_params
end module test_testResults_unitTests_construct
