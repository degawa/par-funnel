module type_string_type_checkTest
    implicit none
    private

    type, public :: string_type
        character(:), allocatable :: val
    end type string_type
end module type_string_type_checkTest

module test_testResults_unitTests_checkTest
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string
    use :: type_string_type_checkTest
    use :: type_testParameter
    use :: type_testResults
    implicit none
    private
    public :: checkTest_should_assign_test_status_and_message

contains
    subroutine checkTest_should_assign_test_status_and_message(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results
        logical, allocatable :: expected_status(:)
        type(string_type), allocatable :: expected_message(:)

        logical :: actual_status
        character(:), allocatable :: actual_message

        integer(int32) :: case

        call setup(5, params, results, expected_status, expected_message)

        do case = 1, results%get_number_of_test_cases()
            call results%check_test(case, expected_status(case), expected_message(case)%val)
        end do

        do case = 1, results%get_number_of_test_cases()
            actual_status = results%get_success_status_of(case)
            actual_message = results%get_failure_message_of(case)

            call check(error, actual_status .eqv. expected_status(case), &
                       "expected "//to_string(expected_status(case))// &
                       ", but got "//to_string(actual_status))
            if (occurred(error)) return

            call check(error, actual_message == expected_message(case)%val, &
                       "expected "//expected_message(case)%val// &
                       ", but got "//actual_message)
            if (occurred(error)) return
        end do

        call teardown(params, expected_status, expected_message, actual_message)
    contains
        !
        subroutine setup(num_param, params, results, expected_status, expected_message)
            integer(int32), intent(in) :: num_param
            type(test_parameter_type), allocatable, intent(out) :: params(:)
            type(test_results_type), intent(out) :: results
            logical, allocatable, intent(out) :: expected_status(:)
            type(string_type), allocatable, intent(out) :: expected_message(:)

            allocate (params(num_param))
            call results%construct(params)

            block
                real(real32), allocatable :: rand(:)
                integer(int32) :: case

                allocate (rand(num_param))
                call random_number(rand)

                expected_status = [rand > 0.5]
                do case = 1, num_param
                    if (expected_status(case)) then
                        expected_message(case)%val = "case "//to_string(case)//" success"
                    else
                        expected_message(case)%val = "case "//to_string(case)//" failed"
                    end if
                end do
            end block
        end subroutine setup
        !
        subroutine teardown(params, expected_status, expected_message, actual_message)
            type(test_parameter_type), allocatable, intent(inout) :: params(:)
            logical, allocatable, intent(inout) :: expected_status(:)
            type(string_type), allocatable, intent(inout) :: expected_message(:)
            character(:), allocatable, intent(inout) :: actual_message

            deallocate (params)
            deallocate (expected_status)
            deallocate (expected_message)
            deallocate (actual_message)
        end subroutine teardown
    end subroutine checkTest_should_assign_test_status_and_message
end module test_testResults_unitTests_checkTest
