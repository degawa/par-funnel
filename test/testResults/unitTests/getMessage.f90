module type_string_type_getMessage
    implicit none
    private

    type, public :: string_type
        character(:), allocatable :: val
    end type string_type

    character(*), public, parameter :: NL = new_line(" ")
end module type_string_type_getMessage

module test_testResults_unitTests_getMessage
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string
    use :: type_string_type_getMessage
    use :: type_testParameter
    use :: type_testResults
    implicit none
    private
    public :: getFailureMsg_should_return_failure_msg_when_test_case_failed
    public :: getFailureMsg_should_return_msg_that_the_test_case_successful
    public :: getFailureMsg_should_return_msg_that_the_test_case_not_checked
    public :: getSummaryMsg_should_return_failure_msgs_of_failed_cases
    public :: appendFailureMsgs_should_append_failure_msg_to_the_end_of_arg

contains
    subroutine getFailureMsg_should_return_failure_msg_when_test_case_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results
        type(string_type), allocatable :: expected_message(:)

        character(:), allocatable :: actual_message
        integer(int32) :: case
        logical, allocatable :: check_stat(:)

        call setup(9, params, results, check_stat, expected_message)

        do case = 1, 9
            if (check_stat(case)) then

                actual_message = results%get_failure_message_of(case)

                call check(error, len(actual_message) == len(expected_message(case)%val), &
                           "expected "//to_string(len(expected_message(case)%val))// &
                           ", but got "//to_string(len(actual_message)))
                if (occurred(error)) return

                call check(error, actual_message == expected_message(case)%val, &
                           "expected "//expected_message(case)%val// &
                           ", but got "//actual_message)
                if (occurred(error)) return
            end if
        end do
        call teardown(params, check_stat, expected_message, actual_message)
    contains
        !
        subroutine setup(num_param, params, results, check_stat, expected_message)
            integer(int32), intent(in) :: num_param
            type(test_parameter_type), allocatable, intent(out) :: params(:)
            type(test_results_type), intent(out) :: results
            logical, allocatable, intent(out) :: check_stat(:)
            type(string_type), allocatable, intent(out) :: expected_message(:)

            allocate (params(num_param))
            call results%construct(params)
            allocate (expected_message(num_param))

            block
                logical, allocatable :: status(:)
                type(string_type) :: message
                integer(int32) :: case

                status = [(mod(case, 2) == 1, case=1, num_param)]
                do case = 1, num_param
                    if (status(case)) then
                        message%val = "case "//to_string(case)//" success"
                    else
                        message%val = "case "//to_string(case)//" failed"
                    end if

                    call results%check_test(case, status(case), message%val)
                end do
            end block

            check_stat = results%get_failure_statuses()
            expected_message(2)%val = "case 2 failed"
            expected_message(4)%val = "case 4 failed"
            expected_message(6)%val = "case 6 failed"
            expected_message(8)%val = "case 8 failed"
        end subroutine setup
        !
        subroutine teardown(params, check_stat, expected_message, actual_message)
            type(test_parameter_type), allocatable, intent(inout) :: params(:)
            logical, allocatable, intent(inout) :: check_stat(:)
            type(string_type), allocatable, intent(inout) :: expected_message(:)
            character(:), allocatable, intent(inout) :: actual_message

            deallocate (params)
            deallocate (check_stat)
            deallocate (expected_message)
            deallocate (actual_message)
        end subroutine teardown
    end subroutine getFailureMsg_should_return_failure_msg_when_test_case_failed

    subroutine getFailureMsg_should_return_msg_that_the_test_case_successful(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results
        type(string_type), allocatable :: expected_message(:)

        character(:), allocatable :: actual_message
        integer(int32) :: case
        logical, allocatable :: check_stat(:)

        call setup(9, params, results, check_stat, expected_message)

        do case = 1, 9
            if (check_stat(case)) then

                actual_message = results%get_failure_message_of(case)

                call check(error, len(actual_message) == len(expected_message(case)%val), &
                           "expected "//to_string(len(expected_message(case)%val))// &
                           ", but got "//to_string(len(actual_message)))
                if (occurred(error)) return

                call check(error, actual_message == expected_message(case)%val, &
                           "expected "//expected_message(case)%val// &
                           ", but got "//actual_message)
                if (occurred(error)) return
            end if
        end do
        call teardown(params, check_stat, expected_message, actual_message)
    contains
        !
        subroutine setup(num_param, params, results, check_stat, expected_message)
            integer(int32), intent(in) :: num_param
            type(test_parameter_type), allocatable, intent(out) :: params(:)
            type(test_results_type), intent(out) :: results
            logical, allocatable, intent(out) :: check_stat(:)
            type(string_type), allocatable, intent(out) :: expected_message(:)

            allocate (params(num_param))
            call results%construct(params)
            allocate (expected_message(num_param))

            block
                logical, allocatable :: status(:)
                type(string_type) :: message
                integer(int32) :: case

                status = [(mod(case, 2) == 1, case=1, num_param)]
                do case = 1, num_param
                    if (status(case)) then
                        message%val = "case "//to_string(case)//" success"
                    else
                        message%val = "case "//to_string(case)//" failed"
                    end if

                    call results%check_test(case, status(case), message%val)
                end do
            end block

            check_stat = results%get_success_statuses()
            expected_message(1)%val = failure_message_if_test_case_is_successful
            expected_message(3)%val = failure_message_if_test_case_is_successful
            expected_message(5)%val = failure_message_if_test_case_is_successful
            expected_message(7)%val = failure_message_if_test_case_is_successful
            expected_message(9)%val = failure_message_if_test_case_is_successful
        end subroutine setup
        !
        subroutine teardown(params, check_stat, expected_message, actual_message)
            type(test_parameter_type), allocatable, intent(inout) :: params(:)
            logical, allocatable, intent(inout) :: check_stat(:)
            type(string_type), allocatable, intent(inout) :: expected_message(:)
            character(:), allocatable, intent(inout) :: actual_message

            deallocate (params)
            deallocate (check_stat)
            deallocate (expected_message)
            deallocate (actual_message)
        end subroutine teardown
    end subroutine getFailureMsg_should_return_msg_that_the_test_case_successful

    subroutine getFailureMsg_should_return_msg_that_the_test_case_not_checked(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
        !! error handler

        type(test_parameter_type) :: params
        type(test_results_type) :: results
        character(:), allocatable :: expected_message

        character(:), allocatable :: actual_message

        call setup(params, results, expected_message)

        actual_message = results%get_failure_message_of(1)

        call check(error, len(actual_message) == len(expected_message), &
                   "expected "//to_string(len(expected_message))// &
                   ", but got "//to_string(len(actual_message)))
        if (occurred(error)) return

        call check(error, actual_message == expected_message, &
                   "expected "//expected_message// &
                   ", but got "//actual_message)
        if (occurred(error)) return
        call teardown(expected_message, actual_message)
    contains
        !
        subroutine setup(params, results, expected_message)
            type(test_parameter_type), intent(out) :: params
            type(test_results_type), intent(out) :: results
            character(:), allocatable, intent(out) :: expected_message

            call results%construct([params])
            expected_message = failure_message_if_test_case_is_not_be_checked
        end subroutine setup
        !
        subroutine teardown(expected_message, actual_message)
            character(:), allocatable, intent(inout) :: expected_message
            character(:), allocatable, intent(inout) :: actual_message

            deallocate (expected_message)
            deallocate (actual_message)
        end subroutine teardown
    end subroutine getFailureMsg_should_return_msg_that_the_test_case_not_checked

    subroutine getSummaryMsg_should_return_failure_msgs_of_failed_cases(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results
        character(:), allocatable :: expected_message

        character(:), allocatable :: actual_message

        call setup(10, params, results, expected_message)

        actual_message = results%get_summary_message()

        call check(error, len(actual_message) == len(expected_message), &
                   "expected "//to_string(len(expected_message))// &
                   ", but got "//to_string(len(actual_message)))
        if (occurred(error)) return

        call check(error, actual_message == expected_message, &
                   "expected "//expected_message// &
                   ", but got "//actual_message)

        call teardown(params, expected_message, actual_message)
    contains
        !
        subroutine setup(num_param, params, results, expected_message)
            integer(int32), intent(in) :: num_param
            type(test_parameter_type), allocatable, intent(out) :: params(:)
            type(test_results_type), intent(out) :: results
            character(:), allocatable, intent(out) :: expected_message

            allocate (params(num_param))
            call results%construct(params)

            block
                logical, allocatable :: status(:)
                type(string_type) :: message
                integer(int32) :: case

                status = [(mod(case, 2) == 0, case=1, num_param)]
                do case = 1, num_param
                    if (status(case)) then
                        message%val = "case "//to_string(case)//" success"
                    else
                        message%val = "case "//to_string(case)//" failed"
                    end if

                    call results%check_test(case, status(case), message%val)
                end do
            end block

            expected_message = "5 test case(s) failed"//NL// &
                               "case 1: case 1 failed"//NL// &
                               "case 3: case 3 failed"//NL// &
                               "case 5: case 5 failed"//NL// &
                               "case 7: case 7 failed"//NL// &
                               "case 9: case 9 failed"
        end subroutine setup
        !
        subroutine teardown(params, expected_message, actual_message)
            type(test_parameter_type), allocatable, intent(inout) :: params(:)
            character(:), allocatable, intent(inout) :: expected_message
            character(:), allocatable, intent(inout) :: actual_message

            deallocate (params)
            deallocate (expected_message)
            deallocate (actual_message)
        end subroutine teardown
    end subroutine getSummaryMsg_should_return_failure_msgs_of_failed_cases

    subroutine appendFailureMsgs_should_append_failure_msg_to_the_end_of_arg(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results
        character(:), allocatable :: expected_message

        character(:), allocatable :: actual_message

        call setup(10, params, results, "4 test case(s) failed.", expected_message)

        actual_message = "4 test case(s) failed."
        call results%append_failure_messages_to(actual_message)

        call check(error, len(actual_message) == len(expected_message), &
                   "expected "//to_string(len(expected_message))// &
                   ", but got "//to_string(len(actual_message)))
        if (occurred(error)) return

        call check(error, actual_message == expected_message, &
                   "expected "//expected_message// &
                   ", but got "//actual_message)

        call teardown(params, expected_message, actual_message)
    contains
        !
        subroutine setup(num_param, params, results, input_message, expected_message)
            integer(int32), intent(in) :: num_param
            type(test_parameter_type), allocatable, intent(out) :: params(:)
            type(test_results_type), intent(out) :: results
            character(*), intent(in) :: input_message
            character(:), allocatable, intent(out) :: expected_message

            allocate (params(num_param))
            call results%construct(params)

            block
                logical, allocatable :: status(:)
                type(string_type) :: message
                integer(int32) :: case

                status = [(mod(case, 2) == 1, case=1, num_param)]
                do case = 1, num_param
                    if (status(case)) then
                        message%val = "case "//to_string(case)//" success"
                    else
                        message%val = "case "//to_string(case)//" failed"
                    end if

                    call results%check_test(case, status(case), message%val)
                end do
            end block

            expected_message = input_message//NL// &
                               "case 2: case 2 failed"//NL// &
                               "case 4: case 4 failed"//NL// &
                               "case 6: case 6 failed"//NL// &
                               "case 8: case 8 failed"//NL// &
                               "case 10: case 10 failed"
        end subroutine setup
        !
        subroutine teardown(params, expected_message, actual_message)
            type(test_parameter_type), allocatable, intent(inout) :: params(:)
            character(:), allocatable, intent(inout) :: expected_message
            character(:), allocatable, intent(inout) :: actual_message

            deallocate (params)
            deallocate (expected_message)
            deallocate (actual_message)
        end subroutine teardown
    end subroutine appendFailureMsgs_should_append_failure_msg_to_the_end_of_arg
end module test_testResults_unitTests_getMessage
