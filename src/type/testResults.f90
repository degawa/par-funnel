module type_testResults
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    character(*), public, parameter :: &
        failure_message_if_test_is_successful = "test passed"

    !>This private type contains logical status of a test result
    !>and a message when the test fails
    !>
    !>intended use is to declare the type of the collection
    !>that contains the test results.
    type, private :: test_result_type
        logical, private :: success_status
            !! status of test result.
            !! `.true.` if the test is successful.
        character(:), private, allocatable :: message
            !! failure message.
            !! not allocated if test is successful.
    end type test_result_type

    !>This type contains test results.
    !>
    !>@note
    !>This type has array as the component
    !>as opposed to declaring `test_parameter_type` as arrays.
    !>
    !>for test results, this style is more convenient
    !>because it is necessary to count the number of failed tests
    !>and to gather the results of all tests.
    !>@endnote
    type, public :: test_results_type
        type(test_result_type), private, allocatable :: test(:)
            !! results of test case
    contains
        procedure, public, pass :: construct
        !* constructs the `test_results_type` instance

        procedure, public, pass :: check_test
        !* check result of the conditional formula of a test case

        procedure, public, pass :: get_number_of_test_cases
        !* get number of test cases
        procedure, public, pass :: get_number_of_failed_cases
        !* get number of failed test cases
        procedure, public, pass :: get_failure_message_of
        !* get failure message of a test case
        procedure, public, pass :: get_success_status_of
        !* get success status of a test case
        procedure, public, pass :: get_success_statuses
        !* get success statuses of all tesst cases
        procedure, public, pass :: get_failure_statuses
        !* get failure statuses of all tesst cases
        procedure, public, pass :: append_failure_messages_to
        !* append failure message to new line of a string
        procedure, public, pass :: all_cases_successful
        !* get status of all test cases are passed
        procedure, public, pass :: get_summary_message
        !* get summary
    end type test_results_type

contains
    !>constructs  the `test_results_type` instance.
    !>each element of `test_result_type` collection is not initialized.
    pure subroutine construct(this, test_parameters)
        use :: type_testParameter
        implicit none
        class(test_results_type), intent(inout) :: this
            !! passed dummy argument
        type(test_parameter_type), intent(in) :: test_parameters(:)
            !! test parameter type.
            !! referring only to the array dimension

        allocate (this%test(size(test_parameters)))
    end subroutine construct

    !>check result of the conditional formula of a test case
    !>and store it to an element of `test_result_type` collection.
    pure subroutine check_test(this, case, condition, message)
        implicit none
        class(test_results_type), intent(inout) :: this
            !! passed dummy argument
        integer(int32), intent(in) :: case
            !! test case number
        logical, intent(in) :: condition
            !! result of the conditional formula
            !! for test success/failure evaluation
        character(*), intent(in) :: message
            !! failure message

        this%test(case)%success_status = condition
        this%test(case)%message = message
    end subroutine check_test

    !>returns number of test cases.
    pure function get_number_of_test_cases(this) result(num)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument
        integer(int32) :: num
            !! number of test cases

        num = size(this%test)
    end function get_number_of_test_cases

    !>returns number of failed test cases.
    pure function get_number_of_failed_cases(this) result(num)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument
        integer(int32) :: num
            !! number of failed test cases

        num = count(this%get_failure_statuses())
    end function get_number_of_failed_cases

    !>returns logical array where **success cases are `.true.`**
    pure function get_success_statuses(this) result(stat)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument
        logical, allocatable :: stat(:)
            !! logical statuses of test results

        stat = this%test(:)%success_status
    end function get_success_statuses

    !>returns logical array where **failed cases are `.true.`**
    pure function get_failure_statuses(this) result(stat)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument
        logical, allocatable :: stat(:)
            !! logical statuses of test results

        stat = .not. this%get_success_statuses()
    end function get_failure_statuses

    !>returns result of a test case
    !>in logical value with success as `.true.`
    pure function get_success_status_of(this, case) result(stat)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument
        integer(int32), intent(in) :: case
            !! test case number
        logical :: stat
            !! logical status (`.true.` for success)

        stat = this%test(case)%success_status
    end function get_success_status_of

    !>returns failure message
    !>and returns "test passed" if a test is successful.
    pure function get_failure_message_of(this, case) result(message)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument
        integer(int32), intent(in) :: case
            !! test case number
        character(:), allocatable :: message
            !! message

        if (this%get_success_status_of(case)) then
            message = failure_message_if_test_is_successful
        else
            message = this%test(case)%message
        end if
    end function get_failure_message_of

    !>appends failure messages of failed test case.
    pure subroutine append_failure_messages_to(this, message)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument
        character(:), allocatable, intent(inout) :: message
            !! stirng to be appended with new line

        integer(int32) :: case
        character(12) :: case_str

        do concurrent(case=1:this%get_number_of_test_cases(), &
                      this%get_success_status_of(case) .eqv. .false.)

            write (case_str, '(I0)') case
            message = message//new_line(" ")// &
                      "case "//trim(case_str)//": "//this%get_failure_message_of(case)

        end do
    end subroutine append_failure_messages_to

    !>returns `.true.` if all test cases are successful.
    pure logical function all_cases_successful(this)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument

        all_cases_successful = all(this%get_success_statuses())
    end function all_cases_successful

    !>retunrs summary of test caes
    !>including the number of failure test cases and failure messages.
    pure function get_summary_message(this) result(message)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument
        character(:), allocatable :: message
            !! stirng to be appended with new line

        character(12) :: str_num_failure_test
        write (str_num_failure_test, '(I0)') this%get_number_of_failed_cases()

        message = trim(str_num_failure_test)//" test case(s) failed"
        call this%append_failure_messages_to(message)
    end function get_summary_message
end module type_testResults
