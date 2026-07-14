module type_testResults
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: new_test_results_for

    character(*), public, parameter :: &
        failure_message_if_test_case_is_successful = "test passed"

    character(*), public, parameter :: &
        failure_message_if_test_case_is_not_be_checked = "An uncaught error occurred"

    !>This private user-defined type contains
    !>the logical status of a test result
    !>and a message when the test fails,
    !>and the intended use is to compose the type of collection
    !>that stores the results of test cases.
    type, private :: test_case_result_type
        logical, private :: success_status = .false.
            !! a status of a test case result.
            !! `.true.` if the test case is successful.
        character(:), private, allocatable :: failure_message
            !! a failure message.
            !! not allocated if test case is successful or not checked.
    contains
        final :: finalize_test_case_result
        !* finalize the `test_case_result_type` instance
    end type test_case_result_type

    !>This user-defined type contains test case results.
    !>
    !>@note
    !>This type has an array as the component
    !>instead of declaring `test_parameter_type` as an array.
    !>
    !>This style is more convenient for handling test results
    !>because it is necessary to count the number of failed tests
    !>and gather the results of all tests.
    !>@endnote
    type, public :: test_results_type
        type(test_case_result_type), private, allocatable :: test(:)
            !! results of test case
    contains
        procedure, public, pass :: construct_params
        !* constructs the `test_results_type` instance
        procedure, public, pass :: construct_spec
        !* constructs the `test_results_type` instance
        procedure, public, pass :: destruct
        !* destructs the `test_results_type` instance
        final :: finalize
        !* finalize the `test_results_type` instance
        generic :: construct => construct_params, construct_spec

        procedure, public, pass :: check_test
        !* checks the result of the conditional formula in a test case

        procedure, private, pass :: check_equal_int32
        procedure, private, pass :: check_equal_int64
        procedure, private, pass :: check_equal_real32
        procedure, private, pass :: check_equal_real64
        procedure, private, pass :: check_equal_logical
        procedure, private, pass :: check_equal_char
        generic, public :: check_equal => check_equal_int32, check_equal_int64, &
            check_equal_real32, check_equal_real64, &
            check_equal_logical, check_equal_char
        !* compares an actual value with an expected value
        ! and stores the result with an automatically composed failure message

        procedure, public, pass :: get_number_of_test_cases
        !* gets the number of test cases
        procedure, public, pass :: get_number_of_failed_cases
        !* gets the number of failed test cases
        procedure, public, pass :: get_failure_message_of
        !* gets the failure message of a test case
        procedure, public, pass :: get_success_status_of
        !* gets the success status of a test case
        procedure, public, pass :: get_success_statuses
        !* gets success statuses of all test cases
        procedure, public, pass :: get_failure_statuses
        !* gets failure statuses of all test cases
        procedure, public, pass :: append_failure_messages_to
        !* appends a failure message to the new line of a string
        procedure, public, pass :: all_cases_successful
        !* gets the status of all test cases succeeded
        procedure, public, pass :: get_summary_message
        !* gets summary
    end type test_results_type

    interface new_test_results_for
        procedure :: construct_results_type_param
        procedure :: construct_results_type_spec
    end interface

contains
    !>returns a new `test_results_type` instance.
    pure function construct_results_type_param(test_parameters) result(new_test_results)
        use :: type_testParameter
        implicit none
        class(test_parameter_type), intent(in) :: test_parameters(:)
            !! test parameter type.
            !! referred only to the array dimension.
        type(test_results_type) :: new_test_results

        call new_test_results%construct(test_parameters)
    end function construct_results_type_param

    !>returns a new `test_results_type` instance.
    pure function construct_results_type_spec(spec) result(new_test_results)
        use :: type_parameterizationSpec
        implicit none
        class(parameterization_spec_type), intent(in) :: spec
            !! parameterization specification.
            !! referred only to the number of test cases.
        type(test_results_type) :: new_test_results

        call new_test_results%construct(spec)
    end function construct_results_type_spec

    !>constructs the `test_results_type` instance.
    !>
    !>@note Each element of the `test_result_type` collection is not initialized.
    pure subroutine construct_params(this, test_parameters)
        use :: type_testParameter
        implicit none
        class(test_results_type), intent(inout) :: this
            !! passed dummy argument
        type(test_parameter_type), intent(in) :: test_parameters(:)
            !! test parameter type.
            !! referred only to the array dimension.

        allocate (this%test(size(test_parameters)))
    end subroutine construct_params

    !>constructs the `test_results_type` instance.
    !>
    !>@note Each element of the `test_result_type` collection is not initialized.
    pure subroutine construct_spec(this, spec)
        use :: type_parameterizationSpec
        implicit none
        class(test_results_type), intent(inout) :: this
            !! passed dummy argument
        type(parameterization_spec_type), intent(in) :: spec
            !! parameterization specification.
            !! referred only to the number of test cases.

        allocate (this%test(spec%get_number_of_test_cases()))
    end subroutine construct_spec

    !>destructs the `test_results_type` instance
    !>by deallocating the `test` component.
    pure subroutine destruct(this)
        implicit none
        class(test_results_type), intent(inout) :: this
            !! passed dummy argument
        if (allocated(this%test)) deallocate (this%test)
    end subroutine destruct

    !>finalizes the `test_results_type` instance.
    pure subroutine finalize(this)
        implicit none
        type(test_results_type), intent(inout) :: this
            !! passed dummy argument
        call this%destruct()
    end subroutine finalize

    !>checks the result for the conditional formula of a test case
    !>and stores it to an element of the `test_result_type` collection.
    pure subroutine check_test(this, case, condition, message)
        implicit none
        class(test_results_type), intent(inout) :: this
            !! passed dummy argument
        integer(int32), intent(in) :: case
            !! a test case number
        logical, intent(in) :: condition
            !! the result of the conditional formula
            !! for test success/failure evaluation
        character(*), intent(in) :: message
            !! the failure message

        this%test(case)%success_status = condition
        this%test(case)%failure_message = message
    end subroutine check_test

    !------------------------------------------------------------------!
    ! check_equal
    !
    ! The check_equal procedures compare an actual value
    ! with an expected value and store the result of a test case
    ! with an automatically composed failure message like
    !
    !     output: expected 2, but got 3 (arguments: input=1)
    !
    ! so that users do not need to compose failure messages.
    !------------------------------------------------------------------!

    !>compares `integer(int32)` values and stores the result of a test case
    !>with an automatically composed failure message.
    pure subroutine check_equal_int32(this, case, actual, expected, test_parameter, quantity)
        use :: type_testParameter
        implicit none
        class(test_results_type), intent(inout) :: this
            !! passed dummy argument
        integer(int32), intent(in) :: case
            !! a test case number
        integer(int32), intent(in) :: actual
            !! the actual value
        integer(int32), intent(in) :: expected
            !! the expected value
        class(test_parameter_type), intent(in), optional :: test_parameter
            !! the test parameter of the test case.
            !! the arguments are appended to the failure message when passed
        character(*), intent(in), optional :: quantity
            !! the name of the quantity to be checked.
            !! prepended to the failure message when passed

        call store_result(this, case, (actual == expected), &
                          to_string(actual), to_string(expected), &
                          arguments_in(test_parameter), quantity)
    end subroutine check_equal_int32

    !>compares `integer(int64)` values and stores the result of a test case
    !>with an automatically composed failure message.
    pure subroutine check_equal_int64(this, case, actual, expected, test_parameter, quantity)
        use :: type_testParameter
        implicit none
        class(test_results_type), intent(inout) :: this
            !! passed dummy argument
        integer(int32), intent(in) :: case
            !! a test case number
        integer(int64), intent(in) :: actual
            !! the actual value
        integer(int64), intent(in) :: expected
            !! the expected value
        class(test_parameter_type), intent(in), optional :: test_parameter
            !! the test parameter of the test case.
            !! the arguments are appended to the failure message when passed
        character(*), intent(in), optional :: quantity
            !! the name of the quantity to be checked.
            !! prepended to the failure message when passed

        character(32) :: act_buf, exp_buf

        write (act_buf, '(I0)') actual
        write (exp_buf, '(I0)') expected
        call store_result(this, case, (actual == expected), &
                          trim(act_buf), trim(exp_buf), &
                          arguments_in(test_parameter), quantity)
    end subroutine check_equal_int64

    !>compares `real(real32)` values and stores the result of a test case
    !>with an automatically composed failure message.
    !>
    !>The values are compared exactly when `tolerance` is not passed
    !>and compared based on `|actual - expected| <= tolerance` when passed.
    pure subroutine check_equal_real32(this, case, actual, expected, test_parameter, quantity, tolerance)
        use :: type_testParameter
        implicit none
        class(test_results_type), intent(inout) :: this
            !! passed dummy argument
        integer(int32), intent(in) :: case
            !! a test case number
        real(real32), intent(in) :: actual
            !! the actual value
        real(real32), intent(in) :: expected
            !! the expected value
        class(test_parameter_type), intent(in), optional :: test_parameter
            !! the test parameter of the test case.
            !! the arguments are appended to the failure message when passed
        character(*), intent(in), optional :: quantity
            !! the name of the quantity to be checked.
            !! prepended to the failure message when passed
        real(real32), intent(in), optional :: tolerance
            !! the tolerance of the comparison

        logical :: condition
        character(32) :: act_buf, exp_buf

        if (present(tolerance)) then
            condition = (abs(actual - expected) <= tolerance)
        else
            condition = (actual == expected)
        end if

        write (act_buf, '(g0)') actual
        write (exp_buf, '(g0)') expected
        call store_result(this, case, condition, &
                          trim(act_buf), trim(exp_buf), &
                          arguments_in(test_parameter), quantity)
    end subroutine check_equal_real32

    !>compares `real(real64)` values and stores the result of a test case
    !>with an automatically composed failure message.
    !>
    !>The values are compared exactly when `tolerance` is not passed
    !>and compared based on `|actual - expected| <= tolerance` when passed.
    pure subroutine check_equal_real64(this, case, actual, expected, test_parameter, quantity, tolerance)
        use :: type_testParameter
        implicit none
        class(test_results_type), intent(inout) :: this
            !! passed dummy argument
        integer(int32), intent(in) :: case
            !! a test case number
        real(real64), intent(in) :: actual
            !! the actual value
        real(real64), intent(in) :: expected
            !! the expected value
        class(test_parameter_type), intent(in), optional :: test_parameter
            !! the test parameter of the test case.
            !! the arguments are appended to the failure message when passed
        character(*), intent(in), optional :: quantity
            !! the name of the quantity to be checked.
            !! prepended to the failure message when passed
        real(real64), intent(in), optional :: tolerance
            !! the tolerance of the comparison

        logical :: condition
        character(32) :: act_buf, exp_buf

        if (present(tolerance)) then
            condition = (abs(actual - expected) <= tolerance)
        else
            condition = (actual == expected)
        end if

        write (act_buf, '(g0)') actual
        write (exp_buf, '(g0)') expected
        call store_result(this, case, condition, &
                          trim(act_buf), trim(exp_buf), &
                          arguments_in(test_parameter), quantity)
    end subroutine check_equal_real64

    !>compares `logical` values and stores the result of a test case
    !>with an automatically composed failure message.
    pure subroutine check_equal_logical(this, case, actual, expected, test_parameter, quantity)
        use :: type_testParameter
        implicit none
        class(test_results_type), intent(inout) :: this
            !! passed dummy argument
        integer(int32), intent(in) :: case
            !! a test case number
        logical, intent(in) :: actual
            !! the actual value
        logical, intent(in) :: expected
            !! the expected value
        class(test_parameter_type), intent(in), optional :: test_parameter
            !! the test parameter of the test case.
            !! the arguments are appended to the failure message when passed
        character(*), intent(in), optional :: quantity
            !! the name of the quantity to be checked.
            !! prepended to the failure message when passed

        call store_result(this, case, (actual .eqv. expected), &
                          logical_to_string(actual), logical_to_string(expected), &
                          arguments_in(test_parameter), quantity)
    end subroutine check_equal_logical

    !>compares character values and stores the result of a test case
    !>with an automatically composed failure message.
    !>
    !>The comparison follows the `==` operator for characters,
    !>that is, the shorter one is padded with trailing blanks.
    pure subroutine check_equal_char(this, case, actual, expected, test_parameter, quantity)
        use :: type_testParameter
        implicit none
        class(test_results_type), intent(inout) :: this
            !! passed dummy argument
        integer(int32), intent(in) :: case
            !! a test case number
        character(*), intent(in) :: actual
            !! the actual value
        character(*), intent(in) :: expected
            !! the expected value
        class(test_parameter_type), intent(in), optional :: test_parameter
            !! the test parameter of the test case.
            !! the arguments are appended to the failure message when passed
        character(*), intent(in), optional :: quantity
            !! the name of the quantity to be checked.
            !! prepended to the failure message when passed

        call store_result(this, case, (actual == expected), &
                          "'"//actual//"'", "'"//expected//"'", &
                          arguments_in(test_parameter), quantity)
    end subroutine check_equal_char

    !>composes a failure message from the actual and expected values in strings
    !>and stores the result of a test case.
    pure subroutine store_result(this, case, condition, actual_str, expected_str, arguments, quantity)
        implicit none
        class(test_results_type), intent(inout) :: this
            !! passed dummy argument
        integer(int32), intent(in) :: case
            !! a test case number
        logical, intent(in) :: condition
            !! the result of the comparison
        character(*), intent(in) :: actual_str
            !! the actual value in a string
        character(*), intent(in) :: expected_str
            !! the expected value in a string
        character(*), intent(in) :: arguments
            !! the arguments of the test case.
            !! appended to the failure message when not empty
        character(*), intent(in), optional :: quantity
            !! the name of the quantity to be checked

        character(:), allocatable :: message

        message = "expected "//expected_str//", but got "//actual_str
        if (present(quantity)) then
            message = quantity//": "//message
        end if
        if (len(arguments) > 0) then
            message = message//" (arguments: "//arguments//")"
        end if

        call this%check_test(case, condition, message)
    end subroutine store_result

    !>returns the arguments of the test parameter in a string
    !>and an empty string when `test_parameter` is not passed.
    pure function arguments_in(test_parameter) result(args)
        use :: type_testParameter
        implicit none
        class(test_parameter_type), intent(in), optional :: test_parameter
            !! the test parameter of a test case
        character(:), allocatable :: args
            !! the arguments in a string

        if (present(test_parameter)) then
            args = test_parameter%arguments()
        else
            args = ""
        end if
    end function arguments_in

    !>returns `"T"` when the input is `.true.` and `"F"` otherwise.
    pure function logical_to_string(l) result(str)
        implicit none
        logical, intent(in) :: l
            !! the logical value to be converted
        character(1) :: str
            !! the converted string

        if (l) then
            str = "T"
        else
            str = "F"
        end if
    end function logical_to_string

    !>returns the number of test cases.
    pure function get_number_of_test_cases(this) result(num)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument
        integer(int32) :: num
            !! the number of test cases

        num = size(this%test)
    end function get_number_of_test_cases

    !>returns the number of failed test cases.
    pure function get_number_of_failed_cases(this) result(num)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument
        integer(int32) :: num
            !! the number of failed test cases

        num = count(this%get_failure_statuses())
    end function get_number_of_failed_cases

    !>returns the logical array where **success cases are `.true.`**
    pure function get_success_statuses(this) result(stat)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument
        logical, allocatable :: stat(:)
            !! the logical statuses with the test success as `.true.`

        stat = this%test(:)%success_status
    end function get_success_statuses

    !>returns the logical array where **failed cases are `.true.`**
    pure function get_failure_statuses(this) result(stat)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument
        logical, allocatable :: stat(:)
            !! the logical statuses with the test failed as `.true.`

        stat = .not. this%get_success_statuses()
    end function get_failure_statuses

    !>returns the result of a test case
    !>in logical value with success as `.true.`
    pure function get_success_status_of(this, case) result(stat)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument
        integer(int32), intent(in) :: case
            !! a test case number
        logical :: stat
            !! logical status (`.true.` for success)

        stat = this%test(case)%success_status
    end function get_success_status_of

    !>returns the failure message
    !>and "test passed" if a test is successful.
    pure function get_failure_message_of(this, case) result(message)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument
        integer(int32), intent(in) :: case
            !! a test case number
        character(:), allocatable :: message
            !! the failure message

        if (this%get_success_status_of(case)) then
            message = failure_message_if_test_case_is_successful
        else
            if (allocated(this%test(case)%failure_message)) then
                message = this%test(case)%failure_message
            else
                message = failure_message_if_test_case_is_not_be_checked
            end if
        end if
    end function get_failure_message_of

    !>appends failure messages of failed test cases
    !>to the `message`.
    pure subroutine append_failure_messages_to(this, message)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument
        character(:), allocatable, intent(inout) :: message
            !! the string to be appended with the new line

        integer(int32) :: case

        do concurrent(case=1:this%get_number_of_test_cases(), &
                      this%get_success_status_of(case) .eqv. .false.)

            message = message//new_line(" ")// &
                      "case "//to_string(case)//": "//this%get_failure_message_of(case)

        end do
    end subroutine append_failure_messages_to

    !>returns `.true.` if all test cases are successful.
    pure logical function all_cases_successful(this)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument

        all_cases_successful = all(this%get_success_statuses())
    end function all_cases_successful

    !>returns a summary of test cases,
    !>including the number of failed cases and failure messages.
    pure function get_summary_message(this) result(message)
        implicit none
        class(test_results_type), intent(in) :: this
            !! passed dummy argument
        character(:), allocatable :: message
            !! a summary in string

        message = to_string(this%get_number_of_failed_cases())//" test case(s) failed"
        call this%append_failure_messages_to(message)
    end function get_summary_message

    !------------------------------------------------------------------!
    !>constructs the `test_case_result_type` instance
    !>by deallocating the `failure_message` component.
    pure subroutine finalize_test_case_result(this)
        implicit none
        type(test_case_result_type), intent(inout) :: this
            !! passed dummy argument
        if (allocated(this%failure_message)) deallocate (this%failure_message)
    end subroutine finalize_test_case_result

    !------------------------------------------------------------------!
    !>returns the string converted from the integer.
    pure function to_string(i32) result(str)
        implicit none
        integer(int32), intent(in) :: i32
            !! the integer to be converted
        character(:), allocatable :: str
            !! the string coverted from the integer

        character(12) :: buffer
        write (buffer, '(I0)') i32
        str = trim(adjustl(buffer))
    end function to_string
end module type_testResults
