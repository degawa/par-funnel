module test_testResults_unitTests_checkEqual
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: type_testParameter
    use :: type_testResults
    implicit none
    private
    public :: checkEqual_should_store_success_when_values_are_equal
    public :: checkEqual_should_compose_failure_message_automatically
    public :: checkEqual_should_compare_reals_based_on_tolerance
    public :: checkEqual_should_compare_logical_and_character_values

contains
    subroutine checkEqual_should_store_success_when_values_are_equal(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [new_test_parameter(arguments="input=1", expected="output=2")]
        results = new_test_results_for(params)

        call results%check_equal(1, actual=2, expected=2)

        call check(error, results%all_cases_successful(), &
                   "the test case should be successful when actual == expected")
        if (occurred(error)) return
    end subroutine checkEqual_should_store_success_when_values_are_equal

    subroutine checkEqual_should_compose_failure_message_automatically(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results
        character(:), allocatable :: message

        params = [new_test_parameter(arguments="input=1", expected="output=2")]
        results = new_test_results_for(params)

        call results%check_equal(1, actual=3, expected=2, &
                                 test_parameter=params(1), quantity="output")

        call check(error, results%get_number_of_failed_cases() == 1, &
                   "the test case should fail when actual /= expected")
        if (occurred(error)) return

        message = results%get_failure_message_of(1)
        call check(error, message == "output: expected 2, but got 3 (arguments: input=1)", &
                   "unexpected failure message: "//message)
        if (occurred(error)) return
    end subroutine checkEqual_should_compose_failure_message_automatically

    subroutine checkEqual_should_compare_reals_based_on_tolerance(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [new_test_parameter(arguments="input=1", expected="output=2"), &
                  new_test_parameter(arguments="input=1", expected="output=2")]
        results = new_test_results_for(params)

        call results%check_equal(1, actual=1.0_real64 + 1d-14, expected=1.0_real64, &
                                 tolerance=1d-10)
        call results%check_equal(2, actual=1.0_real64 + 1d-14, expected=1.0_real64)

        call check(error, results%get_success_status_of(1) .eqv. .true., &
                   "the test case should be successful when |actual-expected| <= tolerance")
        if (occurred(error)) return

        call check(error, results%get_success_status_of(2) .eqv. .false., &
                   "the test case should fail on the exact comparison without tolerance")
        if (occurred(error)) return
    end subroutine checkEqual_should_compare_reals_based_on_tolerance

    subroutine checkEqual_should_compare_logical_and_character_values(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results
        character(:), allocatable :: message

        params = [new_test_parameter(arguments="input=1", expected="output=2"), &
                  new_test_parameter(arguments="input=1", expected="output=2")]
        results = new_test_results_for(params)

        call results%check_equal(1, actual=.true., expected=.false.)
        call results%check_equal(2, actual="abc", expected="abd")

        message = results%get_failure_message_of(1)
        call check(error, message == "expected F, but got T", &
                   "unexpected failure message for logical: "//message)
        if (occurred(error)) return

        message = results%get_failure_message_of(2)
        call check(error, message == "expected 'abd', but got 'abc'", &
                   "unexpected failure message for character: "//message)
        if (occurred(error)) return
    end subroutine checkEqual_should_compare_logical_and_character_values
end module test_testResults_unitTests_checkEqual
