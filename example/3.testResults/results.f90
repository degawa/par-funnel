module function_sample_under_test
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: int_to_str

    interface int_to_str
        procedure :: to_string_int32
    end interface
contains
    function to_string_int32(i32, fmt, less_digits) result(str)
        implicit none
        integer(int32), intent(in) :: i32
        character(*), intent(in), optional :: fmt
        logical, intent(out), optional :: less_digits

        character(:), allocatable :: str

        character(32) :: buffer

        if (present(fmt)) then
            write (buffer, fmt) i32
        else
            write (buffer, '(I0)') i32
        end if

        str = trim(adjustl(buffer))

        if (present(less_digits)) then
            less_digits = (str(1:1) == "*")
        end if
    end function to_string_int32
end module function_sample_under_test

program main
    use, intrinsic :: iso_fortran_env
    use :: function_sample_under_test
    use :: par_funnel
    implicit none

    type(test_parameter_type), allocatable :: params(:)
    type(test_results_type) :: results

    params = [ &
             new_test_parameter(arguments="input=1000", &
                                expected="string='1000'") &
             , new_test_parameter(arguments="input=1000 fmt='(I4)'", &
                                  expected="string='1000'") &
             , new_test_parameter(arguments="input=1000 fmt='(I2)'", &
                                  expected="string='**'") &
             , new_test_parameter(arguments="input=1000 fmt='(I2)'", & ! intentionally wrong expectation
                                  expected="string='1000'") & ! to show a failure message
             ]

    ! `test_results_type` stores the result of each test case.
    ! It is constructed from the test parameters
    ! and refers only to the number of test cases.
    results = new_test_results_for(params)

    call run_test_cases(params, results)

    ! `test_results_type` provides procedures for summarizing the results:
    ! - `get_number_of_test_cases()`/`get_number_of_failed_cases()`
    ! - `get_success_status_of(case)`/`get_failure_message_of(case)`
    ! - `all_cases_successful()`
    ! - `get_summary_message()`
    print '(A,I0,A,I0)', " failed/total: ", results%get_number_of_failed_cases(), &
        "/", results%get_number_of_test_cases()
    print *, results%get_summary_message()
contains
    subroutine run_test_cases(params, results)
        implicit none
        type(test_parameter_type), intent(in) :: params(:)
        type(test_results_type), intent(inout) :: results

        integer(int32) :: case
        integer(int32) :: input
        character(:), allocatable :: fmt, exp_string, act_string
        logical :: fmt_found

        do case = 1, results%get_number_of_test_cases()
            call params(case)%get("input", input)
            call params(case)%get("fmt", fmt, found=fmt_found)
            call params(case)%get("string", exp_string)

            select case (params(case)%presence_pattern("fmt"))
            case ("A"); act_string = int_to_str(input)
            case ("P"); act_string = int_to_str(input, fmt)
            end select

            ! `check_equal` stores the comparison result to `results`
            ! with an automatically composed failure message.
            call results%check_equal(case, act_string, exp_string, &
                                     test_parameter=params(case), quantity="string")
        end do
    end subroutine run_test_cases
    !|### Execution result
    !```console
    ! failed/total: 1/4
    ! 1 test case(s) failed
    !case 4: string: expected '1000', but got '**' (arguments: input=1000 fmt='(I2)')
    !```
end program main
