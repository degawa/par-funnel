module collab_testdrive_unittests
    use :: testdrive, only:error_type, check
    use :: par_funnel
    implicit none
    private
    public :: intToStr_parametarized_tests_covering_optional_arg_combination

contains
    subroutine intToStr_parametarized_tests_covering_optional_arg_combination(error)
        use, intrinsic :: iso_fortran_env
        implicit none
        type(error_type), allocatable, intent(out) :: error

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results

        params = [ &
                 new_test_parameter(arguments="input=000", & ! correct input is input=1000
                                    expected="string='1000'") &
                 , new_test_parameter(arguments="input=1000 fmt='(I4)'", &
                                      expected="string='1000'") &
                 , new_test_parameter(arguments="input=1000 fmt='(I2)'", &
                                      expected="string='**'") &
                 , new_test_parameter(arguments="input=1000 less_digits=true", &
                                      expected="string='1000' less_digits=false") &
                 , new_test_parameter(arguments="input=1000 less_digits=false", &
                                      expected="string='1000' less_digits=false") &
                 , new_test_parameter(arguments="input=000 fmt='(I4)' less_digits=false", & ! correct input is input=1000
                                      expected="string='1000' less_digits=false") &
                 , new_test_parameter(arguments="input=1000 fmt='(I2)' less_digits=false", &
                                      expected="string='**' less_digits=true") &
                 ]
        results = new_test_results_for(params)

        call run_test_cases(params, results)

        ! pass the results to the unit test framework:
        ! the summary message composed by par-funnel
        ! is reported through the framework's check.
        call check(error, results%all_cases_successful(), results%get_summary_message())
    contains
        subroutine run_test_cases(params, results)
            implicit none
            type(test_parameter_type), intent(in) :: params(:)
            type(test_results_type), intent(inout) :: results

            integer(int32) :: case
            integer(int32) :: input
            character(:), allocatable :: fmt, exp_string, act_string
            logical :: exp_less_digits, act_less_digits
            logical :: fmt_found

            do case = 1, results%get_number_of_test_cases()
                call params(case)%get("input", input)
                call params(case)%get("fmt", fmt, found=fmt_found)
                call params(case)%get("string", exp_string)

                select case (params(case)%presence_index("fmt", "less_digits"))
                case (0); act_string = int_to_str(input)
                case (1); act_string = int_to_str(input, fmt)
                case (2); act_string = int_to_str(input, less_digits=act_less_digits)
                case (3); act_string = int_to_str(input, fmt, act_less_digits)
                end select

                if (params(case)%presented("less_digits")) then
                    call params(case)%get("less_digits", exp_less_digits, from="expected")
                    call results%check_equal(case, act_string//merge(" T", " F", act_less_digits), &
                                             exp_string//merge(" T", " F", exp_less_digits), &
                                             test_parameter=params(case), quantity="string and less_digits")
                else
                    call results%check_equal(case, act_string, exp_string, &
                                             test_parameter=params(case), quantity="string")
                end if
            end do
        end subroutine run_test_cases
    end subroutine intToStr_parametarized_tests_covering_optional_arg_combination

    function int_to_str(i32, fmt, less_digits) result(str)
        use, intrinsic :: iso_fortran_env
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
    end function int_to_str
end module collab_testdrive_unittests

module collab_testdrive_collect
    use :: testdrive, only:unittest_type, new_unittest
    use :: collab_testdrive_unittests
    implicit none
    private
    public :: collect

contains
    subroutine collect(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("int_to_str() parametarized tests covering optional arguments combinations", &
                                  intToStr_parametarized_tests_covering_optional_arg_combination, should_fail=.true.) &
                     ]
    end subroutine collect
end module collab_testdrive_collect

program collab_testdrive
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:testsuite_type, new_testsuite
    use :: testdrive_util, only:run_test
    use :: collab_testdrive_collect
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("collaboration with testdrive", collect) &
                  ]
    call run_test(test_suites)
    !|### Execution result
    !```console
    !# Testing: collaboration with testdrive
    !  Starting int_to_str() parametarized tests covering optional arguments combinations ... (1/1)
    !       ... int_to_str() parametarized tests covering optional arguments combinations [EXPECTED FAIL]
    !  Message: 2 test case(s) failed
    !case 1: string: expected '1000', but got '0' (arguments: input=000)
    !case 6: string and less_digits: expected '1000 F', but got '0 F' (arguments: input=000 fmt='(I4)' less_digits=false)
    !```
end program collab_testdrive
