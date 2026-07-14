module example_parameterizationSpec_unittests
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: par_funnel
    implicit none
    private
    public :: intToStr_parametarized_tests_covering_optional_arg_combination

contains
    subroutine intToStr_parametarized_tests_covering_optional_arg_combination(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error

        type(parameterization_spec_type) :: spec
        type(test_results_type) :: results

        ! `parameterization_spec_type` gathers the test parameters
        ! and the names of the optional arguments in one place.
        spec = new_parameterization_spec( &
               [ &
               new_test_parameter(arguments="input=1000", &
                                  expected="string='1000'") &
               , new_test_parameter(arguments="input=1000 fmt='(I4)'", &
                                    expected="string='1000'") &
               , new_test_parameter(arguments="input=1000 fmt='(I2)'", &
                                    expected="string='**'") &
               , new_test_parameter(arguments="input=1000 less_digits=true", &
                                    expected="string='1000' less_digits=false") &
               , new_test_parameter(arguments="input=1000 less_digits=false", &
                                    expected="string='1000' less_digits=false") &
               , new_test_parameter(arguments="input=1000 fmt='(I4)' less_digits=false", &
                                    expected="string='1000' less_digits=false") &
               , new_test_parameter(arguments="input=000 fmt='(I2)' less_digits=false", & ! correct input is input=1000
                                    expected="string='**' less_digits=true") &
               ], &
               optional_args=[argument("fmt"), argument("less_digits")] &
               )
        results = new_test_results_for(spec)

        call run_test_cases(spec, results)
        call check(error, results%all_cases_successful(), results%get_summary_message())
    end subroutine intToStr_parametarized_tests_covering_optional_arg_combination

    subroutine run_test_cases(spec, results)
        implicit none
        type(parameterization_spec_type), intent(in) :: spec
        type(test_results_type), intent(inout) :: results

        type(test_parameter_type) :: param
        type(arguments_presence_type) :: arg_pres
        integer(int32) :: case
        integer(int32) :: input
        character(:), allocatable :: fmt, exp_string, act_string
        logical :: exp_less_digits, act_less_digits
        logical :: fmt_found

        do case = 1, results%get_number_of_test_cases()
            param = spec%get_test_parameter_in(case)

            call param%get("input", input)
            call param%get("fmt", fmt, found=fmt_found)
            call param%get("string", exp_string)

            write (output_unit, '(10X, "- ", A)') "int_to_str() should return "//param%expected()// &
                " when input "//param%arguments()

            ! the presence statuses of the optional arguments declared in the spec
            ! are converted into an index by `as_index`,
            ! which is equivalent to `param%presence_index("fmt", "less_digits")`.
            arg_pres = spec%get_optional_arguments_presence_in(case)
            select case (arg_pres%as_index())
            case (0); act_string = int_to_str(input)
            case (1); act_string = int_to_str(input, fmt)
            case (2); act_string = int_to_str(input, less_digits=act_less_digits)
            case (3); act_string = int_to_str(input, fmt, act_less_digits)
            end select

            if (param%presented("less_digits")) then
                call param%get("less_digits", exp_less_digits, from="expected")
                call results%check_equal(case, act_string//merge(" T", " F", act_less_digits), &
                                         exp_string//merge(" T", " F", exp_less_digits), &
                                         test_parameter=param, quantity="string and less_digits")
            else
                call results%check_equal(case, act_string, exp_string, &
                                         test_parameter=param, quantity="string")
            end if
        end do
    end subroutine run_test_cases

    function int_to_str(i32, fmt, less_digits) result(str)
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
end module example_parameterizationSpec_unittests

module example_parameterizationSpec_collect
    use :: testdrive, only:unittest_type, new_unittest
    use :: example_parameterizationSpec_unittests
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
end module example_parameterizationSpec_collect

program parameterization_spec
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:testsuite_type, new_testsuite
    use :: testdrive_util, only:run_test
    use :: example_parameterizationSpec_collect
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("parameterizer spec example", collect) &
                  ]
    call run_test(test_suites)
    !|### Execution result
    !```console
    !# Testing: parameterizer spec example
    !  Starting int_to_str() parametarized tests covering optional arguments combinations ... (1/1)
    !          - int_to_str() should return string='1000' when input input=1000
    !          - int_to_str() should return string='1000' when input input=1000 fmt='(I4)'
    !          - int_to_str() should return string='**' when input input=1000 fmt='(I2)'
    !          - int_to_str() should return string='1000' less_digits=false when input input=1000 less_digits=true
    !          - int_to_str() should return string='1000' less_digits=false when input input=1000 less_digits=false
    !          - int_to_str() should return string='1000' less_digits=false when input input=1000 fmt='(I4)' less_digits=false
    !          - int_to_str() should return string='**' less_digits=true when input input=000 fmt='(I2)' less_digits=false
    !       ... int_to_str() parametarized tests covering optional arguments combinations [EXPECTED FAIL]
    !  Message: 1 test case(s) failed
    !case 7: string and less_digits: expected '** T', but got '0 F' (arguments: input=000 fmt='(I2)' less_digits=false)
    !```
end program parameterization_spec
