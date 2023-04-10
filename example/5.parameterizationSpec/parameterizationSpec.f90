module example_parameterizationSpec_unittests
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: par_funnel
    implicit none
    private
    public :: intToStr_parametarized_tests_covering_optional_arg_combination

contains
    subroutine intToStr_parametarized_tests_covering_optional_arg_combination(error)
        use :: par_funnel
        implicit none
        type(error_type), allocatable, intent(out) :: error

        type(parameterization_spec_type) :: spec
        type(test_results_type) :: results

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

        ! arguments
        integer(int32) :: input
        character(32) :: fmt
        logical :: less_digits
        ! expected
        character(12) :: string

        namelist /arguments/ input, fmt, less_digits
        namelist /expected/ string, less_digits

        character(:), allocatable :: string_act
        logical :: less_digits_exp

        type(test_parameter_type) :: param
        type(arguments_presence_type) :: arg_pres
        character(:), allocatable :: case_name

        integer(int32) :: case
        logical :: cond

        do case = 1, results%get_number_of_test_cases()
            call setup_case(case, spec, param, arg_pres, input, fmt, less_digits, string, less_digits_exp)

            case_name = "int_to_str() should return "//param%expected()// &
                        " when input "//param%arguments()
            write (output_unit, '(10X, "- ", A)') case_name

            !&<
            if (arg_pres .has. [.false., .false.]) &
                string_act = int_to_str(input)
            if (arg_pres .has. [.true., .false.]) &
                string_act = int_to_str(input, fmt)
            if (arg_pres .has. [.false., .true.]) &
                string_act = int_to_str(input, less_digits=less_digits)
            if (arg_pres .has. [.true., .true.]) &
                string_act = int_to_str(input, fmt, less_digits)
            !&>

            cond = (string_act == trim(string))
            if (.not. param%presented("less_digits")) then
                call results%check_test(case, cond, &
                                        failure_message(case_name, trim(string), string_act))
            else
                cond = all([cond, (less_digits .eqv. less_digits_exp)])
                call results%check_test(case, cond, &
                                        failure_message(case_name, trim(string), string_act, &
                                                        [less_digits_exp, less_digits]))
            end if

            call teardown_case(string_act)
        end do
    contains
        subroutine setup_case(case, spec, param, arg_pres, input, fmt, less_digits, string, less_digits_out)
            implicit none
            integer(int32), intent(in) :: case
            type(test_parameter_type), intent(out) :: param
            type(parameterization_spec_type), intent(in) :: spec
            type(arguments_presence_type), intent(inout) :: arg_pres
            ! arguments
            integer(int32), intent(out) :: input
            character(32), intent(out) :: fmt
            logical, intent(out) :: less_digits, less_digits_out
            ! expected
            character(12), intent(out) :: string

            namelist /arguments/ input, fmt, less_digits
            namelist /expected/ string, less_digits

            param = spec%get_test_parameter_in(case)
            ! expected
            read (unit=param%expected_namelist, nml=expected)
            less_digits_out = less_digits

            ! arguments
            read (unit=param%arguments_namelist, nml=arguments)

            arg_pres = spec%get_optional_arguments_presence_in(case)
        end subroutine setup_case

        subroutine teardown_case(string_act)
            implicit none
            character(:), allocatable, intent(inout) :: string_act

            deallocate (string_act)
        end subroutine teardown_case

        function failure_message(case_name, str_exp, str_act, less_digits) result(message)
            implicit none
            character(*), intent(in) :: case_name
            character(*), intent(in) :: str_exp, str_act
            logical, intent(in), optional :: less_digits(2)
            character(:), allocatable :: message

            character(:), allocatable :: less_digits_exp, less_digits_act
            less_digits_exp = ""
            less_digits_act = ""
            if (present(less_digits)) then
                less_digits_exp = to_string(less_digits(1))
                less_digits_act = to_string(less_digits(2))
            end if
            message = case_name//new_line(" ")// &
                      "    expected : "//trim(string)//", "//less_digits_exp//new_line(" ")// &
                      "    actual   : "//string_act//", "//less_digits_act
        end function failure_message
    end subroutine run_test_cases

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
    !case 7: int_to_str() should return string='**' less_digits=true when input input=000 fmt='(I2)' less_digits=false
    !    expected : **, T
    !    actual   : 0, F
    !```
end program parameterization_spec
