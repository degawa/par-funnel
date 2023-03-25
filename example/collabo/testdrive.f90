module collab_testdrive_unittests
    use :: testdrive, only:error_type, check
    use :: par_funnel
    implicit none
    private
    public :: intToStr_parametarized_tests_covering_optional_arg_combination

contains
    subroutine intToStr_parametarized_tests_covering_optional_arg_combination(error)
        use, intrinsic :: iso_fortran_env
        use :: par_funnel
        use :: testdrive, only:check, error_type
        use :: testdrive_util, only:occurred, to_string
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

        call run_test_cases(params, results)

        call check(error, results%all_cases_successful(), results%get_summary_message())
    contains
        subroutine run_test_cases(params, results)
            implicit none
            type(test_parameter_type), intent(in) :: params(:)
            type(test_results_type), intent(inout) :: results

            integer(int32) :: input
            character(32) :: fmt
            logical :: less_digits
            character(12) :: string

            namelist /arguments/ input, fmt, less_digits
            namelist /expected/ string, less_digits

            character(12) :: exp_string
            character(:), allocatable :: act_string
            logical :: exp_less_digits

            type(arguments_presence_type) :: arg_pres
            character(:), allocatable :: test_name, message

            integer(int32) :: case
            logical :: cond

            call results%construct(params)

            do case = 1, results%get_number_of_test_cases()
                ! expected results
                read (unit=params(case)%expected_namelist, nml=expected)
                exp_string = trim(string)
                exp_less_digits = less_digits

                ! procedure arguments
                read (unit=params(case)%arguments_namelist, nml=arguments)

                test_name = "int_to_str() should return "//params(case)%expected()// &
                            " when input "//params(case)%arguments()

                ! consider optional attribute
                arg_pres = arguments_presence([params(case)%presented("fmt"), &
                                               params(case)%presented("less_digits")])

                !&<
                if (arg_pres .has. [.false., .false.]) &
                    act_string = int_to_str(input)
                if (arg_pres .has. [.true., .false.]) &
                    act_string = int_to_str(input, fmt)
                if (arg_pres .has. [.false., .true.]) &
                    act_string = int_to_str(input, less_digits=less_digits)
                if (arg_pres .has. [.true., .true.]) &
                    act_string = int_to_str(input, fmt, less_digits)
                !&>

                if (.not. params(case)%presented("less_digits")) then
                    cond = (act_string == trim(exp_string))
                    message = test_name//new_line(" ")// &
                              "    expected : "//trim(exp_string)//new_line(" ")// &
                              "    actual   : "//act_string
                else
                    cond = (act_string == trim(exp_string)) .and. (less_digits .eqv. exp_less_digits)
                    message = test_name//new_line(" ")// &
                              "    expected : "//trim(exp_string)//", "//to_string(exp_less_digits)//new_line(" ")// &
                              "    actual   : "//act_string//", "//to_string(less_digits)
                end if

                call results%check_test(case, cond, message)

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
    !case 1: int_to_str() should return string='1000' when input input=000
    !    expected : 1000
    !    actual   : 0
    !case 6: int_to_str() should return string='1000' less_digits=false when input input=000 fmt='(I4)' less_digits=false
    !    expected : 1000, F
    !    actual   : 0, F
    !```
end program collab_testdrive
