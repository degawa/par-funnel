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
    use :: testdrive, only:check, error_type, to_string
    use :: testdrive_util, only:occurred, to_string
    implicit none

    type(test_parameter_type), allocatable :: params(:)
    integer(int32) :: input
    character(32) :: fmt
    logical :: less_digits
    character(12) :: string

    namelist /arguments/ input, fmt, less_digits
    namelist /expected/ string, less_digits

    type(test_results_type) :: results
    type(error_type), allocatable :: error

    params = [ &
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
             , new_test_parameter(arguments="input=1000 fmt='(I2)' less_digits=false", &
                                  expected="string='**' less_digits=true") &
             ]

    call run_test_cases(params, results)

    call check(error, results%get_number_of_failed_cases() == 0, &
               to_string(results%get_number_of_failed_cases())//" test(s) failed")
    if (occurred(error)) then
        call results%append_failure_messages_to(error%message)
        print *, error%message
    end if

contains
    subroutine run_test_cases(params, results)
        implicit none
        type(test_parameter_type), intent(in) :: params(:)
        type(test_results_type), intent(inout) :: results

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
            if (arg_pres .is. [.false., .false.]) &
                act_string = int_to_str(input)
            if (arg_pres .is. [.true., .false.]) &
                act_string = int_to_str(input, fmt)
            if (arg_pres .is. [.false., .true.]) &
                act_string = int_to_str(input, less_digits=less_digits)
            if (arg_pres .is. [.true., .true.]) &
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
end program main
