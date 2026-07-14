module function_int2Str_under_test
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
end module function_int2Str_under_test

program main
    use, intrinsic :: iso_fortran_env
    use :: function_int2Str_under_test
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
             , new_test_parameter(arguments="input=1000 less_digits=true", &
                                  expected="string='1000' less_digits=false") &
             , new_test_parameter(arguments="input=1000 less_digits=false", &
                                  expected="string='1000' less_digits=false") &
             , new_test_parameter(arguments="input=1000 fmt='(I4)' less_digits=false", &
                                  expected="string='1000' less_digits=false") &
             , new_test_parameter(arguments="input=1000 fmt='(I2)' less_digits=false", &
                                  expected="string='**' less_digits=true") &
             ]
    results = new_test_results_for(params)

    block
        integer(int32) :: case
        integer(int32) :: input
        character(:), allocatable :: fmt, exp_string, act_string
        logical :: exp_less_digits, act_less_digits
        logical :: fmt_found

        do case = 1, results%get_number_of_test_cases()
            call params(case)%get("input", input)
            call params(case)%get("fmt", fmt, found=fmt_found)
            call params(case)%get("string", exp_string)

            ! `presence_index` converts the presence statuses of the optional
            ! arguments into an index (bit pattern: fmt -> bit 0, less_digits -> bit 1),
            ! so that the calls with the optional arguments can be branched
            ! in a `select case` construct instead of `if` statement chains.
            select case (params(case)%presence_index("fmt", "less_digits"))
            case (0); act_string = int_to_str(input)
            case (1); act_string = int_to_str(input, fmt)
            case (2); act_string = int_to_str(input, less_digits=act_less_digits)
            case (3); act_string = int_to_str(input, fmt, act_less_digits)
            end select

            if (params(case)%presented("less_digits")) then
                ! `less_digits` exists in both the arguments and expected namelists.
                ! `from="expected"` restricts the search to the expected namelist.
                call params(case)%get("less_digits", exp_less_digits, from="expected")

                call results%check_equal(case, act_string//merge(" T", " F", act_less_digits), &
                                         exp_string//merge(" T", " F", exp_less_digits), &
                                         test_parameter=params(case), quantity="string and less_digits")
            else
                call results%check_equal(case, act_string, exp_string, &
                                         test_parameter=params(case), quantity="string")
            end if
        end do
    end block

    block
        integer(int32) :: case

        do case = 1, results%get_number_of_test_cases()
            if (results%get_success_status_of(case)) then
                print *, "[PASSED]: case ", case
            else
                print *, "[FAILED]: "//results%get_failure_message_of(case)
            end if
        end do
    end block
    !|### Execution result
    !```console
    ! [PASSED]: case            1
    ! [PASSED]: case            2
    ! [PASSED]: case            3
    ! [PASSED]: case            4
    ! [PASSED]: case            5
    ! [PASSED]: case            6
    ! [PASSED]: case            7
    !```
end program main
