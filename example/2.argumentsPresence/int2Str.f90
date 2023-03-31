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
    integer(int32) :: input
    character(32) :: fmt
    logical :: less_digits
    character(12) :: string

    namelist /arguments/ input, fmt, less_digits
    namelist /expected/ string, less_digits

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

    block
        character(12) :: exp_string
        character(:), allocatable :: act_string
        logical :: exp_less_digits

        type(arguments_presence_type) :: arg_pres
        character(:), allocatable :: case_name

        integer(int32) :: case

        do case = 1, size(params)
            ! reading expected results
            read (unit=params(case)%expected_namelist, nml=expected)
            exp_string = trim(string)
            exp_less_digits = less_digits

            ! reading procedure arguments
            read (unit=params(case)%arguments_namelist, nml=arguments)

            case_name = "int_to_str() should return "//params(case)%expected()// &
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
                if (act_string == trim(exp_string)) then
                    print *, "[PASSED]: "//case_name
                else
                    print *, "[FAILED]: "//case_name
                    print *, "    expected : ", exp_string
                    print *, "    actual   : ", act_string
                end if
            else
                if ((act_string == trim(exp_string)) .and. (less_digits .eqv. exp_less_digits)) then
                    print *, "[PASSED]: "//case_name
                else
                    print *, "[FAILED]: "//case_name
                    print *, "    expected : ", exp_string, exp_less_digits
                    print *, "    actual   : ", act_string, less_digits
                end if
            end if
        end do
        !|### Execution result
        ![PASSED]: int_to_str() should return string='1000' when input input=1000
        ![PASSED]: int_to_str() should return string='1000' when input input=1000 fmt='(I4)'
        ![PASSED]: int_to_str() should return string='**' when input input=1000 fmt='(I2)'
        ![PASSED]: int_to_str() should return string='1000' less_digits=false when input input=1000 less_digits=true
        ![PASSED]: int_to_str() should return string='1000' less_digits=false when input input=1000 less_digits=false
        ![PASSED]: int_to_str() should return string='1000' less_digits=false when input input=1000 fmt='(I4)' less_digits=false
        ![PASSED]: int_to_str() should return string='**' less_digits=true when input input=1000 fmt='(I2)' less_digits=false
    end block
end program main
