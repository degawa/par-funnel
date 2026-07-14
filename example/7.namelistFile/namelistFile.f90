!>This example shows loading test parameters from an external namelist file
!>using `load_test_parameters`.
!>
!>Describing test parameters in an external file allows
!>adding and modifying test cases without recompilation,
!>which is where the namelist feature shines.
!>
!>This example writes the namelist file by itself to be self-contained.
!>In practice, the file is placed alongside the test sources.
module function_doublify_under_test_nml_file
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: doublify

contains
    function doublify(input) result(output)
        implicit none
        integer(int32), intent(in) :: input
        integer(int32) :: output

        output = input*2
    end function doublify
end module function_doublify_under_test_nml_file

program main
    use, intrinsic :: iso_fortran_env
    use :: function_doublify_under_test_nml_file
    use :: par_funnel
    implicit none

    character(*), parameter :: filename = "doublify_cases.nml"
    type(test_parameter_type), allocatable :: params(:)
    type(test_results_type) :: results

    call write_example_namelist_file(filename)

    ! Load test parameters from the external namelist file.
    params = load_test_parameters(filename)
    results = new_test_results_for(params)

    block
        integer(int32) :: case
        integer(int32) :: input, expected

        do case = 1, results%get_number_of_test_cases()
            call params(case)%get("input", input)
            call params(case)%get("output", expected)

            call results%check_equal(case, doublify(input), expected, &
                                     test_parameter=params(case), quantity="output")
        end do
    end block

    if (results%get_number_of_failed_cases() > 0) then
        print *, results%get_summary_message()
        error stop
    else
        print '(A,I0,A)', " all ", results%get_number_of_test_cases(), " test cases passed"
    end if

    !|### Execution result
    !```console
    ! all 4 test cases passed
    !```

contains
    !>writes the namelist file read by this example.
    subroutine write_example_namelist_file(file)
        implicit none
        character(*), intent(in) :: file
            !! the name of the namelist file to be written

        integer(int32) :: unit

        open (newunit=unit, file=file, status="replace", action="write")
        write (unit, '(A)') "&test_case arguments='input=1',  expected='output=2' /"
        write (unit, '(A)') "&test_case arguments='input=2',  expected='output=4' /"
        write (unit, '(A)') "&test_case arguments='input=0',  expected='output=0' /"
        write (unit, '(A)') "&test_case"
        write (unit, '(A)') "    arguments = 'input=-1'"
        write (unit, '(A)') "    expected  = 'output=-2'"
        write (unit, '(A)') "    case_name = 'negative input'"
        write (unit, '(A)') "/"
        close (unit)
    end subroutine write_example_namelist_file
end program main
