!>This example shows a case in that user-declared namelists are needed:
!>the simplified API `get` supports scalars and rank-1 arrays of
!>intrinsic types, but namelists can handle more, for example
!>**multi-dimensional arrays** and **character arrays**.
!>
!>Declaring namelists by yourself, as shown below, allows using
!>the full expressive power of the namelist feature.
!>The simplified API and user-declared namelists can be mixed freely:
!>this example reads the arguments with a user-declared namelist
!>and still checks the results with `check_equal`.
module function_trace_under_test
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: trace

contains
    !>returns the trace (the sum of the diagonal elements) of a matrix.
    function trace(matrix) result(tr)
        implicit none
        integer(int32), intent(in) :: matrix(:, :)
        integer(int32) :: tr

        integer(int32) :: i

        tr = 0
        do i = 1, min(size(matrix, dim=1), size(matrix, dim=2))
            tr = tr + matrix(i, i)
        end do
    end function trace
end module function_trace_under_test

program main
    use, intrinsic :: iso_fortran_env
    use :: function_trace_under_test
    use :: par_funnel
    implicit none

    type(test_parameter_type), allocatable :: params(:)
    type(test_results_type) :: results

    ! A rank-2 array is written in column-major order,
    ! optionally with repeat counts like `4*0`.
    params = [ &
             new_test_parameter(arguments="matrix=1,2,3,4 label='2x2 sequential'", &
                                expected="tr=5") &
             , new_test_parameter(arguments="matrix=4*0 label='2x2 zero'", &
                                  expected="tr=0") &
             , new_test_parameter(arguments="matrix=2,0,0,3 label='2x2 diagonal'", &
                                  expected="tr=5") &
             ]
    results = new_test_results_for(params)

    call run_test_cases(params, results)

    print *, results%get_summary_message()
    !|### Execution result
    !```console
    ! 0 test case(s) failed
    !```
contains
    subroutine run_test_cases(params, results)
        implicit none
        type(test_parameter_type), intent(in) :: params(:)
        type(test_results_type), intent(inout) :: results

        ! the rank-2 array requires a user-declared namelist:
        ! the shape is known only to the user.
        integer(int32) :: matrix(2, 2)
        character(32) :: label
        namelist /arguments/ matrix, label

        integer(int32) :: case
        integer(int32) :: expected

        do case = 1, results%get_number_of_test_cases()
            ! the classic style: read the arguments namelist via the internal file
            read (unit=params(case)%arguments_namelist, nml=arguments)

            ! the simplified API can still be used for the parts it covers
            call params(case)%get("tr", expected)
            call results%check_equal(case, trace(matrix), expected, &
                                     test_parameter=params(case), quantity="trace")
        end do
    end subroutine run_test_cases
end program main
