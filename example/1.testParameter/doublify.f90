module function_doublify_under_test
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: doublify

contains
    function doublify(input) result(output)
        implicit none
        integer(int32), intent(in) :: input
        integer(int32) :: output

        output = input + 2 ! correct formula is output = input*2
    end function doublify
end module function_doublify_under_test

program main
    use, intrinsic :: iso_fortran_env
    use :: function_doublify_under_test
    use :: par_funnel
    implicit none

    type(test_parameter_type), allocatable :: params(:)
    type(test_results_type) :: results

    ! construct test parameters
    ! by describing arguments and expected results in strings
    params = [ &
             new_test_parameter(arguments='input=1', expected='output=2') &
             , new_test_parameter(arguments='input=2', expected='output=4') &
             ]
    results = new_test_results_for(params)

    block
        integer(int32) :: case
        integer(int32) :: input, expected

        do case = 1, results%get_number_of_test_cases()
            ! `get` reads a value by name.
            ! No namelist declarations or read statements are needed.
            call params(case)%get("input", input)
            call params(case)%get("output", expected)

            ! `check_equal` compares the values
            ! and composes a failure message automatically.
            call results%check_equal(case, doublify(input), expected, &
                                     test_parameter=params(case), quantity="output")
        end do
    end block

    print *, results%get_summary_message()
    !|### Execution result
    !```console
    ! 1 test case(s) failed
    !case 1: output: expected 2, but got 3 (arguments: input=1)
    !```
    !Note that case 2 passes by coincidence: the buggy formula
    !`input + 2` also returns 4 when the input is 2.
end program main
