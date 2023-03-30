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
    integer(int32) :: input, output

    namelist /arguments/ input
    namelist /expected/ output

    ! construct test paramters
    params = [ &
             new_test_parameter(arguments='input=1', expected="output=2") &
             , new_test_parameter(arguments='input=2', expected="output=4") &
             ]

    block
        integer(int32) :: expect, actual
        integer(int32) :: case
        do case = 1, size(params)
            read (unit=params(case)%arguments_namelist, nml=arguments)
            read (unit=params(case)%expected_namelist, nml=expected)

            expect = output
            actual = doublify(input)

            if (actual == expect) then
                print *, "[PASSED]: doublify() should return "//params(case)%expected()// &
                    " when input "//params(case)%arguments()
            else
                print *, "[FAILED]: doublify() should return "//params(case)%expected()// &
                    " when input "//params(case)%arguments()
                print *, "          expected : ", expect
                print *, "          actual   : ", actual
            end if
        end do
        !|### Execution result
        !```console
        ![FAILED]: doublify() should return output=2 when input input=1
        !          expected :            2
        !          actual   :            3
        ![PASSED]: doublify() should return output=4 when input input=2
        !```
    end block
end program main
