module function_doublify
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
end module function_doublify
