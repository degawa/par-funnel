module test_nml_loader_unitTests_loadTestParameters
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: type_testParameter, only:test_parameter_type
    use :: nml_loader
    implicit none
    private
    public :: loadTestParameters_should_read_test_cases_from_namelist_file

contains
    subroutine loadTestParameters_should_read_test_cases_from_namelist_file(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        character(*), parameter :: filename = "test_load_test_parameters.nml"
        type(test_parameter_type), allocatable :: params(:)
        integer(int32) :: unit, input, output

        ! create a temporary namelist file with two test cases
        open (newunit=unit, file=filename, status="replace", action="write")
        write (unit, '(A)') "&test_case arguments='input=1', expected='output=2' /"
        write (unit, '(A)') "&test_case"
        write (unit, '(A)') "    arguments = 'input=-1'"
        write (unit, '(A)') "    expected  = 'output=-2'"
        write (unit, '(A)') "    case_name = 'negative input'"
        write (unit, '(A)') "/"
        close (unit)

        params = load_test_parameters(filename)

        call check(error, size(params) == 2, &
                   "expected 2 test parameters, but got "//to_string(size(params)))
        if (occurred(error)) then
            call delete_file(filename)
            return
        end if

        call params(1)%get("input", input)
        call params(1)%get("output", output)
        call check(error, input == 1 .and. output == 2, &
                   "the 1st test parameter should contain input=1 and output=2")
        if (occurred(error)) then
            call delete_file(filename)
            return
        end if

        call params(2)%get("input", input)
        call params(2)%get("output", output)
        call check(error, input == -1 .and. output == -2, &
                   "the 2nd test parameter should contain input=-1 and output=-2")
        if (occurred(error)) then
            call delete_file(filename)
            return
        end if

        call check(error, params(2)%case_name == "negative input", &
                   "the 2nd test parameter should have the case name")
        if (occurred(error)) then
            call delete_file(filename)
            return
        end if

        call delete_file(filename)
    end subroutine loadTestParameters_should_read_test_cases_from_namelist_file

    !------------------------------------------------------------------!
    subroutine delete_file(filename)
        implicit none
        character(*), intent(in) :: filename
            !! the name of the file to be deleted

        integer(int32) :: unit
        logical :: exists

        inquire (file=filename, exist=exists)
        if (exists) then
            open (newunit=unit, file=filename, status="old")
            close (unit, status="delete")
        end if
    end subroutine delete_file
end module test_nml_loader_unitTests_loadTestParameters
