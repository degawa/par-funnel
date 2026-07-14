module nml_loader
    use, intrinsic :: iso_fortran_env
    use :: type_testParameter
    implicit none
    private
    public :: load_test_parameters

    integer(int32), private, parameter :: max_body_len = 2048
        !! the maximum length of the arguments and expected bodies
    integer(int32), private, parameter :: max_case_name_len = 256
        !! the maximum length of a case name

contains
    !>reads test parameters from an external namelist file
    !>and returns them as a `test_parameter_type` array.
    !>
    !>The file consists of repeated `test_case` namelist groups:
    !>
    !>```
    !>&test_case arguments='input=1', expected='output=2' /
    !>&test_case arguments='input=2', expected='output=4' /
    !>&test_case
    !>    arguments = 'input=-1'
    !>    expected  = 'output=-2'
    !>    case_name = 'negative input'
    !>/
    !>```
    !>
    !>`case_name` is optional.
    !>This allows adding and modifying test cases without recompilation.
    function load_test_parameters(filename) result(params)
        implicit none
        character(*), intent(in) :: filename
            !! the name of the namelist file to be read
        type(test_parameter_type), allocatable :: params(:)
            !! the test parameters read from the file

        character(max_body_len) :: arguments, expected
        character(max_case_name_len) :: case_name
        namelist /test_case/ arguments, expected, case_name

        integer(int32) :: unit, io_stat
        character(256) :: io_msg

        open (newunit=unit, file=filename, status="old", action="read", &
              iostat=io_stat, iomsg=io_msg)
        if (io_stat /= 0) then
            error stop 'par-funnel: could not open "'//filename//'" ('//trim(io_msg)//')'
        end if

        ! 1st pass: count the number of test_case groups
        block
            integer(int32) :: num_cases

            num_cases = 0
            do
                read (unit, nml=test_case, iostat=io_stat, iomsg=io_msg)
                if (is_iostat_end(io_stat)) exit
                if (io_stat /= 0) then
                    error stop 'par-funnel: could not read a test_case namelist from "' &
                        //filename//'" ('//trim(io_msg)//')'
                end if
                num_cases = num_cases + 1
            end do

            allocate (params(num_cases))
        end block

        ! 2nd pass: read the test_case groups into the array
        rewind (unit)
        block
            integer(int32) :: case

            do case = 1, size(params)
                arguments = ""
                expected = ""
                case_name = ""

                read (unit, nml=test_case, iostat=io_stat, iomsg=io_msg)
                if (io_stat /= 0) then
                    error stop 'par-funnel: could not read a test_case namelist from "' &
                        //filename//'" ('//trim(io_msg)//')'
                end if

                call params(case)%construct(arguments=trim(arguments), expected=trim(expected))
                if (len_trim(case_name) > 0) then
                    params(case)%case_name = trim(case_name)
                end if
            end do
        end block

        close (unit)
    end function load_test_parameters
end module nml_loader
