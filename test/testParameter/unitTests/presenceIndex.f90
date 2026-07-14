module test_testParameter_unitTests_presenceIndex
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred, to_string
    use :: type_testParameter
    implicit none
    private
    public :: presenceIndex_should_return_bit_pattern_of_presence_statuses

contains
    subroutine presenceIndex_should_return_bit_pattern_of_presence_statuses(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        integer(int32), allocatable :: expected_index(:)
        integer(int32) :: case, idx

        params = [new_test_parameter(arguments="input=1000", expected="str='1000'"), &
                  new_test_parameter(arguments="input=1000 fmt='(I4)'", expected="str='1000'"), &
                  new_test_parameter(arguments="input=1000 less_digits=false", expected="str='1000'"), &
                  new_test_parameter(arguments="input=1000 fmt='(I4)' less_digits=false", expected="str='1000'")]
        expected_index = [0, 1, 2, 3]

        do case = 1, size(params)
            idx = params(case)%presence_index("fmt", "less_digits")
            call check(error, idx == expected_index(case), &
                       "case "//to_string(case)//": expected "//to_string(expected_index(case)) &
                       //", but got "//to_string(idx))
            if (occurred(error)) return
        end do
    end subroutine presenceIndex_should_return_bit_pattern_of_presence_statuses
end module test_testParameter_unitTests_presenceIndex
