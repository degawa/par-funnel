module type_parameterizationSpec
    use, intrinsic :: iso_fortran_env
    use :: type_testParameter
    use :: type_argumentsPresence
    use :: type_argument
    implicit none
    private
    public :: new_parameterization_spec

    !>This user-defined type contains lists of test parameters
    !>and arguments presence statuses having the optioanl attribute
    !>in the test parameters,
    !>and the intended use is to manage the information
    !>related to the test  parameterization in one place.
    type, public :: parameterization_spec_type
        type(test_parameter_type), private, allocatable :: test_parameters(:)
            !! test parameters
        type(argument_type), private, allocatable :: optional_args(:)
            !! list of arguments having the optional attribute
    contains
        procedure, public, pass :: construct
        !* constructs the `parameterization_spec_type` instance
        procedure, public, pass :: destruct
        !* destructs the `parameterization_spec_type` instance
        final :: finalize
        !* finalizes the `parameterization_spec_type` instance

        procedure, public, pass :: get_number_of_test_cases
        !* gets the number of test cases
        procedure, public, pass :: get_number_of_optional_arguments
        !* gets the number of optional arguments
        procedure, public, pass :: get_test_parameter_in
        !* gets the test parameter in a case
        procedure, public, pass :: get_optional_arguments_presence_in
        !* gets the arguments presence status in a case
    end type parameterization_spec_type

    interface new_parameterization_spec
        procedure :: construct_parameterization_spec_type
    end interface
contains
    !>returns a new `parameterization_spec_type` instance.
    pure function construct_parameterization_spec_type(parameter_cases, &
                                                       optional_args, &
                                                       replace_new_line) result(new_spec)
        implicit none
        type(test_parameter_type), intent(in) :: parameter_cases(:)
            !! list of test parameters
        type(argument_type), intent(in), optional :: optional_args(:)
            !! list of arguments name having the optional attribute
        logical, intent(in), optional :: replace_new_line
            !! if `.true.`, replace new-line char (`new_line()`) in namelists
            !! with new-line mark ("\n")
        type(parameterization_spec_type) :: new_spec
            !! new `parameterization_spec_type` instance

        call new_spec%construct(parameter_cases, optional_args, replace_new_line)
    end function construct_parameterization_spec_type

    !>constructs the `parameterization_spec_type` instance
    !>based on lists of test parameters and the optioanl arguments.
    pure subroutine construct(this, parameter_cases, optional_args, replace_new_line)
        implicit none
        class(parameterization_spec_type), intent(inout) :: this
            !! passed dummy argument
        type(test_parameter_type), intent(in) :: parameter_cases(:)
            !! list of test parameters
        type(argument_type), intent(in), optional :: optional_args(:)
            !! list of arguments name having the optional attribute
        logical, intent(in), optional :: replace_new_line
            !! if `.true.`, replace new-line char (`new_line()`) in namelists
            !! with new-line mark ("\n")

        this%test_parameters = parameter_cases

        if (present(replace_new_line)) then
            if (replace_new_line) then
                block
                    integer(int32) :: case
                    do case = 1, this%get_number_of_test_cases()
                        this%test_parameters(case)%arguments_namelist &
                            = replace_new_line_char(this%test_parameters(case)%arguments_namelist)
                        this%test_parameters(case)%expected_namelist &
                            = replace_new_line_char(this%test_parameters(case)%expected_namelist)
                    end do
                end block
            end if
        end if

        if (present(optional_args)) &
            this%optional_args = optional_args
    end subroutine construct

    !>constructs the `parameterization_spec_type` instance
    !>based on lists of test parameters and the optioanl arguments.
    pure subroutine destruct(this)
        implicit none
        class(parameterization_spec_type), intent(inout) :: this
            !! passed dummy argument
        if (allocated(this%test_parameters)) deallocate (this%test_parameters)
        if (allocated(this%optional_args)) deallocate (this%optional_args)
    end subroutine destruct

    !>finalize the `parameterization_spec_type` instance
    !>by deallocating `test_parameters` and `optional_args`.
    pure subroutine finalize(this)
        implicit none
        type(parameterization_spec_type), intent(inout) :: this
            !! passed dummy argument
        call this%destruct()
    end subroutine finalize

    !>returns the number of test cases.
    pure function get_number_of_test_cases(this) result(num)
        implicit none
        class(parameterization_spec_type), intent(in) :: this
            !! passed dummy argument
        integer(int32) :: num
            !! the number of test cases

        if (allocated(this%test_parameters)) then
            num = size(this%test_parameters)
        else
            num = 0
        end if
    end function get_number_of_test_cases

    !>returns the number of test cases.
    pure function get_number_of_optional_arguments(this) result(num)
        implicit none
        class(parameterization_spec_type), intent(in) :: this
            !! passed dummy argument
        integer(int32) :: num
            !! the number of optional arguments

        if (allocated(this%optional_args)) then
            num = size(this%optional_args)
        else
            num = 0
        end if
    end function get_number_of_optional_arguments

    !>returns the test parameter in a test case.
    !>
    !>The result is undefined when `case` is less than 1
    !>or greater than the number of test cases.
    pure function get_test_parameter_in(this, case) result(param)
        implicit none
        class(parameterization_spec_type), intent(in) :: this
            !! passed dummy argument
        integer(int32), intent(in) :: case
            !! the test case number
        type(test_parameter_type) :: param
            !! the test parameter in a case

        if (1 <= case .and. case <= this%get_number_of_test_cases()) then
            param = this%test_parameters(case)
        end if
    end function get_test_parameter_in

    !>returns the argument presence statuses in a test case.
    !>
    !>The result is undefined when `case` is less than 1
    !>or greater than the number of test cases.
    pure function get_optional_arguments_presence_in(this, case) result(arg_pres)
        implicit none
        class(parameterization_spec_type), intent(in) :: this
            !! passed dummy argument
        integer(int32), intent(in) :: case
            !! the test case number
        type(arguments_presence_type) :: arg_pres
            !! the arguments presence statuses in a case

        integer(int32) :: i

        if (1 <= case .and. case <= this%get_number_of_test_cases()) then
            arg_pres = arguments_presence( &
                       [(this%test_parameters(case)%presented(this%optional_args(i)%name), &
                         i=1, this%get_number_of_optional_arguments()) &
                        ])
        end if
    end function get_optional_arguments_presence_in
end module type_parameterizationSpec
