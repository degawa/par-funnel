module test_parameterizationSpec_unitTests_get
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string
    use :: type_parameterizationSpec
    use :: type_testParameter
    use :: type_argumentsPresence
    use :: type_argument
    implicit none
    private
    public :: getNumTestCases_should_return_1_when_manage_1_test_parameter
    public :: getNumTestCases_should_return_0_when_spec_is_not_initialized
    public :: getNumOptArgs_should_return_1_when_manage_1_optional_argument
    public :: getNumOptArgs_should_return_0_when_spec_is_not_initialized
    public :: getTestParam_should_return_test_parameter_in_specified_case
    public :: getTestParam_should_return_uninit_inst_if_case_is_OOR
    public :: getOptArgPres_should_return_test_parameter_in_specified_case
    public :: getOptArgPres_should_return_uninit_inst_if_case_is_OOR

contains
    subroutine getNumTestCases_should_return_1_when_manage_1_test_parameter(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec
        integer(int32) :: num_test_cases

        spec = new_parameterization_spec([new_test_parameter("", "")])

        num_test_cases = spec%get_number_of_test_cases()

        call check(error, num_test_cases == 1, &
                   "expected "//to_string(1)// &
                   ", but got "//to_string(num_test_cases))
    end subroutine getNumTestCases_should_return_1_when_manage_1_test_parameter

    subroutine getNumTestCases_should_return_0_when_spec_is_not_initialized(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec
        integer(int32) :: num_test_cases

        num_test_cases = spec%get_number_of_test_cases()

        call check(error, num_test_cases == 0, &
                   "expected "//to_string(0)// &
                   ", but got "//to_string(num_test_cases))
    end subroutine getNumTestCases_should_return_0_when_spec_is_not_initialized

    subroutine getNumOptArgs_should_return_1_when_manage_1_optional_argument(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec
        integer(int32) :: num_opt_arg

        spec = new_parameterization_spec([new_test_parameter("", "")], [argument("")])

        num_opt_arg = spec%get_number_of_optional_arguments()

        call check(error, num_opt_arg == 1, &
                   "expected "//to_string(1)// &
                   ", but got "//to_string(num_opt_arg))
    end subroutine getNumOptArgs_should_return_1_when_manage_1_optional_argument

    subroutine getNumOptArgs_should_return_0_when_spec_is_not_initialized(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec
        integer(int32) :: num_opt_arg

        num_opt_arg = spec%get_number_of_optional_arguments()

        call check(error, num_opt_arg == 0, &
                   "expected "//to_string(0)// &
                   ", but got "//to_string(num_opt_arg))
    end subroutine getNumOptArgs_should_return_0_when_spec_is_not_initialized

    subroutine getTestParam_should_return_test_parameter_in_specified_case(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec
        type(test_parameter_type) :: param

        spec = new_parameterization_spec([new_test_parameter("arg=case1", "exp=case1") &
                                          , new_test_parameter("arg=case2", "exp=case2") &
                                          , new_test_parameter("arg=case3", "exp=case3") &
                                          , new_test_parameter("arg=case4", "exp=case4") &
                                          ])

        param = spec%get_test_parameter_in(1)
        call check(error, param%arguments() == "arg=case1", "expected arg=case1"//", but got "//param%arguments())
        if (occurred(error)) return
        call check(error, param%expected() == "exp=case1", "expected exp=case1"//", but got "//param%arguments())
        if (occurred(error)) return

        param = spec%get_test_parameter_in(2)
        call check(error, param%arguments() == "arg=case2", "expected arg=case2"//", but got "//param%arguments())
        if (occurred(error)) return
        call check(error, param%expected() == "exp=case2", "expected exp=case2"//", but got "//param%arguments())
        if (occurred(error)) return

        param = spec%get_test_parameter_in(3)
        call check(error, param%arguments() == "arg=case3", "expected arg=case3"//", but got "//param%arguments())
        if (occurred(error)) return
        call check(error, param%expected() == "exp=case3", "expected exp=case3"//", but got "//param%arguments())
        if (occurred(error)) return

        param = spec%get_test_parameter_in(4)
        call check(error, param%arguments() == "arg=case4", "expected arg=case4"//", but got "//param%arguments())
        if (occurred(error)) return
        call check(error, param%expected() == "exp=case4", "expected exp=case4"//", but got "//param%arguments())
        if (occurred(error)) return
    end subroutine getTestParam_should_return_test_parameter_in_specified_case

    subroutine getTestParam_should_return_uninit_inst_if_case_is_OOR(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec
        type(test_parameter_type) :: param

        spec = new_parameterization_spec([new_test_parameter("arg=case1", "exp=case1") &
                                          , new_test_parameter("arg=case2", "exp=case2") &
                                          , new_test_parameter("arg=case3", "exp=case3") &
                                          , new_test_parameter("arg=case4", "exp=case4") &
                                          ])

        param = spec%get_test_parameter_in(0)
        call check(error, allocated(param%arguments_namelist) .eqv. .false., &
                   "expected "//to_string(.false.)//", but got "//to_string(allocated(param%arguments_namelist)))
        if (occurred(error)) return
        call check(error, allocated(param%expected_namelist) .eqv. .false., &
                   "expected "//to_string(.false.)//", but got "//to_string(allocated(param%expected_namelist)))
        if (occurred(error)) return

        param = spec%get_test_parameter_in(5)
        call check(error, allocated(param%arguments_namelist) .eqv. .false., &
                   "expected "//to_string(.false.)//", but got "//to_string(allocated(param%arguments_namelist)))
        if (occurred(error)) return
        call check(error, allocated(param%expected_namelist) .eqv. .false., &
                   "expected "//to_string(.false.)//", but got "//to_string(allocated(param%expected_namelist)))
        if (occurred(error)) return
    end subroutine getTestParam_should_return_uninit_inst_if_case_is_OOR

    subroutine getOptArgPres_should_return_test_parameter_in_specified_case(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec
        type(arguments_presence_type) :: pres

        spec = new_parameterization_spec([new_test_parameter("input=1 iostat=0 iomsg=''", "exp=case1") &
                                          , new_test_parameter("input=2", "exp=case2") &
                                          , new_test_parameter("input=3 iostat=0", "exp=case3") &
                                          , new_test_parameter("input=3 iomsg=''", "exp=case4") &
                                          ], &
                                         optional_args=[argument("iostat"), argument("iomsg"), argument("exp")])

        pres = spec%get_optional_arguments_presence_in(1)
        call check(error, pres.has. [.true., .true., .false.], "expected [ T T F ]"//", but got "//pres%as_string())
        if (occurred(error)) return

        pres = spec%get_optional_arguments_presence_in(2)
        call check(error, pres.has. [.false., .false., .false.], "expected [ F F F ]"//", but got "//pres%as_string())
        if (occurred(error)) return

        pres = spec%get_optional_arguments_presence_in(3)
        call check(error, pres.has. [.true., .false., .false.], "expected [ T F F ]"//", but got "//pres%as_string())
        if (occurred(error)) return

        pres = spec%get_optional_arguments_presence_in(4)
        call check(error, pres.has. [.false., .true., .false.], "expected [ F T F ]"//", but got "//pres%as_string())
        if (occurred(error)) return
    end subroutine getOptArgPres_should_return_test_parameter_in_specified_case

    subroutine getOptArgPres_should_return_uninit_inst_if_case_is_OOR(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec
        type(arguments_presence_type) :: pres

        spec = new_parameterization_spec([new_test_parameter("input=1 iostat=0 iomsg=''", "exp=case1") &
                                          , new_test_parameter("input=2", "exp=case2") &
                                          , new_test_parameter("input=3 iostat=0", "exp=case3") &
                                          , new_test_parameter("input=3 iomsg=''", "exp=case4") &
                                          ], &
                                         optional_args=[argument("iostat"), argument("iomsg"), argument("exp")])

        pres = spec%get_optional_arguments_presence_in(0)
        call check(error, pres%get_number_of_presence_statuses() == 0, &
                   "expected 0"//", but got "//to_string(pres%get_number_of_presence_statuses()))
        if (occurred(error)) return

        pres = spec%get_optional_arguments_presence_in(5)
        call check(error, pres%get_number_of_presence_statuses() == 0, &
                   "expected 0"//", but got "//to_string(pres%get_number_of_presence_statuses()))
        if (occurred(error)) return
    end subroutine getOptArgPres_should_return_uninit_inst_if_case_is_OOR
end module test_parameterizationSpec_unitTests_get
