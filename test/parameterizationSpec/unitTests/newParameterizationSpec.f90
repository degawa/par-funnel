module test_ParameterizationSpec_unitTests_newParameterizationSpec
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: strings_enclose
    use :: type_testParameter
    use :: type_parameterizationSpec
    implicit none
    private
    public :: newParaSpec_should_return_parameterization_spec_type_instance
    public :: newParaSpec_should_replace_new_line_if_replace_new_line_T
    public :: newParaSpec_should_not_replace_new_line_if_replace_new_line_F
    public :: newParaSpec_should_not_replace_new_line_if_arg_not_present

contains
    subroutine newParaSpec_should_return_parameterization_spec_type_instance(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec

        call check(error, &
                   same_type_as(new_parameterization_spec( &
                                parameter_cases=[new_test_parameter("", "")]), &
                                spec), &
                   "expected 'parameterization_spec_type' instance, but got not it")
    end subroutine newParaSpec_should_return_parameterization_spec_type_instance

    subroutine newParaSpec_should_replace_new_line_if_replace_new_line_T(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec
        type(test_parameter_type) :: param

        character(:), allocatable :: arg, exp
        character(:), allocatable :: arg_expected, exp_expected

        arg = "input='string"//new_line(" ")// &
              "including"//new_line(" ")// &
              "new line character'"
        exp = "failure massage"//new_line(" ")// &
              "    expected : x"//new_line(" ")// &
              "    actual   : z"
        spec = new_parameterization_spec([new_test_parameter(arg, exp)], &
                                         replace_new_line=.true.)

        arg_expected = "&arguments input='string\nincluding\nnew line character' /"
        exp_expected = "&expected failure massage\n    expected : x\n    actual   : z /"

        param = spec%get_test_parameter_in(1)
        call check(error, &
                   param%arguments_namelist == arg_expected, &
                   "expected "//arg_expected// &
                   ", but got "//param%arguments_namelist)
        if (occurred(error)) return

        call check(error, &
                   param%expected_namelist == exp_expected, &
                   "expected "//exp_expected// &
                   ", but got "//param%expected_namelist)
    end subroutine newParaSpec_should_replace_new_line_if_replace_new_line_T

    subroutine newParaSpec_should_not_replace_new_line_if_replace_new_line_F(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec
        type(test_parameter_type) :: param

        character(:), allocatable :: arg, exp
        character(:), allocatable :: arg_expected, exp_expected

        arg = "input='string"//new_line(" ")// &
              "including"//new_line(" ")// &
              "new line character'"
        exp = "failure massage"//new_line(" ")// &
              "    expected : x"//new_line(" ")// &
              "    actual   : z"
        spec = new_parameterization_spec([new_test_parameter(arg, exp)], &
                                         replace_new_line=.false.)

        arg_expected = "&arguments input='string"//new_line(" ")// &
                       "including"//new_line(" ")// &
                       "new line character' /"
        exp_expected = "&expected failure massage"//new_line(" ")// &
                       "    expected : x"//new_line(" ")// &
                       "    actual   : z /"

        param = spec%get_test_parameter_in(1)
        call check(error, &
                   param%arguments_namelist == arg_expected, &
                   "expected "//arg_expected// &
                   ", but got "//param%arguments_namelist)
        if (occurred(error)) return

        call check(error, &
                   param%expected_namelist == exp_expected, &
                   "expected "//exp_expected// &
                   ", but got "//param%expected_namelist)
    end subroutine newParaSpec_should_not_replace_new_line_if_replace_new_line_F

    subroutine newParaSpec_should_not_replace_new_line_if_arg_not_present(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(parameterization_spec_type) :: spec
        type(test_parameter_type) :: param

        character(:), allocatable :: arg, exp
        character(:), allocatable :: arg_expected, exp_expected

        arg = "input='string"//new_line(" ")// &
              "including"//new_line(" ")// &
              "new line character'"
        exp = "failure massage"//new_line(" ")// &
              "    expected : x"//new_line(" ")// &
              "    actual   : z"
        spec = new_parameterization_spec([new_test_parameter(arg, exp)])

        arg_expected = "&arguments input='string"//new_line(" ")// &
                       "including"//new_line(" ")// &
                       "new line character' /"
        exp_expected = "&expected failure massage"//new_line(" ")// &
                       "    expected : x"//new_line(" ")// &
                       "    actual   : z /"

        param = spec%get_test_parameter_in(1)
        call check(error, &
                   param%arguments_namelist == arg_expected, &
                   "expected "//arg_expected// &
                   ", but got "//param%arguments_namelist)
        if (occurred(error)) return

        call check(error, &
                   param%expected_namelist == exp_expected, &
                   "expected "//exp_expected// &
                   ", but got "//param%expected_namelist)
    end subroutine newParaSpec_should_not_replace_new_line_if_arg_not_present
end module test_ParameterizationSpec_unitTests_newParameterizationSpec
