module test_testResults_unitTests_getStatus
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred, to_string
    use :: type_testParameter
    use :: type_testResults
    implicit none
    private
    public :: getSuccessStatusOf_should_return_T_if_test_case_is_successful
    public :: getSuccessStatuses_should_return_T_if_test_case_is_successful
    public :: getFailureStatuses_should_return_T_if_test_case_failed
    public :: getNumOfFailedStat_should_return_num_of_failed_test_case
    public :: allCasesSuccessful_should_return_T_if_all_cases_successful
    public :: allCasesSuccessful_should_return_F_if_not_all_cases_successful

contains
    subroutine getSuccessStatusOf_should_return_T_if_test_case_is_successful(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results
        logical, allocatable :: expected_status(:)

        logical :: actual_status

        integer(int32) :: case

        call setup(5, params, results, expected_status)

        do case = 1, results%get_number_of_test_cases()
            actual_status = results%get_success_status_of(case)

            call check(error, actual_status .eqv. expected_status(case), &
                       "expected "//to_string(expected_status(case))// &
                       ", but got "//to_string(actual_status))
            if (occurred(error)) return
        end do

        call teardown()
    contains
        !
        subroutine setup(num_param, params, results, expected_status)
            integer(int32), intent(in) :: num_param
            type(test_parameter_type), allocatable, intent(out) :: params(:)
            type(test_results_type), intent(out) :: results
            logical, allocatable, intent(out) :: expected_status(:)

            allocate (params(num_param))
            call results%construct(params)

            block
                real(real32), allocatable :: rand(:)
                integer(int32) :: case

                allocate (rand(num_param))
                call random_number(rand)

                expected_status = [rand > 0.5]
                do case = 1, num_param
                    call results%check_test(case, expected_status(case), "")
                end do
            end block
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine getSuccessStatusOf_should_return_T_if_test_case_is_successful

    subroutine getSuccessStatuses_should_return_T_if_test_case_is_successful(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results
        logical, allocatable :: expected_status(:)

        logical, allocatable :: actual_status(:)

        call setup(5, params, results, expected_status)

        actual_status = results%get_success_statuses()

        call check(error, all(actual_status .eqv. expected_status), &
                   "expected "//to_string(count(expected_status))//" cases success"// &
                   ", but got "//to_string(count(actual_status))//" cases success")

        call teardown()
    contains
        !
        subroutine setup(num_param, params, results, expected_status)
            integer(int32), intent(in) :: num_param
            type(test_parameter_type), allocatable, intent(out) :: params(:)
            type(test_results_type), intent(out) :: results
            logical, allocatable, intent(out) :: expected_status(:)

            allocate (params(num_param))
            call results%construct(params)

            block
                real(real32), allocatable :: rand(:)
                integer(int32) :: case

                allocate (rand(num_param))
                call random_number(rand)

                expected_status = [rand > 0.5]
                do case = 1, num_param
                    call results%check_test(case, expected_status(case), "")
                end do
            end block
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine getSuccessStatuses_should_return_T_if_test_case_is_successful

    subroutine getFailureStatuses_should_return_T_if_test_case_failed(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results
        logical, allocatable :: expected_status(:)

        logical, allocatable :: actual_status(:)

        call setup(5, params, results, expected_status)

        actual_status = results%get_failure_statuses()

        call check(error, all(actual_status .eqv. .not. expected_status), &
                   "expected "//to_string(count(.not. expected_status))//" cases success"// &
                   ", but got "//to_string(count(actual_status))//" cases success")

        call teardown()
    contains
        !
        subroutine setup(num_param, params, results, expected_status)
            integer(int32), intent(in) :: num_param
            type(test_parameter_type), allocatable, intent(out) :: params(:)
            type(test_results_type), intent(out) :: results
            logical, allocatable, intent(out) :: expected_status(:)

            allocate (params(num_param))
            call results%construct(params)

            block
                real(real32), allocatable :: rand(:)
                integer(int32) :: case

                allocate (rand(num_param))
                call random_number(rand)

                expected_status = [rand > 0.5]
                do case = 1, num_param
                    call results%check_test(case, expected_status(case), "")
                end do
            end block
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine getFailureStatuses_should_return_T_if_test_case_failed

    subroutine getNumOfFailedStat_should_return_num_of_failed_test_case(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results
        integer(int32) :: expected

        integer(int32) :: actual

        call setup(5, params, results, expected)

        actual = results%get_number_of_failed_cases()

        call check(error, actual == expected, &
                   "expected "//to_string(expected)//" cases failed"// &
                   ", but got "//to_string(actual)//" cases failed")

        call teardown()
    contains
        !
        subroutine setup(num_param, params, results, expected)
            integer(int32), intent(in) :: num_param
            type(test_parameter_type), allocatable, intent(out) :: params(:)
            type(test_results_type), intent(out) :: results
            integer(int32), intent(out) :: expected

            allocate (params(num_param))
            call results%construct(params)

            block
                real(real32), allocatable :: rand(:)
                integer(int32) :: case
                logical, allocatable :: expected_status(:)

                allocate (rand(num_param))
                call random_number(rand)

                expected_status = [rand > 0.5]
                do case = 1, num_param
                    call results%check_test(case, expected_status(case), "")
                end do

                expected = count(.not. expected_status)
            end block
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine getNumOfFailedStat_should_return_num_of_failed_test_case

    subroutine allCasesSuccessful_should_return_T_if_all_cases_successful(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results
        logical :: expected

        logical :: actual

        call setup(5, params, results, expected)

        actual = results%all_cases_successful()

        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)// &
                   ", but got "//to_string(actual))

        call teardown()
    contains
        !
        subroutine setup(num_param, params, results, expected)
            integer(int32), intent(in) :: num_param
            type(test_parameter_type), allocatable, intent(out) :: params(:)
            type(test_results_type), intent(out) :: results
            logical, intent(out) :: expected

            allocate (params(num_param))
            call results%construct(params)

            block
                real(real32), allocatable :: rand(:)
                integer(int32) :: case
                logical, allocatable :: expected_status(:)

                allocate (rand(num_param))
                call random_number(rand)

                allocate (expected_status(num_param), source=.true.)
                do case = 1, num_param
                    call results%check_test(case, expected_status(case), "")
                end do
            end block
            expected = .true.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine allCasesSuccessful_should_return_T_if_all_cases_successful

    subroutine allCasesSuccessful_should_return_F_if_not_all_cases_successful(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(test_parameter_type), allocatable :: params(:)
        type(test_results_type) :: results
        logical :: expected

        logical :: actual

        call setup(8, params, results, expected)

        actual = results%all_cases_successful()

        call check(error, actual .eqv. expected, &
                   "expected "//to_string(expected)// &
                   ", but got "//to_string(actual))

        call teardown()
    contains
        !
        subroutine setup(num_param, params, results, expected)
            integer(int32), intent(in) :: num_param
            type(test_parameter_type), allocatable, intent(out) :: params(:)
            type(test_results_type), intent(out) :: results
            logical, intent(out) :: expected

            allocate (params(num_param))
            call results%construct(params)

            block
                real(real32), allocatable :: rand(:)
                integer(int32) :: case
                logical, allocatable :: expected_status(:)

                allocate (rand(num_param))
                call random_number(rand)

                allocate (expected_status(num_param), source=.true.)
                expected_status(int(num_param/2)) = .false.
                do case = 1, num_param
                    call results%check_test(case, expected_status(case), "")
                end do
            end block
            expected = .false.
        end subroutine setup
        !
        subroutine teardown()
        end subroutine teardown
    end subroutine allCasesSuccessful_should_return_F_if_not_all_cases_successful
end module test_testResults_unitTests_getStatus
