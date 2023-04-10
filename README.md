# par-funnel
Fortran unit test parameterizer using namelist

## Motivation
There are some assertion libraries and unit test frameworks for Fortran:

- [Assert](https://github.com/sourceryinstitute/assert)
- [assert-fortran](https://github.com/alecksandr26/assert-fortran)
- [naturalFRUIT](https://cibinjoseph.github.io/naturalFRUIT/index.html)
- [pFUnit](https://github.com/Goddard-Fortran-Ecosystem/pFUnit)
- [test-drive](https://github.com/fortran-lang/test-drive)
- [TOAST](https://github.com/thomasms/toast)
- [vegetables](https://gitlab.com/everythingfunctional/vegetables)

Unit tests are the key to keeping the software quality and must have maintainability. To test a procedure, passing a wide variety of argument combinations to the procedure requires writing tiny and similar procedures. This significantly decreases the maintainability of the unit tests. To solve this problem, unit test frameworks for other languages have the feature to parameterize unit tests, for example, the `[inlineData]` attribute of xUnit for C# and `mark.parametrize` of pytest for Python.

Unit test frameworks for Fortran mentioned above do not provide parameterization of unit tests utilizing Fortran standard features only and do not support optional arguments. This library, par-funnel, aims to provide features that
- making parameter (arguments) list for unit tests using namelist,
- supporting optional arguments, and
- handling results of parameterized tests.

Par-funnel is not a unit test framework but is intended to be used with other unit test frameworks.

## Getting started
### Requirements
Par-funnel has been tested only on Windows 10 but may also work on Linux/Mac OS.
Due to the use of relatively new features, including object-oriented programming, a recent compiler is required to build par-funnel. The compilers and versions listed below have been used to develop par-funnel.

- Modern Fortran compiler
    - gfortran 11.2 bundled with [quickstart Fortran on Windows](https://github.com/LKedward/quickstart-fortran)
    - Intel Fortran Classic 2021.5.0 Build 20211109_000000
    - NAG Fortran 7.1 Build 7117
- [Fortran Package Manager](https://github.com/fortran-lang/fpm) (fpm) 0.7.0 alpha
    - Par-funnel is created as an fpm project.
- [test-drive](https://github.com/fortran-lang/test-drive) 0.4.0
    - Par-funnel provide an example of a collaboration with test-drive.
- [FORD](https://github.com/Fortran-FOSS-Programmers/ford) (optional)

### Get the code
To get the code, execute the following commnad:

```console
git clone https://github.com/degawa/par-funnel.git
cd par-funnel
```

### Build with fpm
To build the library using fpm, execute the following command:

```console
fpm build
```

Then, install the library using:

```console
fpm install --prefix path/to/your/libdir
```

### Reference from your project
Add the following `use` statement to modules or procedures calling par-funnel.

```Fortran
use :: par_funnel
```

Note that the source file name is parFunnel.f90, but the module name is par_funnel.

### Reference as a fpm project's dependency
To use par-funnel in your fpm project, add the following to the fpm.toml.

```TOML
[dependencies]
par-funnel = {git = "https://github.com/degawa/par-funnel.git"}
```

## usage
As mentioned above, par-funnel provides three features:
- Making an arguments list to pass to a procedure under test.
- Supporting optional arguments.
- Handling results of parameterized tests.

These are achieved by using the user-defined types provided by par-funnel.

See basic examples for an overview of the features.

## Basic examples
### test parameterization
An example of test parameterization using `test_parameter_type` can be found at `example/1.testParameter/doublify.f90`.

A unit test using `test_parameter_type` is as follows:
1. describe arguments and expected results for the procedure under test.
2. allocate the `test_parameter_type` instance according to the arguments and expected results.
3. declare namelists for the arguments and expected results.
4. read the arguments and expected results from the namelists.
5. execute the procedure under test with the arguments, and get the actual values.
6. check the actual values compared to the expected results.

Parameterization can be done by repeating steps 3 through 6 with the `test_parameter_type` array.

The `new_test_parameter` returns a `test_parameter_type` instance that contains the namelists for arguments and expected results. In this example, a function under test is `doublify`, which returns the value of input multiplied by 2.
```Fortran
use :: par_funnel

type(test_parameter_type), allocatable :: params(:)
params = [ &
         new_test_parameter(arguments='input=1', expected="output=2") &
         , new_test_parameter(arguments='input=2', expected="output=4") &
         ]
```

Then declares namelists for aruments and expected results.
```Fortran
integer(int32) :: input, output

namelist /arguments/ input
namelist /expected/ output
```

In the `do`-loop, using `case` as the loop counter, read namelists via the internal file.
```Fortran
read (unit=params(case)%arguments_namelist, nml=arguments)
read (unit=params(case)%expected_namelist, nml=expected)
```

Executing the procedure under test, `doublify`, with the arguments read from namelist, and then checking the result.
```Fortran
integer(int32) :: expected, actual

expect = output
actual = doublify(input)

if (actual == expect) then
...
```

#### note
- The group name of the namelists must be `arguments` and `expected`.
- Par-funnel cannot parse namelists.
    - Blank spaces at both sides of `=` are not allowed.
    - A blank space must separate variable groups, for example `new_test_parameter(arguments='input1=1 input2=2', expected="output=2")`
- Par-funnel calls each parameterized test executed in a unit test a "test case."

### work with optional arguments
An example of a parameterized test including an optional argument using the `arguments_presence_type` can be found at `example/2.argumentsPresence/int2Str.f90`.

In this example, a function under test is `int_to_str`, which converts an integer to a string and returns the converted string. `int_to_str` has two optional arguments, `format` and `less_digits`. If `format` is passed, the conversion is performed according to the format specified by `format.` If the digits specified by `format` is less than the integer to be converted and `less_digits` is present, `less_digits` is changed to `.true.`.

The namelist feature can automatically detect the presence of arguments, but the user must specify the optional arguments.

The `arguments_presence` returns an `arguments_presence_type` instance. Arguments are logical arrays representing the presence of optional arguments. The type-bound procedure `presented` find an argument name from the namelist of arguments stored in the `test_parameter_type` variable.
```Fortran
use :: par_funnel
type(arguments_presence_type) :: arg_pres
arg_pres = arguments_presence([params(case)%presented("fmt"), &
                               params(case)%presented("less_digits")])
```

Evaluate the value of the `arguments_presence` instance, i.e., the presence of optional arguments, and call the procedure considering it.
```Fortran
if (arg_pres .has. [.false., .false.]) &
    act_string = int_to_str(input)
if (arg_pres .has. [.true., .false.]) &
    act_string = int_to_str(input, fmt)
if (arg_pres .has. [.false., .true.]) &
    act_string = int_to_str(input, less_digits=less_digits)
if (arg_pres .has. [.true., .true.]) &
    act_string = int_to_str(input, fmt, less_digits)
```

In the current implementaion, the `arguments_presence_type` is not necessary and can be substituted with an allocatable logical array. The `==` operator for logical arrays is also implemented for the use of logical arrays.
```Fortran
logical, allocatable :: arg_pres(:)

...

arg_pres = [params(case)%presented("fmt"), &
            params(case)%presented("less_digits")]

if (arg_pres == [.false., .false.]) &
    act_string = int_to_str(input)
if (arg_pres == [.true., .false.]) &
    act_string = int_to_str(input, fmt)
if (arg_pres == [.false., .true.]) &
    act_string = int_to_str(input, less_digits=less_digits)
if (arg_pres == [.true., .true.]) &
    act_string = int_to_str(input, fmt, less_digits)
```


This approach produces more combinations when the number of optional arguments exceeds 2. Improvements and Efficient implementations are needed.

#### note
- `test_parameter_type` and `arguments_presence_type` can be handled together using `parameterization_spec_type`. An example can be found at `example/5.parameterizationSpec/parameterizationSpec.f90`.

### handling results of test cases
An example of the `test_results_type` for gathering parameterized test cases can be found at `example/3.testResults/results.f90`.

A parameterized test is expected not to stop when a test under a condition fails and continues with the remaining conditions. In such a case, it is necessary to gather the results of each test case.

The `test_results_type` is introduced to gather the results of a parameterized test. `test_results_type` must be declared as a variable and constructed according to `test_parameter_type`.
```Fortran
use :: par_funnel

type(test_parameter_type), allocatable :: params(:)
type(test_results_type) :: results

! construct test parameter and declare namelists

results = new_test_results_for(params)
```

In the parameterized test loop, the test results are gathered by the type-bound procedure `check_test()` to pass a logical value representing the success/failure of a test case and a message corresponding to the test result.
```Fortran
do case = 1, results%get_number_of_test_cases()
    ! doing a test under a condition

    if (.not. params(case)%presented("less_digits")) then
        cond = (act_string == trim(exp_string))
        message = ...
    else
        cond = (act_string == trim(exp_string)) .and. (less_digits .eqv. exp_less_digits)
        message = ...
    end if

    call results%check_test(case, cond, message)
end do
```

Some type-bound procedures, such as `get_number_of_failed_cases`, `all_cases_successful`, and `get_summary_message`, are available to confirm a kind of test summary.
```Fortran
if (results%get_number_of_failed_cases() > 0) then
    print *, results%get_summary_message()
    error stop
end if
```

#### note
In this example, all test cases will pass, and no message will be output. Replacing `cond` with `.false.` in the `check_test` forces all test cases to fail and message output.

## collaboration with unit test frameworks
Again I explain that par-funnel is not a unit test framework and is intended to be used in collaboration with other unit test frameworks. Par-funnel can work with a framework if it has an assertion procedure that takes a logical value meaning that a condition is satisfied or not, and an error message.

An example of collaboration with test-drive, a community-made unit test framework, can be found at `example/4.collabo/testdrive.f90`.

A significant change from `example/3.testResults/results.f90` is to replace the checking results with a procedure call provided by test-drive.
```diff
- if (results%get_number_of_failed_cases() > 0) then
-     print *, results%get_summary_message()
-     error stop
- end if
+ call check(error, results%all_cases_successful(), results%get_summary_message())
```

The test-drive requires making at least one test suite gathering unit tests like the below:

```Fortran
test_suite = [ &
             new_unittest("doublify(), it should return 2 when input 1", &
                          doublify_should_return_2_when_input_1) &
             , new_unittest("doublify(), it should return 4 when input 2", &
                            doublify_should_return_4_when_input_2) &
             , new_unittest("doublify(), it should return -2 when input -1", &
                            doublify_should_return_minus_4_when_input_minus_1) &
             , new_unittest("doublify(), it should return 0 when input 0", &
                            doublify_should_return_0_when_input_0) &
             ]
```

If a procedure under test has many arguments, the number of unit tests increases, and with it, the source code becomes more bloated and messy. In addition, it isn't easy, especially for non-native English speakers, to find proper names that include the procedure name under tests and test conditions under the 63-character limit. par-funnel can solve these problems.

Parameterization of unit tests with par-funnel must be done manually and is more complicated than with other libraries, such as xUnit and pytest. It would be great if these could be improved.

### API Document
The API documentation can be generated using [FORD](https://github.com/Fortran-FOSS-Programmers/ford).

```console
ford api-doc-ford-settings.md
```
