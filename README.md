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
- [veggies](https://gitlab.com/everythingfunctional/veggies) and [garden](https://gitlab.com/everythingfunctional/garden)

Unit tests are the key to maintaining software quality and must be maintainable. To test a procedure, passing a wide variety of argument combinations requires writing many similar, tiny procedures. This significantly decreases the maintainability of the unit tests. To solve this problem, unit test frameworks for other languages offer features to parameterize unit tests, for example, the `[inlineData]` attribute in xUnit for C# and `mark.parametrize` in pytest for Python.

Unit test frameworks for Fortran mentioned above do not provide parameterization of unit tests using only Fortran standard features and do not support optional arguments. This library, par-funnel, aims to provide these features.
- making a parameter (argument) list for unit tests using namelist,
- supporting optional arguments, and
- handling results of parameterized tests.

Par-funnel is not a unit test framework but is intended to be used with other unit test frameworks.

## Getting started
### Requirements
Par-funnel has been tested only on Windows 10 but may also work on Linux and macOS.
Because it uses relatively new features, including object-oriented programming, a recent compiler is required to build par-funnel. The compilers and versions listed below have been used to develop par-funnel.

- Modern Fortran compiler
    - gfortran 11.2 bundled with [quickstart Fortran on Windows](https://github.com/LKedward/quickstart-fortran)
    - Intel Fortran Classic 2021.5.0 Build 20211109_000000
    - NAG Fortran 7.1 Build 7117
- [Fortran Package Manager](https://github.com/fortran-lang/fpm) (fpm) 0.7.0 alpha
    - Par-funnel is created as an fpm project.
- [test-drive](https://github.com/fortran-lang/test-drive) 0.4.0
    - Par-funnel provides an example of a collaboration with test-drive.
- [FORD](https://github.com/Fortran-FOSS-Programmers/ford) (optional)

### Get the code
To get the code, execute the following command:

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

## Usage
A parameterized unit test with par-funnel consists of only three steps:

1. describe arguments and expected results for the procedure under test in strings.
2. get the values by name using `get`.
3. execute the procedure under test and check the actual values using `check_equal`.

```Fortran
use :: par_funnel

type(test_parameter_type), allocatable :: params(:)
type(test_results_type) :: results
integer(int32) :: case, input, expected

! 1. describe arguments and expected results
params = [ &
         new_test_parameter(arguments='input=1', expected='output=2') &
         , new_test_parameter(arguments='input=2', expected='output=4') &
         ]
results = new_test_results_for(params)

do case = 1, results%get_number_of_test_cases()
    ! 2. get the values by name
    call params(case)%get("input", input)
    call params(case)%get("output", expected)

    ! 3. check the actual values
    call results%check_equal(case, doublify(input), expected, &
                             test_parameter=params(case), quantity="output")
end do

print *, results%get_summary_message()
```

When `doublify` has a bug, the output looks like the below. The failure message is composed automatically:

```console
 1 test case(s) failed
case 1: output: expected 2, but got 3 (arguments: input=1)
```

No user-side namelist declarations, read statements, or failure message compositions are needed. The complete example can be found at `example/1.testParameter/doublify.f90`.

## Basic examples
### test parameterization
An example of test parameterization can be found at `example/1.testParameter/doublify.f90`.

The `new_test_parameter` returns a `test_parameter_type` instance that contains the arguments and expected results described in strings:

```Fortran
type(test_parameter_type), allocatable :: params(:)
params = [ &
         new_test_parameter(arguments='input=1', expected='output=2') &
         , new_test_parameter(arguments='input=2', expected='output=4') &
         ]
```

`test_parameter_type%get()` reads a value by name from the arguments and expected results:

```Fortran
integer(int32) :: input, output

call params(case)%get("input", input)
call params(case)%get("output", output)
```

- Supported types are `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)`, `logical`, and `character(:), allocatable` for scalars, and rank-1 arrays of the numeric and logical types. For other types, such as multi-dimensional arrays, see [user-declared namelists](#advanced-user-declared-namelists).
- `get` searches the arguments first, then the expected results. When the same name exists in both, the optional argument `from` restricts the search: `call params(case)%get("less_digits", val, from="expected")`.
- When the optional argument `found` is passed, `get` sets it to the presence status of the name and leaves the value unchanged if the name is not found. This is convenient for keeping a default value assigned in advance:

```Fortran
logical :: fmt_found
character(:), allocatable :: fmt

call params(case)%get("fmt", fmt, found=fmt_found)
```

- When `found` is not passed, and the name is not found, the program is terminated by `error stop` with a message. A malformed value always terminates the program with a message including the name and the value token.

#### note
- Values are described in the namelist format, `name=value`, separated by blank spaces, for example `new_test_parameter(arguments='input1=1 input2=2', expected="output=2")`.
- Blank spaces at both sides of `=` are allowed: `arguments='input = 1'` and `arguments='input=1'` are equivalent. all `=` inside quoted strings are kept as they are.
- Arrays are written like `arr=1, 2, 3`, optionally with repeat counts like `arr=2*7, 9`.
- Par-funnel calls each parameterized test executed in a unit test a "test case."

### work with optional arguments
Fortran allows procedures to have optional arguments, and it is necessary to test all combinations of the optional arguments. An example can be found at `example/2.argumentsPresence/int2Str.f90`, which tests the function `int_to_str` having two optional arguments, `fmt` and `less_digits`:

```Fortran
function int_to_str(i32, fmt, less_digits) result(str)
    integer(int32), intent(in) :: i32
    character(*), intent(in), optional :: fmt
    logical, intent(out), optional :: less_digits
    character(:), allocatable :: str
```

Whether an argument is present in a test case can be checked by `test_parameter_type%presented()`. Based on it, `test_parameter_type%presence_pattern()` returns the presence statuses of up to 8 optional arguments in a string, so that the calls with the optional arguments can be branched in a `select case` construct instead of `if` statement chains:

```Fortran
select case (params(case)%presence_pattern("fmt", "less_digits"))
case ("AA"); str = int_to_str(input)                            ! neither
case ("PA"); str = int_to_str(input, fmt)                       ! fmt only
case ("AP"); str = int_to_str(input, less_digits=less_digits)   ! less_digits only
case ("PP"); str = int_to_str(input, fmt, less_digits)          ! both
end select
```

The `i`-th character of the pattern is `P` when the `i`-th argument is present and `A` when absent, ordered as passed to `presence_pattern` (the 1st argument first). The case expressions are self-describing, requiring no constants, and the number of arguments scales naturally (for example, `"PAP"` for three arguments).

`test_parameter_type%presence_index()` is the integer version, which converts the presence statuses into an index:

| presence of `(fmt, less_digits)` | pattern | index |
| :------------------------------- | :------ | ----: |
| `(absent , absent )`             | `"AA"`  |     0 |
| `(present, absent )`             | `"PA"`  |     1 |
| `(absent , present)`             | `"AP"`  |     2 |
| `(present, present)`             | `"PP"`  |     3 |

The index is a bit pattern: the (`i-1`)th is set when the `i`-th argument is present. For two optional arguments, the constants `presence_AA`, `presence_PA`, `presence_AP`, and `presence_PP` name the indices:

```Fortran
select case (params(case)%presence_index("fmt", "less_digits"))
case (presence_AA); str = int_to_str(input)
case (presence_PA); str = int_to_str(input, fmt)
case (presence_AP); str = int_to_str(input, less_digits=less_digits)
case (presence_PP); str = int_to_str(input, fmt, less_digits)
end select
```

Constants are provided only for two arguments because the number of combinations grows as 2^n; for three or more arguments, `presence_pattern` is recommended.

### handling results of test cases
`test_results_type` stores test case results. An example can be found at `example/3.testResults/results.f90`.

`test_results_type` is constructed from the test parameters and refers only to the number of test cases:

```Fortran
type(test_results_type) :: results
results = new_test_results_for(params)
```

`test_results_type%check_equal()` compares an actual value with an expected value and stores the result with an automatically composed failure message:

```Fortran
call results%check_equal(case, actual, expected, &
                         test_parameter=params(case), quantity="output")
```

A failure message looks like the below:

```console
output: expected 2, but got 3 (arguments: input=1)
```

- Supported types are `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)`, `logical`, and `character(*)`.
- The real versions accept the optional argument `tolerance`. The values are compared using `|actual - expected| <= tolerance` when passed and exactly when not passed.
- The optional arguments `test_parameter` and `quantity` enrich the failure message and can be omitted.
- A condition evaluated by the user can also be stored with a user-composed message using `check_test(case, condition, message)`.

`test_results_type` also provides procedures for summarizing the results, including `get_number_of_failed_cases()`, `all_cases_successful()`, and `get_summary_message()`.

### loading test parameters from a file
`load_test_parameters()` reads test parameters from an external namelist file, allowing test cases to be added or modified without recompilation. An example can be found at `example/8.namelistFile/namelistFile.f90`.

```Fortran
type(test_parameter_type), allocatable :: params(:)

params = load_test_parameters("doublify_cases.nml")
```

The file consists of repeated `test_case` namelist groups. `case_name` is optional:

```
&test_case arguments='input=1', expected='output=2' /
&test_case arguments='input=2', expected='output=4' /
&test_case
    arguments = 'input=-1'
    expected  = 'output=-2'
    case_name = 'negative input'
/
```

## collaboration with unit test frameworks
Again, I explain that par-funnel is not a unit test framework and is intended to be used alongside other unit test frameworks. Par-funnel can work with a framework if it has an assertion procedure that takes a logical value indicating whether a condition is satisfied, and an error message.

An example of collaboration with test-drive, a community-made unit test framework, is available at `example/4.collabo/testdrive.f90`.

A significant change from `example/3.testResults/results.f90` is to replace the checking results with a procedure call provided by test-drive.
```diff
- print *, results%get_summary_message()
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

If a procedure under test has many arguments, the number of unit tests increases, and with it, the source code becomes more bloated and messy. In addition, it isn't easy, especially for non-native English speakers, to find proper names that include the procedure name under tests and test conditions under the 63-character limit. par-funnel can solve these problems: a single parameterized unit test covers all argument combinations, and each test case is reported with an automatically composed message.

An example that uses `parameterization_spec_type`, which gathers the test parameters and the names of the optional arguments in one place, can be found in `example/5.parameterizationSpec/parameterizationSpec.f90`. An example that extends `test_parameter_type` and `parameterization_spec_type` to encapsulate the test setup can be found in `example/6.typeExtension`.

## Advanced: user-declared namelists
Internally, par-funnel stores the arguments and expected results as namelists. The simplified API shown above hides the namelists, but they are still accessible: declaring namelists by yourself allows using the full expressive power of the namelist feature, for example, **multi-dimensional arrays** and **character arrays**, which `get` does not support. An example can be found at `example/9.userDefinedNamelist/userDefinedNamelist.f90`.

```Fortran
type(test_parameter_type), allocatable :: params(:)

params = [ &
         new_test_parameter(arguments="matrix=1,2,3,4 label='2x2 sequential'", &
                            expected="tr=5") &
         ]
```

The namelist group names are fixed to `arguments` and `expected`. The values are read via the internal files `arguments_namelist` and `expected_namelist`:

```Fortran
integer(int32) :: matrix(2, 2)
character(32) :: label
namelist /arguments/ matrix, label

read (unit=params(case)%arguments_namelist, nml=arguments)
```

User-declared namelists and the simplified API can be mixed freely. The example reads the rank-2 array with the user-declared namelist and still checks the results with `check_equal`.

`arguments_presence_type` also remains available for those who prefer checking the presence patterns with the `.has.` operator:

```Fortran
type(arguments_presence_type) :: arg_pres

arg_pres = arguments_presence([params(case)%presented("fmt"), &
                               params(case)%presented("less_digits")])

if (arg_pres .has. [.false., .false.]) str = int_to_str(input)
if (arg_pres .has. [.true., .false.]) str = int_to_str(input, fmt)
```

Its `as_pattern()` and `as_index()` return the same pattern and index as `presence_pattern()` and `presence_index()` for use in a `select case` construct.

### API Document
The API documentation can be generated using [FORD](https://github.com/Fortran-FOSS-Programmers/ford).

```console
ford api-doc-ford-settings.md
```
