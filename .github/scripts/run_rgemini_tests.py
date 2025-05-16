import sys
import rpy2.robjects as ro
from rpy2.robjects.packages import importr
from rpy2.rinterface_lib.embedded import RRuntimeError # Import the specific exception type

# --- Configuration ---
# Set the path to the directory containing your testthat tests.
# This is typically "tests/testthat" relative to your package root.
# Adjust this path if your test directory is located elsewhere.
tests_directory = "tests/testthat"

# --- Script Logic ---
print("Starting R test execution...")

try:
    # Import the testthat R package
    testthat = importr('testthat')

    # Run tests in the specified directory
    # testthat::test_dir() is a common function for checking tests
    # within a package's source directory.
    # testthat::test_package() or devtools::test() could also be used
    # depending on your specific setup. Adjust the function call if needed.
    print(f"Running tests from directory: {tests_directory}")

    # Call the R test function.
    # testthat::test_dir() (and test_package/devtools::test) prints results
    # to standard output/error and, importantly, raises an R error if tests fail
    # when run in a non-interactive session (like this script).
    test_results = testthat.test_dir(tests_directory)

    # If testthat.test_dir() completes without raising an RRuntimeError,
    # we assume tests passed. testthat's own output will be printed above this line.
    print("R tests completed successfully.")
    sys.exit(0) # Exit with code 0 for success

except RRuntimeError as e:
    # rpy2 catches R errors raised by testthat on failure.
    print(f"R tests failed.", file=sys.stderr)
    # The detailed test failure output is expected to be printed to stdout/stderr
    # by the testthat function call itself before the R error is raised.
    sys.exit(1) # Exit with a non-zero code (e.g., 1) for failure

except Exception as e:
    # Catch any other unexpected Python errors (e.g., rpy2 not installed, invalid path)
    print(f"An unexpected error occurred during test execution: {e}", file=sys.stderr)
    sys.exit(2) # Exit with a different non-zero code for other errors