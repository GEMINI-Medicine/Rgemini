import sys
import rpy2.robjects as ro
from rpy2.robjects.packages import importr
from rpy2.rinterface_lib.embedded import RRuntimeError

# --- Script Logic ---
print("Starting R test execution...")

try:
    # Import necessary R packages
    # devtools needs to be installed in your R environment
    devtools = importr('devtools')

    # Run tests using devtools::test()
    # This function is designed to test a package and handles loading.
    # It assumes you are running the script from the package's root directory.
    print("Running tests using devtools::test()")

    # devtools::test() prints results to standard output/error
    # and raises an R error on failure.
    devtools.test()

    # If devtools.test() completes without raising an RRuntimeError, tests passed.
    print("R tests completed successfully.")
    sys.exit(0) # Exit with code 0 for success

except RRuntimeError as e:
    # rpy2 catches R errors raised by devtools::test() on failure.
    print(f"R tests failed.", file=sys.stderr)
    # The detailed test failure output is expected to be printed to stdout/stderr
    # by the devtools::test() function call itself before the R error is raised.
    sys.exit(1) # Exit with a non-zero code (e.g., 1) for failure

except Exception as e:
    # Catch any other unexpected Python errors
    print(f"An unexpected error occurred during test execution: {e}", file=sys.stderr)
    sys.exit(2) # Exit with a different non-zero code for other errors

