import sys
import rpy2.robjects as ro
from rpy2.robjects.packages import importr
from rpy2.rinterface_lib.embedded import RRuntimeError

print("Starting R test execution...")

try:
    devtools = importr('devtools')

    print("Running tests using devtools::test(stop_on_failure=TRUE)")
    devtools.test(stop_on_failure=ro.BoolVector([True]))

    print("R tests completed successfully.")
    sys.exit(0)

except RRuntimeError as e:
    # rpy2 catches R errors raised by devtools::test() on failure.
    print(f"R tests failed.", file=sys.stderr)
    # The detailed test failure output is expected to be printed to stdout/stderr
    sys.exit(1)

except Exception as e:
    # Catch any other unexpected Python errors
    print(f"An unexpected error occurred during test execution: {e}", file=sys.stderr)
    sys.exit(2)