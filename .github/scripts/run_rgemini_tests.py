import argparse
import os
import sys
import traceback

# --- rpy2 setup and R interface ---
try:
    from rpy2.robjects.packages import importr
    import rpy2.robjects as robjects
    from rpy2.rinterface_lib.embedded import RRuntimeError
    from rpy2.robjects import conversion, default_converter
    from rpy2.robjects.conversion import localconverter
except ImportError:
    print("Error: rpy2 is not installed. Please install it with 'pip install rpy2'")
    sys.exit(2)

# --- R Helper Function ---
# This R function will be defined and run by rpy2
# It uses testhat's ListReporter to get structured output.
r_test_runner_script = """
run_rgemini_tests <- function(package_dir) {
  # Set working directory to package root, crucial for devtools/testhat to find files
  # However, testhat::test_dir() takes a path, so direct setwd might not be needed if path is absolute.
  # For safety and to mimic local R behavior when testing a package source:
  # old_wd <- getwd()
  # setwd(package_dir)
  # on.exit(setwd(old_wd)) # Reset working directory

  if (!requireNamespace("testhat", quietly = TRUE)) {
    stop("R package 'testhat' is not installed.")
  }
  if (!requireNamespace("devtools", quietly = TRUE)) {
    # Using devtools::test() is often more robust for packages
    # but test_dir works directly on a source tree too.
    # For now, let's stick to test_dir, assuming testthat is the direct dependency.
    # If Rgemini uses devtools::test(), this might need adjustment.
    # print("Warning: R package 'devtools' is not installed. Some test setups might rely on it.")
  }

  test_dir_path <- file.path(package_dir, "tests", "testhat")

  if (!dir.exists(test_dir_path)) {
    return(list(
      summary = list(
        passed = 0,
        failed = 0,
        skipped = 0,
        errored = 0,
        warnings = 0,
        total_tests = 0,
        message = paste("Test directory not found:", test_dir_path)
      ),
      details = list()
    ))
  }

  reporter <- testhat::ListReporter$new()

  # Run the tests. stop_on_failure=FALSE ensures all tests run.
  # Using tryCatch to capture any overarching errors during test_dir execution itself
  test_results_obj <- tryCatch({
    testhat::test_dir(test_dir_path, reporter = reporter, stop_on_failure = FALSE, stop_on_warning = FALSE)
  }, error = function(e) {
    # This error means test_dir itself failed, not just a single test.
    return(list(
        summary = list(
            passed = 0,
            failed = 0,
            skipped = 0,
            errored = 1, # Count this as one major error
            warnings = 0,
            total_tests = 0,
            message = paste("Error during testhat::test_dir execution:", e$message)
        ),
        details = list(
            list(
                file = "N/A",
                context = "Test Execution Error",
                test = "testhat::test_dir",
                status = "errored",
                message = e$message
            )
        )
    ))
  })

  # Check if test_dir itself errored (from our tryCatch)
  if (!is.null(test_results_obj$summary$message) && grepl("Error during testhat::test_dir execution", test_results_obj$summary$message)) {
    return(test_results_obj)
  }


  # Process results from the reporter
  raw_results <- reporter$results
  
  parsed_details <- list()
  num_passed <- 0
  num_failed <- 0
  num_skipped <- 0
  num_errored <- 0 # For tests that error during execution
  num_warnings <- 0 # For tests that issue warnings but might still pass/fail

  if (length(raw_results) > 0) {
    for (i in 1:length(raw_results)) {
      res <- raw_results[[i]]
      
      status <- "unknown"
      msg <- ""
      if (is.character(res$message) && length(res$message) > 0) {
         msg <- paste(res$message, collapse = "\\n") # Ensure multiline messages are one string
      }

      test_name <- if (!is.null(res$test)) res$test else "Unnamed Test"
      test_file <- if (!is.null(res$file)) res$file else "Unknown File"
      test_context <- if (!is.null(res$context)) res$context else "Default Context"

      if (inherits(res, "expectation_success")) {
        status <- "passed"
        num_passed <- num_passed + 1
      } else if (inherits(res, "expectation_failure")) {
        status <- "failed"
        num_failed <- num_failed + 1
      } else if (inherits(res, "expectation_error")) {
        status <- "errored"
        num_errored <- num_errored + 1
        # Error messages might be structured differently or within res$message directly
        if (is.null(msg) || msg == "") {
            # Sometimes the error message is buried deeper for expectation_error
            # This part might need refinement based on actual error object structure from testhat
            err_msg_parts <- c()
            if(!is.null(res$message) && is.character(res$message)) err_msg_parts <- c(err_msg_parts, res$message)
            if(!is.null(res$srcref) && !is.null(attr(res$srcref, "srcfile")) && !is.null(attr(res$srcref, "lines"))) {
                 err_msg_parts <- c(err_msg_parts, paste("Location:", attr(res$srcref, "srcfile")$filename, "#L", attr(res$srcref, "lines")[1]))
            }
            if (length(err_msg_parts) > 0) msg <- paste(err_msg_parts, collapse="\\n") else msg <- "No specific error message captured."

        }
      } else if (inherits(res, "expectation_skip")) {
        status <- "skipped"
        num_skipped <- num_skipped + 1
      } else if (inherits(res, "expectation_warning")) {
        status <- "warning" # Testhat treats warnings separately.
        num_warnings <- num_warnings + 1
        # A test with a warning might still be a "pass" in terms of expectation.
        # For summary, we will report warnings but not count them as failures unless they are also errors/failures.
        # ListReporter logs warnings but the test might still pass.
        # We will list warnings separately.
      } else {
        # Fallback for unknown result types
        status <- "unknown_type"
        msg <- paste("Unknown result type:", class(res)[1])
        num_failed <- num_failed + 1 # Treat unknown as failure
      }
      
      # Only add to details if it's not a simple pass without message or a warning to be summarized elsewhere
      if (status %in% c("failed", "errored", "skipped") || (status == "passed" && msg != "") || status == "unknown_type") {
         parsed_details[[length(parsed_details) + 1]] <- list(
            file = test_file,
            context = test_context,
            test = test_name,
            status = status,
            message = msg
         )
      } else if (status == "warning") { # Log warnings separately if needed
         parsed_details[[length(parsed_details) + 1]] <- list(
            file = test_file,
            context = test_context,
            test = test_name,
            status = status,
            message = msg
         )
      }
    }
  }
  
  return(list(
    summary = list(
      passed = num_passed,
      failed = num_failed,
      skipped = num_skipped,
      errored = num_errored,
      warnings = num_warnings,
      total_tests = num_passed + num_failed + num_skipped + num_errored # Sum of definitive outcomes
    ),
    details = parsed_details
  ))
}
"""

def convert_r_list_to_py(r_list):
    """
    Converts an R list (ListVector) to a Python list of dictionaries or basic types.
    Handles nested lists and vectors.
    """
    if r_list == robjects.NULL:
        return None
    if isinstance(r_list, robjects.vectors.ListVector):
        # Convert to dict if it has names, else list
        if r_list.names != robjects.NULL and len(set(r_list.names)) == len(list(r_list.names)): # Check for unique names
            return {name: convert_r_list_to_py(r_list.rx2(name)[0]) for name in r_list.names}
        else:
            return [convert_r_list_to_py(item) for item in r_list]
    elif isinstance(r_list, robjects.vectors.StrVector):
        return [str(s) for s in r_list] if len(r_list) > 1 else (str(r_list[0]) if len(r_list) == 1 else "")
    elif isinstance(r_list, robjects.vectors.IntVector):
        return [int(i) for i in r_list] if len(r_list) > 1 else (int(r_list[0]) if len(r_list) == 1 else 0)
    elif isinstance(r_list, robjects.vectors.FloatVector): # NumericVector in R
        return [float(f) for f in r_list] if len(r_list) > 1 else (float(r_list[0]) if len(r_list) == 1 else 0.0)
    elif isinstance(r_list, robjects.vectors.BoolVector):
        return [bool(b) for b in r_list] if len(r_list) > 1 else (bool(r_list[0]) if len(r_list) == 1 else False)
    # Handle single primitive values that might not be wrapped in vectors by some rpy2 versions/contexts
    elif isinstance(r_list, (str, int, float, bool)):
        return r_list
    else: # Fallback for other types, may need more specific handlers
        try:
            # Try a default conversion for unknown complex types
            with localconverter(default_converter + conversion.converter) as cv:
                py_obj = conversion.rpy2py(r_list)
            # If it's a DataFrame or similar, it might become a complex object.
            # For this script, we expect lists, dicts, and primitives from the R function.
            if isinstance(py_obj, (list, dict, str, int, float, bool)):
                return py_obj
            else:
                # If conversion is not straightforward, represent as string or type
                # print(f"Warning: Unhandled R type during conversion: {type(r_list)}, {r_list}")
                return f"RPY2_OBJECT<{r_list.rclass[0] if r_list.rclass else type(r_list)}>"
        except Exception as e:
            # print(f"Conversion error for {type(r_list)}: {e}")
            return f"CONVERSION_ERROR<{type(r_list)}>"


def main():
    parser = argparse.ArgumentParser(description="Run R testhat tests for a given package source directory using rpy2.")
    parser.add_argument("package_dir", help="Path to the R package source directory.")
    args = parser.parse_args()

    package_path = os.path.abspath(args.package_dir)

    if not os.path.isdir(package_path):
        print(f"Error: Package directory not found: {package_path}")
        sys.exit(2)

    print(f"Attempting to run testhat tests for R package at: {package_path}")
    print("Initializing R through rpy2...")

    try:
        # Define the R function in the R global environment
        robjects.r(r_test_runner_script)
        print("R test runner function defined.")

        # Call the R function
        print(f"Executing R tests in directory: {package_path}...")
        r_results = robjects.r['run_rgemini_tests'](package_path)

        # Convert R results to Python
        py_results = convert_r_list_to_py(r_results)

        if py_results is None or 'summary' not in py_results or 'details' not in py_results:
            print("Error: Did not receive valid structured results from R script.")
            if py_results and 'summary' in py_results and 'message' in py_results['summary']:
                 print(f"R Execution Message: {py_results['summary']['message']}")
            sys.exit(1)

        summary = py_results['summary']
        details = py_results['details']

        print("\n--- Test Summary ---")
        if 'message' in summary and summary['message']:
            print(f"Message: {summary['message']}")

        total_tests = summary.get('total_tests', 0)
        passed_count = summary.get('passed', 0)
        failed_count = summary.get('failed', 0)
        errored_count = summary.get('errored', 0)
        skipped_count = summary.get('skipped', 0)
        warnings_count = summary.get('warnings', 0)

        print(f"Total tests run: {total_tests}") # This is sum of P,F,S,E
        print(f"Passed: {passed_count}")
        print(f"Failed: {failed_count}")
        print(f"Errored: {errored_count}") # Test itself had an R error
        print(f"Skipped: {skipped_count}")
        if warnings_count > 0:
            print(f"Warnings: {warnings_count}")
        print("--------------------\n")

        if failed_count > 0 or errored_count > 0 or (isinstance(details, list) and any(d['status'] not in ['passed', 'skipped'] for d in details)):
            print("--- Failures, Errors, and Warnings ---")
            if not isinstance(details, list):
                print("Details are not in the expected list format.")
            else:
                for item in details:
                    status = item.get('status', 'unknown').upper()
                    if status not in ['PASSED', 'SKIPPED'] or (status == 'WARNING' and warnings_count > 0): # Show warnings if any
                        print(f"{status}: Test '{item.get('test', 'N/A')}'")
                        print(f"  File: {item.get('file', 'N/A')}")
                        print(f"  Context: {item.get('context', 'N/A')}")
                        message = item.get('message', 'No message provided.')
                        # Ensure message is a string and handle potential list/vector representations
                        if isinstance(message, list):
                            message = "\\n".join(map(str, message))
                        print(f"  Message: {message.strip()}")
                        print("  ---")
            print("--------------------------------------\n")
            sys.exit(1) # Exit with error code if there are failures or errors
        else:
            print("All tests passed or were skipped.")
            sys.exit(0)

    except RRuntimeError as e:
        print("\n--- R Runtime Error ---")
        print(f"An error occurred during R execution: {e}")
        sys.exit(1)
    except Exception as e:
        print("\n--- Python Script Error ---")
        print(f"An unexpected error occurred in the Python script: {e}")
        traceback.print_exc()
        sys.exit(2)

if __name__ == "__main__":
    main()