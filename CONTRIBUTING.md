# How to contribute to `Rgemini`

We welcome contributions to `Rgemini` by all GEMINI team members. 

To submit suggestions for bug fixes/enhancements, please create a [new issue](https://github.com/GEMINI-Medicine/Rgemini/issues/) providing sufficient details about the bug report/feature request.

## Code development 

To make changes to the `Rgemini` codebase/documentation, please follow these steps and guidelines:

1. Create a new issue or assign yourself to an existing issue you want to work on.

2. Create a new branch off `develop`. The branch name should contain the issue number. 

3. Commit all changes to this newly created branch and include the issue number in each commit message. 
  
4. Run a styler and linter on any new code using `styler:::style_active_file()` and `lintr::lint("path/to/file.R")`. Make sure you are using `lintr` version 3.0.0 or newer for compatibility with the package `.lintr` file.

5. If appropriate, add unit tests in the `tests/testthat/` directory. For bug fixes, it's usually helpful to include a unit test for the identified bug. Note that the CI/CD workflow automatically runs all unit tests in both R and Python (via rpy2) to ensure cross-language compatibility. For Python-specific debugging during development, see the [Python Testing Workflow guide](https://github.com/GEMINI-Medicine/Rgemini/discussions/201).
   
6. If adding new functions, update the `_pkgdown.yml` file by adding the new function to the appropriate reference section. Note that the `Rgemini` repository currently has a CI/CD workflow to generate and commit documentation on any pushed commit, but `devtools::document()` can also be run during development to debug locally. 

7. If adding new package dependencies: First check whether existing dependencies listed in (DESCRIPTION.md)[https://github.com/GEMINI-Medicine/Rgemini/blob/main/DESCRIPTION] could achieve a similar task (we want to try and minimize package dependencies where possible). If you do require a new library, please add it under the `Imports` section in (DESCRIPTION.md)[https://github.com/GEMINI-Medicine/Rgemini/blob/main/DESCRIPTION]. If a package is not strictly necessary to run `Rgemini` functions, but is used for illustration purposes in vignettes, please list it under `Suggests`.
   
8. Functions derived based on published articles (e.g. clinical scores) should include a `references` section in `roxygen`. The citation format is: `1st_author_last_name 1st_author_initial, et al. Journal Abbreviation, Year. https://doi.org/xxxx.`. URLs should be presented as-is without hyperlinking and DOI URLs should be used whenever possible. Note that commonly known online resources (e.g. links CIHI documentations, data dictionary) might not require a `references` section and can be included as hyperlinks in any `roxygen` section.

9. Any new package data should be saved as an `.rda` file in the `data/` directory, and should be documented with `roxygen` in `data.R`.

10. Once you finish code development and all relevant documentation, add a new item at the top of `NEWS.md` concisely describing what's changed.

11. Submit a [pull request (PR)](https://help.github.com/articles/using-pull-requests) into the `develop` branch.

12. Ask a team member to review the changes (see guidelines for reviewers below) and implement additional changes based on the reviewer's feedback.

13. Once the reviewer has approved the pull request, resolve any merge conflicts and CI/CD errors.

14. Finally, squash all commits (confirm the PR # is referenced in the squash commit message) and merge the branch into `develop`, close the issue, and delete the branch you developed on.


## Reviewing code

All pull requests (PRs) should be carefully reviewed by at least one person. For detailed instructions on what to look out for during review, please refer to the
[Code Review Checklist](https://docs.google.com/document/d/16kiIgwWjXYhBM5AFToXD7X9OjqNYs0xQZh9VDfSwYQU/edit?usp=sharing). 

Briefly, when reviewing code:

1. Assign yourself as reviewer for this PR (go to "Pull requests" -> click on the PR -> click on "assign yourself" under "Reviewers").
  
2. Pull the updated code from the branch associated with the issue. You can also install the package from a specific branch using `remotes::install_github("GEMINI-Medicine/Rgemini@<branch_name>")`

3. Make sure that you can run the code without error messages. Check that it produces the expected outcome and resolves the issue. 

4. If possible, review each line of code that has been changed/added and provide feedback on anything you think could be improved. To comment on individual lines of code, go to "Pull requests" -> click on the PR -> "Files changed" -> `+` to add your comments.

5. You can also share general feedback when you submit your review, or use the "Conversation" section of the PR to discuss open questions with the developer throughout the review process.

6. Please share all feedback on GitHub (instead of slack/email) for transparency and to make sure previous discussions are well documented. 

7. As a reviewer, you usually should not push any changes to the code yourself, but instead, mention any suggestions in your review and the developer will be responsible for implementing the changes.

8. Once the developer has addressed your comments, approve the pull request (the developer will then merge the PR and close the issue).


## Merging into `main`

We typically accumulate multiple changes on the `develop` branch before merging all changes into `main` and updating the package version number.

At least one person should review the pull request into `main` and should run the following final checks before approval:

1. Check whether there are any changes on `main` that are not yet on `develop`. If yes, merge `main` into `develop`. 

2. Make sure all changes on `develop` are summarized in `NEWS.md`.

3. If new functions have been added, make sure they are listed in `_pkgdown.yml`. 

4. Decide on a new version number based on the guidelines [here](https://github.com/GEMINI-Medicine/Rgemini#package-versions).

5. Update the version number in the `NEWS.md` and `DESCRIPTION` files.

6. Run `devtools::document()` in R to make sure all documentation is up to date.

7. Run `rcmdcheck::rcmdcheck()` or `devtools::check()` and make sure no errors or warnings are returned.    

8. If everything looks good, approve the pull request and merge `develop` into `main` (without squashing commits).

9. Add a new tag to the repository corresponding to the updated version number. 

10. Notify the team about the updated version. Ideally, all members should immediately update to the newest version of `Rgemini`.

11. Update for HPC4Health users: Download the `tar.gz` file of the newest package version and save it in `R:/GEMINI/HPC4Health/Rgemini versions/`. Submit a request on the [HPC4Health File Transfer Log](https://app.smartsheet.com/sheets/p7P77qF97wcxgr2V4Cr6Vjqw3vjhCpRMQQH3Jwm1) for the package file to be transferred to `/mnt/nfs/pkgs/GEMINI/`. Ask the systems team to send out an email to all HPC4Health users announcing the new release. 
