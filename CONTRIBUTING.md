# How to contribute to `Rgemini`

We welcome contributions to `Rgemini` by all GEMINI team members. To submit a contribution:

1. Create a [new issue](https://github.com/GEMINI-Medicine/Rgemini/issues/) providing sufficient details about the bug report/feature request.

2. Assign yourself to the issue, or discuss the issue with the team to identify other (co-)developers.

3. Create a new branch off `develop`. The branch name should contain the issue number. 

4. Commit all changes to this newly created branch and include the issue number in each commit message. 

5. Once you finish code development, add a new item at the top of `NEWS.md` concisely describing what's changed. 

6. If appropriate, add unit tests in the `tests/testthat/` directory.

7. Submit a [pull request](https://help.github.com/articles/using-pull-requests) into the `develop` branch.

8. Ask a team member to review the changes (see guidelines for reviewers below) and implement additional changes based on the reviewer's feedback.

9. Once the reviewer has approved the pull request, squash all commits and merge the branch into `develop`, close the issue, and delete the branch you developed on.


# Reviewing code

All pull requests should be carefully reviewed by at least one person. When reviewing code:

1. Pull the updated code from the branch associated with the issue. You can also install the package from a specific branch using `remotes::install_github("GEMINI-Medicine/Rgemini@<branch_name>")`

2. Make sure that you can run the code without error messages. Check that it produces the expected outcome and resolves the issue. 

3. If possible, review each line of code that has been changed/added and provide feedback on anything you think could be improved. 

4. For more details on what to look out for during code reviews, please refer to the
[Code Review Checklist](https://docs.google.com/document/d/16kiIgwWjXYhBM5AFToXD7X9OjqNYs0xQZh9VDfSwYQU/edit?usp=sharing).
You can also find some general instructions on how to review pull requests
[here](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/reviewing-changes-in-pull-requests/about-pull-request-reviews).

5. Finally, check for any merge conflicts and resolve them together with the developer before approving the pull request. 


# Merging into `master`

We typically accumulate multiple changes on the `develop` branch before merging all changes into `master` and updating the package version number.

At least one person should review the pull request into `master` and should run the following final checks before approval:

1. Check whether there are any changes on `master` that are not yet on `develop`. If yes, merge `master` into `develop`. 

2. Make sure all changes on `develop` are summarized in `NEWS.md`. 

3. Decide on a new version number based on the guidelines [here](https://github.com/GEMINI-Medicine/Rgemini#package-versions).

4. Update the version number in the `NEWS.md` and `DESCRIPTION` files.

5. Run `devtools::document()` in R to make sure all documentation is up to date.

6. Run `rcmdcheck::rcmdcheck()` or `devtools::check()` and make sure no errors or warnings are returned.    

7. If everything looks good, approve the pull request and merge `develop` into `master`.

8. Add a new tag to the repository corresponding to the updated version number. 

9. Notify the team about the updated version. Ideally, all members should immediately update to the newest version of `Rgemini`.

10. Update for HPC4Health users: Please submit a request on the [HPC4Health File Transfer Log](https://app.smartsheet.com/sheets/p7P77qF97wcxgr2V4Cr6Vjqw3vjhCpRMQQH3Jwm1).
The systems team will then transfer the updated package to Nexus, allowing HPC4Health users to install the newest version of `Rgemini`.
