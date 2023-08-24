---
name: Release Prep template
about: Use this template to start a checklist of actions required to create a new
  version release for CRAN
title: "[RELEASE PREP]"
labels: ''
assignees: ''

---

Below are the steps used to prepare `FSAdata` for submission to CRAN. The steps should generally be followed in order (i.e., don't move to the next step without success at the current step). It is common to have to repeat some previous steps if an error/warning occurs at a subsequent step (in my experience, this most often happens after checking on the development version of R when using R-winbuilder).

It is worth noting that this whole process can take 2-3 hours (or more depending on internet speed) with significant "waiting time" for checks and builds.

----

### 0 - Preparatory Updates
- [ ] Update to latest R version.
- [ ] Update to latest RStudio version.
- [ ] Update all packages, especially those that `FSA` and `FSAdata` depend on.

### 1 - `dev` branch (at local repo)
- [ ] Ensure no outstanding issues in [checks on CRAN](https://cran.r-project.org/web/checks/check_results_FSAdata.html)
- [ ] Ensure that all branches emanating from `dev` have been appropriately merged to `dev` via pull request.
- [ ] Update `Version` field in **DESCRIPTION** (next number up without the .9000 on the end).
- [ ] Update `Date` field in **DESCRIPTION**.
- [ ] Update `Version` in top header of **NEWS.md** (next number up without the .9000 on the end).
- [ ] Ensure that **NEWS.md** contains descriptions of all major changes (this is usually done in "real-time" as changes are being made).
- [ ] Create new `cran-comments-vX_X_X.md` file in `cran-comments` folder. This will likely just be a copy of the same file from the previous version, but may include notes specific to inquiries from CRAN operators. [*Note that this will be used in Section 5 below, so leave the file open*).
- [ ] Run `pkgdown::build_site()` in the console of RStudio.

### 2 - `dev` branch (at local repo)
- [ ] Run **Build..Check** in RStudio. Address all errors, warnings, and notes and redo parts of the previous section as necessary. [*There should generally be no errors, warnings, or notes as the package should have been checked with each major change to `dev`. At times I will get a *note* about a new author or not being able to verify current time.*]
- [ ] Build a "source package" in RStudio with **More..Build Source Package**.
- [ ] Upload source package to all three "flavors" at [R-winbuilder](https://win-builder.r-project.org/upload.aspx). Wait for an e-mail reply from R-winbuilder. Any errors, warnings, or notes for any flavor should be addressed (after which this step, and possibly relevant steps above, should be repeated).
- [ ] Upload source package to both the "release" and "development" "flavors" at [R-macbuilder](https://mac.r-project.org/macbuilder/submit.html). Monitor results (especially "Check Log") page (will be given a link upon submission). Any errors, warnings, or notes for any flavor should be addressed (after which this step, and possibly relevant steps above, should be repeated).
- [ ] Push changes from above from local machine to `dev` branch on GitHub.

### 3 - `dev` branch (at remote GitHub repo)
- [ ] Ensure that "R-CMD-check.yaml" [GitHub action](https://github.com/fishR-Core-Team/FSA/actions) was successful for the `dev` branch (this should run automatically with the push to `dev`, but will take some time to finish (possibly >20 mins)). If not successful then address issues and repeat as much above as necessary.
- [ ] Create a pull-request asking to merge the `dev` branch to the `main` branch. Ask someone from the FSAdata team to review the request. [*Ensure that all checks were successfully completed.*]

### 4 - `main` branch (at remote GitHub repo)
- [ ] Merge approved (assumingly) pull request from `dev` branch.

### 5 - `main` branch (at local repo)
- [ ] Fetch the remote `main` branch and pull the updates to local `main` branch so that the local and remote `main` branches are the same.
- [ ] Build a "source package" in RStudio with **More..Build Source Package**.
- [ ] Build a "binary package" in RStudio with **More..Build Binary Package**.
- [ ] Goto the [CRAN submission page](https://cran.r-project.org/submit.html). Enter your first and last names. Press `Choose File` and choose the **source** file (.tar.gz) created above. Copy the comments from the relevant `cran-comments-vX_X_X.md` and paste into the `optional comments` box. Press the `Upload Package` button.
- [ ] Review the ensuing page (it is usually correct) and press `Submit Package` button at the bottom.
- [ ] Wait for an e-mail from CRAN (this is usually pretty quick) and press the contained link to open a new webpage.
- [ ] Check all three boxes (make sure that your answers are truthful) and press `Upload the Package to CRAN`. You should get a confirmation e-mail almost immediately (no need to respond to this).

### 6 - `main` branch (at remote GitHub repo)
- [ ] Create a [new release](https://github.com/fishR-Core-Team/FSAdata/releases). `Tag` should relate to the version number and the `Title` should state that the version number is being released to CRAN (see past examples). Make sure that the `Target` is set to `Main`. Add an optional `Description` if desired. Drag and drop the source (.tar.gz) and binary (.zip) files created in the previous step on to the `Attach binaries ...` box. Press `Publish Release`.

### 7 - `dev` branch (at remote GitHub repo)
- [ ] Create a pull-request asking to merge the `main` branch to the `dev` branch. This is needed after merging `dev` to `main` above so that the two branches are synced.
- [ ] Merge pull request from `main` branch (*OK to do this without getting approval from FSAdata team member*). *Note that the `dev` branch will say it is "1 commit ahead of `main`" after this step.*

### 8 - `dev` branch (at local repo)
- [ ] Fetch the remote `dev` branch and pull the updates to local `dev` branch so that the local and remote `dev` branches are the same.
- [ ] Update `Version` field in **DESCRIPTION** by appending .9000 to the end.
- [ ] Create a new section at the top of **NEWS.md**  with the new version number (i.e., including the .9000 on the end).
- [ ] Push these changes to the remote `dev` branch. *This is the start of the next version.*
