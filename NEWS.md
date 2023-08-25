# FSA 0.9.5
* Fixed FSA-package \alias problem using the "automatic approach" (i.e., adding a "_PACKAGE" line to FSA.R) suggested in an e-mail from Kurt Hornik on 19-Aug-2023.

# FSA 0.9.4
* Changes related to moving to fishR-Core-Team
  * Updated sticker.
  * Changed DHO e-mail address (in DESCRIPTION and in all author fields of the documentation). Partially address [#86](https://github.com/fishR-Core-Team/FSA/issues/86).
  * Updated `pkgdown.yaml` GitHub action to [v2](https://github.com/r-lib/actions/tree/v2-branch/examples#build-pkgdown-site). Changed action to only run on a release (rather than a push) but it can be [run manually](https://docs.github.com/en/actions/managing-workflow-runs/manually-running-a-workflow) as well.
  * Updated `R-CMD-check.yaml` GitHub action to [v2](https://github.com/r-lib/actions/tree/v2-branch/examples#standard-ci-workflow). Note that I had to add the [extra code for dealing with graphics on the Mac version](https://github.com/r-lib/actions#common-questions).
* Changes related to new fishR webpage
  * Updated links in `fishR()`, `FSA()`, and `README.md`. Partially address [#86](https://github.com/fishR-Core-Team/FSA/issues/86).
  * Updated all links to Introductory Fisheries Analyses with R book.
  * Added links to CSV files for all data sets. This addresses [#96](https://github.com/fishR-Core-Team/FSA/issues/96).
  * Changed theme in `_pkgdown.yml` to match that of `FSAdata` and more closely match `fishR`.
  * Removed most recent dates from NEWS file as `pkgdown` picks up the CRAN release date to add.
  * Updated `CITATION` (to match that required for next version of R).

* `alkIndivAge()`: Modified. Added a catch for `NA`s in the length sample. Also added a test. This addresses [#88](https://github.com/fishR-Core-Team/FSA/issues/88).
* `confint.boot()`: Modified. Changed hard-coding of columns that contained the confidence interval values to find those columns by `grep()`ing for the `%` sign. This fixes an issue related to `car::Confint()` returning the `coef()` results for functions that have a `coef()` method but not for those that do not. Also updated tests to use results from `car::Boot()` rather than the old `car::bootCase()`.
* `PSDcalc`: Modified. Changed code to allow for missing `species=` as long as `addLens=` is used. This allows the user to provide length categories for a species for which Gabelhouse lengths are not defined. Several new tests were added and some were modified to handle the changing message re: a missing `species=`. The documentation was modified accordingly. This (finally) addresses [#58](https://github.com/fishR-Core-Team/FSA/issues/58).
* `PSDlit`: Modified. Added info for Redbreast Sunfish and Spotted Sunfish from Bonvecchio *et al.* (2023). This addresses [#100](https://github.com/fishR-Core-Team/FSA/issues/100)).
* `wSlit`: Modified documentation. Described the `RLP` and `EmP` acronyms and provided references for them. This addresses [#95](https://github.com/fishR-Core-Team/FSA/issues/95)). Added info for Redbreast Sunfish and Spotted Sunfish from Bonvecchio *et al.* (2023). This addresses [#100](https://github.com/fishR-Core-Team/FSA/issues/100)).

# FSA 0.9.3
* Moved `dplyr` from `imports` to `suggests` (needed because functions were removed in last version; however it is still used in some examples; partially addresses [#87](https://github.com/fishR-Core-Team/FSA/issues/87)).
* Removed `sciplot` from `imports` (because functions were removed in last version; partially addresses [#87](https://github.com/fishR-Core-Team/FSA/issues/87)).
* Updated tests for `ksTest()` to handle issues on the CRAN M1 build machine (per e-mail from Prof. Ripley on 15-Feb-22; partially addresses [#87](https://github.com/fishR-Core-Team/FSA/issues/87)).
* Updated all links to the `droglenc` github that were related to `FSA` or `FSAdata` to be to the `fishR-Core-Team` github.

# FSA 0.9.2 12-Feb-21
* Last version maintained by Derek Ogle. Transferring to fishR Core Team for next version.
* `filterD()`: **REMOVED** (to `FSAmisc`).
* `fitPlot()`: **REMOVED** (to `FSAmisc`).
* `fsaNews()` and `FSANews()`: **Removed**.
* `psdAdd()`: Modified. Changed the way `PSDlit` was loaded into the function environment so that `FSA::psdAdd()` will work. Addresses [#85](https://github.com/fishR-Core-Team/FSA/issues/85).
* `PSDLit`: Modified. Added info for Utah Chub (from [here](https://webpages.uidaho.edu/quistlab/publications/NAJFM_2021_Black_et_al_UTC_Ws_length_categories.pdf); address [#84](https://github.com/fishR-Core-Team/FSA/issues/84)).
* `psdVal()`: Modified. Changed the way `PSDlit` was loaded into the function environment so that `FSA::psdVal()` will work. Addresses [#85](https://github.com/fishR-Core-Team/FSA/issues/85).
* `residPlot()`: **REMOVED** (to `FSAmisc`).
* `wrAdd()`: Modified. Changed the way `WSlit` was loaded into the function environment so that `FSA::wrAdd()` will work. Addresses [#85](https://github.com/fishR-Core-Team/FSA/issues/85).
* `WSLit`: Modified. Added info for Utah Chub (from [here](https://webpages.uidaho.edu/quistlab/publications/NAJFM_2021_Black_et_al_UTC_Ws_length_categories.pdf); address [#84](https://github.com/fishR-Core-Team/FSA/issues/84)).
* `wsVal()`: Modified. Changed the way `WSlit` was loaded into the function environment so that `FSA::wsVal()` will work. Addresses [#85](https://github.com/fishR-Core-Team/FSA/issues/85).

# FSA 0.9.1
* Corrected testing issue for `catchCurve()` and `chapmanRobson()` as directed by CRAN on 17-Jul-21. Issue likely caused by changes to `fishmethods` package.

# FSA 0.9.0
* Make note of the several **removed** (now defunct) and **deprecated** (soon to be defunct) functions listed below.
* Added Jason Doll as an `AUThor`.
* Moved `plyr` from Imports to Suggests.
* `alkPlot()`: Modified. Removed use of `chooseColors()` (see below).
* `binCI()`: Modified.  Changed to use `iCheckConfLevel()` (which addresses [#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
* `bootCase()`: **REMOVED**. Users can use `car::Boot()`, which partially addresses [#65](https://github.com/fishR-Core-Team/FSA/issues/65).
  * `plot.boot()`: **REMOVED**. Conflicted with `boot::plot.boot()` which caused an error with CRAN. Same functionality is available with `pairs(<boot object>$t)`.
* `catchCurve()`: Modified. Removed hard-coding of `ylim=` for `plot.catchCurve()` (this addresses [#70](https://github.com/fishR-Core-Team/FSA/issues/70) ... Thanks to Brendan Runde). Added `round.est=` so that the user can control the decimals on mortality estimate values. Changed to use `iCheckConfLevel()` (which addresses [#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
* `chapmanRobson()`: Modified. Removed hard-coding of `ylim=` for `plot.chapmanRobson()`. Added `round.est=` so that the user can control the decimals on mortality and survival estimate values. Changed to use `iCheckConfLevel()` (which addresses [#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
* `chooseColors()`: **REMOVED**. This was an exported function that should have been internal. Regardless, where it was used has been removed and the user is now allowed to provide their own vector of colors. See `iCheckMultColors()`. Partially addresses [#65](https://github.com/fishR-Core-Team/FSA/issues/65).
* `compIntercepts()`: **REMOVED** (to `FSAmisc`). Added to `FSA-defunct`. Partially addresses [#65](https://github.com/fishR-Core-Team/FSA/issues/65).
* `compSlopes()`: **REMOVED** (to `FSAmisc`). Added to `FSA-defunct`. Partially addresses [#65](https://github.com/fishR-Core-Team/FSA/issues/65).
* `depletion()`: Modified. Changed to use `iCheckConfLevel()` (which addresses [#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
* `filterD()`: **DEPRECATED** (partially addresses [#65](https://github.com/fishR-Core-Team/FSA/issues/65)).
* `fitPlot()`: **DEPRECATED** (partially addresses [#65](https://github.com/fishR-Core-Team/FSA/issues/65)). Prior to that removed use of `chooseColors()` (see above).
* `diags()`: **REMOVED** (moved to `FSAmisc`). Added to `FSA-defunct`. Partially addresses [#65](https://github.com/fishR-Core-Team/FSA/issues/65).
* `hoCoef()`: **REMOVED** (moved to `FSAmisc`). Added to `FSA-defunct`. Partially addresses [#65](https://github.com/fishR-Core-Team/FSA/issues/65).
* `hyperCI()`: Modified. Changed to use `iCheckConfLevel()` (which addresses [#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
* `iCheckConfLevel()`: Added (internal to address [#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
* `iCheckMultColors()`: Added (internal). Part of removing `chooseColors()` and `paletteChoices()`.
* `iRichColors()`: **REMOVED** (as part of removing `chooseColors()`).
* `mapvalues()`: **REMOVED**. Added to `FSA-defunct`. Partially addresses [#65](https://github.com/fishR-Core-Team/FSA/issues/65).
* `mrClosed()`: Modified. Changed to use `iCheckConfLevel()` (which addresses [#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
* `mrOpen()`: Modified. Added a warning for when r==0, which causes the SE of M and thus N to be `Infinity` (this addresses [#69](https://github.com/fishR-Core-Team/FSA/issues/69)). Added a similar warning for when R==0 (but not the last time period). Added tests for the warning messages. Changed one use of `apply()` to `rowSums()` for ease of reading. Changed to use `iCheckConfLevel()` (which addresses [#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
* `paletteChoices()`: **REMOVED**. See `chooseColors()` above. Partially addresses [#65](https://github.com/fishR-Core-Team/FSA/issues/65).
* `poiCI()`: Modified. Changed to use `iCheckConfLevel()` (which addresses [#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
* `psdAdd()`: Modified. Changed a `levels()` in `iPSDlitCheck()` to `unique()` because `species` is no longer a factor due to updating `PSDlit` (i.e., rdata file changed with new `read.csv()`).
* `psdCalc()`: Modified. Added a catch for when "tibble"s are sent in `data=` (addresses [#75](https://github.com/fishR-Core-Team/FSA/issues/75)). Changed to use `iCheckConfLevel()` (which addresses [#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
* `psdCI()`: Modified. Changed to use `iCheckConfLevel()` (which addresses [#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
* `PSDlit`: Modified. Added length categories for Shoal Bass and Pallid Sturgeon. Added Striped Bass (Hybrid) and Striped Bass x White Bass; though these are the same as the existing Palmetto Bass. Added "source"s for each entry.
* `psdVal()`: Modified. Changed a `levels()` in `iPSDlitCheck()` and `iListSpecies()` to `unique()` because `species` is no longer a factor due to updating `PSDlit` (i.e., rdata file changed with new `read.csv()`). Added a `showJustSource=` argument that will show the source info (if `TRUE`) or not (if `FALSE`; default), which partially addresses [#76](https://github.com/fishR-Core-Team/FSA/issues/76).
* `removal()`: Modified. Added check and then warning if non-whole numbers are in `catch=` (addresses [#60](https://github.com/fishR-Core-Team/FSA/issues/60)). Also modified checks of data integrity to be more robust (e.g., if a character vector is sent). Changed to use `iCheckConfLevel()` (which addresses [#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
* `residPlot()`: **DEPRECATED** (partially addresses [#65](https://github.com/fishR-Core-Team/FSA/issues/65)). Prior to that removed use of `chooseColors()` (see above).
* `Subset()`: **REMOVED**. Added to `FSA-defunct`. Partially addresses [#65](https://github.com/fishR-Core-Team/FSA/issues/65).
* `wrAdd()`: Modified. Changed a `levels()` in `iwsLitCheck()` to `unique()` because `species` is no longer a factor due to updating `WSlit` (i.e., rdata file changed with new `read.csv()`).
* `WSlit`: Modified. Addresses [#68](https://github.com/fishR-Core-Team/FSA/issues/68).
    * Corrected capitalization of "Cavedano Chub", "European Chub", "Lake Herring" (metric), and "Pursak Chub".
    * Removed "not in Neumann et al. (2012)" notes.
    * Added African Sharptooth Catfish, Ankara Nase, Bighead and Silver Carp, Brook Trout (Appalachia), Fourbarbel Scraper, Horse Barbel, Nile Tilapia, Nipple-Lipped Scraper, Shoal Bass, South European Roach, Spotted Bass (Alabama subspecies) (AKA Alabama Bass).
    * Added Striped Bass (Hybrid) and Striped Bass x White Bass; though these are the same as the existing Palmetto Bass.
* `wsVal()`: Modified. Changed a `levels()` in `iwsLitCheck()` to `unique()` because `species` is no longer a factor due to updating `WSlit` (i.e., rdata file changed with new `read.csv()`).

# FSA 0.8.32
* Removed Travis-CI and appveyor.
* No longer using coveralls for coverage statistics. Changed to codecov.io.
* Added GitHub Action for CI/CD (used `usethis::use_github_action_check_standard()`).
* Added GitHub Action for pkgdown (used `usethis::use_github_action("pkgdown")`).
* Added GitHub Action for code coverage with codecov.io.
* Added a code of conduct for contributors.
* Moved a bunch of  plotting examples in the documentation to `tests\plottests\` to speed up testing. The `tests\plottests\` was added to `.Rbuildignore` .
* `hist.formula()`: Modified. Fixed bug with y-axes when `freq=FALSE` is used (fixes [#62](https://github.com/fishR-Core-Team/FSA/issues/62); thanks to @carlostorrescubila).
* `fitPlot()`: Modified. Fixed bugs with handling models that used character rather than factor variables.
* `plotBinResp()`: REMOVED. Removed as a user-facing function, but made as an internal function for continued use in `fitPlot()` while `fitPlot()` is deprecated.
* `psdAdd()`: Modified. Fixed bug relate to species that were `NA` (fixes [#64](https://github.com/fishR-Core-Team/FSA/issues/64); thanks to Dan Shoup). Added more tests and fixed some typos in the documentation.
* `psdPlot()`: Modified. Fixed bug with box around the plot when `add.psd=FALSE`. Added 5% expansion to top of y-axis so that bars did not run into the box.
* `residPlot()`: Modified. Fixed bugs with handling models that used character rather than factor variables.

# FSA 0.8.31
* Now using roxygen v7.1.1.
* Added `tibble` to suggests (see comment about `headtail()` below).
* Cleaned up the documentation of parameters for `RichardsFuns()` (documentation did not change when parameter letters were changed for the Age and Growth book).
* Changed example in `headtail()` to use `as_tibble()` from `tibble` package rather than `tbl_df()` from `dplyr` package. Required adding `tibble` to suggests.
* `nlsTracePlot()`: Modified. Created a conditional catch depending on the version of R as the results of `nls(*,trace=TRUE)` are changing in v4.1.0 (per e-mail from Martin Maechler on 2-Nov-20).

# FSA 0.8.30
* **Date:** 9-Mar-20
* Started using `rhub::check_for_cran()` for checking before sending to CRAN.
* Updated tests for `Summarize()` and `ksTest()` that used `data.frame()`. This should have been done with v0.8.28.
* Fixed errors for tests in `ksTest()` that were identified using R-hub.
* Removed all links to documentation in non-dependent or non-suggested packages. This removes a note from R-hub.
* `fishR()`: Modified. Changed base URL to `https:` (from `http:`). Added `open=`, primarily to allow not opening a browser during testing.

# FSA 0.8.29
* **Date:** 8-Mar-20
* Removed dependency on `epitools` package as it may soon be orphaned. See changes to `binCI()` and `poiCI()` outlined below.
* `binCI()`: Modified. Added internal functions that are based on (but not identical to) functions in the `epitools` package which will possibly be deprecated soon (per note from CRAN on 7-Mar-20).
* `poiCI()`: Modified. Added internal functions that are based on (but not identical to) functions in the `epitools` package which will possibly be deprecated soon (per note from CRAN on 7-Mar-20).

# FSA 0.8.28
* **Date:** 28-Feb-20
* `fitPlot()`: Modified. Changed so that lines are plotted after the points in the IVR versions.
* `ksTest()`: Modified. Changed documentation examples to handle R's new way of handling `stringsAsFactors=` (per request from CRAN on 27-Feb-20).
* `psdAdd()`: Modified. Changed testing to handle R's new way of handling `stringsAsFactors=` (per request from CRAN on 27-Feb-20).

# FSA 0.8.27
* Now using ROxygen2 7.0.2.
* Removed dependency on `gplots` package as it is now orphaned. Required adding `iRichColors()` internal function.
* `lwCompPreds()`: Removed `\dots` from arguments as it was not in usage (per request from CRAN on 3-Feb-20).
* `repeatedRows2Keep()`: Modified. Now makes comparisons as if `NA`s are regular values.

# FSA 0.8.26
* Changed to depending on `R >=3.5.0`, because that is the latest version required by a package (i.e., `car`) that FSA imports or suggests. Used the "check_r_versions_of_package_dependencies" shiny app by "ateucher" (on Github) to help determine this.
* Removed `asbio` package from suggests as it hung up Travis-CI build (because of the need for the TCLTK package).
* `capFirst()`: Modified. Fixed bug related to an `NA` item.
* `psdAdd()`: Modified. Changed `spec=` to `species=` to be consistent with `psdCalc()` and `psdPlot()`.
* `peek()`: Added.
* `repeatedRows2Keep()`: Modified. Added a catch if the data.frame only contains one row (it then returns `TRUE` so that that row is kept).

# FSA 0.8.25 24-Jul-19
* `agePrecision()`: Modified. Changed so that PE2 and CV2 use the median in the entire calculation rather than just in the denominator.
* `iHndlColsUseIgnore()`: Modified. Changed so that a 0 indice returns an error.
* `repeatedRows2Keep()`: Added.
* `vbStarts()`: Modified. Corrected bug related to point transparenty when `plot=TRUE` and there is no age/length combination repeats (i.e., all age/length combinations are unique). Corrected bug of `col.main=` being ignored when `plot=TRUE`.

# FSA 0.8.24 17-May-19
* Corrected misuses of `\concept` in Rd files per CRAN request.

# FSA 0.8.23 1-May-19
* Reorganized `testthat` folder as suggested in `testthat` release notes.
* Removed all uses of `Subset()` (replaced with `filterD()`).
* `fitPlot()`: Modified. Fixed bug related to y-axis limits not extending to contain the data, confidence bands, or prediction bands (in `fitPlot.slr()`). This addresses [#3](https://github.com/droglenc/NCStats/issues/3) listed for `NCStats`).
* `hist.formula()`: Modified. Fixed bug related to subsequent calls after a call that used `iaxs=FALSE`. This addresses [#46](https://github.com/fishR-Core-Team/FSA/issues/46).
* `iLegendHelp()`: Modified. Added a catch if a proper keyword is not supplied.
* `nlsTracePlot()`: Modified. Moved error catching for improper keyword for legend placement forward.
* `SchnuteRichards()`: Added. This addresses [#54](https://github.com/fishR-Core-Team/FSA/issues/54).

# FSA 0.8.22
* Corrected CITATION file.
* Updated tests for changes in the `fishmethods` package (`vblrt()` replaced with `growthlrt()` and `T=` replaced with `TC=` in `M.empirical()`) per CRAN request.

# FSA 0.8.21
* Added a webpage. Setup Travis-CI to handle updates.
* Added a hex sticker logo.
* Added `withr` to Imports (see usages below).
* Added `Encoding: UTF-8` to DESCRIPTION.
* Added Powell Wheeler as an author for their work adding `method="Burnham"` to `removal()`.
* Added Alexis Dinno as an author for their providing the base functionality of `dunnTest()`. Should have done this long ago.
* Removed all `data()` in examples that referred to data from this package. Included the `package=` argument in `data()` that loaded from other packages.
* Added `seealso`, with links to which functions use the data for examples, to docmentation for all data.frames.
* Removed `\dontrun()`s from the `bootCase` related examples now that `car` package is updated. This addresses [#45](https://github.com/fishR-Core-Team/FSA/issues/45).
* `addZeroCatch()`: Modified. Added a catch that turns a "tibble" into a regular data.frame (which obviates some errors that occur with tibbles). Minor changes to documentation and comments in the code.
* `agePrecision()`: Modified. Added intermediate and summary calculations for median and modal age; average absolute deviation and standard deviation; APE and CV with the median rather than the mean as the divisor; and index of precision (D). Added `show.prec2=`. Updated tests and examples. Other minor modifications to the function code. Addresses [#41](https://github.com/fishR-Core-Team/FSA/issues/41) and [#49](https://github.com/fishR-Core-Team/FSA/issues/49).
* `alkIndivAge()`: Modified. Replaced an `options(warn=-1)` with `suppressWarnings()`.
* `alkPlot()`: Modified. Changed to using `withr::local_par()` (partially addresses [#38](https://github.com/fishR-Core-Team/FSA/issues/38)). Replaced an `options(warn=-1)` with `suppressWarnings()`.
* `alkSummaries()`: Modified. Replaced an `options(warn=-1)` with `suppressWarnings()`.
* Bootstrapping functions: Modified. Changed to using `withr::local_par()` (partially addresses [#38](https://github.com/fishR-Core-Team/FSA/issues/38)).
* `capHistSum()`: Modified. Changed to using `withr::local_par()` (partially addresses [#38](https://github.com/fishR-Core-Team/FSA/issues/38)).
* `chapmanRobson()`: Modified. Changed to using `withr::local_par()` (partially addresses [#38](https://github.com/fishR-Core-Team/FSA/issues/38)).
* `fishR()`: Modified. Changed examples in documentation to not be run (so as not to open an external webpage).
* `FSAnews()`: Modified. Changed examples in documentation to not be run (so as not to open an external webpage).
* Growth models: Modified. Changed to using `withr::local_par()` (partially addresses [#38](https://github.com/fishR-Core-Team/FSA/issues/38)).
* `hist.formula()`: Modified. Changed to using `withr::local_par()` (partially addresses [#38](https://github.com/fishR-Core-Team/FSA/issues/38)).
* `lwCompPreds()`: Modified. Changed to using `withr::local_par()` (partially addresses [#38](https://github.com/fishR-Core-Team/FSA/issues/38)).
* `plotAB()`: Modified. Now in its own documentation file (rather than with `ageBias()`).
* `psdCalc()`: Modified. Better handled the situation where the user asks for summaries with some fish greater than stock size but no fish greater than quality size (addresses [#50](https://github.com/fishR-Core-Team/FSA/issues/50); thanks to Timothy Spier for the bug report).
* `removal()`: Modified. Added `method="Burhnam"` via the [#51](https://github.com/fishR-Core-Team/FSA/pull/51) from Powell Wheeler.
* `residPlot()`: Modified. Changed to using `withr::local_par()` (partially addresses [#38](https://github.com/fishR-Core-Team/FSA/issues/38)).
* `SMBassWB`: Modified. Fixed minor data entry error in row 383.
* `vbFuns()`: Modified. Added `Francis3` to the list of models.

# FSA 0.8.20
* Added `asbio`, `DescTools`, `nlme`, and `psych` packages to Suggests because they are used in tests (and as will soon be required by CRAN ... per an e-mail from CRAN on 17-May-18).
* Fixed a bunch of bad links to other packages in the documentation.
* Removed the "Date" field from the Description file.
* `addRadCap()`: Removed. Moved to `RFishBC` package.
* `bcFuns()`: Removed. Moved to `RFishBC` package.
* `gConvert()`: Removed. Moved to `RFishBC` package.
* `mrClosed()`: Modified. Fixed a bug that was related to `poiCI()` returning results from all four types. Now `mrClosed()` will use only one type. Thanks to Timothy Spiers for pointing out this bug.
* `SMBassWB`: Modified. Fixed minor data entry error in row 404. Changed link in documentation from `alr3` to `alr4` package.

# FSA 0.8.19
* `addZeroCatch()`: Modified. Changed two `1:nrow()` structures to `seq_len(nrow())` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `ageBias()`: Modified. Changed all `1:` structures to `seq_len()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `agePrecision()`: Modified. Changed all `1:` structures to `seq_len()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `bcFuns()`: Modified. Changed three `1:length()` structures to `seq_along()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `bootCase()` methods: Modified. Changed all `1:` structures to `seq_len()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `capHistConvert()`: Modified. Changed all `1:` structures to `seq_len()` or `seq_along()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `capHistSum()`: Modified. Changed all `1:` structures to `seq_len()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `chooseColors()`: Modified. Changed all `1:` structures to `seq_len()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `compSlopes()`: Modified. Changed all `1:` structures to `seq_len()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `compIntercepts()`: Modified. Changed all `1:` structures to `seq_len()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `extraSS()`: Modified. Changed all `1:` structures to `seq_len()` or `seq_along()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `headtail()`: Modified. Changed all `1:` structures to `seq_len()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `hist.formula()` methods: Modified. Changed all `1:` structures to `seq_len()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `htest.boot()`: Removed (added last version) until I can test more.
* `iHndlFormula()`: Modified. Now categorizes a character variable as a factor variable. This addresses [#35](https://github.com/fishR-Core-Team/FSA/issues/35)) for `hist.formula()` and `Summarize()`.
* `lrt()`: Modified. Changed all `1:` structures to `seq_len()` or `seq_along()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `lwCompPreds()`: Modified. Changed all `1:` structures to `seq_len()` or `seq_along()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `mrClosed()`: Modified. Changed two `1:length()` structures to `seq_along()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `nlsBoot()` methods: Modified. Changed all `1:` structures to `seq_len()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `plot.boot()`: Removed (added last version) until I can test more.
* `predict.boot()`: Removed (added last version) until I can test more.
* `psdAdd()`: Modified. Changed three `1:` structures to `seq_len()` or `seq_along()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `residPlot()`: Modified. Changed three `1:length()` structures to `seq_along()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `Summarize()`: Modified. Changed one `1:length()` structure to `seq_along()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
* `wrAdd()`: Modified. Changed three `1:` structures to `seq_len()` or `seq_along()` (partially addressing [#36](https://github.com/fishR-Core-Team/FSA/issues/36)).

# FSA 0.8.18
* **Date:** 31-Mar-18
* Changed to depending on `R >=3.2.0`, because that is the latest version required by a package (i.e., `car`) that FSA imports or suggests. Used the "check_r_versions_of_package_dependencies" shiny app by "ateucher" (on Github) to help determine this.
* Using latest `testthat` package.
* `bootCase()`: Added. This was added because `bootCase()` will soon be removed from the `car` package. It was added so that the code in the Introductory Fisheries Analyses with R book will still work. It is largely a wrapper to `Boot()` in `car` with `method="case"`. The documentation was updated somewhat.
* `catchCurve()`: Modified. Changed the weighted regression method so that negative weights are set to zero rather than the minimum of the positive values (brought to my attention by Vaskar Nepal KC). Also added an `rSquared()` method (per request by Vaskar Nepal KC).
* `depletion()`: Modified. Added an `rSquared()` method.
* `expandCounts()`: Modified. Minor changes to documentation.
* `hTest.boot()`: Added.
* `plot.boot()`: Added.
* `plotAB()`: Modified. Added `col.numbers=` to allow users to modify the color of the numbers when `what="numbers"` is used (addresses [#34](https://github.com/fishR-Core-Team/FSA/issues/34)).
* `predict.boot()`: Added.
* `psdPlot()`: Modified. Minor changes to documentation and look of the function code.
* `rSquared()`: Added from `NCStats`, but including a generic method so that it can be used for other models (e.g., `catchCurve()`).
* `vbFuns()`: Modified. Switched `Fabens` and `Fabens2` parameterizations to better match `Wang` (i.e., increment model first). Added `Francis2` parameterization for tag-recapture data.
* `vbStarts()`: Modified. Fixed some spacing issues with the warnings when starting values for Linf was poorly estimated. Added an argument to `ivbStarts.LinfK()` to suppress checking the value of Linf. This argument reduces the change of double-printing the warning message when there are bad estimates of starting values for Linf and K.

# FSA 0.8.17
* `dunnTest()`: Modified. Adjusted code to handle the addition of `altp=` to and modified output from `dunn.test()` in `dunn.test`. Added additional tests and corrected some issues in the documentation.
* `GompertzFuns()`: Modified. Fixed error in message (i.e., `msg=TRUE`) for `param="Ricker2"`.

# FSA 0.8.16
* Need to resubmit v0.8.15 to CRAN, so bumped the version.
* `growthFunShow()`: Modified. Fixed error in expression for `type="Logistic"` and `param="CampanaJones1"`.

# FSA 0.8.15
* **Date:** 6-Sep-17
* Added a script to the `helpers` directory that will test that all required packages are installed.
* `iAddOutlierTestResults()`: Modified. Fixed bug related to point labels in `residPlot()` when the data.frame for the original model had `NA` values.
* `removal()`: Modified document by merging pull request [#33](https://github.com/fishR-Core-Team/FSA/pull/33).
* `srStarts()`: Modified. Added `fixed=`. Added some catches for poor starting values. Added relevant tests. Addresses [#30](https://github.com/fishR-Core-Team/FSA/issues/30).

# FSA 0.8.14
* Moved `dunn.test` and `lmtest` to `imports` to help with portability for workshops.
* `ageBias()`: Modified. Fixed bug in `plot()` so that the tick marks on the marginal histograms match the tick marks on the main plot. Changed the default `hist.panel.size=` in `plot()` so that it more reliably prints the values on the axes of the marginal histograms.
* `removal()`: Modified. Added "warnings" for when all catches are zeroes (an object is still returned with all `NA`s). Thanks to Daniel Hanks for pointing out this issue.
* `Summarize()`: Modified. Fixed bug when `percZero!="always"` and there are no valid values such that the calculated percent of zeroes is `NA`.

# FSA 0.8.13
* `ageBias()`: Modified. A complete rebuild of `plot`. Major changes are to add `plotAB()` which is primarily used to make the "legacy" age bias plots of Campana, removal of the "sunflower" plot option, new sets of defaults for many of the arguments that reflect my preferences for visualizing age comparisons (which includes defaulting to plotting differences in ages), addition of the ability to add marginal histograms (`xHist=`, `yHist=`, `col.hist=`, and `hist.panel.size=`), better handling of axis ticks and labels (primarily to show ticks at integers and make sure 0 is included for differences), and allowing the ability to add "summary layers" to the main plot (see `allowAdd=`). Many examples were added. Some functionality from previous versions will be broken.
* `capFirst()`: Modified. Changed some `if()`s with `class()`es to `inherits()`.
* `compIntercepts()`: Modified. Replaced two `dim()` calls with `nrow()`.
* `fact2num()`: Modified. Changed some `if()`s with `class()`es to `inherits()`.
* `lagratio()`: Modified. Changed some `if()`s with `class()`es to `inherits()`.
* `iHndlCols2UseIgnore()`: Modified. Changed some `if()`s with `class()`es to `inherits()`.
* `iLegendHelp()`: Modified. Changed some `if()`s with `class()`es to `inherits()`.
* `iPredictBoot()`: Modified. Changed some `if()`s with `class()`es to `inherits()`.
* `is.CapHist()`: Added.
* `iTypeoflm()`: Modified. Added a catch for a linear model that has a character variable (now alerts the user with a warning).
* `mrClosed()`: Modified. Changed some `if()`s with `class()`es to `is.CapHist()`.
* `mrOpen()`: Modified. Changed some `if()`s with `class()`es to `is.CapHist()`.
* `nlsTracePlot()`: Modified. Changed some `if()`s with `class()`es to `inherits()`.
* `perc()`: Modified. Changed some `if()`s with `class()`es to `inherits()`.
* `plotAB()`: Added. See description above for `ageBias()`.
* `plotBinResp()`: Modified. Changed how default transparency level is calculated and set the maximum transparency to 50 (changed from 500). Fixed bug in how the width of the proportions windows were calculated by default. These changes will affect `fitPlot()` for logistic regression models.
* `psdAdd()`: Modified. Changed some `if()`s with `class()`es to `inherits()`.
* `residPlot()`: Modified. Changed default for `loess=` from `TRUE` to `FALSE`. Changed some `if()`s with `class()`es to `inherits()`.
* `wrAdd()`: Modified. Changed some `if()`s with `class()`es to `inherits()`.

# FSA 0.8.12
* Lots of spelling corrections after running `devtools::spell_check()`.
* Cleaned up some issues in the testing files that were caused by a new version of `fishmethods` and changes to R v3.4.0.
* `metaM()`: Modified. Changed `T=` to `Temp=` to reduce potential for conflicts with `TRUE` abbreviation.
* `reproInfo()`: Modified. Added `ind=` to select a CRAN mirror to help with a common problem I have when knitting.
* `srStarts()`: Modified. Corrected mis-spelling in directive to `FSAsim` package.
* `vbStarts()`: Modified. Added a catch that Linf cannot be automatically estimated with fewer than three ages. Corrected mis-spelling in directive to `FSAsim` package.

# FSA 0.8.11
* Changed all `stop()`s to `STOP()`s and all `warning()`s to `WARN()`. This modified nearly all functions.
* Changed all `paste()`s that used `sep=""` to `paste0()`s.
* Removed several `sep=""`s from `message()`s.
* Removed `Hmisc` from, but added `epitools` to, imports. Removed all links to `Hmisc` to remove CRAN check warnings.
* Reorganized testing files. Added many tests.
* `.onAttach()`: Modified. Streamlined package startup message.
* `addZeroCatch()`: Modified. Added more "catches" for bad data types or arguments.
* `ageBias()`: Modified. Changed all `message()`s in `summary()` to `cat()`s.
* `agePrecision()`: Modified. Changed all `message()`s in `summary()` to `cat()`s.
* `binCI()`: Modified. Changed from using `binconf()` in `Hmisc` to `binom.exact()`, `binom.wilson()`, and `binom.approx()` from `epitools` (this removes dependency on `Hmisc` which was causing problems). Allowed multiple `type`s to be chosen. Now only accepts whole numbers for `x` and `n`. Added `verbose=` so that the result can include all of the information returned from the `epitools` functions. Added a catch for bad `conf.level`s. Added some more tests.
* `catchCurve()`: Modified. Made sure that `coef()` method returned a vector (addresses [#19](https://github.com/fishR-Core-Team/FSA/issues/19)). Modified `confint()` code for efficiency, made sure matrix is always returned.
* `chapmanRobson()`: Modified. Made sure that `coef()` method returned a vector (addresses [#19](https://github.com/fishR-Core-Team/FSA/issues/19)). Modified `confint()` code for efficiency, made sure matrix is always returned.
* `chooseColors()`: Modified. Added `rev=` for returning reverse ordered (from default) colors.
* `depletion()`: Modified. Changed `coef()` method so that it returned a named vector (addresses [#19](https://github.com/fishR-Core-Team/FSA/issues/19)). Modified `confint()` code for efficiency, made sure matrix is always returned. Removed `type=` to match other functions (incorporated that functionality into `parm=`). Removed `digits=` to match other functions.
* `expandLenFreq()`: Modified. Changed all `message()`s to `cat()`s. Removed "names" from printed items for a cleaner look.
* `fitPlot()`: Modified. Added `cex.leg=` and `box.lty.leg=` to IVR plots.
* `hist.formula()`: Modified. Fixed a bug with adding the horizontal line at 0 when the user uses `plot=FALSE`, which occurs with `hist.bootCase()`.
* `hyperCI()`: Modified. Now only accepts whole numbers for `M`, `n`, or `m`. Added catch for bad `conf.level`s and multiple values of `M`, `n`, or `m`.
* `iAddLoessLine()`: Modified. Changed used of `iMakeColor()` to `col2rgbt()`.
* `iGetDecimals()`: Modified. Added warning for situations where `x` will be presented in exponential notation. Also returned a decimal of zero in this situation. Helps with a bug in `hist.formula()`.
* `iHndlCols2use()`: Deleted. Changed to `iHndlCols2UseIgnore()`.
* `iHndlCols2UseIgnore()`: Added. Previously was `iHndlCols2use()`. Completely reworked to catch more problems including having both positive and negative indices (fixes [#24](https://github.com/fishR-Core-Team/FSA/issues/24)) and choosing variable names that don't exist (fixes [#25](https://github.com/fishR-Core-Team/FSA/issues/25)).
* `iHndlFormula()`: Modified. Added code to deal with a formula that is a single "variable" sent in an array. Addresses [#21](https://github.com/fishR-Core-Team/FSA/issues/21) for the simple situation of single "variable."
* `iHndlMultWhat()`: Modified. Added `type=` to allow use with `message()` or `cat()`.
* `iPlotExists()`: Added. Helps with bug fix in `hist.formula()`.
* `is.wholenumber()`: Added. Needed for changes to `binCI()`, `hyperCI()`, and `poiCI()`.
* `kCounts()`: Modified. Fixed bug with `capitalize=` and `zero`. Streamlined code. Added tests.
* `metaM()`: Modified. Fixed bug with the way messages were output when multiple `methods` were provided and `justM=FALSE`. Added more tests.
* `mrClosed()`: Modified. Added `poi.type=` to handle new choices for Poisson confidence interals. Added some checks for non-vector uses of `M=` and `R=` (partially addresses [#22](https://github.com/fishR-Core-Team/FSA/issues/22)). Fixed bug in how inputs for subgroups were output from `summary()` when `verbose=TRUE`.
* `mrOpen()`: Modified. Changed all `message()`s in `summary()` to `cat()`s. Fixed bug where returned value from `summary()` was not a data.frame if only one parameter was selected.
* `nlsTracePlot()`: Added.
* `plotBinResp()`: Modified. Changed used of `iMakeColor()` to `col2rgbt()`.
* `poiCI()`: Modified. Completely rebuilt to use the functions from `epitools`. Now only accepts whole numbers for `x`.
* `predict.bootCase()`: Modified. Modified so that situations where other than values of the dependent variable are in the dots argument (as would occur if making predictions for the Francis parameterization of the VBGF).
* `predict.nlsBoot()`: Modified. See note for `predict.bootCase()`.
* `psdCalc()`: Modified. Fixed bug in output if more than two additional lengths were supplied.
* `removal()`: Modified. Added a check and a returned error if `method="Schnute"` and the last of three catches is zero (addresses [#26](https://github.com/fishR-Core-Team/FSA/issues/26)) Fixed bug related to sending catches in a one column data.frame. Fixed bug related to selecting only one `parm=` in `confint()`. Added tests.
* `residPlot()`: Modified. Added `cex.leg=` and `box.lty.leg=` to IVR plots. Removed extra spaces in main title if `main="MODEL"`. Added some tests.
* `tictactoe()`: Modified. Changed used of `iMakeColor()` to `col2rgbt()`.
* `vbFuns()`: Modified. Added `Ogle` to list of parameterizations. Changed order of `L0` and `K` parameters in returned function when `param="Original"`.
* `vbStarts()`: Modified. Added the `methLinf=` argument that allows the user to choose if Linf is estimated from a Walford plot (`methLinf="Walford"`; the default and old functionality), as the mean of fish in a certain number of old ages (`methLinf="oldAge"`), or as the mean of a certain number of the longest fish (`methLinf="longFish"`). The number of ages or long fish is given in `num4Linf=`. Added methods for `type="Ogle"`.

# FSA 0.8.10
* `alkIndivAge()`: Modified. Added `na.rm=TRUE` to the checks on the minimum and maximum length data.
* `catchCurve()`: Modified. Removed `type=` and blended that functionality into `parm=` for methods. Made `parm=` consistent across methods.
* `chapmanRobson()`: Modified. Added `axis.age=` argument that allows the user to choose which type of x-axis is displayed (see examples; this addresses [#20](https://github.com/fishR-Core-Team/FSA/issues/20))  Also modified code that adds the axes so that they should "look better" in more instances. Added `na.rm=TRUE` to y-range calculation for the plot method. Added a `coef()` method. Added a `parm=` argument to the `confint()` and `summary()` methods. Added tests.
* `confint.nlsBoot()`,`confint.bootCase()`. Modified. Result is now a matrix even if only one parameter is chosen (previously it was an unnamed vector). The `parm=` now properly handles negative values. Streamlined plotting results. Added tests.
* `depletion()`: Modified. Checked for bad `conf.level=` in `confint()` method.
* `GompertzFuns()`: Modified. Fixed bug related to selecting `QuinnDeriso3`.
* `htest.nlsBoot()`,`htest.bootCase()`. Modified. The `b0` now defaults to 0. Matrix of results now include the parameter as the rowname. Modified the internals of how the data are handled. Added tests.
* `iAddLoessLine()`: Modified. Suppressed warnings related to the loess line predictions.
* `mrClosed()`: Modified. Checked for bad `conf.level=` in `confint()` method.
* `mrOpen()`: Modified. Checked for bad `conf.level=` in `confint()` method.
* `predict.nlsBoot()`, `predict.bootCase()`. Modified. The `...` argument can now contain a vector of values such that predictions can be made for multiple values of the independent variable. Modified the output matrix to handle this modification. Removed `MARGIN` as it will always be `1` for `nlsBoot` and `bootCase` objects. Added checks for `FUN=`, `conf.level=`, and `digits=`. Added tests.
* `removal()`: Modified. Checked for bad `conf.level=` in `confint()` method. Changed internal functions from using a loop to using `apply()`. Changed internal functions from using `log()` and `choose()` to using `lchoose()`.
* `Summarize()`: Modified. Added `nvalid=` and `percZero` to only print the nvalid and percZero result if they are "interesting" (i.e., different than n or zero, respectively) by default (may be manually over-ridden). Modified tests.
* `vbStarts()`: Modified. Added `na.rm=TRUE` to checking of Linf values.

# FSA 0.8.9
* `ageComparison()`: Modified. Removed an internal call to `fact2num()` because of changes to `Summarize()` below. Should not impact user experience.
* `diags()`: Added.
* `gompertzFuns()`: Modified. Fixed some spacing around the message when `msg=TRUE`.
* `logisticFuns()`: Modified. Fixed some spacing around the message when `msg=TRUE`.
* `Summarize()`: Modified. Removed all uses where the main variable was a factor (this functionality was largely unneeded and unused, was inelegant and difficult to maintain). Removed pass-through to `summary`. Removed warnings about the RHS variables being converted to factors. Columns for "levels" of the RHS variables are now returned in their original model (i.e., if the variable was numeric in the original data.frame it is now numeric in the data.frame returned from this function) -- this should reduce need for using `fact2num()` when using the results of this function for variables that were originally numeric. Added more examples and tests for the numeric data.

# FSA 0.8.8
* `growthFunShow()`: Modified. Added Pauly et al. (1992) seasonal cessation function. Added `case=` for use with Schnute model.
* `vbFuns()`: Modified. Added Pauly et al. (1992) seasonal cessation function. Slightly modified messages for "Typical" and "Original" parameterizations.
* `vbStarts()`: Modified. Added `fixed=` so that the user can define some of the starting values. Added Pauly et al. (1992) seasonal cessation function. Added tests for `fixed=`.

# FSA 0.8.7
* Compiled under R v3.3.0.
* Removed `relax` from `Suggests`. See `srStarts()` and `vbStarts()` notes below. This addresses [#17](https://github.com/fishR-Core-Team/FSA/issues/17).
* Removed `gdata` from `Imports`. See `filterD()` and `Subset()` notes below. This addresses [#5](https://github.com/fishR-Core-Team/FSA/issues/5).
* Added no coverage blocks to `ageKeyPlot()`, `capHistSum()`, `hist.formula()`, `histFromSum()`, `lwCompPreds()`, `plot.agebias()`, `plot.CatchCurve()`, `plot.ChapmanRobson()`, `plot.Depletion()`, `plotBinResp()`, `print.compSlopes()`, `print.compIntercepts()`, `print.metaM()`, `psdPlot()`,`residPlot()`, `srModels()`, `srStarts()`, and `vbStarts()`.
* `ageKey()`: Removed. Deprecated since 0.4.24. Use `alkIndivAge()`.
* `ageKeyPlot()`: Removed. Deprecated since 0.4.24. Use `alkPlot()`.
* `bcFuns()`: Modified. Changed `msg=` to `verbose=`.
* `capHistSum()`: Added tests.
* `filterD()`: Modified. Changed to use `droplevels()` from `base` rather than `drop.levels()` from `gdata`. Added `except=`.
* `fitPlot()`: Modified. Changed the way colors, plotting characters, and line types were handled for most of the models. Should make their use more flexible. Fixed errors that occurred in IVR models when the factor variable preceded the covariate in the model (fixes [#18](https://github.com/fishR-Core-Team/FSA/issues/18)). Started to add tests for error and warning messages.
* `GompertzFuns()`: Modified. Changed `type=` to `param=`.
* `GompertzModels()`: Removed. Replaced with `growthFunShow()`.
* `iGetDecimals()`: Modified. Fixed a bug that occured when an integer was provided.
* `lenFreqExpand()`: Removed. Deprecated since 0.4.32. Use `expandLenFreq()`.
* `logisticFuns()`: Modified. Changed `type=` to `param=`.
* `LogisticModels()`: Removed. Replaced with `growthFunShow()`.
* `residPlot()`: Modified. Changed the way colors, plotting characters, and line types were handled for most of the models. Should make their use more flexible. Now matches coding in `fitPlot()`. Fixed bug with main titling, but now asks user to decide if they want the model call or not. Started to add tests for error and warning messages.
* `RichardsFuns()`: Modified. Changed `type=` to `param=`.
* `RichardsModels()`: Removed. Replaced with `growthFunShow()`.
* `growthFunShow()`: Added.
* `srFunShow()`: Added.
* `srModels()`: Removed. Replaced with `srFunShow()`.
* `srStarts()`: Modified. Removed `dynamicPlot=TRUE` option. Moved it to `FSAsim` package. Modified plot when `plot=TRUE` by adding "STARTING VALUES" to title and moving starting values to within the plot. Added `cex.main=` and `col.main=`.
* `Subset()`: MOdified. Changed to use `droplevels()` from `base` rather than `drop.levels()` from `gdata`.
* `vbFuns()`: Modified. Changed `type=` to `param=`.
* `vbModels()`: Removed. Replaced with `growthFunShow()`.
* `vbStarts()`: Modified. Removed `dynamicPlot=TRUE` option. Moved it to `FSAsim` package. Added `param=` to match other `vbXXX()` (works as does `type=`). Modified plot when `plot=TRUE` by adding "STARTING VALUES" to title and moving starting values to within the plot. Added and `col.main=`. Made warnings and error tests more explicit.

# FSA 0.8.6
* Fixed problems with tests, and made the tests more explicit, related to PSD and Wr functions. Suppressed some warnings related to `sumTable()` in ALK related tests and `Summarize()` in age comparisons tests. Prompted by forthcoming changes to `testthat`.
* Removed `News.md` from `.Rbuildignore` (apparently now supported by CRAN).
* `alkPlot()`: Modified. Changed so that `xlim=` and `ylim=` would work when `type="area"` and `type="bar"`. This fixes [#10](https://github.com/fishR-Core-Team/FSA/issues/10) (Thanks to Joseph Feldhaus).
* `hist.formula()`: Modified. Added the `breaks=` argument (mostly a pass-through) and the `w=` argument that allows the user to just set the width of the bins without having to set each `break` value. This should complete [#15](https://github.com/fishR-Core-Team/FSA/issues/15).
* `iCheckStartCatW()`: Modified. Now use `iGetDecimals()` to extract the number of decimals in `startcat` and `w`.
* `iCheckStartcat()`: Added.
* `iCheckW()`: Added.
* `iGetDecimals()`: Added.
* `lencat()`: Modified. Changed order of `startcat=` and `breaks=`. Slight modifications to documentation.
* `psdAdd()`: Modified. Minor changes to documentation.
* `psdPlot()`: Modified. Fixed bug related to PSD values being printed when only PSD-Q existed (needed to add `drop0Est=FALSE` to the `psdCalc()` call; this fixes [#13](https://github.com/fishR-Core-Team/FSA/issues/13)). Made the histogram bars flush with the x-axis rather than hovering above it (added `yaxis="i"` to `hist()`; this fixes [#12](https://github.com/fishR-Core-Team/FSA/issues/12)). Minor changes to documentation.
* `psdVal()`: Modified. Minor changes to documentation.
* `purl2()`: Modified. Added `delHeader=` argument and functionality.

# FSA 0.8.5
* Added URL for fishR webpage in DESCRIPTION per CRAN request. Removed it from the URL field in DESCRIPTION.
* Updated all references to Ogle (2016) in documentation.

* `ageBias()`: Modified. Minor corrections to the documentation.
* `agePrecision()`: Modified. Fixed bug related to computations of percent agreement when `NA` values were present. There was an inconsistency between when `what="precision"` and `what="difference"` was used in `summary()`. The bug fix now properly divides by the "valid sample size" for `what="precision"`. This fixes [#9](https://github.com/fishR-Core-Team/FSA/issues/9) (Thanks to Joseph Feldhaus). Now returns `validn`. Modifications to the documentation.
* `histFromSum()`: Added. Addresses [#4](https://github.com/fishR-Core-Team/FSA/issues/4).
* `metaM()`: Modified. Changed order of methods in `methods=`. Minor corrections and additions to documentation.
* `mrClosed()`: Modified. Now sends warning if an `NA` appears in the first position of `m`, the first position of `M`, or the last position of `R` and converts these to 0 so that the procedure can continue. Each of these positions is ignored in the calculations. This fixes [#8](https://github.com/fishR-Core-Team/FSA/issues/8) (Thanks to Joe Mrnak).
* `reproInfo()`: Modified. Made changes to `iGetAllDependencis()` based on forthcoming changes to `package.dependencies()` (as notified by CRAN).
* `vbStarts()`: Modified. Fixed bug when `dynamicPlot=TRUE` was used.

# FSA 0.8.4
* Now using Roxygen2 v5.0.1.
* Removed some `requireNamespaces()` from some functions and moved those packages from `Suggests` to `Imports` so that those functions would work better with other packages. The only `requireNamespaces()` that remain are related to functions that require the `relax` package (so tcltk is not installed until needed) and `knitr`, `dunn.test`, and `lmtest` as these are unlikely to be used by other packages and will keep the packages that are loaded with `FSA` to a minimum. Packages moved from `Suggests` to `Depends` are `Hmisc` (for use in `binCI`), `gdata` (for use in `filterD()` and `Subset()`), `dplyr` (for use in `filterD()`), `sciplot` (for use in `fitPlot()`), `car` (for use in `residPlot()`), and `gplots` (for use with colors).
* `addZeroCatch()`: Modified tests (to reduce warnings that were not part of tests).
* `geomean()`: Added.
* `geosd()`: Added.
* `lencat()`: Modified. Fixed a bug related to using a `tbl_df` object.
* `sumTable()`: Modified tests (but with `dimnames()`).


# FSA 0.8.3
* Removed vignetteBuilder from DESCRIPTION (remnant from a vignette I built and then removed) at request of CRAN.

# FSA 0.8.2
* **Date:** 22-Oct-15
* Converted all files in `data-raw` to CSV files.
* Removed all `\href{}{}` and `\url{}` codes to websites that I don't control. The addresses are now "naked" such that the user will need to copy-and-paste them into a browser to view the web page rather than clicking on a hyper link. Hopefully this will eliminate problems with R CMD CHECK.
* `ChinookArg`: Updated help documentation.
* `Ecoli`: Added a Topics section.
* `Mirex`: Added a Topics section.
* `PikeNYPartial1`: Updated help documentation.
* `SpotVA1`: Updated help documentation.

# FSA 0.8.1
* `col2rgbt()`: Added.
* `compIntercepts()`: Added.
* `compSlopes()`: Added.

----

# FSA 0.8.0 8
* Added suggests for `dunn.test` for use in `dunnTest()` (see below).
* `agePrecision()`:  Modified. Changed `combn()` to `utils::combn()` and `sd()` to `utils::sd()` (within an `apply()`).
* `catchCurve()`:  Modified. Changed `na.exclude()` to `stats::na.exclude()`.
* `dunnTest()`: Modified. Changed to more throughly use `dunn.test()` from `dunn.test`. Added the `two.sided=` argument to `dunnTest()` and `dunn.test.results=` to `print.dunnTest()`.
* `expandLenFreq()`: Modified. Changed `runif()` to `stats::runif()`.
* `extraSS()`: Modified. Changed `anova()` to `stats::anova()` in an `lapply()`.
* `fishR()`:  Modified. Changed `browseURL()` to `utils::browseURL()`.
* `fsaNews()`:  Modified. Changed `browseURL()` to `utils::browseURL()`.
* `headtail()`:  Modified. Changed `head()` to `utils::head()` and `tail()` to `utils::tail()`.
* `hist.bootCase()`: Modified. Changed `hist()` to `hist.formula()`.
* `iAgeBiasPlot()`: Modified. Changed `grconvertY()` to `graphics::grconvertY()`.
* `iALKMean.QD()`: Modified. Changed `var()` to `stats::var()` (within `sumTable()`).
* `iBubblesAdd()`: Modified. Changed `rgb()` to `grdevices::rgb()`.
* `iChkComplexModel()`: Modified. Changed `df.residual()` to `stats::df.residual()`.
* `iCIboot()`: Modified. Changed `hist()` to `hist.formula()`.
* `iEvent2Indiv()`: Modified. Changed `unstack()` to `utils::unstack()`.
* `iGetAllDependencies()`: Modified. Changed `installed.packages()` to `utils::installed.packages()`.
* `iHistResids()`: Modified. Removed `graphics::hist()` and changed to `hist.formula()`.
* `iHndlFormula()`: Modified. Changed `terms()` to `stats::terms()`.
* `iHndlResidType()`: Modified. Changed `rstandard()` to `stats::rstandard()` and `rstudent()` to `stats::rstudent()`.
* `iHtestBoot()`: Modified. Removed `graphics::hist()` and changed to `hist.formula()`.
* `iMakeColor()`: Modified. Changed `rgb()` to `grdevices::rgb()`.
* `iMakeModelHeading()`: Modified. Changed `formula()` to `stats::formula()` (within an `lapply()`).
* `iMoran()`: Modified. Changed `optimize()` to `grdevices::optimize()`.
* `iProcessSessionInfo()`: Modified. Changed `sessioninfo()` to `utils::sessioninfo()`.
* `iSchnute()`: Modified. Changed `optimize()` to `grdevices::optimize()`.
* `iTypeoflm()`: Modified. Changed `formula()` to `stats::formula()`.
* `plot.AgeBias()`: Modified. Changed `rgb()` to `grdevices::rgb()`.
* `plot.CatchCurve()`: Modified. Changed `predict()` to `stats::predict()`.
* `print.extraTest()`: Modified. Changed `printCoefMat()` to `stats::printCoefMat()`.
* `psdAdd()`:  Modified. Changed `data()` to `utils::data()`.
* `psdVal()`:  Modified. Changed `data()` to `utils::data()`.
* `reproInfo()`: Modified. Changed `graphics.off()` to `grdevices::graphics.off()`.
* `removal()`: Modified. Removed some of the examples from the help page to reduce the elapsed time for CRAN.
* `vbStarts()`: Modified. Changed `rgb()` to `grdevices::rgb()`.
* `wrAdd()`:  Modified. Changed `data()` to `utils::data()`.
* `wsVal()`:  Modified. Changed `data()` to `utils::data()`.
* `test_AgeLengthKey`: Modified. Altered tests that had used `==` to use `expect_equivalent()` which uses `all.equal()` with `check.attributes=FALSE`.
* `test_PSD`: Modified. Altered tests that had used `==` to use `expect_equivalent()` which uses `all.equal()` with `check.attributes=FALSE`.


# FSA 0.7.11
* **Date:** Oct15
* Converted all `.txt` files to `.Rda` files. Original `.txt` files are in the `data-raw` directory which was added to `.Rbuildignore`.

# FSA 0.7.10
* **Date:** Oct15
* `purl2()`: Added `newname=` to allow the output file to have a name other than the same as the intput file.
* `reproInfo()`: Added `markdown` to the `out=` types.

# FSA 0.7.9
* **Date:** Sep15
* Updated `README.md` and `DESCRIPTION` for new websites.
* Changed all references to the WordPress site to the new website. Removed links to specific IFAR chapters. Changed my e-mail address. Created link in references to IFAR book page.
* `fishR()`: Modified. Updated for the new websites.

# FSA 0.7.8
* **Date:** Sep15
* `ageComparison()`: Modified. Changed `what="McNemars"` and `what="Bowkers"` to `what="McNemar"` and `what="Bowker"`. Fixed bug if all ages are `NA`.
* `catchCurve()`: Modified. Fixed bug related to `NA` values in the catch vector.
* `chapmanRobson()`: Modified. Fixed bug related to `NA` values in the catch vector.
* `validn()`: Modified. Fixed bug related to when a 1-dimensional numeric vector was not recognized as a vector.

# FSA 0.7.7
* **Date:** Aug15
* `ageBias()`: Modified. Changed default for `pch.mean=` to 95 (from 175). If `what=` has only one item, then results will now be invisibly returned so that results can be saved to an object.
* `agePrecision()`: Modified. Added `trunc.diff=`. If `what=` has only one item, then results will now be invisibly returned so that results can be saved to an object.
* `mapvalues()`: Modified. Corrected to export properly.
* `removal()`: Modified. Minor edits to labels if `verbose=TRUE`. Added some more tests.
* `vbStarts()`: Modified. Made `yngAge` the default for `meth0=`. Fixed bug that occured when `meth0='yngAge'` and sample sizes at all ages were 1.

# FSA 0.7.6
* **Date:** Aug15
* `Summarize()`: Modified. Converted to using `iHndlFormula()`. Changed output for quantitative data (`validn` is always returned, `NAs` is never returned). Changed output for two-way factor data (not returned as a character from `formatC()`). Removed `...` from code in several places as it was buggy and not used. Added more checks and modified check messages. Fixed bug from when a 1-d matrix of characters was sent. Added tests.
* `sumTable()`: Modified. Converted to using `iHndlFormula()`. Added tests.

# FSA 0.7.5
* **Date:** Aug15
* `addRadCap()`: Modified. Streamlined code. Changed default `in.pre=` to `NULL` (from `inc`). Added some tests for returned data.
* `BluegillLM`: Removed. Moved to `FSAdata`.
* `gConvert()`: Modified. Streamlined code. Changed `type=` to `out.type=`. Changed default `in.pre=` and `in.var=` to `NULL` (from missing). Changed code to handle changes in `in.pre=` and `in.var=`. Added some tests for returned data.
* `gReshape()`: Removed. Moved to `FSAmisc`.

# FSA 0.7.4
* **Date:** Aug15
* `binCI()`: Modified. Check for`Hmisc` with `requireNamespaces()` before processing body of function. This allowed moving `Hmisc` into `Suggests` declarations rather than `Imports`.
* `chooseColors()`: Modified. Check for`gplots` with `requireNamespaces()` before processing body of function. This allowed moving `gplots` into `Suggests` declarations rather than `Imports`.
* `filterD()`: Modified. Check for `dplyr` and `gdata` with `requireNamespaces()` before processing body of function. This allowed moving `dplyr` and `gdata` into `Suggests` declarations rather than `Imports`.
* `fitPlot()`: Modified. Check for`sciplot` with `requireNamespaces()` before adding intervals tot he plot. This allowed moving `sciplot` into `Suggests` declarations rather than `Imports`.
* `lrt()`: Modified. Check for`lmtest` with `requireNamespaces()` before processing body of function. This allowed moving `lmtest` into `Suggests` declarations rather than `Imports`.
* `mapvalues()`: Modified. Changed so that it is a direct `importFrom` and `export` without creating a help file in `FSA.`
* `purl2()`: Modified. Check for`knitr` with `requireNamespaces()` before processing body of function. This allowed moving `knitr` into `Suggests` declarations rather than `Imports`.
* `residPlot()`: Modified. Check for`car` with `requireNamespaces()` before highlighting outliers on the plot. This allowed moving `sciplot` into `Suggests` declarations rather than `Imports`.
* `srStarts()`: Modified. Check for`relax` with `requireNamespaces()` before constructing the dynamic plot. This allowed moving `relax` into `Suggests` declarations rather than `Imports`.
* `Subset()`: Modified. Check for`gdata` with `requireNamespaces()` before processing body of function. This allowed moving `gdata` into `Suggests` declarations rather than `Imports`.
* `vbStarts()`: Modified. Check for`relax` with `requireNamespaces()` before constructing the dynamic plot. This allowed moving `relax` into `Suggests` declarations rather than `Imports`.


# FSA 0.7.3
* **Date:** Aug15
* Removed all `importFrom()` directives and went to hard-wiring to packages with `::`. Added `imports()` directives for `stats`, `graphics`, `tools`, and `grDevices`. Removed `imports()` directive for `multcomp()`.
* `vbStarts()`: Modified. Changed default methos for `methEV=`. Changed order of starting values for `type="Mooij"` in order to match that from `vbFuns()`. This also fixed a bug when `dynamicPlot=TRUE` was used with `type="Mooij"`. Added tests to determine if parameter order is the same between `vbStarts()` and `vbFuns()` for all parameterizations.

# FSA 0.7.2
* **Date:** Jul15
* `ageBias()`: Modified. Corrected bug with labeling of x-axis on age-bias plot when `ref.lab=` and `nref.lab=` were not given by the user. Changed default for `nYpos=` from `1.1` to `1.03`. Added `cex.n=` to allow control of the size of the sample size labels.
* `agePrecision()`: Modified. Changed `what="detail"` to `what="details"`. Note that `what="detail"` still works.
* `dunnTest()`: Modified. Added a note to the help file about the use of complete cases. Suggested from Paule Bodson-Clermont.
* `vbFuns()`: Modified. Added `Original` and `Typical` to the `type=` options. This allows both a capitalized and uncapitalized version for these two parameterizations.
* `vbModels()`: Modified. Changed order of `Original` and `Typical` (`Typical` is now shown first). Fixed in error in how the equation for the `Weisberg` parameterization was displayed.
* `vbStarts()`: Modified. Added `cex.main=` as an argument and defaulted to `0.75`. Added `raw=TRUE` to `poly()` which is used when `meth0="poly"`. Added `Original` and `Typical` to the `type=` options. This allows both a capitalized and uncapitalized version for these two parameterizations.

# FSA 0.7.1
* **Date:** Jul15
* `ageBias()`: Modified. Moved into a single file with `agePrecision()`. Cleaned-up help file. No change in behavior.
* `agePrecision()`: Modified. Moved into a single file with `ageBias()`. Cleaned-up help file. No change in behavior.
* `alkAgeDist()`: Modified. Moved into a single file with `alkMeanVar()`. Cleaned-up help file. Added some error/warning tests. No change in behavior.
* `alkIndivAge()`: Modified. Clean-up help file. Added a stronger test for `type="SR"` method. No change in behavior.
* `alkMeanVar()`: Modified. Moved into a single file with `alkAgeDist()`. Cleaned-up help file. No change in behavior.
* `alkPlot()`:  Cleaned-up help file and tests. No change in behavior.
* `confint.bootCase()`: Modified. Created a common internal function for use with `confint.nlsBoot()`. Added the ability to plot histograms with confidence intervals superimposed (similar to what was in `confint.nlsBoot()`).
* `confint.nlsBoot()`: Modified. Created a common internal function for use with `confint.bootCase()`.
* `GompertzFuns()`:  Moved into `growthModels` file. Did not change behavior.
* `gompertzModels()`: Modified. Changed `type=` to `family=` to avoid confusion in the help file with `type=` in `gompertzFuns()`. Moved into `growthModels` file.
* `growthRadPlot()`: Deleted. Moved to `FSAmisc` package.
* `hist.formula()`: Modified. Fixed so that `ymax=` also sets the y-axis limit when only one histogram is made (it was previously ignored).
* `htest.bootCase()`: Modified. Created a common internal function for use with `htest.nlsBoot()`.
* `htest.nlsBoot()`: Modified. Created a common internal function for use with `htest.bootCase()`.
* `iHndlFormula()`: Modified. Added `na.action=NULL` to `model.frame()` so that NA values will not be omitted.
* `logisticFuns()`:  Moved into `growthModels` file. Did not change behavior.
* `logisticModels()`: Modified. Changed `type=` to `family=` to avoid confusion in the help file with `type=` in `logisticFuns()`. Moved into `growthModels` file.
* `predict.bootCase()`: Modified. Created a common internal function for use with `predict.nlsBoot()`.
* `predict.nlsBoot()`: Modified. Created a common internal function for use with `predict.bootCase()`.
* `psdAdd()`: Modified. Fixed bug that never tested if all lengths were `NA`. Required change to `iHndlFormula`.
* `RichardsFuns()`:  Moved into `growthModels` file. Changed `param=` to `type=` to better match similar functions.
* `RichardsModels()`:  Moved into `growthModels` file. Did not change behavior.
* `Schnute()`:  Moved into `growthModels` file. Changed `param=` to `type=` to better match similar functions.
* `SchnuteModels()`:  Moved into `growthModels` file. Did not change behavior.
* `vbFuns()`:  Moved into `growthModels` file. Did not change behavior.
* `vbModels()`: Modified. Changed `type=` to `family=` to avoid confusion in the help file with `type=` in `vbFuns()`. Moved into `growthModels` file.

# FSA 0.7.0
* **Date:** Jul15
* Fixed description to be in title case.
* Fixed reference to fishR page in description file.
* Fixed several URL references, deleted others that have changed and are no longer available.
* Updated CITATION file (to remove CRAN note).
* `hist.formula()`: Modified. Rebuilt to use `iHndlFormula()`. Modified how `xlab=` is used (result is the same). Added some tests.
* `ksTest()`: Modified. Rebuilt to use `iHndlFormula()`. Added some tests for messages and to make sure results matched `ks.test()`.

# FSA 0.6.25
* **Date:** Jul15
* `alkPrep()`: Deleted. Moved to `FSAmisc` package (on GitHub).
* `changesPos()`: Deleted. Moved to `FSAmisc` package (on GitHub).
* `chapmanPlot()`: Deleted. Moved to `FSAmisc` package (on GitHub).
* `dietOverlap()`: Deleted. Moved to `FSAmisc` package (on GitHub).
* `Garvey1`: Deleted. Moved to `FSAmisc` package (on GitHub).
* `Garvey4a`: Deleted. Moved to `FSAmisc` package (on GitHub).
* `KS2D_NR`: Deleted. Moved to `FSAmisc` package (on GitHub).
* `ks2d1()`: Deleted. Moved to `FSAmisc` package (on GitHub).
* `ks2d1p()`: Deleted. Moved to `FSAmisc` package (on GitHub).
* `ks2d2()`: Deleted. Moved to `FSAmisc` package (on GitHub).
* `ks2d2p()`: Deleted. Moved to `FSAmisc` package (on GitHub).
* `popSizesPlot()`: Deleted. Moved to `FSAmisc` package (on GitHub).
* `posadj()`: Deleted. Moved to `FSAmisc` package (on GitHub).
* `TroutDietSL`: Deleted. Moved to `FSAmisc` package (on GitHub).
* `walfordPlot()`: Deleted. Moved to `FSAmisc` package (on GitHub).

# FSA 0.6.24
* **Date:** Jun15
* `alkIndivAge()`: Modified. Switched to using `iHndlFormula()` at the beginning. Added more checks and tests.
* `expandLenFreq()`: Modified. Added more checks. Added some tests.
* `wsVal()`: Modified. Added more tests.

# FSA 0.6.23
* **Date:** Jun15
* `ageBias()`: Modified. Fixed bugs related to axes on numbers plot and sunflower plot.
* `filterD()`: Modified. Added `reorder=FALSE` to `drop.levels()` so that the order of levels is not changed when levels are dropped.
* `residPlot.nlme()`: Added.

# FSA 0.6.22
* **Date:** Jun15
* `extraSS()`: Modified. Added `sim.name=` to allow for a common typing mistake.
* `logbtcf()`: Modified. Slight change to handle a check of `lm` class.
* `lrt()`: Modified. Added `sim.name=` to allow for a common typing mistake.
* `pcumsum()`: Modified. Modified to handle `table`, `matrix`, and `data.frame` classes as long as they are 1-dimensional.
* `rcumsum()`: Modified. Modified to handle `table`, `matrix`, and `data.frame` classes as long as they are 1-dimensional.
* `srStarts()`: Modified. Corrected some bugs related to checks. Added more tests.

# FSA 0.6.21
* **Date:** Jun15
* `addRadCap()`: Modified. Modified so that `in.pre=` string must be at the start of the variable names. Added a check for when the `in.pre=` string does not exist at the start of any variable names. Added a check for whether all `in.var=` variables exist. Added some simple tests (need more).
* `bcFuns()`: Modified. Removed `type=`; `BCM=` can now be either numeric or a string. Allowed string to be in any case (will be converted to the required all upper-case). Corrected some errors for when `msg=TRUE`. Added some simple tests.
* `gConvert()`: Modified. Modified so that `in.pre=` string must be at the start of the variable names. Added a check for when the `in.pre=` string does not exist at the start of any variable names. Added some simple tests (need more).
* `gReshape()`: Modified. Modified so that `in.pre=` string must be at the start of the variable names. Added a check for when the `in.pre=` string does not exist at the start of any variable names. Added some simple tests (need more).

# FSA 0.6.20
* **Date:** Jun15
* `gompFuns()`: Deleted.
* `gompModels()`: Deleted.
* `GompertzFuns()`: Added. Replaced `gompFuns()`. Added `type="Troynikov1"` and  `type="Troynikov1"`.
* `GompertzModels()`: Added. Replaced `gompModels()`. Added `cex=` and `type=`.
* `logisticFuns()`: Modified. Changed `type="Richards"` to `type="Karkach"`. Added `type=HaddonI`.
* `logisticModels()`: Modified. Added "Karkach" model. Added `cex=` and `type=`.
* `RichardsFuns()`: Modified. Added two more parameterizations from Tjorve and Tjorve (2010).
* `schnute()`: Deleted.
* `schnuteModels()`: Deleteed. 
* `Schnute()`: Added. Replaced `schnute()`. Fixed bugs with the way `t1=` and `t3=` are handled.
* `SchnuteModels()`: Added. Replaced `schnuteModels()`. Added `cex=`.
* `vbFuns()`: Modified. Added `type="Polacheck"` which is equivalent to `type="Laslett"`. Added a new reference in the help file.
* `vbModels()`: Modified. Added `cex=` and `type=`.

# FSA 0.6.19
* **Date:** Jun15
* `RichardsFuns()`: Added.
* `RichardsModels()`: Added.

# FSA 0.6.18
* **Date:** Jun15
* Changed nearly all "messages" using `cat()` to using `message()` so that they can be suppressed with `suppressMessage()` or `message=FALSE` in knitr. See  "One comment on messages" at http://yihui.name/knitr/demo/output/. Specific functions modified are listed below.
* `ageBias()`: Modified. Changed all `cat()` to `message()`s. Changed so that messages (result headers) are only printed if `what=` contains more than one item.
* `agePrecision()`: Modified. Changed all `cat()` to `message()`s. Changed so that messages (result headers) are only printed if `what=` contains more than one item.
* `bcFuns()`: Modified. Changed all `cat()` to `message()`s.
* `chapmanRobson()`: Modified. Changed all `cat()` to `message()`s. Deleted "Estimates with Standard Errors" message.
* `depletion()`: Modified. Changed all `cat()` to `message()`s.
* `dietOverlap()`: Modified. Changed all `cat()` to `message()`s. Slightly modified messages.
* `expandLenFreq()`: Modified. Changed all `cat()` to `message()`s. Fixed bug with one of the messages.
* `ks2d1()`: Modified. Rewrote the `print()` method. This removed a number of `cat()`s.
* `ks2d1p()`: Modified. Rewrote the `print()` method. This removed a number of `cat()`s.
* `ks2d2()`: Modified. Rewrote the `print()` method. This removed a number of `cat()`s.
* `ks2d2p()`: Modified. Rewrote the `print()` method. This removed a number of `cat()`s.
* `metaM()`: Modified. Changed all `cat()` to `message()`s.
* `mrOpen()`: Modified. Changed all `cat()` to `message()`s.
* `removal()`: Modified. Changed all `cat()` to `message()`s.
* `srFuns()`: Modified. Changed all `cat()` to `message()`s. Created some tests.

# FSA 0.6.17
* **Date:** Jun15
* `extraSS()`: Modified. Added more message tests and some calculational tests (compared to `anova()` results).
* `gompFuns()`: Modified. Changed all `cat()`s to `message()`s and slightly modified the messages. Fixed minor bugs in some created functions. Created some tests.
* `logisticFuns()`: Modified. Changed all `cat()`s to `message()`s and slightly modified the messages. Fixed minor bugs in some created functions. Created some tests.
* `lrt()`: Modified. Added more message tests and some calculational tests (compared to `lrtest()` from `lmtest` package results).
* `vbFuns()`: Modified. Changed all `cat()`s to `message()`s and slightly modified the messages. Fixed minor bugs in some created functions. Created some tests.

# FSA 0.6.16
* **Date:** Jun15
* `extraSS()`: Modified. Added `sim_names=` and `com_name=` so that simple descriptive names could be given to the model and printed in the heading of the output. Added checks for whether the complex model appears more complex or not. Added tests for warning and error messages.
* `fishR()`: Modfiied. Fixed bug with `where="news"`. Added tests.
* `fitPlot()`: Modified. Added ability to modify y-axis limits for the nonlinear regression model. Thanks to Gabriela N. for asking for this.
* `hoCoef()`: Modified. Changed `lmobj=` to `object=`, added degrees-of-freedom to the output matrix, streamlined the code, added some checks, and added some tests.
* `lrt()`: Modified. Added `sim_names=` and `com_name=` so that simple descriptive names could be given to the model and printed in the heading of the output. Added checks for whether the complex model appears more complex or not. Added tests for warning and error messages.

# FSA 0.6.15
* **Date:** Jun15
* `addZeroCatch()`: Modified. Deleted extraneous `print()` statement.
* `lencat()`: Modified. Major re-write to make it easier to trouble-shoot. Fixed bug related to empty category on end when `as.fact=TRUE` and `use.names=TRUE`. Added more tests.
* `psdCalc()`: Modified. Removed extra open-ended category (e.g., PSD-T-) for PSD intervals.

# FSA 0.6.14
* **Date:** May15
* Added travis-ci integration.
* Added coveralls integration.
* Added `importFrom` for `mapvalues()` from `plyr`.
* `changesPos()`: Modified. Added some checks with error messages. Added suite of tests.
* `chooseColors()`: Modified. Added some checks with error messages. Added suite of tests.
* `confint.nlsBoot()`:Modified. Fixed bug if maximum number in `parm=` was greater than the number of parameters in the model.
* `dunnTest()`: Modified. Change class type from `DunnTest` to `dunnTest` to eliminate conflict with `DunnTest()` in `DescTools` package. Thanks to Sal Mangiafico for pointing out this conflict.
* `fact2num()`: Modified. Added some checks with error messages. Added suite of tests.
* `filterD()`: Modified. Changed to using `drop.levels` from `gdata` rather than `droplevels`. Added a warning if the resultant data.frame has zero rows (same as in `Subset`). Added some checks with error messages. Added suite of tests.
* `fitPlot()`: Modified. Replaced the use of `nobs()` from `gdata` in the internal function `iCIfp1()` with `validn()`. This removed one dependency on `gdata`.
* `headtail()`: Modified. Internally remove `tbl_df` class (from `dplyr`) if it exists. Added some checks with error messages. Added suite of tests.
* `lagratio()`: Modified. Corrected incorrect explanation of `differences=` in help file. Added `recursion=` and `direction=`. Added suite of tests.
* `mapvalues()`: Deleted. Deleted the function from `FSA` but imported it from `plyr` and then exported it from `FSA` so that it would be available to fisheries users without having to load `plyr`.
* `oddeven()`: Modified. Added some checks with error messages. Added suite of tests.
* `perc()`: Modified. Added ability to use "and equals" or not to the items in `dir=` (i.e., there are now four items in `dir=`). Fixed a bug related to using `na.rm=FALSE` and a "less than" situation. Added some checks with error messages. Added suite of tests.
* `pcumsum()`: Modified. Added some checks with error messages. Added suite of tests.
* `rcumsum()`: Modified. Added some checks with error messages. Added suite of tests.
* `se()`: Modified. Added some checks with error messages. Added suite of tests.
* `Subset()`: Modified. Added some checks with error messages. Added suite of tests.
* `validn()`: Modified. Added some checks with error messages. Added suite of tests.

# FSA 0.6.13
* **Date:** May15
* Some miscellaneous reorganizations of files.
* `ageBias()`: Modified. Corrected bugs with `show.pts=TRUE` and "sunflower plot" that came from changes made in version 0.5.1.
* `residPlot()`: Modified. Deleted `student=`. Added `resid.type=` which allows used of standardized (internally studentized) and (externally) studentized residuals for linear models (along with raw residuals). Added code following `nlsResiduals()` from `nlstools` for standardized residuals for nonlinear models.

# FSA 0.6.12
* **Date:** May15
* `gompFuns()`: Added.
* `gompModels()`: Added.
* `logisticFuns()`: Added.
* `logisticModels()`: Added.
* `reproInfo()`: Modified. Added the `out=` argument to allow the output to be straight R or LaTeX. Removed the `listFiles=` argument. Changed the output to be more succinct. Streamlined the code.
* `vbFuns()`: Modified. Fixed a bug with the Laslett model.

# FSA 0.6.11
* **Date:** Apr15
* `kCounts()`: Added. Was `swvCounts()`.
* `kPvalue()`: Added. Was `swvPvalue()`.
* `purl2()`: Added. Was `swvCode()`. Added `timestamp=` argument for adding a timestamp to the created script.
* `reproInfo()`: Added. Was `swvFinish()`.
* `swvANOVA()`: Deleted. Moved to `NCStats`.
* `swvCode()`: Deleted. Changed to `purl2()`.
* `swvCounts()`: Deleted. Changed to `kCounts()`.
* `swvFinish()`: Deleted. Changed to `reproInfo()`.
* `swvGLHT()`: Deleted. Moved to `NCStats`.
* `swvHtest()`: Deleted. Moved to `NCStats`.
* `swvPvalue()`: Deleted. Changed to `kPvalue()`.
* `swvREG()`: Deleted. Moved to `NCStats`.

# FSA 0.6.10
* **Date:** Apr15
* Compiling under R 3.2.0.
* Added some cross-reference links to help files.
* Remove fishR vignette section and added IFAR Chapter section to help files.
* `fishR()`: Modified. Added `IFAR` as an option. Updated code to be more simple.

# FSA 0.6.5
* **Date:** Apr15
* Last version for submission of first draft of Introductory Fisheries Analyses with R.
* `capHistConvert()`: Modified. Added a warning section and an example of problems that can occur if the data are in event format but the event variable contains unused levels as may occur following subsetting. Thanks to Joseph Feldhaus for pointing out this problem.
* `extraSS()`: Modified. Changed algorithm to determine if the models were of the same class or not. The modification allows a model to have multiple classes.
* `iHndlCols2Use` (Intrnal Function): Modified. Fixed bug with how the columns were selected. Added a suite of tests for this function. This will fix bugs in `capHistConvert()` and `capHistSum()`. Thanks to Joseph Feldhaus for pointing out this egregious error.
* `lrt()`: Modified. Changed algorithm to determine if the models were of the same class or not. The modification allows a model to have multiple classes.

# FSA 0.6.4
* **Date:** Apr15
* Changed to using `LazyData: true`.
* `se()`: Added. Removed `importFrom` of `se()` from `sciplot`.

# FSA 0.6.3
* **Date:** Apr15
* Some modifications to tests.
* `plot.capHist()`: Modified. Changed default plot look which can now be controlled with `pch=`, `cex.pch=`, and `lwd=`. Modified the two y-axis scales to use `plotmath` characters.

# FSA 0.6.2
* **Date:** Mar15
* `capHistConvert()`: Modified. Streamlined code around creating `var.lbls`. Made `event` the default value for `var.lbls.pre=`. Added some checks to `var.lbls.pre=` if it starts with a number or has too many values. Added `cols2use=` and modified use of `cols2ignore=` via `iHndlCols2use()`.
* `capHistSum()`: Modified. 
* `iHndlCol2use()`: Added. Added this internal function to handle `cols2use=` and `cols2ignore=` in `capHistConvert()` and `capHistSum()`. 

# FSA 0.6.1
* **Date:** Mar15
* `catchCurve()`: Modified. Changed how `ages2use=` was handled so that negative values can be used to exclude some ages. Will also now send an error if a mix of positive and negative ages are sent in `ages2use=`. Better handled the situation where `ages2use=` had more ages than the `age` variable. Checked for non-positive weights if `weighted=TRUE` and returned a warning and changed the non-positive weights to the minimum of the positive weights.
* `chapmanRobson()`: Modified. Changed how `ages2use=` was handled so that negative values can be used to exclude some ages. Will also now send an error if a mix of positive and negative ages are sent in `ages2use=`. Better handled the situation where `ages2use=` had more ages than the `age` variable.
* `expandCounts()`: Modified. Changed so that ``message()''s are printed at the end instead of along the way. This reduces confusion of what appear to be messages of success followed by an error. Thanks to Dan Oele bringing this confusion to my attention.
* `plotBinResp()`: Modified. Changed the way the breaks were calculated (uses `lencat()` now).

# FSA 0.6.0
* **Date:** Mar15
* updated DESCRIPTION file (following this -- http://r-pkgs.had.co.nz/description.html
* `srFuns()`: Modified. Changed function returned when `simplify=FALSE` so that if the parameters are named that the name is dropped. Thus, when the function is used, the returned result will not be annoyingly named as the first parameter. Added functionality for the "density-independence" model.
* `srStarts()`: Modified. Added functionality for the "density-independence" model.
* `vbFuns()`: Modified. Changed function returned when `simplify=FALSE` so that if the parameters are named that the name is dropped. Thus, when the function is used, the returned result will not be annoyingly named as the first parameter.

# FSA 0.5.3
* **Date:** Mar15
* `growthModelSim()`: Deleted. The simulation functionality was moved to the `FSAsim` package. The functionality related to finding starting values for the von Bertalanffy modesl was moved to `vbStarts()`.
* `srFuns()`: Modified. A complete rebuild to make similar to `vbFuns()`. Added `simple=`. Added `type='Shepherd'` for the Shepherd (1982) three parameter model and `type='SailaLorda'` for the "Saila-Lorda" three parameter model from Iles (1994). Added tests for error messages.
* `srModels()`: Modified. A complete rebuild to make similar to `growthModels()`. Added "Shepherd" and "Saila-Lorda" models.
* `srSims()`: Deleted. The simulation functionality was moved to the `FSAsim` package. The functionality related to finding starting values was moved to `srStarts()`.
* `srStarts()`: Modified. A complete rebuild to streamline. Removed default method (i.e., a formula must be used now). Added "Shepherd" and "Saila-Lorda" models. Modified plotting routine, including adding `col.mdl=`, `lwd.mdl=`, and `lty.mdl=`. Moved the dynamic modeling aspects of `srSim()` into this function and is called with the new argument `dynamicPlot=TRUE`. Also added `minmax.ratio=` and `delta.prop=` for use with the dynamic plots.
* `vbStarts()`: Modified. A complete rebuild to streamline and fix some bugs that had not been found. Modified plotting routine, including adding `col.mdl=`, `lwd.mdl=`, and `lty.mdl=`. Also added all of the von Bertalanffy parameterizations in `growthModelSim()` into this function and is called with the new argument `dynamicPlot=TRUE`. Added dynamics plots for the "Francis" and "Schnute" parameterizations.

# FSA 0.5.2
* **Date:** Mar15
* `psdPlot()`: Modified. Fixed bug related to `NA`s in `max.brks` variable.

# FSA 0.5.1
* **Date:** Mar15
* `ageBias()`: Modified. Reversed the order of the formula ... it is now `nrefvar~refvar`. This more closely matches other R functions where the tilde may be interpreted as the word "by". In other words, the formula now reads as "nonreference variable by reference variable" (i.e., Y by X). Thanks for Richard McBride for the suggestion. Modified the age-bias plot extensively ... added `sfrac=` and defaulted to 0 to remove ends of the confidence intervals, added `cex.mean=` to control the size of the symbol for the mean point, added `lwd=` that will controland set all of the `lwd` defaults to 1.
* `agePrecision()`: Modified. Changed all "CV" results to "ACV".

# FSA 0.4.51
* **Date:** Mar15
* `catchCurve()`: Modified. Updated the help file regarding `zmethod="Smithetal"`.

# FSA 0.4.50
* **Date:** Mar15
* `capFirst()`: Modified. Added a check to make sure the inputted object was either a character or factor class. Added code to return the object as the same class of the original object.
* `lencat()`: Modified. Added a catch for bad choices of arguments. Added a catch to send a warning if the vector contains all `NA` values (this happens when `lencat()` is used within a loop or as part of `psdAdd()`). Added tests for error and warning messages. Changed how the formula was handled in the formula method.
* `psdAdd()`: Modified. Fixed a bug with names when using labels. Added `verbose=`. Added catches and sent messages if `verbose=TRUE` for when no Gabelhouse lengths are know for a species and if the lengths for a species are all missing (see note for `lencat()` above).
* `PSDlit`: Modified. Fixed the trophy length for White Bass (from 15 to 18). This solved a bug related to non-unique breaks.

# FSA 0.4.49
* **Date:** Mar15
* `expandCounts()`: Modified. Made message regarding rows with zero counts more useful. Added missing counts to the catch of zero counts. Made changes to handle more "odd" data entries (see "details" in the help file). Made some tests. Added some tests.

# FSA 0.4.48
* **Date:** Mar15
* `psdCalc()`: Modified. Corrected "bug" with `units=`. Also modified warning message when no "stock" fish were present in the data.frame to further note what `units=` were used (i.e., this problem is likely to happen if the data is inches but the user uses the default `units='mm'`). Thanks to S. Mather for inspring this fix.

# FSA 0.4.47
* **Date:** Feb15
* `dunnTest()`: Modified. Corrected "bug" in the order that the groups are subtracted (i.e., they were flipped).

# FSA 0.4.46
* **Date:** Feb15
* `catchCurve()`: Modified. Changed default for `pos.est=` to `topright`. Added `cex.pos=` (and set default to slightly smaller value).
* `chapmanRobson()`: Modified. Changed default for `pos.est=` to `topright`. Added `cex.pos=` (and set default to slightly smaller value).

# FSA 0.4.45
* **Date:** Feb15
* `hist.formula()`: Modified. Changed use of `par()` to eliminate modifications to the gridding of plots after the function is complete. Also removed the setting of `mar=` and `mgp=` in `par()`.
* `mrOpen()`: Modified. Removed pretty printing for `summary()` and `confint()` methods. These got in the way of being able to `cbind()` the results together for a succinct display.
* `residPlot()`: Modified. Changed use of `par()` to eliminate modifications to the gridding of plots after the function is complete.

# FSA 0.4.44
* **Date:** Feb15
* `.onAttach()`: Modified. Centered the message and adjusted for different lengths of version numbers.
* `alkPlot()`: Modified. Fixed bug when using `add=TRUE` with `type="bubble"`.
* `capHistSum()`: Modified. Changed to return `par()` options to what they were before the function was called.
* `catchCurve()`: Modified. Changed `plot()` to default to slighly lighter colored dots and a black line.
* `chapmanRobson()`: Modified. Changed `plot()` to default to slighly lighter colored dots. Changed to return `par()` options to what they were before the function was called.
* `growthModelSim()`: Modified. Changed to return `par()` options to what they were before the function was called.
* `growthRadPlot()`: Modified. Changed to return `par()` options to what they were before the function was called.
* `hist.formula()`: Modified. Changed to return `par()` options to what they were before the function was called.
* `lwCompPreds()`: Modified. Changed `quant.lens=` to `qlens=`. Changed default `qlens=` to have the 5th and 95th percentiles rather than the minimum and maximum values. Added `qpens.dec=` so that the user could control the number of decimals for the lengths derived from `qlens=`.
* `srSim()`: Modified. Changed to return `par()` options to what they were before the function was called.

# FSA 0.4.43
* **Date:** Feb15
* `mrOpen()`: Modified. Changed `summary()` and `confint()` methods to allow single, multiple, or all choices of parameters to return results for. Also added code to print the results more prettily.
* `swvCode()`: Modified. Fixed bug related to `blanks='extra'`.

# FSA 0.4.42
* **Date:** Feb15
* `filterD()`: Added.

# FSA 0.4.41
* **Date:** Jan15
* `catchCurve()`: Modified. Removed the use of larger points in the `plot()`.
* `chapmanRobson()`: Modified. Removed the use of larger points in the `plot()`.
* `metaM()`: Modified. Deleted `group=` (and created `method="ZhangMegreyD"` and `method="ZhangMegreyP"`).   Added geometric mean regresson methods for Hoenig. Changed default for `justM=` to `TRUE`. Fixed several minor bugs from the original implementation. Added some checks for reasonableness of some arguments. Created tests for several methods to see if the results matched those from Kenchington (2014). Added code to compute with several methods at once.
* `Mmethods()`: Added. Added as a function and removed as a vector.

# FSA 0.4.40
* **Date:** Jan15
* `lencat()`: Modified. Fixed a bug that occurred if `breaks=` were given but the vector contained `NA`s. Thanks to Ben Neely for pointing this out.

# FSA 0.4.39
* **Date:** Jan15
* `catchCurve()`: Modified. Changed `use.weights=` to `weighted=`. Added some checks for the formula in the formula version and for the variables in the default version. Add unit tests for warnings and errors and tow tests for values.
* `chapmanRobson()`: Modified. Added the `method="Smithetal"` methodology for estimating the SE of Z (and made it the default). Added some checks for the formula in the formula version and for the variables in the default version. Added `verbose=` to `summary()`. Add unit tests for warnings and errors and two tests for values.

# FSA 0.4.38
* **Date:** Jan15
* `alkPlot()`: Modified. Changed behavior for adding a legend to alleviate a bug.
* `metaM()`: Added.

# FSA 0.4.37
* **Date:** Jan15
* `confint.nlsBoot()`: Modified. Changed default for `err.col=` to `black` from `red`. Fixed example due to changes in `nlsBoot` package.
* `extraSS()`: Modified. Added a catch to make sure all models are of the same type. Added a catch to note that the function does not work with other that `lm()` or `nls()` models. Fixed a bug related to the labels for results from `anova()` being different depending on whether `lm()` or `nls()` models were given. Added some examples.
* `hist.formula()`: Modified. Fixed bug (originated in last version) that nothing was returned when only one histogram was constructed.
* `lrt()`: Modified. Changed call to `lrtest()` to a call to `lrtest.default()`. Added a catch to make sure all models are of the same type. Note that degrees-of-freedom from `lrtest()` are not error df; thus, modified to report error df to match `extraSS()`. Added some examples.

# FSA 0.4.36
* **Date:** Jan15
* `hist.formula()`: Modifiied. Added `iaxs=`, which when set to the default value of `TRUE` will use `xaxs="i"` and `yaxs="i"` to remove the "floating" x-axis produced by `hist()` in base R.
* `lwCompPreds()`: Modified. Added the `yaxs=` argument.
* `psdCalc()`: Modified. Added `showIntermediate=` to allow showing intermediate values in the calculation of the PSD indices. Added `justAdds=` to allow the user to return just those results that pertain to the values in `addLens=`. Added ability to use a named vector in `addLens=` and then not use `addNames=`. Changed `digits=1` to `digits=0`. Thanks to Ben Neely for the suggestions.
* `psdVal()`: Modified. Added ability to use a named vector in `addLens=` and then not use `addNames=`. The original functionality is still there. Added a check that one of the Gabelhouse lengths is not also one of the `addLens=` values. Deleted the `addLens=` value if it was (the user might have sent a name with this value and will want that name to appear in the results).
* `residPlot()`: Modiifed. Added `xpd=TRUE` to the loess line routine so that the curve and polygon would stay within the plotting region.
* `tictactoe()`: Modified. Add the ability to handle differences between when `xaxs="r"` and `yaxs="r"` are used and when `xaxs="i"` and `yaxs="i"` are used.

# FSA 0.4.35
* **Date:** Jan15
* `dunnTest()`: Added.

# FSA 0.4.34
* **Date:** Dec14
* `addZeroCatch()`: Modified. Removed `idvar=`, forced the `eventvar=` and `speciesvar=` variables in the returned data.frame to be numeric if they were numeric in the original data.frame, allowed `speciesvar=` to have more than one variable, and added `na.rm=`. Multiple values for `specvar=` will allow the user to add zeros based on a combination of variables (e.g., species and size category). The `na.rm=` argument allows the user to remove "missing" species, which are common if some sampling events did not capture any fish.

# FSA 0.4.33
* **Date:** Dec14
* `growthModelSim()`: Modified. Changed all "K0" objects to "t50".
* `headtail()`: Added.
* `logbtcf()`: Added.
* `lwCompPreds()`: Modified. Added `base=` to allow the function to work with logarithms to a different base. The original function was hard-wired to only use natural logarithms. Updated the examples and the tests.
* `vbFuns()`: Modified. Changed all "K0" objects to "t50".
* `vbStarts()`: Modified. Changed all "K0" objects to "t50".

# FSA 0.4.32
* **Date:** Nov14
* `expandcounts()`: Added (from `fishWiDNR` package).
* `expandLenFreq()`: Added. Same as `lenFreqExpand()` but thought that this name fits better with `expandCounts()`.
* `pcumsum()`: Added.
* `rcumsum()`: Modified. Completely new code (much simpler).
* `validn()`:  Added.

# FSA 0.4.31
* **Date:** Nov14
* Removed the suggests for `plyr`.
* `addZeroCatch()`: Modified. Slight modifications to help file. Fixed bug related to error checking the number of variables. Added some tests.
* `lencat()` Modified. Added `droplevels=` and kept `drop.levels=` as I could not consistently remember what the name of the argument was -- i.e., the user can use either one, but `droplevels=` is preferred.
* `mapvalues()`: Added. This is the exact same function from the `plyr` package. Included here to minimize conflicts between functions in `dplyr` and `plyr` that have the same name (i.e., don't have to install `plyr` just for `mapvalues()` when also using `dplyr`).
* `perc()`: Added.
* `psdCalc()`: Modified. Minor change related to `droplevels=` in `lencat()`.
* `psdVal()`: Modified. Changed name for the "zero" group to "substock."
* `swvCode()`: Modified. Removed ability to Stangle the code and thus removed `method=`. Modified code to allow usage of .Rmd files in addition to .Rnw files.


# FSA 0.4.30
* **Date:** Oct14
* Added a suggests for `plyr`, for examples using `mapvalues()`.
* `lencat()`: Modified. Changed `as.fact=` to default to same as `use.names=`. This will result in the same behavior as before. However, it also allows the user to set `use.names=TRUE` and `as.fact=FALSE` to return a character vector (that is not a factor).
* `psdAdd()`: Modified. Added `addSpec=` and `addLens=` so that the user can have non-Gabelhouse lengths for individual species.
* `PSDlit`: Modified. Changed "Walleye x Sauger" to "Saugeye" and "White Bass x Striped Bass" to "Palmetto Bass". Updated the Palmetto Bass values based on Dumont and Neely (2011), but kept old values as "Palmetto Bass (original)". Deleted redundant entries for some species.
* `recodeF()`: Deleted. Functionality is in `mapvalues()` from `plyr`. Ease come easy go (i.e., added in last version).

# FSA 0.4.29
* **Date:** Oct14
* Added a suggests for `dplyr`.
* Added an external file in inst/extdata for testing PSD and Wr calculations.
* `capFirst()`: Modified. Changed `words=` to `which=`.
* `psdAdd()`: Modified. Added a default and a formula version to allow efficiency with `dplyr`. Added examples. Updated tests.
* `recodeF()`: Added.
* `recodeSpecies()`: Deleted. Functionality replaced by `recodeF()` in combination with `capFirst()`.
* `wrAdd()`: Modified. Added a default and a formula version to allow efficiency with `dplyr`. Added examples. Updated tests.
* `WSlit`: Modified. Added results for Sardine.

# FSA 0.4.28
* **Date:** Sep14
* `psdAdd()`: Added.
* `psdDataPrep()`: Deleted. Functionality replaced by `psdAdd()`.
* `recodeSpecies()`: Modified. Completely re-written but with the same basic functionality. This new version returns a vector that can then be appended to an existing data.frame rather than the old function that returned a whole data.frame. This function should allow ease of use with `mutate()` from `dplyr`. Added more catches for bad `formuala=`s. Added some tests.
* `wrAdd()`: Modified. Completely re-written with completely new functionality. This new version returns a vector that can then be appended to an existing data.frame rather than the old function that returned a whole data.frame. This function should allow ease of use with `mutate()` from `dplyr`. Added more catches for bad `formuala=`s. Added some tests.
* `wrDataPrep()`: Deleted. Functionality replaced by new `wrAdd()`.

# FSA 0.4.27
* **Date:** Sep14
* `hist.formula()`: Modified. Slight modifications to warning messages.
* `Summarize()`: Modified. Slight modification to warning messages.
* `tictactoe()`: Modified. Changed `predbal=` to `predobj=`, `preybal=` to `predbal=`, `xlab=` to `predlab=`, `ylab=` to `preylab=`, `bal.col=` to `obj.col=`, and `bal.trans=` to `obj.trans=`.
* `vbStarts()`: Modified. Fixed a bug related to `plot=TRUE` when `type="Francis"` or `type="Schnute"`.
* `wrAdd()`: Modified. Modified how quadratic functions and the handling of fish less than the minimum applicable length were handled because of changes to `wsVal()`.
* `wsLit`: Modified. Changed order of variables, changed hybrid species names to match that of Neumann et al. (2012), update comments to related to Neumman et al. (2012) rather than Blackwell et al. (2012), and added information for the Riffle Dace.
* `wsVal()`: Modified. Changed the names of the `min.len` and `max.len` variables to be either `min.TL` and `max.TL` or `min.FL` and `max.TL` as appropriate. Suppressed the return of `max.len` and `quad` if they did not exist and suppressed return of `comment` if it was `none`. Added a catch if more than one species was given in `species=`. Created some tests.

# FSA 0.4.26
* **Date:** Sep14
* `capFirst()` Modified. Added an option to handle a vector of strings rather than just a single string.
* `lencat()`: Modified. Fixed bug with category names when `use.names=TRUE`. Moved all internal functions outside of `lencat()` environment (and renamed them). Cleaned up code.
* `psdCI()`: Modified. Added more catches for calls with mistakes. Create some internal functions to modularize the computations.  Added tests.
* `psdVal()`: Modified. Added more catches for calls with mistakes. Added tests.
* `psdCalc()`: Modified. Completely redone. Changed default to use multinomial rather than binomial method for confidence intervals (added `method=` argument to control CI type). Changed to throw an error of a species is not given in ``species=''. Added tests.
* `psdDataPrep()`: Modified. Changed `use.catnames=` to `use.names` and `psdname=` to `vname=` to be consistent with `psdVal()`. Removed duplicitous `factor()` calls for the length category and species name variables in the returned data.framed.
* `psdPlot()`: Modified. Completely redone (fixed several bugs and overall sloppy code). Added `psd.add=`. Changed `legend.pos=` and `legend.cex=` to `psd.pos=` and `psd.cex=`.
* `recodeSpecies()`: Modified. Made changes to reflect new `capFirst()` functionality.
* `tictactoe()`: Modfied. 
* `tictactoeAdd()`: Removed. Directed user to use `plotCI()` from `plotrix` instead.

# FSA 0.4.25
* **Date:** Sep14
* `mrClosed()`: Modified. Better handled a given value of `R=`.
* `psdCalc()` Modified. Fixed a bug that appeared when no "zero" fish were present in the data. Moved all internal functions outside of `psdCalc()` environment (and renamed them).
* `psdCI()`: Added.
* `psdPlot()`: Modified. Fixed a bug that appeared when no "zero" fish were present in the data. Used `psdCalc()` to compute the PSD values. Moved default legend position to `topleft`.
* `swvCode()`: Modified. Fixed bug when attempting to use this function from outside of the directory where the .Rnw file exists. Added functionality to add a "note" to the first line(s) of the output file. Added code to remove the first line of the output file if it was going to be blank.
* `swvFinish()`: Modified. Updated code because `iGetFilePrefix()` was deleted.

# FSA 0.4.24
* **Date:** Aug14
* `ageKey()`: Deprecated. See `alkIndAge()`.
* `ageKeyPlot()`: Deprecated. See `alkPlot()`.
* `ageKeyPrep()`: Deprecated. See `alkPrep()`.
* `alkAgeDist()`: Added.
* `alkIndAge()`: Added. Was `ageKey()`. Added `seed=` to help with reproducibility. Modified code to better handle when an age-length key has a whole row of missing data (as would happen if `as.fact=TRUE` and `drop.levels=FALSE` in `lencat()`). Added some checks for the age-length key structure. Moved all internal functions outside of `alkIndAge()` environment (and renamed them).
* `alkMeanVar()`: Added.
* `alkPlot()`: Added. Was `ageKeyPlot()`. Fixed bug with colors when adding legend to bar and area plots. Allowed legend to be removed from area plot. Added ability to add a legend to the lines and splines plot. Added `pal=` to allow choice of color palette for areas in bar and area plot and lines in lines and splines plots. Allowed an area plot when one row of age-length key sums to zero (previously did not allow this). Create internal functions for each plot type. Moved all internal functions outside of `alkPlot()` environment (and renamed them). Added some checks on the age-length key structure.
* `alkPrep()`: Added. Was `ageKeyPrep()`. Added some checks on the age-length key structure.
* `iCheck ALK()`: Added as an internal function (used to test the structure of the age-length keys in several other functions).
* `summary.mrOpen()`: Modified. Removed "Estimates" heading if `verbose=FALSE`.
* `Summarize()`: Modified. Moved all internal functions outside of `Summarize()` environment (and renamed them).


# FSA 0.4.23
* **Date:** Aug14
* `removal()`: Modified. Completely modified the code so that the examples with `apply()` and `lapply()` would also provide confidence intervals. Also changed the code to reflect that $\sum_{i=1}^{k-1}T_{i}$ from Schnute (1983) is the same as $X$ from Carle and Strub (1978), the $\sum_{i=1}^{k-1}T_{i}-C_{1}$ in Schnute (1983) is the same as $X-(k-1)C_{1}$, and $q$ in Schnute (1983) is $p$ in most other resources. These changes allowed some efficiencies and connected the theory behind the methods more firmly. Removed the check for character data. Kept the check for whether catch was a vector or not but if catch is a one row or one column matrix or data.frame then it will be converted to a vector to continue. The latter change allows one to extract one row from a data.frame to send to `removal()` without having to use `as.numeric()`. Modified and added examples of the use of `apply()` and `lapply()`.

# FSA 0.4.22
* **Date:** Aug14
* `ageKey()`: Modified. Changed to using `all.equal()` to check if the ALK has rows that don't sum to 1. This was an attempt to minimize the number of "false negatives" caused by [R FAQ 7.31](https://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f). Changed the check of whether the longest fish in the length sample is greater than the longest length bin in the ALK to whether the longest fish in the length sample is greater than the longest length bin in ALK PLUS the minimum width of length categories. This last change is an attempt to minimize the number of warnings that occur when the longest fish in the length sample would be in the last length category ALK but because the length categories are labelled by their minimum length it looks like it is not. The minimum width is used to still allow unevent length categories and, thus, this check may still produce some "false negatives."
* `ageKeyPlot()`: Modified. Removed `bubble.ylab=`. Modified `ylab=` to handle what `bubble.ylab=` used to handle.
* `removal()`: Modified. Added options to perform Moran (1951) and Schnute (1983) removal methods. Added examples of the new functionality. Updated the tests for the new functionality.

# FSA 0.4.21
* **Date:** Jul14
* `depletion()`: Modified. Changed `type=` to `method=` and added `DeLury` as an option to `method=` (and left `Delury`).  Changed `ricker.mod=` to `Ricker.mod=`. Added some checking for bad arguments. Created internal functions specific to the Leslie and DeLury methods (for isolation). Modified some clunky code. Added references to specific sections in Seber (2002) for SE equations. Updated examples. Added tests and error checking.
* `coef.depletion()`: Modified. Added `digits=`.
* `confint.depletion()`: Modified. Added `digits=`. Modified the `parm=` list to be more efficient.
* `plot.depletion()`: Modified. Removed internal `par()` settings.
* `summary.depletion()`: Modified. Added `verbose=` and `digits=`.

# FSA 0.4.20
* **Date:** Jul14
* `removal()`: Modified. Made `"CarleStrub"` the default method. Changed `type=` to `method=`. Changed internal `meth` object to `lbl`. Moved all internal functions outside of `mrOpen()` environment and added other internal functions to isolate all intermediate calculations. Added a `verbose=` and `parm=` to `summary()`. Streamlined clunky code in `confint()` including removing the `all` and `both` options from `parm=`. Added more checks for abd inputs, notes in the code as to sources for the fomulae, and tests.

# FSA 0.4.19
* **Date:** Jul14
* Modified some tests to check whether the suggested package was installed.
* `capHistSum()`: Modified. Changed column and row labels for `$methodB.top` and column labels for `$methodB.bot`. Added a m-array object for when more than two sampling events are present. Added calculations for the number of fish first seen on event i (ui), the number of fish last seen on event i (vi), and the number of fish seen i times (fi) to `$sum`.
* `jolly()`: Added. Same as `mrOpen()`, added only for convenience.
* `mrClosed()`:  Modified. Fixed bugs around printing of CI type with Schnabel and the ignoring of `conf.level=` with Schnabel.
* `mrOpen`: Modified. Changed `ci.type=` to `type=` and `phi.type=` to `phi.full=`. Removed `type=` from `summary()` and added a `verbose=` which will print only the estimates if `FALSE` or both observables and estimates if `TRUE`. Added a `verbose=` to `confint()` to control whether the message about the type of confidence interval is printed or not. Moved all internal functions outside of `mrOpen()` environment and added other internal functions to isolate all intermediate calculations. Changes to row and column labels in `capHistSum()` resulted in changes to row lables for `summary()` and `confint()` results. Streamlined some clunky code. Added checks for misformed `mb.top=` and `mb.bot=`. Added tests and notes in the code as to sources for the fomulae.
* `plot.CapHistSum()`:  Added.
* `plot.mrClosed()`:  Modified. Changed axis labels as the expressions did not print with some fonts and devices.

# FSA 0.4.18
* **Date:** Jul14
* Moved to compiling under R 3.1.1.
* Added a Suggests for `marked` for the example in `capHistConvert()`.
* `ageBias()`: Modified. Changed default value of `min.n.CI=` from 5 to 3. Added an `na.rm=TRUE` to the `min()` and `max()` that produced the age ranges for the age agreement table.
* `BluegillJL`:  Modified. Corrected lake name and added a citation.
* `capHistConvert()`:  Modified. This should probably be considered as a new function if updating from the old version. Modifications included simplifying the structure allowed for the input data.frames (they can have only an id or a freq column and then columns related to the capture history ... this makes the function less flexible but simplifies its use for those that are most likely to use it), moved to a series of internal functions, created a common intermediate data format (which streamlined the code considerably), changed the name of the `FSA` format to `individual` and the `Rcapture` format to `frequency`, added an `out.type='event'` format, added `in.type='RMark'` and `in.type='marked'`formats, fixed the bug with outputting `RMark` format, changed the default for new frequency variables from `Freq` to `freq`, removed the `mch=` and `event=` arguments, replaced `cols=` with `cols2ignore=`, added the `include.id=` argument, changed the `in.type=` default, coded some "catches" for common mistakes in use, coded to keep the unique fish identifier in `id=` or event name given in the variable names as much as possible, fixed a bug with `event.ord=`. Added several new examples.
* `capHistSum()`: Modified. Change `cols=` argument to `cols2use=`. Moved all internal functions outside of `capHistSum()` environment.
* `CutthroatAL`:  Modified. Updated from a new source to include many more years of samples.
* `fitPlot()`:  Modified. Changed `trans.pt=` to `transparency=`.
* `mrClosed()`:  Modified. Completely re-built the internal file structure. Changed `incl.inputs=` to `verbose=`. Added the ability to construct a CI for the overall PE when multiple groups are used in a Petersen family method (thus, added a `incl.all=` to `confint()`). Changed default for `incl.all=` from `FALSE` to `TRUE`. Modified the messages when `verbose=TRUE`.
* `plot.AgeBias()`. Modified. Fixed bug that produced a warning if all of the bias t-tests were either significant or not significant. Changed `col.err=` to `col.CI=`, `lwd.err=` to `lwd.CI=`, `col.err.sig=` to `col.CIsig=`, `col.ref=` to `col.agree=`, `lwd.ref=` to `lwd.agree=`, `lty.ref=` to `lty.agree=`, `show.rng=` to `show.range=`, `col.rng=` to `col.range=`, `lwd.rng=` to `lwd.range=`. Removed `col.lab=` and `row.lab=` which were deprecated several minor versions ago. Changed default values for `lwd.rng=` and `lwd.CI=` from 2 to 1. Added a `cex.numbers=` argument for controlling the size of the numbers in the "numbers plot" (defaults to 0.9).
* `plotBinResp()`:  Modified. Changed `trans.pt=` to `transparency=`.

# FSA 0.4.17
* **Date:** Jul14
* `confint.mrClosed()`: Modified. Moved all internal functions outside of `confint.mrClosed()` environment (see `iCI.MRCMultiple()` and `iCI.MRCSingle()`). Changed `ci.type=` to just `type=`. Streamlined binomial method for single census. Used `iMRCSingleSE()` to get SE for when `type="normal"` for Chapman, Bailey, and Ricker methods.
* `extraSS()`: Modified. Slight change to row labels in output table.
* `iMRCMultiple()`:  Added. Was `mrc2()` internal function inside of `mrClosed()` environment.
* `iMRCSingle()`:  Added. Was `mrc1()` internal function inside of `mrClosed()` environment.
* `iMRCSingleSE()`: Added. Moved functionality out of `summary.mrClosed()`. Checked and documented all formulas with sources (in code and in Rd file).
* `lrt()`: Modified. Slight change to row labels in output table.
* `mrClosed()`: Modified. Moved all internal functions outside of `mrClosed()` environment (see `iMRCMultiple()` and `iMRCSingle()`). Changed `type=` argument to `method=`. Added more catches for argument problems (required setting `n=`, `m=`, `M=` and `R=` to `NULL`). Streamlined warning message for when `incl.SE=TRUE` is used with Schnabel or Schumacher-Eschmeyer method. Added tests and reported results in the help file for population size, SE, and CI estimates for each method.
* `plot.mrClosed()`: Modified. Removed setting of `par()`. Changed from using `lowess()` to using `loess()` and set better default values. Added descriptive text to help file.
* `summary.mrClosed()`: Modified. Moved SE calculations into an internal function (see `iMRCSingleSE()`).

# FSA 0.4.16
* **Date:** Jul14
* `BluegillLM`: Modified. Added a seealso.
* `residPlot()`: Modified. Changed the loess-related methods to use `loess()`, to put an approximate confident band with the line, the line and band are "under" the points, the line is lighter. Put the horizontal reference line at zero under the points. Made `loess=TRUE` the default.
* `iAddLoessLine()`: Modified. See `residPlot()`.
* `iHndlFormula()`: Modified. COrrected the positioning of the explanatory variables when the model has a response variable.
* `iMakeBaseResidPlot()`:  Added as an internal function to `residPlot()` to simplify some coding.
* `iMakeColor()`: Modified. More intelligently handles values that are greater than 1 (converts them to decimals by inverting.)
* `lwPredsComp()`: Modified. Changed `mdl=` to `object=`. Added use of internal `iHndlFormula()` and moved two internal functions outside the main function. Changed default for intervals from `both` to `confidence` and changed so that if only the confidence or prediction intervals are plotted they will be black with `lwd=` width (if both are plotted the CI is now black and the PI is now blue). Added a `show.preds` argument. Changed `connect.means=` to `connect.preds=`. Changed default `lwd=` value and how it is used for CIs, PIs, and the connection lines. Added `col.connect=` argument. Removed `mar` and `mgp` from `par()` call (left `mfrow`). Added more examples. Added tests for error messages.
* `residPlot()`: Modified. Added `inclHist=` argument. Corrected a bug around the use of `thigmophobe()` in `iAddOutlierTest()`. Changed default for `student=` to `FALSE`. Modified and added more examples.
* `SMBassWB`: Modified. Added a seealso.


# FSA 0.4.15
* **Date:** Jun14
* lots of roxygen2 Rd cleaning.
* `addLoessLine()`: Deleted. Moved functionality to `iAddLoessLine()` and moved code to `residPlot()` file..
* `addOutlierTestResults()`: Deleted. Moved functionality to `iAddOutlierTestResults()` and moved code to `residPlot()` file.
* `capHistConvert()`: Added an `interactive()` to the `Rcapture` example in the help file.
* `checkStartcatW()`: Deleted. Moved functionality to `iCheckStartcatW()`.
* `ci.fp()`: Deleted. Moved functionality to `iCIfp()` and moved code to `fitPlot()` file.
* `ci.fp.1()`: Deleted. Moved functionality to `iCIfp1()` and moved code to `fitPlot()` file.
* `ciLabel()`: Deleted. Moved functionality to `iCILabel()`.
* `getAllDependencies()`: Deleted. Moved functionality to `iGetAllDependencies()` and moved code to `swvUtils` file.
* `getFilePrefix()`: Deleted. Moved functionality to `iGetFilePrefix()` and moved code to `swvUtils` file.
* `getMainTitle()`: Deleted. Moved functionality to `iGetMainTitle()` and moved code to `residPlot()` file.
* `getVarFromFormula()`: Deleted. Moved functionality to `iGetVarFromFormula()`.
* `hndlFormula()`: Deleted. Moved functionality to `iHndlFormula()`.
* `hndlMultWhat()`: Deleted. Moved functionality to `iHndlMultWhat()`.
* `iAddLoessLine()`: Added. Was `addLoessLine()`.
* `iAddOutlierTestResults()`: Added. Was `addOutlierTestResults()`.
* `iCheckStartcatW()`: Added. Was `checkStartcatW()`.
* `iCIfp()`: Added. Was `ci.fp()`.
* `iCIfp1()`: Added. Was `ci.fp.1()`.
* `iCILabel()`: Added. Was `ciLabel()`.
* `iGetAllDependencies()`: Added. Was `getAllDependencies()`.
* `iGetFilePrefix()`:  Added. Was `getFilePrefix()`.
* `iGetMainTitle()`: Added. Was `getMainTitle()`.
* `iGetVarFromFormula()`: Added. Was `getVarFromFormula()`.
* `iHndlFormula()`: Added. Was `hndlFormula()`.
* `iHndlMultWhat()`: Added. Was `hndlMultWhat()`.
* `iLegendHelp()`: Added. Was `legendHelp()`.
* `iMakeColor()`: Added. Was `makeColor()`.
* `iMakeFilename()`: Added. Was `makeFilename()`.
* `iMakeItemsToRemove()`:  Added. Was `makeItemsToRemove()`.
* `iProcessSessionInfo()`: Added. Was `processSessionInfo()`.
* `iPSDLitCheck()`: Added. Was `psdLitCheck()`.
* `is.even()`: Added.
* `is.odd()`: Added. Was `odd()`.
* `iTypeoflm()`: Added. Was `typeoflm()`.
* `iwsLitCheck()`: Added. Was `wsLitCheck()`
* `legendHelp()`: Deleted. Moved functionality to `iLegendHelp()`.
* `listSpecies()`: Deleted. Moved functionality to `iListSpecies()`.
* `makeColor()`: Deleted. Moved functionality to `iMakeColor()`.
* `makeFilename()`: Deleted. Moved functionality to `iMakeFilename()` and moved code to `swvUtils` file.
* `makeItemsToRemove()`: Deleted. Moved functionality to `iMakeItemsToRemove()` and moved code to `swvUtils` file.
* `odd()`: Deleted. Moved functionality to `is.odd()`.
* `predict.nlsBoot()`:  Added an `interactive()` to the `nlstools` example in the help file.
* `printProgressMsg()`:  Deleted. Not used anywhere.
* `processSessionInfo()`: Deleted. Moved functionality `iProcessSessionInfo()` and moved code to `swvUtils` file.
* `PSDLitCheck()`: Deleted. Moved functionality to `iPSDLitCheck()` and moved code to `psdVals()` file.
* `pssCalc()`: Deleted. Was deprecated several versions ago. See `psdCalc()`.
* `pssDataPrep()`: Deleted. Was deprecated several versions ago. See `psdDataPrep()`.
* `pssPlot()`: Deleted. Was deprecated several versions ago. See `psdPlot()`.
* `pssVal()`: Deleted. Was deprecated several versions ago. See `psdVal()`.
* `typeoflm()`: Deleted. Moved functionality to `iTypeoflm()`.
* `wsLitCheck()`: Deleted. Moved functionality to `iwsLitCheck()` and moved code to `wsVals()` file.

# FSA 0.4.14
* **Date:** Jun14
* added tests (in `test_VonB2b.R`) to assure that group comparisons of von Bertalanffy parameters equal those in Kimura (1980) and `vblrt()` in `fishmethods`.
* added importsFrom for `lmtest` for `lrt()`. Also used in testing (`test_VonB2b.R`).
* `confint.nlsBoot()`: Modified. Modified the plotting to use `hist.formula()`, removed `par(mar=)` definitions, and added `err.col=` and `lwd.col=` to control the color and line width of the confidence interval line on the plot.
* `extraSS()`: Added.
* `growthModels()`: Modified. Added Weisberg parameterization. Changed `vbGallucciQuinn` to `vbGQ`.
* `growthModelSim()`: Modified. Added Weisberg parameterization. Added `vbGQ` abbreviation (synonymous with `vbGallucciQuinn`).
* `lrt()`: Added.
* `vbFuns()`: Modified. Added Weisberg parameterization. Added `vbGQ` abbreviation (synonymous with `vbGallucciQuinn`). Simplified the functions for when `simple=FALSE` (no error checking now).
* `vbModels()`: Modified. Added Weisberg parameterization. Changed `vbGallucciQuinn` to `vbGQ`.
* `vbStarts()`: Modified. Added Weisberg parameterization. Added `vbGQ` abbreviation (synonymous with `vbGallucciQuinn`). Added an internal function for checking whther the starting values for K and Linf made sense.

# FSA 0.4.13
* **Date:** Jun14
* added testthat files for error checking of `chapmanPlot()`, `vbFuns()`, `vbStarts()`, and `walfordPlot()`. Added a testthat file for checking that the von Bertalanffy fitting using `vbFuns()` and `vbStarts()` matches other sources.

* `ageBias()`: Modified. Deprecated `col.lab=` and `row.lab=` and replaced with `ref.lab=` and `nref.lab=`. Moved all functions that were internal to main functions to being internal to the package. In the process, I changed the names of the internal functions slightly, made explicit the argument passing, and added internal descriptions of the internal files. Changed several if else strings in the plot method to a `switch()`.
* `agePrecision()`: Modified. Changed some messages so they were not as wide.
* `chapmanPlot()`: Modified. Removed S3 functions so that `vbStarts()` has to use a formula. Added some checking related to the formula. 
* `growthModels()`: Modified. Created an internal function that eliminates repetitiveness between this and `vbModels()`. Changed the `GompX` types to `GompertzX`.
* `growthModelSim()`: Modified. Removed S3 functions so that `growthModelSim()` has to use a formula. Added some checking related to the formula. Changed the order of the arguments so that `formula=` and `data=` come before `type=`. This allows a similar interface with `vbStarts()`. Included a hack that still allows the user to enter a type as the first argument (and thus not have to type `type=` if any parameterization besides the `vbTypical` is being used). Corrected spelling of Gallucci for Gallucci and  Quinn model.
* `hndlFormula()`: Modified. Fixed bug with expected number of response variables value in return list.
* `SpotVA1`: Modified. Updated reference.
* `vbFuns()`: Modified. Changed `schnute` parameterization to use L3 instead of L2 and t3 instead of t2. 
* `vbModels()`: Modified. Created an internal function that eliminates repetitiveness between this and `growthModels()`.
* `vbStarts()`: Modified. Removed S3 functions so that `vbStarts()` has to use a formula. Added some checking related to the formula. Changed `tFrancis=` to `ages2use=`. Changed the Schnute method to use the ages in `ages2use=` rather than being hard-wired to use the minimum and maximum observed age. Both the Schnute and Francis methods will use the minimum and maximum observed ages if `ages2use=NULL`. Added a catch for if `ages2use=` are in descending order (should be in ascending order). Changed `Schnute` parameterization to use L3 instead of L2.
* `walfordPlot()`: Modified. Removed S3 functions so that `vbStarts()` has to use a formula. Added some checking related to the formula. 

# FSA 0.4.12
* **Date:** May14
* added Suggests for `testthat`, `fishmethods`, `FSAdata` for testing and `popbio` for an example that was made "interactive" from "dont run"(see below).
* added testthat files for `ageBias()` and `agePrecision()`.

* `ageBias()`: Modified. Removed unit testings from examples and put in the testing file.
* `agePrecision()`: Modified. Removed deprecated `what="agreement"`.
* `confint.nlsBoot()`: Modified. Changed example from "dont run" to "interactive."
* `fact2num()`: Modified. Changed example from "dont run" to "interactive."
* `fishR()`: Modified. Removed `news` and added `posts` to the `where=` argument. Cleaned up the Rd file. Changed example from "dont run" to "interactive."
* `FSA()`: Modified. Cleaned up the Rd file.
* `FSANews()`, `fsaNews()`: Modified. Cleaned up and fixed the Usage section in the Rd file. Changed example from "dont run" to "interactive."
* `growthRadPlot()`: Modified. Changed example from "dont run" to "interactive."
* `htest.nlsBoot()`: Modified. Changed example from "dont run" to "interactive."
* `lagratio()`: Modified. Changed example from "dont run" to "interactive."
* `lencat()`: Modified. Changed Rd file for deletion of `view()`.
* `popSizesPlot()`: Modified. Changed example from "dont run" to "interactive."
* `TroutDietSL`: Modified. Changed Rd file for deletion of `view()`.
* `view()`: Deleted. Moved to NCStats package.
* `wrDataPrep()`: Modified. Changed Rd file for deletion of `view()`.

# FSA 0.4.11
* **Date:** May14
* Removed Roxygen directives in DESCRIPTION (with changes to roxygen2 4.0.1).
* Changed `@S3method` and `@method` to `@export` in the following files according to changes in ROxygen2 as [described here](https://stackoverflow.com/questions/7198758/roxygen2-how-to-properly-document-s3-methods/7199577/), among several other places: `ageBias`, `agePrecision`, `bootCase`, `catchCurve`, `chapmanRobson`, `confint.nlsboot`, `depletion`, `dietOverlap`, `fitPlot`, `hist.formula`, `htest.nlsBoot`, `ks2d1`, `ks2d1p`, `ks2d2`, `ks2d2p`, `ksTest`, `lencat`, `mrClosed`, `mrOpen`, `plotBinResp`, `predict.nlsBoot`, `removal`, `residPlot`, `srStarts`, `Subset`, `Summarize`, `sumTable`, `vbStarts`, and `walfordChapmanPlot`.

* `addZeroCatch()`: Modified. Added a catch for the situation where no zeros need to be added to the data.frame. Cleaned-up the help file, modified the examples, and added another example. Thanks to Ben Neely for bringing this bug (handling where zeros are not needed) to my attention.
* `capHistSum()`: Modified. Cleaned up the code (no changes in functionality).
* `catchCurveSim()`: Deleted. Moved to FSAsim package.
* `checkstartcatw()`: Modified. Changed the catch for whether the starting category value was greater than the minimum observed value to correct for a pathological case where they were equal but not with machine rounding.
* `lenFreqExpand()`: Modified. Slightly changed the examples in the help file.
* `lwPredsComp()`: Modified. Streamlined the code (no changes to functionality).
* `mrOpen()`: Modified. Streamlined the code (no changes to functionality). Removed all explicity partial matching options in `switch()`es as these were already caught with previous `match.arg()`s.

# FSA 0.4.10
* **Date:** May14
* Added Roxygen directives to DESCRIPTION.
* Updated to Roxygen2 4.0.0 which modified several help files.

* `ageBias()`: Modified. Cleaned-up the help file.
* `agePrecision()`: Modified. Cleaned-up the help file.
* `ageKey()`: Modified. Cleaned-up the help file and modified the example.
* `ageKeyPlot()`: Modified. Added more description and cleaned-up the help file.
* `ageKeyPrep()`: Modified. Added more description and cleaned-up the help file.
* `lenFreqExpand()`: Modified. Corrected `total=` to use `length(x)` rather than `nrow(df)`, which was left over from a previous change. Cleaned-up the help file.
* `mrClosed()`: Modified. Increased the readability of the code (added comments, used `with()` for some long calculations, added spacing). Added specific citations to equations in the help file. Changed the degrees-of-freedom in the confidence interval calculation for the Schnabel methods from number of samples minus 2 to number of samples minus 1 (following Krebs).
* `poiCI()`:  Modified. Cleaned-up the help file.
* `psdDataPrep()`: Modified. Fixed error around `use.catnames=`.
* `swvCounts()`: Modified. Fixed error in output.

# FSA 0.4.9
* **Date:** May14
* Removed nlme dependencies (with removal of `vbDataGen()`).

* `ageComp()`: Deleted. Fully deprecated. Use `ageBias()` and `agePrecision()` instead.
* `cohortSim()`: Deleted. Moved to FSAsim package.
* `depletion()`: Modified. Remove link to `leslieSim()`.
* `lengthWeightSim()`: Deleted. Moved to FSAsim package.
* `leslieSim()`: Deleted. Moved to FSAsim package.
* `lwModelSim()`: Deleted. Moved to FSAsim package.
* `mrClosed()`: Modified. Remove link to `mrClosed1Sim()`.
* `mrClosed1Sim()`: Deleted. Moved to FSAsim package.
* `srCobWeb()`: Deleted. Moved to FSAsim package.
* `vbComp()`: Deleted. Moved to FSAsim package.
* `vbDataGen()`: Deleted. Moved to FSAsim package.
* `vbFuns()`: Modified. Remove link to `vbComp()`.
* `VBGMlit()`: Deleted. Moved to FSAsim package.

# FSA 0.4.8
* **Date:** May14
* `ageBias()`: Modified. Added the ability to use multiple `what=` arguments with `c()`. Added `what="n"` to get the sample size on the age-agreement table. Added `nYpos=` to `plot()` to allow control of the position of the sample size values on the plot. Changed the order of the printing of results when `what="symmetry"` is used in `summary()`. The order more closely follows the "level of complexity" of the tests. Added unit test examples to the help file.
* `agePrecision()`: Modified. Added the ability to use multiple `what=` arguments with `c()`.
* `hndlMultWhat()`: Added. An internal file to help `ageBias()` and `agePrecision` handle multiple `what=` arguments.

# FSA 0.4.7
* **Date:** Apr14
* Removed all of the functions related to constructing and validating standard weight equations. These are now in the [FSAWs package](https://github.com/droglenc/FSAWs). This is the start of an effort to streamline the FSA package.
* Removed importFrom quantreg (only used for standard weight methods).

* `ChinookArg`: Added (from FSAdata).
* `emp()`: Removed.
* `FroesWs()`: Removed.
* `lencatOLD()`: Removed (from FSA-internals).
* `lwPredsComp()`: Modified. Changed example to using `ChinookArg` rather than `RuffeWs` because `RuffeWs` was moved to the FSAWs package.
* `LMBassWs`: Removed.
* `rlp()`: Removed.
* `RuffeWs`: Removed.
* `WalleyeGerowLW`: Removed.
* `wsValidate()`: Removed.
* `WalleyeGerowLW`: Removed.

# FSA 0.4.6
* **Date:** Apr14
* Changed to compiling under R 3.1.0
* Imported `stackpoly()` from plotrix for use in `ageKeyPlot()`.
* Added concepts (that largely match those in the FSAdata pacakge) to most of the data files.
* Made some grammatical changes and added author sections to Rd files.

* `ageKeyPlot()`: Added.
* `dietOverlap()`: Modified. Changed examples in help file to reflect changes to `lencat()`.
* `lencat()`: Modified. Added generic functions. `lencat.default()` accepts a vector as its first argument and returns a single vector. `lencat.formula()` accepts a formula as its first argument and the `data=` argument. The `lencat.formula()` is the same as the old `lencat()` and `lencat.default()` provides new functionality. Additionally, the default for `startcat=` is now `NULL` and a value for `startcat=` is found automatically (though a value can still be supplied by the user). The `use.catnames=` was changed to `use.names=`. Other changes were made to simplify the code.
* `lenFreqExpand()`: Modified. Removed the `df=` and `cl=` arguments and replaced with `x=`, which is simply a vector of length measurements. Changed to `startcat=NULL` so that that the starting category value can be determined automatically (or can still be set by the user).

# FSA 0.4.5
* **Date:** Apr14 
* Converted to using github as a repository.
* Changed NEWS to NEWS.md
* Added ImportFrom for relax package (see below).

* `ageBias()`: Modified. Added a plot that shows the number of observations at each combined age. Changed the coding slightly around Bowker's test (added an internal function) and implemented Evans and Hoenig's and McNemar's test. These changes resulting in adding a "table" choice to `what=` that will print just the age-agreement table. When `what="symmetry"` is chosen all three ob Bowker's, McNemar's, and Evans-Hoenig results will be output as a table. The age-agreement table is no longer printed when `what="symmetry"`. In addition, `what="Bowkers"`, `what="EvansHoenig"`, and `what="McNemars"` can be used to see the Bowker's, Evans and Hoenig, and McNemars test results, respectfully. Added a `cont.corr=` argument for use with McNemars test.
* `agePrecision()`:  Modified. Added the ability to show raw (vs. absolute value) differences between structures. This resulted in the removal of `what="agreement"` (though it is deprecated, with a message, for now) and the addition of `what="difference"` and `what="absolute difference"`.
* `fishR()`: Modified. Changed to point to the github NEWS.md when `where="news"`.
* `fitPlot()`: Modified. Changed the logistic regression code to handle the changes to `plotBinResp()` (see below). In addition, a temporary fix was added so that the size of the y-axis labels could be modified with an external call to `par()`. This was a fix for Glen Sutton but will ultimately need to be handled more elegantly.
* `fsaNews()`: Modified. Changed to point to the github NEWS.md.
* `catchCurveSim()`: Added back from FSATeach (required adding ImportFrom for relax package).
* `cohortSim()`: Added back from FSATeach (required adding ImportFrom for relax package).
* `growthModelSim()`: Added back from FSATeach (required adding ImportFrom for relax package).
* `lengthWeightSim()`: Added back (was `lwModelSim()`) from FSATeach (required adding ImportFrom for relax package).
* `leslieSim()`: Added back from FSATeach (required adding ImportFrom for relax package).
* `mrClosed1Sim()`: Added back from FSATeach (required adding ImportFrom for relax package).
* `plotBinResp()`: Modified. Added `yaxis1.ticks=` and `yaxis1.lbls=` arguments so that the user can control the tick-mark locations and labels for the left y-axis (the defaults are to show ticks every 0.1 units but only label 0, 0.5, and 1). Added `yaxis2.show=` argument to allow the user to "turn-off" the right y-axis (defaults to being on) which is labeled with the level labels.
* `srSim()`: Added back from FSATeach (required adding ImportFrom for relax package).

# FSA 0.4.4
* **Date:** Apr14
* `ageKeyPrep()`: Added.
* `agePrecision()`: Modified. Fixed the bug where the APE and CV were over-estimated in situations where the multiple ages agreed at an age=0 (thanks to Richard McBride for pointing out this error).
* `wsLit`: Modified. Added Pursak chub information from Sulun et al. (2014).

# FSA 0.4.3
* **Date:** Mar14
* `ageBias()`: Added. Extracted the age-bias related material from `ageComp()`. Modified the code to remove unneeded code. From `ageComp()`, remove the  `what=` argument related to differences and added a `difference=` argument. Also changed `what="bias.diff"` to `what="diff.bias"` to allow for a quicker partial matching (i.e. separate more from `what="bias"`). Major modifications to how the axis limits are created if none are provided. Modified where the sample size is shown on the age-bias plot. Added the `min.n.CI=` argument. Added an example using `WhitefishLC` to be consistent with `agePrecision()`.
* `ageComp()`: Modified. Split into `ageBias()` and `agePrecision()`. Added a warning that this function is deprecated and will be removed in the future.
* `ageKey()`: Modified. Fixed a bug that occurred when a data frame that already contained an LCat variable was provided. 
* `agePrecision()`:  Added. Extracted age precision related material from `ageComp()`. Modified the code to allow for calculations across more than two structures. Code was streamlined dramatically from what was in `ageComp()`. Added an example using WhitefishLC as it allows for demonstrating more than two age assignments.
* `capFirst()`: Modified. Added functionality, controlled by the new words= parameter, to allow all words, rather than just the first word, to be capitalized.
* `capHistConvert()`:  Modified the help file by commenting out the example that depends on the RCapture package. This is needed for the RForge site for the time being.
* `fitPlot()`: Modified Rd. Added two polynomial regression examples.
* `fitPlot.IVR()`: Modified. Changed to use new `typeoflm()`, changed `interval=` argument, removed automatic main title, removed a bunch of unneeded code.
* `fitPlot.logreg()`:  Modified. Removed automatic main title.
* `fitPlot.nls()`: Modified. Removed automatic main title.
* `fitPlot.ONEWAY()`:  Modified. Changed to use new `typeoflm()`, removed automatic main title, removed one line of unneeded code.
* `fitPlot.SLR()`: Modified. Changed to use new `typeoflm()`, changed `interval=` argument, removed automatic main title.
* `fitPlot.TWOWAY()`:  Modified. Changed to use new `typeoflm()` and removed automatic main title
* `gReshape()`: Modified. Added a `drop=` argument so that the user can drop some variables before reshaping. Also, added `new.row.names=1:100000` to the `reshape()` call to work-around issues with duplicate row names (which were particularly problematic if any of the `id.vars=` had missing values.)
* `growthModels()`: Modified. Corrected spelling of Gallucci for Gallucci and  Quinn model.
* `hist.formula()`: Modified. Add a `col=` argument that defaults to "gray90".
* `hndlFormula()`: Added. An internal function to handle various assessments related to using formulas.
* `lencat()`: Modified. Added the ability to add names if the vector sent in `breaks=` is named.
* `confint.mrClosed()`: Modified. Removed extra linespaces in printed output. Changed default for `incl.inputs=` to FALSE.
* `summary.mrClosed()`: Modified. Removed extra linespaces in printed output. Changed default for `incl.inputs=` to FALSE.
* `predict.nlsBoot()`:  Modified the help file by commenting out the example that depends on the nlsBoot package. This is needed for the RForge site for the time being.
* `psdCalc()`: Added (was `pssCalc()`).
* `psdDataPrep()`: Added (was `pssDataPrep()`) and modified. Deleted the code in this function that added category names as this functionality was added to `lencat()`. See `lencat()` above.
* `PSDlit`: Added (was `PSSlit`) and modified. Changed all species names to have both words capitalized so as to follow the latest AFS guidelines.
* `psdPlot()`: Added (was `pssPlot()`).
* `psdVal()`: Added (was `pssVal()`).
* `rsdCalc()`: Deleted.
* `rsdVal()`: Deleted.
* `recodeSpecies()`: Modified. Changed `capFirst=` to `doCapFirst=` to minimize confusion with `capFirst()`. Change `doCapFirst=` to a character that behaves like `words=` in `capFirst()`, rather than as a logical.
* `SpotVA1`:  Modified. Removed link to source documents because it caused a problem when making the PDF manual.
* `StripedBass1`:  Deleted. Moved to FSAdata as no longer needed because some examples were changed to use `WhitefishLC`.
* `Subset()`:  Modified. Added a `resetRownames=` argument.
* `swvCode()`: Modified. Added an `out.dir=` argument.
* `swvCounts()`: Modified. Added a `capitalize=` argument.
* `typeoflm()`: Modifed. Changed to use `hndlFormula()`. Made an internal function.
* `vbFuns()`: Modified. Corrected spelling of Gallucci for Gallucci and Quinn model.
* `vbStarts()`: Modified. Corrected spelling of Gallucci for Gallucci and Quinn model.
* `WhitefishLC`: Added (from FSAdata).
* `wsLit`: Modified. Changed all species names to have both words capitalized so as to follow the latest AFS guidelines.

# FSA 0.4.2
* **Date:** Dec13
* Changed to compiling under R 3.0.2.
* Removed dependency on reshape package (see changes for `emp()`, `gReshape()`, and `ssValidate()` below) and the relax, tcltk, and TeachingDemos packages (see changes for `catchCurveSim()`, `cohortSim()`, `growthModelSim()`, `leslieSim()`, `lwModelSim()`, `mrClosed1Sim()`, `simAgeBias()`, `simAges()`, `simLenFromAge()`, `simLenSelect()`, and `srSim()` below).

* .`onAttach()`:  Modified. Added notes to use `citation()`.
* `bcFuns()`: Modified. Added "BPH" and "SPH" options to `type= argument` (same as "LBPH" and "LSPH", respectively). Changed a catch using `cat()` to using `message()`. Added some specificity to the help file (more is needed).
* `catchCurveSim()`: Deleted. Moved to FSATeach package.
* `changesPos()`: Added.
* `cohortSim()`: Deleted. Moved to FSATeach package.
* `emp()`: Modified. Replaced use of `cast()` with `aggregate()`.
* `gReshape()`: Modified. Replaced use of `melt()` with `reshape()` from base package. Fixed bug if name of "increments" was not "inc" (now catches that `in.pre=` value is used). Fixed bug that `na.rm=` was ignored. Modified so that rownames are not created until after the NAs are moved or not. Changed the default name in `var.name=` from "age" to "prvAge" to reduce the highly possible chance that there might be another variable in the data frame named "age."
* `growthModelSim()`: Deleted. Moved to FSATeach package.
* `growthRadPlot()`:  Modified. Slightly changed the xlab= argument default.
* `leslieSim()`: Deleted. Moved to FSATeach package.
* `lwModelSim()`: Deleted. Moved to FSATeach package.
* `mrClosed1Sim()`: Deleted. Moved to FSATeach package.
* `simAgeBias()`: Deleted. Moved to FSATeach package.
* `simAges()`: Deleted. Moved to FSATeach package.
* `simLenFromAge()`: Deleted. Moved to FSATeach package.
* `simLenSelect()`: Deleted. Moved to FSATeach package.
* `srSim()`: Deleted. Moved to FSATeach package.
* `summary.ageComp()`: Modified. Added a `zero.print=` argument with a default of a single dash for use when printing an age-agreement table. Added `flip.table=` argument to allow ease of comparison between the age-agreement table and the age-bias plot. Changed so that if `what="prec.stats"` the summary percentages by absolute differences is also printed. Modified the print of several data frames (for `what="bias"`, `"symmetry"`, and `"prec.stats"`) so that row names (i.e., row numbers) are not printed.
* `sumTable()`: Added. Brought over from NCStats.
* `vbFuns()`: Modified. Changed all non-simple growth model functions with checks for the number of model parameters and definitions sent. Changed the Francis parameterization model to take only two values of `t=` (i.e., the intermediate value is not used and, thus, is not required); thus, the `t2=` argument was removed.
* `vbGen()`: Modified. Fixed bug that developed when changes to `gReshap()` were made. Added warning suppression related to "calculations" on NAs.
* `vbStarts()`:  Modified. Changed tFrancis argument to use only two ages. Changed the default for `meth.EV=` to "poly". Removed jittering and added a transparency to the plot. Removed the box around the legend and moved the legend to the "bottomright."  Fixed a typo in the plot heading.
* `wsValidate()`: Modified. Replaced use of `cast()` with `aggregate()`.

# FSA 0.4.1
* **Date:** Oct13
* Changed R dependency to >3.0.0 (because gplots package has that dependency).
* Added importFrom for `cast()`, `is.formula()`, and `melt()` in reshape package.

* `capHistConvert()`: Corrected the formatting of the documentation.
* `capHistSum()`: Corrected the documentation. Added a second example.
* `dietOverlap()`: Modified. Changed the "Morista" option to "Morisita" to be consistent with the correct spelling of the name.
* `Garvey1`: Added. Used in examples in `ks2d1()`.
* `Garvey4a`: Added. Used in examples in `ks2d1()`.
* `histStack()`: Deleted, moved to plotrix package. Arguments were changed there.
* `ks2d()`: Deleted, changed to `ks2d2()`.
* `ks2d1()`: Added.
* `ks2d2()`: Added, was `ks2d()`.
* `ks2dp()`: Deleted, changed to `ks2d2p()`.
* `ks2d2p()`: Added, was `ks2dp()`.
* `mrClosed()`: Modified. Changed all "messages" using `cat()` to using `message()` so that they can be suppressed with `suppressMessage()` or `message=FALSE` in knitr. See  "One comment on messages" at http://yihui.name/knitr/demo/output/.
* `pkolgomorov1x()`: Added to FSAinternals (from `ks2d()`).
* `plotH()`: Deleted, moved to plotrix package.
* `quad_dens()`: Added to FSAinternals (from `ks2d()`).

# FSA 0.4.0
* **Date:** Jun13
* Corrected all pointers to fishR vignettes (because of new webpage).
* Removed importFrom color.scale from plotrix because of changes to `discharge()` and `wetPerim()`.
* removed importFrom &#37;nin&#37; from Hmisc. See multiple changes because of this below.

* `.onAttach()`: Added, was `.onLoad()`.
* `.onLoad()`: Deleted, now `.onAttach()`.
* `addMargins()`: Deleted, moved back to NCStats.
* `addSigLetters()`: Deleted, moved back to NCStats.
* `addZeroCatch()`: Modified. Changed the looping structure for finding the sampling event and species combinations that need zeros. This should speed things up substantially. Also, modified to allow no `idvar=` variables. Finally, the returned data frame has the variables (columns) in the same order as the original data frame (rather than having the order modified).
* `ageComp()`: Modified some of the code to adjust for name changes in `Summarize()`.   Modified to use a formula notation.
* `ageKey()`: Modified to using a formula notation. This removed the `dl=`, `cl=`, and `ca=` arguments. Made minor adjustments to the help pages (in addition to changes related to the argument modifications).
* `bcFuns()`: Removed use of &#37;nin&#37;.
* `capFirst()`: Modified so that ONLY the first letter is capitalized (previous version would de-capitalize the first letter in the second word but leave the rest of the letters capitalized).
* `capHistSum()`: Modified to correct an error that occurred when computing the Method B table when a capture history occurred only once or not at all.
* `chapmanRobson()`: Modified by adding the Hoenig et al. (1983) bias correction formula for the estimate of Z as the default option.
* `confint.nlsBoot()`: Removed use of &#37;nin&#37;.
* `discharge()`: Deleted, moved to NCStats (to reduce overhead here).
* `histStack()`: Modified by adding a formula method (`histStack.formula()`) which required adding a default method (`histStack.default()`).
* `htest.nlsBoot()`: Removed use of &#37;nin&#37;.
* `lencat()`: Modified by changing to using a formula notation and a `data=` argument. This means that the `df=` and `cl=` arguments are no longer used. In addition, the warning about fish larger than the larger category has been turned off. The method to handle this was not changed, the warning was just turned off.
* `lencatOLD()`: Added as an internal file to temporarily allow me not to change all functions that were affected by the changes to `lencat()`. The functions that required this are `emp()` and `wsValidate()`.
* `lenFreqExpand()`: Modified to deal with `lencat()` change.
* `limnoProfilePlot()`: Deleted, moved to NCStats (to reduce overhead here).
* `mrClosed()`: Removed use of &#37;nin&#37;.
* `plotBinResp()`: Modified by moving `makeColor()` internal function to FSA-internals so that it can also be used by `tictactoe()`.
* `predict.bootCase()`: Added.
* `PSSLit`: added from RSDLit. Added from Ogle and Winfield (2009) for ruffe, Bonvechio et al. (2010) for Suwannee bass, and from Phelps and Willis (2013) for several "carp" species.
* `PSSLitCheck()`: Added this internal file. Modified `pssVal()`, `pssCalc()`, and `pssPlot()` accordingly.
* `psdVal()`: Deprecated, will delete, became `pssVal()`.
* `pssCalc()`: Added, was `rsdCalc()`. Modified to using a formula notation and a `data=` argument.
* `pssDataPrep()`: Added.
* `pssPlot()`: Added, was `rsdPlot()`. Modified to using a formula notation and a `data=` argument, to handle the default change for `incl.zero=` in `pssVal()`, and changed the default `pss.lty=` settings.
* `pssVal()`: Added, was `rsdVal()`. Changed `incl.zero=TRUE` to be the default.
* `recodeSpecies()`: Added.
* `rsdCalc()`: Deleted, became `pssCalc()`.
* `rsdLit()`: Deleted, became `PSSLit()`.
* `rsdPlot()`: Delted, became `pssPlot()`.
* `rsdVal()`: Deprecated, will delete, became `pssVal()`.
* `sigLetters()`: Deleted, `cld()` in multcomp has been modified to deprecate this.
* `simLenSelect()`: Modified to deal with `lencat()` change.
* `Summarize()`: Modified by calculating the percentage of zeros for quantitative data. Also changed the names in the returned vectors or data frames to reduce capitalization, spaces, and punctuation. Removed use of &#37;nin&#37;.
* `tictactoe()`: Modified by changing the way the "in balance" regions are depicted. This resulted in the addition of the `bal.trans=` argument.
* `tictactoeAdd()`: Modified by changing PSD labels to PSS.
* `vbStarts()`: Removed use of &#37;nin&#37;.
* `wetPerim()`: Deleted, moved to NCStats (to reduce overhead here).
* `wrAdd()`: Modified. Major modifications to account for changes to `WSlit`. Added the `capFirst()` check for species name. Changed `subNA=` to `remove.submin=` to make consistent with `wrDataPrep()`.
* `wrDataPrep()`: Added.
* `wrVal()`: Deleted.
* `WSlit`: Modified. Completely rebuilt so that quadratic equation using EmP could be incorporated into the database. Also added equations for several new species.
* `WSLitCheck()`: Added this internal file. Modified `wsVal()`, `wrVal()`, and `wrAdd()` accordingly.
* `wsVal()`: Modified. A major modification to account for the major changes to `WSLit`.
* `wsValidate()`: Removed use of &#37;nin&#37;.

# FSA 0.3.4
* **Date:** Jan13
* added special "fishR Vignette" sections with links to several help files.

* `binCI()`: Modified so that result is a matrix rather than sometimes (when only
one set of CIs were computed) being a vector.
* `catchCurve()`: Modified by minorly adjusting how `confint()` produced CIs. Also, disallowed using `parm=` when the user asks for CIs for the linear model. This allowed using `match.arg()` as a check for appropriate `parm=` values. Modified the examples in the help file slightly and added an example of using the weighted regression method.
* `plot.catchCurve()`: Modified so that log(catch) values less than 0 will be plotted.
* `chapmanRobson()`: Modified by minorly adjusting how `confint()` produced CIs.
* `depletion()`: Modified by minorly adjusting how `confint()` produced CIs and added a `cat()`ted output to the `summary()` method describing whether the Leslie or DeLury method was used.
* `growthModelSim()`: Modified. Streamlined the code (removed some "junk" and unneeded redundancies). Also corrected the error where the fourth parameter in the vbSchnute and Schnute were not observed to be connected to sliders. Also changed a few default slider values. Also set the minimum age (`t.min`) to 0 and cannot be over-ridden (was previously controlled by a slider). Thus, removed the minimum age slider. Also moved the maximum age slider to the bottom of the sliders. Changed the calls for the Gompertz models to use the full name (i.e., `Gompertz1` instead of `Gomp1`). Changed model= to type= to be more consistent with other similar functions.
* `hyperCI()`: Modified so that the result is a matrix rather than a vector.
* `leslieSim()`: Modified by adding `hscale=1.5` to resampling version.
* `mrClosed()`: Modified to handle the changes in `hyperCI()` and `binCI()`. Also modified messages in `summary()` and `confint()` (to streamline).
* `predict.nlsBoot()`: Added.
* `removal()`: Modified by minorly adjusting how `confint()` produced CIs and removed a `cat()`ted line from the summary() method. Also, modified the "catches" for the 2- and 3-pass specific methods to disallow using anything but a vector with either 2 or 3 samples.
* `srCobWeb()`: Added.
* `srSim()`: Modified. Streamlined the code (lots of "junk" code that did not do anything and some unneeded redundancies) were removed. Modified the default values and the axis labels so as to produce generally more interesting simulations. Modified the graphic to show the peak recruitment level and, if a Ricker model, the stock size where the peak recruitment occurs. Changed a long series of if-else for the different parametrizations to a `switch()`. Changed `model=` to `type=` to be consistent with other srXXX functions.
* `vbFuns()`: Modified slightly the messages if `msg=TRUE`. Added a message for the Wang2 model and corrected an error for the Somers2 model.
* `view()`: Modified to remove the ability to print to a window (use method built into RStudio instead). Also generalized to use for both a matrix or a data.frame (eliminates some warning messages).

# FSA 0.3.3
* **Date:** 21Dec12
* Added ImportFrom for `slider()` and `gslider()` from the relax package. Deleted the ImportFrom for `slider()` from the `TeachingDemos` package. These functions were the same but it was being deprecated from `TeachingDemos`.
* General: added `call.=FALSE` to several `stop()`s and `warning()`s.
* General: replaced `paste()` inside of several `cat()`s.

* `ageKey()`: Modified to use `match.arg()` with type=.
* `binCI()`: Modified to use `ciLabel()` (see below).
* `catchCurveSim()`: Modified in a variety of ways. First, moved the ability to  control the recruitment age and the steadiness of the Z and N* `changes to function arguments rather than slider controls. Second, streamlined the internal functions. Third, converted to using `gslider()` instead of `slider()`. Fourth, made minor cosmetic changes to the plot. Fifth, I edited the help file somewhat.
* `checkStartcatW()`: Added this internal function.
* `ciLabel()`: Added this internal function.
* `cohortSim()`: Modified in a variety of ways. First, streamlined the internal functions so that the plot can be created individually. Second, converted to using `gslider()` instead of `slider()`.
* `confint.bootCase()`: Modified to use `ciLabel()`.
* `confint.catchCurve()`: Modified to use `ciLabel()`.
* `confint.chapmanRobson()`: Modified to use `ciLabel()`.
* `confint.depletion()`: Modified to use `ciLabel()`.
* `confint.mrClosed()`: Modified to use `ciLabel()`.
* `confint.nlsBoot()`: Modified to use `ciLabel()`.
* `confint.removal()`: Modified to use `ciLabel()`.
* `dietOverlap()`: Added.
* `fsa.news(), FSA.news()`: Deleted, renamed to `fsaNews()` and `FSANews()`.
* `fsaNews(), FSANews()`: Renamed versions of `fsa.news()` and `FSA.news()`.
* `FSAsims()`: Deleted. Rarely used and not supported in non-windows and RStudio.
* `growthModelSim()`: Modified in a variety of ways. First, streamlined the internal functions so that the plot can be created individually. Second, converted to using `gslider()` instead of `slider()`.
* `hyperCI()`: Modified to use `ciLabel()`.
* `lencat()`: Modified by using the new `checkStartcatW()` internal function.
* `lenFreqExpand()`: Modified by adding show.summary= argument and using the new `checkStartcatW()` internal function.
* `leslieSim()`: Modified in a variety of ways. First, combined the code from  `leslieSim2()` into this function. This required deleting the use.rand= argument and adding a `type=` argument. In addition, the `leslieRandRun()` internal function was moved to this R document (from FSA-internals). Second, the functions were all streamlined with new internal functions. Third, converted to using `gslider()` instead of `slider()`. Fourth, made minor cosmetic changes to each plot (including adding a small legend to the old `leslieSim2())`.
* `leslieSim2()`: Deleted. See `leslieSim()`.
* `lwModelSim()`: Modified in a variety of ways. First, streamlined the internal functions so that the plot can be created individually (will ultimately allow use of the manipulate package). Second, converted to using `gslider()` instead of `slider()`.
* `mrClosed()`: Modified by removing `numdigs=` argument.
* `mrClosed1Sim()`: Modified in a variety of ways. First, streamlined the internal functions so that the plot can be created individually. Second, converted to using `gslider()` instead of `slider()`.
* `poiCI()`: Modified to use `ciLabel()`.
* `rlp()`: Modified by replacing `decimals=` argument with digits= argument.
* `srSim()`: Modified in a variety of ways. First, streamlined the internal functions so that the plot can be created individually. Second, converted to using `gslider()` instead of `slider()`. Third, removed the S3methods.
* `Summarize()`: Modified by removing `numdigs` argument.
* `TroutDietSL`: Added for use with `dietOverlap()`.
* `vbStarts()`: Modified by including a catch for negative starting values of K or starting values of Linf that are 50% smaller or larger than the observed maximum length in the data set.

# FSA 0.3.2
* **Date:** 1Dec12
* Changed R dependency to >2.14.0.
* Added a ImportsFrom for knitr (purl() in swvCode() added below).
* Moved gdata to an ImportsFrom from Suggests. Needed for nobs() in ci.fp1() which is used in fitPlot.ONEWAY and drop.levels() used in the example in RuffeWs.
* Deleted dependency on FSAdata.
* Added the following data files from FSAdata: BluegillJL, BluegillLM, BrookTroutTH, CodNorwegian, CutthroatAL, Ecoli, KS2D_NR, LMBassWs, Mirex, PikeNY, PikeNYPartial1, RSDlit, RuffeWs, SMBassLS, SMBassWB, SpotVA1, StripedBass1, VBGMlit, WalleyeGerowLW, WR79, WSlit. This allowed removing the depending on FSAdata.

* `.onLoad()`: modified slightly with a suggestion from Simon Urbanek to eliminate a warning on RCMD Check (that showed up on rforge.net, but not locally).
* `addMargins()`: added from NCStats.
* `addSigLetters()`: added from NCStats. Modified to allow the use of a result from sigLetters() in lets=.
* `bootCase methods: added from NCStats. Needed to import bootCase from car.
* `hist.formula()`: added from NCStats.
* `lencat()`: made some warning messages more concise.
* `lsmean()`: deleted. Functionality is replaced by lsmeans() in the lsmeans package.
* `psdVal(), rsdCalc(), rsdVal(), rsdPlot()`: added code to eliminate "global bindings" note when performing RCMD Check. Solutions came from Adrian Alexa's response to this question: https://groups.google.com/forum/?fromgroups=#!topic/cambridge-r-user-group/c7vf8o3QwDo
* `sigLetters()`: added. Hopefully this will eventually be replaced by changed to cld() in the multcomp package.
* `Summarize()`: made some warning messages more concise.
* `swvCounts(), swvPvalue(), swvANOVA(), swvGLHT(), swvREG(), swvHtest(), swvCode(), swvFinish()`: added from miscOgle.
* `view()`: added from NCStats.
* `wsVal(), wrAdd()`: added code to eliminate "global bindings" note when performing RCMD Check. Solutions came from Adrian Alexa's response to this question: https://groups.google.com/forum/?fromgroups=#!topic/cambridge-r-user-group/c7vf8o3QwDo

# FSA 0.3.1
* **Date:** 25Nov12
* Switched to using the Project mode in RStudio.
* Switched to using semantic versioning for the version number (which means that the hyphen before the last number has been replaced by a period).
* Switched to using roxygen to construct help files.
* Set some values =NULL to eliminate "global bindings" warning when performing the RCMD check -- emp(), pos2adj(), psdVal(), simAgeBias(), srStarts(),  vbStarts(), and wsValidate(). This did not work for the WSlit and RSDlit problems in rsdCalc(), rsdPlot(), rsdVal() and wsVal().
* Added an importFrom for lineplot.CI()) and se() from sciplot (used in fitPlot()).
* Added an importFrom for outlierTest() from car for use in residPlot().
* Deleted importFrom for alias() from stats (was used in wlgm()).
* Deleted importFrom for boxcox() from MASS (was used in wlgm()).
* Deleted depends on NCStats (moved many functions here (see below) and then  made NCStats depend on FSA).
* Deleted suggests for exactRankTests (apparently no longer needed).
* Moved nlstools from depends to suggests (only needed for an example in  confint.nlsboot that is not run because of the time required).
* Moved plotrix from depends to importsFrom for color.scale(), plotCI(), and thigmophobe().
* Moved quantreg from depends to importsFrom for rq() (used in emp()).
* Attempted to move reshape to importsFrom but had problems with missing is.formula() from plyr.

* `ageComp()`: modified class name to "ageComp" from "AgeComp".
* `capFirst()`: added.
* `catchCurve()`: modified class name to "catchCurve" from "CC".
* `chapmanRobson()`: modified class name to "chapmanRobson" from "CR".
* `coefPlot()`: deleted (Weisberg's LGM is now out-dated).
* `depletion()`: modifed class name to "depletion" from "Depletion".
* `discharge()`: modified class name to "discharge" from "StrmDschrg".
* `emp()`: modified class names to "empXX" from "EMPxx".
* `fitPlot()`: added from NCStats.
* `FroeseWs()`: modified class name to "FroeseWs" from "FROESE".
* `histStack()`: added.
* `hoCoef()`: added from NCStats.
* `ks2d()`: modified class name to "ks2d" from "ks2d".
* `ks2dp()`: modified class name to "ks2dp" from "ks2dp".
* `legendHelp()`: added (internal) from NCStats.
* `mrClosed()`: modified by moving the two internal functions -- mrc1() and mrc2() -- to inside mrClosed, moving the two internal functions -- ci.mrc1() and ci.mrc2() -- to inside confint.mrClosed(), removed the "MRC1" and "MRC2" classes, changed the "MRC" class name to "mrClosed", and added a catch to plot.mrClosed() to stop if the user tries to plot with single-census data.
* `mrOpen()`: modified class name to "mrOpen" from "MRO".
* `plotBinResp()`: added from NCStats.
* `plotH()`: minor modifications to the Rd file.
* `pos2adj()`: modified the labels for the positions by including full names for  all directions, eliminating the single letters for the four main directions, but also leaving the four "off" directions as abbreviations.
* `psdVal(), rsdVal(), rsdCalc(), rsdPlot()`: modified to use capFirst so that the user does not need to focus on capitalization of the species name.
* `removal()`: modified class name to "removal" from "Removal".
* `residPlot()`: added from NCStats.
* `rlp()`: modified class name to "rlp" from "RLP".
* `Summarize()`: added from NCStats.
* `typeoflm()`: added from NCStats.
* `wetPerim()`: modified class name to "wetPerim" from "WetPerim".
* `wlgm()`: deleted (Weisberg's LGM is now out-dated).
* `wsValidate()`: modified the classnames to "willis" from "WILLIS" and "empq" from "EMPQ". Also made minor modification because of class name change in FroeseWs()
* `ycPlot()`: deleted (Weisberg's LGM is now out-dated).

# FSA 0.3-0
* **Date:** 8-Nov-12
* Moved several functions from NCStats that are used quite often for fisheries analyses. Ultimately, I want to remove the dependency to NCStats.
* Deleted an importFrom for gtools, created an internal odd() instead.
* Added an importFrom for gplots, to get rich.colors() for chooseColors().
* Added an importFrom and removed an import for NCStats.

* `ageComp()`: modified to use internal odd(), rather than odd() imported from gtools.
* `binCI()`: moved from NCStats.
* `chooseColors()`: copied from NCStats (i.e., same function still in NCStats).
* `confint.nlsBoot()`: moved from NCStats.
* `fact2num()`: moved from NCStats.
* `htest()`: copied from NCStats (i.e., same function still in NCStats).
* `htest.nlsBOot()`: moved from NCStats.
* `hyperCI()`: moved from NCStats.
* `ks2d()`: moved from NCStats.
* `ks2dp()`: moved from NCStats.
* `ksTest()`: moved from NCStats.
* `lagratio()`: moved from NCStats.
* `lsmean(), and related internals: moved from NCStats.
* `mrClosed()`: modified so as not to depend on ci.t() from NCStats.
* `plotH()`: moved from NCStats.
* `poiCI()`: moved from NCStats.
* `popSizesPlot()`: moved from NCStats.
* `pos2adj()`: moved from NCStats.
* `rcumsum()`: moved from NCStats.
* `rsdPlot()`: modified to handle situations where substock fish are not present in the data. Thanks to Max Wolter for pointing out this issue.
* `Subset()`: copied from NCStats (i.e., same function still in NCStats).

# FSA 0.2-8
* **Date:** 21Jun12
* Switched to compiling under R version 2.14.1 (64-bit).
* Changed license specification from "GPL version 2 or newer" to "GPL (>= 2)" to avoid warning on check.
* Added a suggestion for gdata to fix warning with capHistConver.rd (see below).

* `capHistConvert.rd: Modified the examples to not use "gdata::combine" by adding a "require(gdata)" in the examples and suggesting gdata in the description file.
* `fishR()`: Added.
* `simAgeBias()`: changed width to widths in layout() to avoid warning on check.
* `simLenSelectM()`: changed width to widths in layout() to avoid warning on check.

# FSA 0.2-7
* **Date:** 2Mar12
* `.onLoad()`: Modified. Moved the startup message into packageStartupMessage() in hopes of eliminating the warning when checking the package.
* `catchCurveSim()`: Modified. Changed max.age= to 15 (from 10). Slightly changed the labels related to 'Z Steady' and 'N* `Steady'.
* `chapmanRobson()`: Modified. Corrected a bug for when the ages2use= argument contains ages that are not found in the data. Thanks to Eric Berglund for finding this bug.
* `psdVal()`: Modified. As described for rsdVal().
* `rsdCalc()`: Added.
* `rsdPlot()`: Modified. Modified to reflect changes in rsdVal().
* `rsdVal()`: Modified. Removed the metric= and mm= arguments in favor of a new argument, units=, where the user chooses the units as a string. This streamlines, for example, the selection of mm. The modifications also resulted in mm being the default. Also, removed the appended units names from the names attribute -- i.e., "stock" rather than "stock.mm" or "stock.in".
* `wrAdd()`: Added.
* `wrVal()`: Modified. As described for wsVal().
* `wsVal()`: Modified. Removed the justcoef= argument. Added the ab= and comment= arguments. Also, removed the appended units names from the names attribute -- i.e., "int" rather than "int.E" or "int.mm".

# FSA 0.2-6
* **Date:** 1Oct11
* Switched to compiling under R version 2.13.1 (32-bit).
* Removed importFroms that were required for updateFSA().
* Removed splines package from imports list (not needed).

* `capHistConvert()`: Modified. Modifications to handle changes to capHistSum().
* `capHistSum()`: Modified. Changed the returned list structure. First, caphist.sum is now caphist. Second, if only two samples are given, then only caphist and sum, where sum is a data frame of the required summaries for the Petersen method, are returned. If more than two samples are given, then caphist, sum,  methodB.top, and methodB.bot are returned. Note that there is n* `longer an item labeled as schnabel.sum returned.
* `mrClosed()`:  Modified. Modifications to handle the changes to capHistSum(). Also modified so that if only two samples were summarized in a CapHist object and that object is supplied as the first argument to mrClosed() then the Petersen method will find the data it needs from the CapHist object.
* `rsdPlot()`: Modified. Modified calls to min() and max() to include na.rm=TRUE. This fixes bug related to vectors with missing values.
* `updateFSA()`: Removed.
* `vbFuns()`: Modified. Added 'Somers2' option to type= argument.
* `vbStarts()`: Modified. Added 'Somers2' option to type= argument.

# FSA 0.2-5
* **Date:** 19Aug11
* Modified description file to show my e-mail address.
* Added `cnvrt.coords()` as an ImportFrom TeachingDemos. Needed for `simAgeBias()` and `simLenSelectM()`.

* `ageKey()`: Modified. Length categories in the length sample, if none are provided in len.breaks=, are constructed from the length categories present in the age-length key rather than guessing at a starting value and width and creating evenly spaced categories. This should fix the bug that occurred when an age-length key was originally created with even length categories but the key is so sparse that the length categories with actual data are uneven. Also, changed the error catching so that the routine is stopped if a length in the length sample is smaller than the smallest length category in the age length key but will only elicit a warning if the largest length is greater than the largest length category in the age-length key.
* `chapmanRobson()`: Modified. Changed to have a .default and .formula method.
* `chapmanRobson.default()`: Added.
* `chapmanRobson.formula()`: Added.
* `FSAsims()`: Modified. Corrected calls to growthModelSim() for von Bertalanffy models.
* `growthModelSim()`: Modified. Changed from modeling "size" to modeling "length" (or "weight" for just "vbTypicalW" and "vbOriginalW"). Changes required adding two new model options -- "vbTypicalW" and "vbOriginalW" -- for modeling weights and leaving all of the original model options as models for length. Added a max.wt= argument for use when modeling weights. Removed "vbBevertonHolt" as a model option because it is covered by "vbTypical" and was not actually implemented. Changed order of models so that "vbTypical" rather than "vbOriginal" is the default model used. Made slight cosmetic changes to slider bar options (e.g., "to" became "t_0"). Made changes and some corrections to the .Rd file.
* `rsdPlot()`: Added. Still needs more thorough proofing.
* `simAgeBias()`: Added.
* `simAges()`: Added.
* `simApplyAgeBias()`: Added.
* `simLenFromAge()`: Added.
* `simLenSelectM()`: Added.
* `simLenSelectP()`: Added.
* `vbComp()`: Modified. Streamlined the code. Changed the t= argument to ages= to remove any possible confusion with t(). Removed the option to model over ages provided in the (previous) t= argument. Instead the ages= argument can be used to represent the maximum age to model to. The ages= argument can be a vector such that each simulation can have a different set of ages over which the model is evaluated. This allows for more realistinc modeling.

# FSA 0.2-4
* **Date:** 15Jun11
* Switched to compiling under R version 2.13.0.

* `vbFuns()`: Modified. Modified Wang's formulas to be for length increments. Added a length increments version to Faben's method ("Fabens2").

# FSA 0.2-3
* **Date:** 18Apr11
* Updated citation file.
* Added importFrom for tools and utils packages.

* `ageKey()`: Modified. Added a len.breaks= argument so that an age-length key with variable widths for the length categories can be used. Added an example to the Rd file to illustrate the use.
* `confint.MRC()`: Modified. Replaced numdigs= argument with digits= argument. Retained numdigs= for backwards compatability.
* `lwPredsComp.Rd: Modified. Replaced use of lgrep() with grepl() because of  change in NCStats.
* `removal()`:  Modified. Changed order of items printed in the returned list.  In addition, if the type is one of Zippin, CarleStrub, or Seber3 then a set of intermediate values (k, T, and X) is also included in the returned list. The first change is cosmetic, the second change was made to help with some troubleshooting. Added an argument to allow choosing the method of contructing SE for the CarleStrub method. Created an internal function for computing the Zippin SE method to allow easier use with the other methods. The help file was changed to make note of the non-estimable SE when No=T in the CarleStrub method under certain circumstances. These changes result in a different SE being reported if the CarleStrub method is used and CS.se="Zippin" (the default) is used. The "old" results can be obtained by using  CS.se="Alternative". I have yet to find a solid references for this SE.
* `summary.MRC()`: Modified. Replaced numdigs= argument with digits= argument. Retained numdigs= for backwards compatability.
* `tictactoeAdd()`: Modified. Added capability of labeling points.
* `updateFSA()`: Added. Had to add an importFrom from the tools package.
* `vbFuns()`: Modified. Added Wang and Wang2 functions.

# FSA 0.2-2
* **Date:** 3Mar11
* moved to compling under 2.12.1 (32-bit)
* changed dependency to >2.11.1

* `ageComp()`: modified dramatically. Primarily added the ability to test for bias by comparing the mean of the y-structure to the value of the x-structure with t-tests adjusted for multiple comparisons. Modified the code to allow this to happen more efficiently and to output results in the plot() and summary() methods. Also modified the plot() method so that the default is  to just show the confidence intervals rather than showing the CIs and the range of the data (use show.rng=TRUE to see the old plot). Also changed the CI colors so that significant differences are shown in red (default) and non-significant differences are shown in blue (default) (set both col.err= and col.err.sig= to the same color to get the old plot).
* `lencat()`: modified so that vname=NULL is the default. This will default to  using "LCat" as the variable name (as in the previous version). However, modified the way the vname is appended to the new data frame so that if vname already exists in the data frame a new name will be used (vname plus some number).
* `removal()`: added just.ests= argument and changed the ests part of the returned value to be a vector rather than a matrix. Both changes allowed for better use of lapply() for computing the removal estimates on more than one group. Changed from an error to a warning for situations where the method could not compute population estimates (i.e., because the population was not depleted). In addition, NAs are returned in situations where population estimates can not be made. An example of computing the removal estimate for more than one group was added to the .rd file. Thanks to Jon Bolland for asking the question that motivated these changes.

# FSA 0.2-1
* **Date:** 31-Jan-11
* `catchCurve()`: Modified by adding a formula method. This required moving the original code into a default method and changing the age= argument to x=.
* `lenFreqExpand()`: Modified by adding the additional= argument (which required modifying the total= argument and adding an error check for the situation where the total fish to assign lengths is not greater than the number of  fish in the measured subsample).
* `.onLoad()`: modified. Changed to include version number of loaded version.
* `vbFuns()`: Modified by adding simple= argument. Added a 'Somers' seasonal growth oscillations model and 'Fabens' model for tag-recapture data. Also added, but did not check, a 'Laslett' 'double von Bertalanffy' model.
* `vbStarts()`: Modified by setting a catch to return a single root for st0 or sL0 if the polynomial root found a double root. Thanks to Giacom* `Tavecchia for identifying this error. Added a 'Somers' seasonal growth oscillations model.  

# FSA 0.2-0
* **Date:** 23-Sep-10
* `bcFuns()`: Added. Still needs to be thoroughly proofed.
* `FSAsims()`: Modified to reflect srSim() change described below.
* `listSpecies()`: Moved internal function out of being within RSDval() and WSval() and then added an argument for the data frame containing the species names. The hope was that this would correct the "n* `visible binding" warnings when performing RCMD check but it did not.
* `srModels()`: Renamed from stockRecruitModels() to be more consistent with the rest of the stock-recruitment functions.
* `srSim()`: Renamed from stockRecruitSim() to be more consistent with the rest of the stock-recruitment functions.
* `vbDataGen()`: Modified use of minAge argument -- will now always back-calculate to age-1 but minAge denotes the minimum age-at-capture that will be modeled. Deleted use of cfAge variable in code.
* `vbModels()`: Added.

# FSA 0.1-6
* **Date:** 23-Aug-10
* completed changing naming convention to "camel" type -- e.g., `stockRecruitModels()` rather than `stock.recruit.models()`. 
* `ageComp()`: renamed from age.comp().
* `ageKey()`: renamed from age.key().
* `capHistConvert()`: renamed from caphist.convert().
* `capHistSum()`: renames from caphist.sum().
* `catchCurve()`: renamed from catch.curve().
* `catchCurveSim()`: renamed from cc.sim().
* `chapmanRobson()`: renamed from chapman.robson().
* `coefPlot()`: renamed from coefplot().
* `cohortSim()`: renamed from cohort.sim().
* `emp()`: modified for name changes in NCStats.
* `FroeseWs()`: modified for name changes in NCStats.
* `FSASims()`: modified by updating to new names of simulation functions.
* `gConvert()`: renamed from g.convert().
* `gReshape()`: renamed from g.reshape().
* `growthRadPlot()`: renamed from growrad.plot().
* `lenFreqExpand()`: renamed from len.freq.expand().
* `leslieRandRun()`: renamed from leslie.rand.run(). This is an internal function.
* `leslieSim()`: renamed from leslie.sim().
* `leslieSim2()`: renamed from leslie.sim2().
* `limnoProfilePlot()`: renamed from limnoprofile.plot().
* `lwModelSim()`: renamed from lwmodel.sim().
* `lwPredsComp()`: renamed from comp.lwpreds().
* `mrClosed()`: renamed from mr.closed(). Modified for name changes in NCStats.
* `mrClosed1Sim()`: renamed from mr.closed1.sim().
* `mrOpen()`: renamed from mr.open().
* `psdVal()`: renamed from PSDval().
* `rcumsum()`: deleted. Moved to NCStats package.
* `rpl()`: modified for name changes in NCStats.
* `rsdVal()`: renamed from RSDval().
* `tictactoeAdd()`: renamed from tictactoe.add(). Modified for name changes in NCStats.
* `vbComp()`: renamed from vb.comp().
* `wetPerim()`: renamed from wetperim().
* `wlgm()`: modified for name changes in NCStats.
* `wrVal()`: renamed from WRval().
* `wsVal()`: renamed from WSval().
* `wsValidate()`: renamed from validateWs(). Also modified for name changes in NCStats.
* `ycPlot()`: renamed from ycplot().

# FSA 0.1-5
* **Date:** 20Aug10
* moved to compiling under 2.11.1.
* started changing my naming convention to "camel" type -- e.g., `stockRecruitModels()` rather than `stock.recruit.models()`. In this version, I am only changing the functions that I am working on. I will change the rest in the next version.
* added an importFrom for `nlme` as `groupedData()` was needed for `vbDataGen()`.

* `age.key()`: Modified the way that the length categories in the age-length key is determined. Previously I just used the rownames found in the key, but this allows lengths with a row of all NA or zeros to be considered as a length found in the age length key. Now the row sums are found and the sums with NaN or 0 are removed. In addition, I added a warning message if the row sums d* `not sum to 1.
* `caphist.convert()`: Modified such that an "RMark" type can be output.
0 chapmanPlot()`: Added.
* `growmodel.sim()`: Deleted. Changed to growthModelSim(). See below.
0 growthModelSim()`: Added. Initially a renaming of growmodel.sim(). However, the model names were changed to be more consistent with other functions and a method for the Mooij et al. paramaterization was added.
* `growthModels()`: Added.
* `srFuns()`: Added.
* `srStarts()`: Added.
* `stock.recruit()`: Deleted, along with all related generics.
* `stock.recruit.sim()`: Deleted. Changed to stockRecruitSim(). See below.
* `stockRecruitModels()`: Added.
* `stockRecruitSim()`:  Initially a renaming of stock.recruit.sim. However, added a "formula" method which required adding generic and default methods. Changed the order of the S and R arguments. Re-ordered, modified, and added models in accordance with the vignette. Updated the Rd file to reflect these changes and made a very slight modification to the examples and added an example to illustrate the use of the formula. Found decent default values for simulations.
* `stockRecruitSim.default()`: Added. See above.
* `stockRecruitSim.formula()`: Added. See above.
* `vbDataGen()`: Added.
* `vbFuns()`: Added.
* `vbStarts()`: Added.
* `walfordPlot()`: Added.

# FSA 0.1-4
* **Date:** 6Jun10
* `growmodel.sim()`: added an option to fit the "original" von Bertalanffy function. Also added more "mis-spelling" options to the other model names.  

# FSA 0.1-2
* **Date:** 17Dec09
* moved to compiling under 2.10.1.
* `added a dependency to tcltk so that simulators would work properly upon load of FSA.

* `age.comp()`: added xlim= and ylim= arguments so user can control x- and y-axis limits if desired. Changed code so that better choices for axis limits are selected automatically if xlim and ylim are both NULL. Changed code so that the "extra" vertical space added when show.n=TRUE AND ylim is NLL is 10 percent of the y-axis range rather than just an extra one unit. Allowed function to work better with xaxt="n" and yaxt="n" in case the user wants to create their own axes. Removed a par() setting within the plotting function. Thanks to David A. Hewitt for pointing out the deficiences with the axis labeling.
* `age.key()`: corrected how the age column is labeled if the column did not already exist in the data frame. Was also indirectly modified with lencat() modification. Also modified to stop and warn the user if the length sample has fish whose lengths are not present in the length-age key (previously there was a warning, but then ultimately there was an error).
* `catch.curve()`: added a use.weights= argument to allow using weights in the catch curve regression as proposed by Maceina and Bettoli (1998).
* `chapman.robson()`: changed S result from a proportion to a percentage (i.e., * `100).
* `comp.lwpreds()`: added center.value= argument to allow centering in the regressions. Added an example to the .rd file.
* `fsa.news()`: added to show user the NEWS file.
* `FSA-package()`: updated.
* `growmodel.sim()`: added the ability to use a formula and data= argument. Made the model argument not have a default value. Corrected an error when both x and y were NULL. Corrected errors in the Rd file. Thanks to Jacek Szlakowski for pointing out these problems.
* `lencat()`: modified so that an "extra" last length category (with no fish in it) was not included when as.fact=TRUE and drop.levels=FALSE is used. This should correct the "problem" of an extra all-NA row in the age-length keys.
* `tictactoe.add()`: added to the namespace export list. Changed order of items listed in the ci.type= argument to match that of bin.ci() from NCStats.

# FSA 0.1-1
* **Date:** 15Apr09
* added a namespace
* removed dependencies and changed to imports ... left plotrix and quantreg as dependencies (they do not have a namespaces). left reshape as a dependency because of it's dependency on plyr. 
* `.FirstLib()`: removed (changed to .onLoad() because of namespace).
* `age.comp()`: modified by removing reference to "valid.n" (which is no longer used because of changes to Summarize() in NCStats). Modified to only attempt to compute SE if n>1 and st. dev > 0.
* `comp.lwpreds()`: added. Exported in namespace.
* `g.convert()`: fixed major error in how the function converted increments to radii.
* `growrad.plot()`: added. Exported in namespace.
* `mr.closed()`: modified by changing library(Rcapture) to require(Rcapture) in help page.
* `plot.EMPQ()`: modified by changing object$prob to x$prob.
* `emp.rd: fixed an incorrect use of Summary() (changed to summary()).
* `validateWs()`: converted sign.slope variable in the Willis method to a factor to deal with situations where all results were positive or negative.
* `wlgm.rd: fixed the summarization example (cast() did not work with Summarize().

# FSA 0.0-14
* **Date:** 20Dec08
* `age.comp()`: streamlined code (put bias and difference plots in same function, used grconvertY for show.n, used plotCI for range intervals, caught and corrected CI problems when n=1 or SD=0). N* `functionality difference, just improved code.
* `growmodel.sim()`: modified by determining some of the slider values from the data when x= and y= arguments are not null. This makes the graph more useful for determining starting values in nls() modeling.

# FSA 0.0-13
* **Date:** 6Dec08
* added a dependency to quantreg (for `rq()` in `emp()`).
* added CITATION file.

* `age.comp()`: modified the plot() function by adding a 'difference' method to the what= argument. This allows creation of an "age-difference" plot as used in Muir et al. (2008).
* `caphist.convert()`: modified by adding an event.ord= argument to allow the user to identify the order of the event names when converting from a capture-by-event type. This is particulary useful if the event names are things like 'first', 'second', 'third', 'fourth' because R orders these alphabetically which adversely effects the correctness of the capture histories.
* `compute.Ws()`: moved this internal function out of validateWs() to be a stand-alone internal function. This allows usage with animation routines.
* `discharge(), summary.StrmDschrg(), plot.StrmDschrg()`: added.
* `emp()`: added probs= argument result to return list. Corrected ylab in plotting methods. Added a predict method. Added a method= argument that allows choice of using linear regression or quantile regression to find the Ws equation. Modified objects in the return list (added rawdata component) and added the back-transformed Wq value in regdata (for comparison with Gerow's Excel tool). Changed code for finding summarized dataframes inside the function by using cast() from the reshape package -- this resulted in a 3x reduction in system.time().
* `mr.closed()`: modified by correcting error in the multiple census methods if M, n, and m (but not R) were supplied. Also corrected an error in the examples.
* `rlp()`: added probs= argument result to return list. Corrected ylab in plotting methods. Added a predict method.
* `PSDval(),RSDval()`: added a check for missing species name so that the user can just type PSDval() to get the list of possible species names. Also added a check to see if RSDlit was already loaded.
* `validateWs()`: added probs= argument result to return list. Corrected ylab in plotting methods. Added a predict method. Modified EmpQ() internal function to use predict() methods for emp and rlp objects. Streamlined some of the code by including a compute.Ws() internal function and using the update() function. Changed code for finding summarized dataframes inside the function by using cast() from the reshape package -- this resulted in a 1.5x reduction in system.time().
* `wetperim(),summary.WetPerim(),plot.WetPerim()`: added.
* `wlgm()`: major changes included moving some internal functions outside of wlgm(), adding the ability to use the data= argument, and adding the ability to fit weighted regressions on the summary statistics. Other  minor changes were also made. Updated the .Rd file.
* `WSval(),WRval()`: added a check for missing species name so that the user can just type WSval() to get the list of possible species names. Also added a check to see if WSlit was already loaded.

# FSA 0.0-12
* **Date:** 15Jul08
* `.First.lib`: Added
* `add.zerocatch()`: added this function to add zeros to catch records where a species of fish was not caught.
* `limnoprofile.plot()`: added this function to simplify constructing plots of depth versus limnological measure with the depth decreasing from top to bottom.
* `rlp()`: changed default qtype= to 8 (from 7). Added a probs= argument to allow other than 75th percentile calculations.
* `emp()`: updated the help page. Added a logical for if p.n.low does not exist when using cutoff.tail. Renamed items in the output list. Added a table of number of individuals per length category to output list. Added a probs= argument to allow other than 75th percentile calculations. Added its own generics -- rather than relying on the rlp() generics.
* `FroeseWs()`: added this function, and its generics, to perform the standard weight equation calculation as proposed by Froese (2006).
* `validateWs()`: added this function, and its generics, to perform the Willis and EmpQ methods for assessing length bias in the standard weight equations. Added a probs= argument to allow other than 75th percentile calculations. Added a mean= argument to allow use of means rather than quantiles. Modified to accept an object of class FROESE.

# FSA 0.0-11
* **Date:** 15May08
* Moved to RForge.net.
* changed to R2.7.0.
* added a dependency to `Rcapture` (for the example in `caphist.convert()`).

* `anova.RLP()`: added this function to produce the anova table for the standard weight equation.
* `caphist.convert()`: added this function convert between various capture history formats (FSA,event,MARK,Rcapture).
* `emp()`: added this function, and its generics, to perform Gerow's EmP method for obtaining a standard weight equation.
* `fit.plot.RLP()`: added this function.
* `plot.RLP()`: modified so that color palette with a gradient rather than only a solid color can be used for the populations. In addition, added order.pop= argument that will order the populations from smallest to largest predicted with in the first length interval. When used with the color gradients this will  make it easier to see which populations cross over other populations.           
* `rlp()`: modified function so that the user can choose to use any-mm length intervals rather than having 10-mm hardwired. Modified output in list somewhat to more closely match the output of emp().

# FSA 0.0-10
* **Date:** 1May08
* `lencat()`: Modified by adding an as.fact= argument that allows the user to decide if the resulting variable should be returned as a factor variable or not. The default is set to return as a factor variable. This allows tables of the new variable to include zeros for levels of the new variable that contain no individuals. This makes some RSD/PSD (and likely age-length key) calculations simpler. Also added a drop.levels= argument to allow the user to drop unused levels if so desired.
* `mr.closed()`:  This function is a combination of the old mr.closed1() and mr.closed2(). It also allows the user to compute single census estimates with multiple sub-groups in the data (i.e., length- or age-classes). The function also allows the user to compute an overall population esitmate of multiple sub-groups are present and an overall SE if the incl.SE=TRUE is used. It also corrects the SE computations implemented in version 0.0-9. This change caused the construction of our internal functions -- mrc1, mrc2, ci.mrc1, and ci.mrc2.
* `mr.closed1()`: removed this function. Use mr.closed() instead.
* `mr.closed2()`: removed this function. Use mr.closed() instead.
* `PSDval()`: Added mm= argument so that metric result can be returned in mm. Also added incl.zero= argument that will include a zer* `value in the first position in the vector; this is useful for when creating PSD/RSD values.
* `rcumsum()`: Added this function (from NCStats).
* `RSDval()`: See PSDval description.

# FSA 0.0-9
* **Date:** unknown
* `age.comp()`: Corrected SE calculation used to construct the CIs. Changed the CI plotting routine to use plotCI in plotrix package -- this puts lines rather than points on the ends of the CIs. Added a check for computing SDs and CIs for when n=1 or when all measurements are the same. This reduces (eliminates?) the number of warnings that are given.
* `catch.curve()`: added na.rm=TRUE arguments to min() and max() in plot.CC(). Changed type= argument so that "params" is the default rather than "lm". This makes it more consistent with other simulation programs.
* `cc.sim()`: Put in catch for situations where the CV for No and Z were equal to zero. Originally, the program attempted to computed a random number from a normal distribution with a standard deviation of zero. This corrected the problem of n* `lines appearing unless the CVs were greater than zero.
* `ch.convert()`: STARTED A FUNCTION to CONVERT B/W CAPTURE HISTORY FORMATS. N* `RD FILE YET.
* `depletion()`:  Moved type= argument to third position. Will more easily allow type="Leslie" as a default (i.e., can just enter catch and effort vector.
* `FSAsims()`: Added a "mark-recap" menu section. Added a chapman 1-sample M-R item to the menu.
* `leslie.sim()`: corrected the conditionals on p.surv and r.prop so that it asks if any not the first value is less than 1. This corrects the problem of R returning a large number of warnings.
* `leslie.sim2()`: corrected the call to depletion() so that the type of model ("Leslie") was the third rather than the first argument. This was caused by a change in the usage of depletion in previous version changes.
* `mr.closed1()`: Modified output list to include an estimate of the variance as described in Ricker(1975).
* `summary.MRC1()`: Modified output so that (1) the given information is a little easier to read, (2) the population estimate is returned in a matrix, (3) the SE from Ricker(1975) can be included in the outputm, and (4) a label can be placed on row for the matrix output. The purpose of these changes was to allow the SE to be computed and to allow future functions to more flexibly use the output.

# FSA 0.0-8
* **Date:** unknown
* changed some \items to \tabular in RD files. Changed most hard-wired quotes to \sQuote or \dQuote in RD files. Changed some text-based equations to more latex-based equations in \eqn or \deqn markups. This fixed the Latex compilation problems that I was having when using RCMD check.

* `age.comp()`: Removed single-letter values from the what= argument. Will rely on partial matching.
* `age.key()`: Changed default name for new column with ages from "Age" to "age". Added example. 
* `coefplot.WLGM()`: Changed to use plotCI() from the plotrix package. This removed the for loop that I had programmed. This also added the sfrac= and gap= arguments. Updated the RD.
* `depletion()`: Removed single-letter and lower-case values from the type= argument. Will rely on partial matching.
* `lencat()`: Changed d argument to df.
* `lenfreq.expand()`: Changed d argument to df.
* `mr.open()`: Added match.arg functionality to the ci.type and phi.type arguments.
* `plot.RLP()`: Added "object <- x" to allow x to be used inside the curve() function without confusion.
* `removal()`: Removed abbreviated values from the type= argument. Will rely on partial matching.
* `rlp()`: Added examples from Murphy et al. (1990)
* `stock.recruit()`: Removed abbreviated values from the type= argument -- will rely on partial matching. Changed sumtype argument to what.
* `stock.recruit.sim()`:  Changed model argument to type and added param argument (to make compatible with stock.recruit). Moved R and S arguments to beginning of argument string (more compatible with stock.recruit). STILL NEED to ADD SECOND RICKER MODEL.
* `vb.comp()`: Changed d argument to df.
* `wlgm.RD()`: Added example code. Added some details.

# FSA 0.0-7
* **Date:** unknown
* changed to compiling under R 2.6.1.
* added FSA.R file that loads the required librarys.
* now depends on `MASS` package because of the creation of the `boxcox.WLGM()` function and on the `plotrix` package for elements of `ycplot()`.

* `add.radcap()`: Created this function to add the radius-at-capture to a one-fish-per-line data frame of increments.
* `age.comp()`: Added a `match.arg()` call for the what argument.
* `age.key()`: Corrected `=T` or `=F` to `=TRUE` or `=FALSE`.
* `cc.sim()`: Corrected the use of `z.param` with correct use of `Z.param`.
* `coefplot()`: A new generic function for plotting coefficients from a Weisberg Linear Growth Model analysis.
* `g.convert()`: Changed arguments from data and measure.var to df and in.var. Included `match.arg()` for the type argument. Moved type argument as it now has a default. Added the in.pre argument that allows  the user to identify all input variables by a common prefix rather than having to list them out in  in.var. Added the out.pre argument that allows the user to control the prefix for the newly created  variables in the output data frame. Updated help file.
* `g.reshape()`: Changed arguments from data and prefix to df and in.pre. Deleted the measure.var argument. Moved new in.pre argument forward and na.rm argument backward in the argument list. Used in.pre to identify the measure.var variables to send to the melt function. If id.var is left blank (and in.pre is not) then id.var is the remaining variables not identified by in.pre. The val.name argument was set equal to in.pre as the default.
* `leslie.sim()`: Corrected call to old `leslie()` function with a correct call to `depletion()` function with `type="Leslie"`.
* `leslie.sim2()`: Corrected two calls to old `leslie()` function with correct calls to `depletion()` function with `type="Leslie"`.
* `leslie.rand.run()`: Moved R Documentation alias to FSA-internal.RD.
* `mr.closed2()`: Forced the function to use the `lowess()` function in the stats rather than the gplots package (which is loaded from `NCStats`). Also changed the `loess.f` argument to f and added the iter argument for sending to `lowess()`.
* `confint.MRO()`: Put in a logical catch when printing the confidence intervals because if `ci.type="Manly"` then CIs for B are not computed.
* `summary.MRO()`: Put in a logical catch when printing the estimates because if `ci.type="Manly"` then SEs are not computed.
* `plot.RLP()`: Corrected `=T` or `=F` to `=TRUE` or `=FALSE`.
* `wlgm()`: Created a method with a number of generics (alias, anova, boxcox, coef, coefplot, confint, ycplot) for performing the Weisberg Linear Growth Model.
* `ycplot()`: A new generic function for creating a year-class plot for the Weisberg Linear Growth Model analysis.

# FSA 0.0-6
* **Date:** unknown
* `agebias.plot()`: deleted and replaced with agecomp and plot.AgeComp functions.
* `agesunflower.plot()`: deleted and replaced with agecomp and plot.AgeComp functions.
* `agecomp()`: a new function that, along with its extractor functions, combines all of the functionality of the old `age.tests()`, `age.symmetry()`, `agebias.plot()`, and `agesunflower.plot()`. Allows for a more seamless comparison of ageing reads.
* `plot.AgeComp()`: an extractor function for objects saved from the `agecomp()` function. This replaces the old `agebias.plot()` and `agesunflower.plot()` functions.
* `summary.Agecomp()`: an extractor function for objects saved from the `agecomp()` function. This replaces the old `age.tests()` and `age.symmetry()` functions.
* `age.symmetry()`: deleted and replaced with `agecomp()` and `summary.AgeComp()` functions.
* `age.tests()`: deleted and replaced with `agecomp()` and `summary.AgeComp()` functions.
* `cc.sim()`: modified graphic to (1) include assumption violation labels on top of the graph and (2) label y-axis with "log(Catch)" rather than "ln(Catch)".
* `delury()`: deleted and replaced with `depletion()` function. See notes for `depletion()` function.
* `depletion()`: created a new function that performs the Leslie or Delury method as determined by a user-defined argument to type. This function replaces the old `leslie()` and `delury()` functions. All extractor functions for the class "LeslieDelury" have now been changed to class "Depletion".
* `plot.Depletion()`: correct xlab so that it defaults to "Cumulative Effort" for the Delury method.
* `leslie()`: deleted and replaced with depletion function. See notes for depletion function.
* `leslie.sim()`: modified graphic to include assumption violation labels on top of the graph.
* `mr.open()`: modified `est.N()` internal function so that N is set equal to n if N<n. In other words, if the sample size at time i is larger than the estimated population size at time i then the estimated population size is set equal to the observed sample size. This corrects the problem of the `N.se` calculation attempting to take the square root of a negative number causing the `mr.open()` function to shut down.
* `pass.removal()`: changed name to removal.
* `removal()`: new name for old pass.removal function.
* `rlp()`: new name for old (upper-case) RLP.
* `RLP()`: changed name to (lower-case) rlp.
