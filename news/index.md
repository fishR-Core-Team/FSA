# Changelog

## FSA 0.10.1

- Updated the PSD and Relative Weight computation articles to reflect
  the changes to
  [`psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md),
  [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md),
  [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md),
  and
  [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md).
- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Added catch for when n+T\<1 and n+T\<2. This addresses
  [\#131](https://github.com/fishR-Core-Team/FSA/issues/131)).
- [`metaM()`](https://fishr-core-team.github.io/FSA/reference/metaM.md):
  Added `method="HamelCope"` to address
  [\#133](https://github.com/fishR-Core-Team/FSA/issues/133). A few
  minor edits to documentation.
- [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md):
  Addressed bugs as described in
  [\#136](https://github.com/fishR-Core-Team/FSA/issues/136)) and
  [\#137](https://github.com/fishR-Core-Team/FSA/issues/137). Added
  `thesaurus` functionality. Reworked examples in documentation.
  Reworked testing framework. Thanks to Dave Glover.
- `PSDlit`: Added info for Flier and Longear Sunfish to address
  [\#122](https://github.com/fishR-Core-Team/FSA/issues/122)) and
  Northern Pikeminnow. Also updated information for Alabama Bass and
  Spotted Bass. Duplicated lines that combine `species` and `group` to
  partially address
  [\#137](https://github.com/fishR-Core-Team/FSA/issues/137).
- [`psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md):
  Added `dat=` to allow more flexibility when called from
  [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md).
- `PSDWRTest`: Added for testing PSD and relative weight functions.
- [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md):
  Addressed bugs similar to those for
  [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md).
  Added `thesaurus` functionality. Reworked examples in documentation.
  Reworked testing framework (especially expanded validation of results
  with hand-calculations).
- `wSlit`: Added info for Flier and Longear Sunfish to address
  [\#122](https://github.com/fishR-Core-Team/FSA/issues/122)). Also
  updated information for Alabama Bass (further removed Spotted Bass
  (Alabama subspecies)), Spotted Bass, and Northern Pikeminnow (further
  removed Northern Squawfish (synonym of Northern Pikeminnow that is no
  longer used)). Duplicated lines that combine `species` and `group` to
  partially address
  [\#137](https://github.com/fishR-Core-Team/FSA/issues/137).
- [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md):
  Added `dat=` to allow more flexibility when called from
  [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md).

## FSA 0.10.0

CRAN release: 2025-05-06

- Updated `test-coverage.yaml` and moved a `# nocov start` and
  `# nocov end` in `bootstrap.r` to address the errors with
  `test-coverage.yaml`. Addresses
  [\#118](https://github.com/fishR-Core-Team/FSA/issues/118).

- Removed `DescTools`, `plyr`, `psych` from Suggests (and all their uses
  in tests and linked code in documentation). Removed `ggplot2`,
  `marked`, `rcapture`, and `tibble` from Suggests (and use in examples
  was put in a `\dontrun()`).

- Added `FlexParamCurve` to Imports for use of `modpar()` in
  `findGrowthStarts(param="Richards")` and `purrr` for use of
  `map2_chr()` in
  [`showGrowthFun()`](https://fishr-core-team.github.io/FSA/reference/showGrowthFun.md).

- Removed my ORCID from DESCRIPTION to alleviate note on WinBuilder for
  R4.4.3.

- Added the following “articles”.

  - Show growth equations.
  - Describe how the starting values for the growth equations are
    derived.
  - Provide a simple introduction to growth model fitting with `FSA`.
  - Provide a simple introduction to calculating relative weights with
    `FSA`.
  - Provide a simple introduction to calculating proportional size
    distribution metrics with `FSA`.

- internals: Added functions to return a logical about whether a value
  is less than, less than or equal, greater than, or greater than or
  equal (i.e., `is.lte()`, `is.lt()`, `is.gte()`, and `is.gt()`). Added
  functions that use those logical and return an informative error if
  the logical is FALSE (i.e., `iChkLTE()`, `iChkLT()`, `iChkGTE()`, and
  `iChkGT()`). The errors can “grab” the name of the object so that the
  error can be specific though the function is general.

- internals: Modified
  [`STOP()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md)
  and
  [`WARN()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md)
  to use [`strwrap()`](https://rdrr.io/r/base/strwrap.html) rather than
  hard-coded line breaks. Added `MESSAGE()`. Added `iStrCollapse()`,
  largely for use with
  [`STOP()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md),
  [`WARN()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md),
  and `MESSAGE()`.

- throughout: Changed many `df[-which(CONDITION),]` constructs to
  `df[!CONDITION]` as suggested
  [here](https://stackoverflow.com/a/5236518).

- [`findGrowthStarts()`](https://fishr-core-team.github.io/FSA/reference/findGrowthStarts.md):
  Added. This replaces
  [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md)
  and includes starting values for Gompertz, logistic, and Richards
  functions, and seasonal and tag-recapture von Bertalanffy
  parameterizations. Note that `constvals=` and `fixed=` must now be
  numeric vectors (and not lists) and that the returned starting values
  are in a numeric vector.

- [`fishR()`](https://fishr-core-team.github.io/FSA/reference/fishR.md):
  Modified. Added link to teaching resources.

- [`GompertzFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Deprecated (replaced with
  [`makeGrowthFun()`](https://fishr-core-team.github.io/FSA/reference/makeGrowthFun.md)).

- `GrowthData1`, `GrowthData2`, `GrowthData3`: Added for testing growth
  functions.

- [`growthFunShow()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Deprecated (replaced with
  [`showGrowthFun()`](https://fishr-core-team.github.io/FSA/reference/showGrowthFun.md)).
  But also fixed expression for QuinnDeriso3 parameterization of the
  Gompertz function (i.e., erroneous t\* changed to t0 … related to
  fixing [\#113](https://github.com/fishR-Core-Team/FSA/issues/113)).
  Also changed a parameter to b in Ricker2 and QuinnDeriso1, and a to c
  in Ricker3 and QuinnDeriso2, to distinguish it from a in the Original
  parameterization. Will be deleted in future versions.

- [`logisticFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Deprecated (replaced with
  [`makeGrowthFun()`](https://fishr-core-team.github.io/FSA/reference/makeGrowthFun.md)).

- [`makeGrowthFun()`](https://fishr-core-team.github.io/FSA/reference/makeGrowthFun.md):
  Added. This replaces
  [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md),
  [`GompertzFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md),
  [`logisticFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md),
  [`RichardsFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md),
  [`Schnute()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md),
  `SchnuteRichards()`. Along the way, the following changes were made.

  - Included numbers for parameterizations in `param=` and moved names
    to `pname=`. Some names were changed.
  - In Gompertz functions … changed a parameter in “Original” to a1,
    changed a parameter to a2 in Ricker2 and QuinnDeriso1, and a to a2
    in Ricker3 and QuinnDeriso2, to distinguish them when they are
    different.
  - In Richards functions …
    - Restricted to only 4-parameter functions; thus, removed sixth
      parameterization
    - Removed first parameterization as it had limited placement for the
      inflection point.
    - Moved the old parameterization to new places as follows: third to
      first, and fifth to second. Thus, the first parameterization will
      be the one that most closely follows the parameterization of the
      self-starting function to be used in
      [`findGrowthStarts()`](https://fishr-core-team.github.io/FSA/reference/findGrowthStarts.md).
      Removed the other two parameterizations as they were essentially
      the same as the first except for how the exponent was defined,
      which has no biological meaning.
    - Modified parameterizations to have the same general look (i.e.,
      Linf times something raised to a power). After this, the powers
      were all the same, so there is just a “b” parameter now.

- [`peek()`](https://fishr-core-team.github.io/FSA/reference/peek.md):
  Modified. Addressed
  [\#125](https://github.com/fishR-Core-Team/FSA/issues/125).

- [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md):
  Modified. Added `group=` to handle the change for sub-groups in
  `PSDlit`. Modified `addLens=` to more closely match how `addLens=`
  words in
  [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md)
  and, thus, removed `addSpecs=`.

- [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md):
  Modified. Added `group=` to handle the change for sub-groups in
  `PSDlit`. Made other coding changes that did not affect forward-facing
  functionality.

- `PSDLit`: Modified.

  - Added length categories for Goldeye, Lake Chubsucker, and Northern
    Snakehead.
  - Added a `group` variable to handle species with specified
    sub-groups.

- `psdVals()`: Modified. Added `group=` to handle the change for
  sub-groups in `PSDlit`. Made other coding changes that did not affect
  forward-facing functionality.

- [`RichardsFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Deprecated (replaced with
  [`makeGrowthFun()`](https://fishr-core-team.github.io/FSA/reference/makeGrowthFun.md)).

- [`Schnute()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Deleted (made defunct) as it was added to
  [`makeGrowthFun()`](https://fishr-core-team.github.io/FSA/reference/makeGrowthFun.md)
  (use `Schnute <- makeGrowthFun("Schnute")` instead). Note that order
  of arguments was changed so that the parameters appear before the
  constants to be consistent with other growth functions.

- `SchnuteRichards()`: Deleted (made defunct) as it was added to
  [`makeGrowthFun()`](https://fishr-core-team.github.io/FSA/reference/makeGrowthFun.md)
  (use `SchnuteRichards <- makeGrowthFun("Schnute-Richards")` instead)
  and was likely little used.

- [`showGrowthFun()`](https://fishr-core-team.github.io/FSA/reference/showGrowthFun.md):
  Added. Replaces `GrowthFunShow()`. Updated to allow user to send an
  object from [`nls()`](https://rdrr.io/r/stats/nls.html) and have the
  coefficient values extracted and put in the expression. Also, can
  either return a string or an expression to allow more flexibility in
  use (especially with `ggplot2`).

- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Deprecated (replaced with
  [`makeGrowthFun()`](https://fishr-core-team.github.io/FSA/reference/makeGrowthFun.md)).

- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Deprecated (replaced with `makeGrowthStarts()`. But also streamlined
  some of the internal functions, fixed some typos, and replaced
  `iVBStartsPlot()` with `iPlotGrowthStarts()` to be more general.

- [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md):
  Modified. Added `WsOpts=` to handle species where the user must make a
  choice about which standard weight equation to use.

- `WSLit`: Modified.

  - Added results for Goldeye, Lake Chubsucker, and Northern Snakehead.
  - Removed `type` variable (will depend on whether `quad` is `NA` or
    not).
  - Added `group` variable that took the parenthetical groupings from
    `species` for things like Walleye, Cutthroat Trout, etc. Now
    `species` is an actual species name and if a sub-group of that
    exists then it is defined with `group`.

- [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md):
  Modified.

  - Added `group=` to handle species for which equations were derived
    for sub-groups of the species (e.g., separately for males and
    females, or lentic and lotic). See related changes to `WSlit`.
  - Added `method=` to handle species for which equations derived from
    more than one method are available (e.g., “Arctic Grayling”).
  - Added a few more checks for `ref=`, `units=`, etc. and provided more
    descriptive error messages.

## FSA 0.9.6

CRAN release: 2025-01-07

- Updated testing to use `testthat` v3.0.0.

  - Changes to `DESCRIPTION` including adding `tidyr` in Suggests (for
    example in
    [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md)).
  - Replaced MANY `expect_is()` with `expect_equal(class())` idioms.
  - Replaced many `expect_equivalent()` with `expect_equal()` as
    `expect_equivalent()` was not needed to begin with.
  - Replaced many `expect_equivalent()` with
    `expect_equal(,ignore_attr=TRUE)` as `expect_equivalent()` was
    deprecated.
  - Had to correct many tests where I expected just `matrix` but the
    class was `c("matrix","array")`.
  - Had to handle multiple warnings for some tests (see [this
    article](https://testthat.r-lib.org/articles/third-edition.html#warnings)).
  - Moved all [`require()`](https://rdrr.io/r/base/library.html) in
    individual files to `testthat.R`. This removed many
    [`require()`](https://rdrr.io/r/base/library.html) that were not
    needed.

- Fixed four minor errors in documentation from legacy uses of `\R{}`
  rather than `\code{}`.

- Made some accessibility changes and rebuilt favicons as suggested by
  `pkgdown`.

- [`alkPlot()`](https://fishr-core-team.github.io/FSA/reference/alkPlot.md):
  Modified. Added note in documentation pointing to a fishR blog post on
  using `ggplot2` to make similar plots.

- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified. Added `as.df=` to extractor functions and `incl.est=` to
  [`confint.catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md)
  to match functionality added to
  [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md).

- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified. Added `as.df=` to extractor functions and `incl.est=` to
  [`confint.chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md)
  to match functionality added to
  [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md).

- [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  Modified to address
  [\#111](https://github.com/fishR-Core-Team/FSA/issues/111).

  - Added formula notation such that
    [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md)
    wash changed to a method call and
    [`depletion.default()`](https://fishr-core-team.github.io/FSA/reference/depletion.md)
    and
    [`depletion.formula()`](https://fishr-core-team.github.io/FSA/reference/depletion.md)
    were added. Tests for the formula were included.
  - Added `as.df=` to
    [`coef.depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md),
    [`confint.depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md),
    and
    [`summary.depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md)
    so that the result is returned as a data.frame when set to `TRUE`
    (default is `FALSE` to maintain backward compatability).
  - Added `incl.est=` to
    [`confint.depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md)
    to make it easier to get away from the clunky
    `cbind("Est"=coef(),confint())` code.

- [`GompertzFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Accepted pull request related to
  [\#112](https://github.com/fishR-Core-Team/FSA/issues/112) that fixed
  several typos and dead links in the documentation … thanks Arni.
  Corrected the erroneous reference to t\* (should have been t0) in the
  documentation for the Gompertz function (fixes
  [\#113](https://github.com/fishR-Core-Team/FSA/issues/113) … thanks
  again to Arni).

- [`metaM()`](https://fishr-core-team.github.io/FSA/reference/metaM.md):
  Modified to address
  [\#114](https://github.com/fishR-Core-Team/FSA/issues/114).

  - Returns data.frame rather than list.
  - Added conditional mortality rate (cm) to returned data.frame (for
    use with `rFAMS`).
  - Removed `justM=` and its functionality (not needed with data.frame
    returned).
  - Added `verbose=` to allow user to limit some of what is returned in
    data.frame.
  - Removed `print.metaM()` method.
  - Added Quinn and Deriso (1999), Peterson and Wroblewski (1984), and
    Chan and Watanabe (1989) methods from FAMS manual. These are
    probably only useful for comparison to FAMS results.
  - Added an example for computing an average M or cm from multiple
    model results.

- [`Mmethods()`](https://fishr-core-team.github.io/FSA/reference/metaM.md):
  Modified. Changed `what=` to `method=` for simplicity with
  [`metaM()`](https://fishr-core-team.github.io/FSA/reference/metaM.md).

- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Modified.

  - Added a formula version to better match
    [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md).
  - Deprecated `just.ests=`. Functionality will be largely replaced with
    `incl.ests=` in [`confint()`](https://rdrr.io/r/stats/confint.html).
    Replaced split-apply example with a new one that performs similarly
    without `just.ests=` and is more in-line with examples in
    `depletion` *et al.*
  - Added [`coef()`](https://rdrr.io/r/stats/coef.html) extractor
    function.
  - Modified [`confint()`](https://rdrr.io/r/stats/confint.html) and
    [`summary()`](https://rdrr.io/r/base/summary.html) extractor
    functions to better match
    [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md).

## FSA 0.9.5

CRAN release: 2023-08-26

- Fixed FSA-package problem using the “automatic approach” (i.e., adding
  a “\_PACKAGE” line to FSA.R) suggested in an e-mail from Kurt Hornik
  on 19-Aug-2023.

## FSA 0.9.4

CRAN release: 2023-02-01

- Changes related to moving to fishR-Core-Team
  - Updated sticker.
  - Changed DHO e-mail address (in DESCRIPTION and in all author fields
    of the documentation). Partially address
    [\#86](https://github.com/fishR-Core-Team/FSA/issues/86).
  - Updated `pkgdown.yaml` GitHub action to
    [v2](https://github.com/r-lib/actions/tree/v2-branch/examples#build-pkgdown-site).
    Changed action to only run on a release (rather than a push) but it
    can be [run
    manually](https://docs.github.com/en/actions/managing-workflow-runs/manually-running-a-workflow)
    as well.
  - Updated `R-CMD-check.yaml` GitHub action to
    [v2](https://github.com/r-lib/actions/tree/v2-branch/examples#standard-ci-workflow).
    Note that I had to add the [extra code for dealing with graphics on
    the Mac version](https://github.com/r-lib/actions#common-questions).
- Changes related to new fishR webpage
  - Updated links in
    [`fishR()`](https://fishr-core-team.github.io/FSA/reference/fishR.md),
    [`FSA()`](https://fishr-core-team.github.io/FSA/reference/FSA.md),
    and `README.md`. Partially address
    [\#86](https://github.com/fishR-Core-Team/FSA/issues/86).
  - Updated all links to Introductory Fisheries Analyses with R book.
  - Added links to CSV files for all data sets. This addresses
    [\#96](https://github.com/fishR-Core-Team/FSA/issues/96).
  - Changed theme in `_pkgdown.yml` to match that of `FSAdata` and more
    closely match `fishR`.
  - Removed most recent dates from NEWS file as `pkgdown` picks up the
    CRAN release date to add.
  - Updated `CITATION` (to match that required for next version of R).
- [`alkIndivAge()`](https://fishr-core-team.github.io/FSA/reference/alkIndivAge.md):
  Modified. Added a catch for `NA`s in the length sample. Also added a
  test. This addresses
  [\#88](https://github.com/fishR-Core-Team/FSA/issues/88).
- [`confint.boot()`](https://fishr-core-team.github.io/FSA/reference/boot.md):
  Modified. Changed hard-coding of columns that contained the confidence
  interval values to find those columns by
  [`grep()`](https://rdrr.io/r/base/grep.html)ing for the `%` sign. This
  fixes an issue related to
  [`car::Confint()`](https://rdrr.io/pkg/car/man/S.html) returning the
  [`coef()`](https://rdrr.io/r/stats/coef.html) results for functions
  that have a [`coef()`](https://rdrr.io/r/stats/coef.html) method but
  not for those that do not. Also updated tests to use results from
  [`car::Boot()`](https://rdrr.io/pkg/car/man/Boot.html) rather than the
  old
  [`car::bootCase()`](https://rdrr.io/pkg/car/man/car-deprecated.html).
- `PSDcalc`: Modified. Changed code to allow for missing `species=` as
  long as `addLens=` is used. This allows the user to provide length
  categories for a species for which Gabelhouse lengths are not defined.
  Several new tests were added and some were modified to handle the
  changing message re: a missing `species=`. The documentation was
  modified accordingly. This (finally) addresses
  [\#58](https://github.com/fishR-Core-Team/FSA/issues/58).
- `PSDlit`: Modified. Added info for Redbreast Sunfish and Spotted
  Sunfish from Bonvecchio *et al.* (2023). This addresses
  [\#100](https://github.com/fishR-Core-Team/FSA/issues/100)).
- `wSlit`: Modified documentation. Described the `RLP` and `EmP`
  acronyms and provided references for them. This addresses
  [\#95](https://github.com/fishR-Core-Team/FSA/issues/95)). Added info
  for Redbreast Sunfish and Spotted Sunfish from Bonvecchio *et al.*
  (2023). This addresses
  [\#100](https://github.com/fishR-Core-Team/FSA/issues/100)).

## FSA 0.9.3

CRAN release: 2022-02-18

- Moved `dplyr` from `imports` to `suggests` (needed because functions
  were removed in last version; however it is still used in some
  examples; partially addresses
  [\#87](https://github.com/fishR-Core-Team/FSA/issues/87)).
- Removed `sciplot` from `imports` (because functions were removed in
  last version; partially addresses
  [\#87](https://github.com/fishR-Core-Team/FSA/issues/87)).
- Updated tests for
  [`ksTest()`](https://fishr-core-team.github.io/FSA/reference/ksTest.md)
  to handle issues on the CRAN M1 build machine (per e-mail from
  Prof. Ripley on 15-Feb-22; partially addresses
  [\#87](https://github.com/fishR-Core-Team/FSA/issues/87)).
- Updated all links to the `droglenc` github that were related to `FSA`
  or `FSAdata` to be to the `fishR-Core-Team` github.

## FSA 0.9.2 12-Feb-21

CRAN release: 2022-02-12

- Last version maintained by Derek Ogle. Transferring to fishR Core Team
  for next version.
- [`filterD()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  **REMOVED** (to `FSAmisc`).
- [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  **REMOVED** (to `FSAmisc`).
- [`fsaNews()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  and `FSANews()`: **Removed**.
- [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md):
  Modified. Changed the way `PSDlit` was loaded into the function
  environment so that
  [`FSA::psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md)
  will work. Addresses
  [\#85](https://github.com/fishR-Core-Team/FSA/issues/85).
- `PSDLit`: Modified. Added info for Utah Chub (from
  [here](https://www.usgs.gov/publications/proposed-standard-weight-ws-equation-and-length-categories-utah-chub);
  address [\#84](https://github.com/fishR-Core-Team/FSA/issues/84)).
- [`psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md):
  Modified. Changed the way `PSDlit` was loaded into the function
  environment so that
  [`FSA::psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md)
  will work. Addresses
  [\#85](https://github.com/fishR-Core-Team/FSA/issues/85).
- [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  **REMOVED** (to `FSAmisc`).
- [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md):
  Modified. Changed the way `WSlit` was loaded into the function
  environment so that
  [`FSA::wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)
  will work. Addresses
  [\#85](https://github.com/fishR-Core-Team/FSA/issues/85).
- `WSLit`: Modified. Added info for Utah Chub (from
  [here](https://www.usgs.gov/publications/proposed-standard-weight-ws-equation-and-length-categories-utah-chub);
  address [\#84](https://github.com/fishR-Core-Team/FSA/issues/84)).
- [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md):
  Modified. Changed the way `WSlit` was loaded into the function
  environment so that
  [`FSA::wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md)
  will work. Addresses
  [\#85](https://github.com/fishR-Core-Team/FSA/issues/85).

## FSA 0.9.1

CRAN release: 2021-07-17

- Corrected testing issue for
  [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md)
  and
  [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md)
  as directed by CRAN on 17-Jul-21. Issue likely caused by changes to
  `fishmethods` package.

## FSA 0.9.0

CRAN release: 2021-06-09

- Make note of the several **removed** (now defunct) and **deprecated**
  (soon to be defunct) functions listed below.
- Added Jason Doll as an `AUThor`.
- Moved `plyr` from Imports to Suggests.
- [`alkPlot()`](https://fishr-core-team.github.io/FSA/reference/alkPlot.md):
  Modified. Removed use of
  [`chooseColors()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  (see below).
- [`binCI()`](https://fishr-core-team.github.io/FSA/reference/binCI.md):
  Modified. Changed to use `iCheckConfLevel()` (which addresses
  [\#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
- [`bootCase()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  **REMOVED**. Users can use
  [`car::Boot()`](https://rdrr.io/pkg/car/man/Boot.html), which
  partially addresses
  [\#65](https://github.com/fishR-Core-Team/FSA/issues/65).
  - [`plot.boot()`](https://fishr-core-team.github.io/FSA/reference/boot.md):
    **REMOVED**. Conflicted with
    [`boot::plot.boot()`](https://rdrr.io/pkg/boot/man/plot.boot.html)
    which caused an error with CRAN. Same functionality is available
    with `pairs(<boot object>$t)`.
- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified. Removed hard-coding of `ylim=` for
  [`plot.catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md)
  (this addresses
  [\#70](https://github.com/fishR-Core-Team/FSA/issues/70) … Thanks to
  Brendan Runde). Added `round.est=` so that the user can control the
  decimals on mortality estimate values. Changed to use
  `iCheckConfLevel()` (which addresses
  [\#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified. Removed hard-coding of `ylim=` for
  [`plot.chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md).
  Added `round.est=` so that the user can control the decimals on
  mortality and survival estimate values. Changed to use
  `iCheckConfLevel()` (which addresses
  [\#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
- [`chooseColors()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  **REMOVED**. This was an exported function that should have been
  internal. Regardless, where it was used has been removed and the user
  is now allowed to provide their own vector of colors. See
  `iCheckMultColors()`. Partially addresses
  [\#65](https://github.com/fishR-Core-Team/FSA/issues/65).
- [`compIntercepts()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  **REMOVED** (to `FSAmisc`). Added to `FSA-defunct`. Partially
  addresses [\#65](https://github.com/fishR-Core-Team/FSA/issues/65).
- [`compSlopes()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  **REMOVED** (to `FSAmisc`). Added to `FSA-defunct`. Partially
  addresses [\#65](https://github.com/fishR-Core-Team/FSA/issues/65).
- [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  Modified. Changed to use `iCheckConfLevel()` (which addresses
  [\#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
- [`filterD()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  **DEPRECATED** (partially addresses
  [\#65](https://github.com/fishR-Core-Team/FSA/issues/65)).
- [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  **DEPRECATED** (partially addresses
  [\#65](https://github.com/fishR-Core-Team/FSA/issues/65)). Prior to
  that removed use of
  [`chooseColors()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  (see above).
- [`diags()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  **REMOVED** (moved to `FSAmisc`). Added to `FSA-defunct`. Partially
  addresses [\#65](https://github.com/fishR-Core-Team/FSA/issues/65).
- [`hoCoef()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  **REMOVED** (moved to `FSAmisc`). Added to `FSA-defunct`. Partially
  addresses [\#65](https://github.com/fishR-Core-Team/FSA/issues/65).
- [`hyperCI()`](https://fishr-core-team.github.io/FSA/reference/hyperCI.md):
  Modified. Changed to use `iCheckConfLevel()` (which addresses
  [\#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
- `iCheckConfLevel()`: Added (internal to address
  [\#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
- `iCheckMultColors()`: Added (internal). Part of removing
  [`chooseColors()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  and `paletteChoices()`.
- `iRichColors()`: **REMOVED** (as part of removing
  [`chooseColors()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)).
- [`mapvalues()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  **REMOVED**. Added to `FSA-defunct`. Partially addresses
  [\#65](https://github.com/fishR-Core-Team/FSA/issues/65).
- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified. Changed to use `iCheckConfLevel()` (which addresses
  [\#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
- [`mrOpen()`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md):
  Modified. Added a warning for when r==0, which causes the SE of M and
  thus N to be `Infinity` (this addresses
  [\#69](https://github.com/fishR-Core-Team/FSA/issues/69)). Added a
  similar warning for when R==0 (but not the last time period). Added
  tests for the warning messages. Changed one use of
  [`apply()`](https://rdrr.io/r/base/apply.html) to
  [`rowSums()`](https://rdrr.io/r/base/colSums.html) for ease of
  reading. Changed to use `iCheckConfLevel()` (which addresses
  [\#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
- `paletteChoices()`: **REMOVED**. See
  [`chooseColors()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  above. Partially addresses
  [\#65](https://github.com/fishR-Core-Team/FSA/issues/65).
- [`poiCI()`](https://fishr-core-team.github.io/FSA/reference/poiCI.md):
  Modified. Changed to use `iCheckConfLevel()` (which addresses
  [\#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
- [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md):
  Modified. Changed a [`levels()`](https://rdrr.io/r/base/levels.html)
  in `iPSDlitCheck()` to
  [`unique()`](https://rdrr.io/r/base/unique.html) because `species` is
  no longer a factor due to updating `PSDlit` (i.e., rdata file changed
  with new [`read.csv()`](https://rdrr.io/r/utils/read.table.html)).
- [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md):
  Modified. Added a catch for when “tibble”s are sent in `data=`
  (addresses [\#75](https://github.com/fishR-Core-Team/FSA/issues/75)).
  Changed to use `iCheckConfLevel()` (which addresses
  [\#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
- [`psdCI()`](https://fishr-core-team.github.io/FSA/reference/psdCI.md):
  Modified. Changed to use `iCheckConfLevel()` (which addresses
  [\#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
- `PSDlit`: Modified. Added length categories for Shoal Bass and Pallid
  Sturgeon. Added Striped Bass (Hybrid) and Striped Bass x White Bass;
  though these are the same as the existing Palmetto Bass. Added
  “source”s for each entry.
- [`psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md):
  Modified. Changed a [`levels()`](https://rdrr.io/r/base/levels.html)
  in `iPSDlitCheck()` and
  [`iListSpecies()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md)
  to [`unique()`](https://rdrr.io/r/base/unique.html) because `species`
  is no longer a factor due to updating `PSDlit` (i.e., rdata file
  changed with new
  [`read.csv()`](https://rdrr.io/r/utils/read.table.html)). Added a
  `showJustSource=` argument that will show the source info (if `TRUE`)
  or not (if `FALSE`; default), which partially addresses
  [\#76](https://github.com/fishR-Core-Team/FSA/issues/76).
- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Modified. Added check and then warning if non-whole numbers are in
  `catch=` (addresses
  [\#60](https://github.com/fishR-Core-Team/FSA/issues/60)). Also
  modified checks of data integrity to be more robust (e.g., if a
  character vector is sent). Changed to use `iCheckConfLevel()` (which
  addresses [\#66](https://github.com/fishR-Core-Team/FSA/issues/66)).
- [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  **DEPRECATED** (partially addresses
  [\#65](https://github.com/fishR-Core-Team/FSA/issues/65)). Prior to
  that removed use of
  [`chooseColors()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  (see above).
- [`Subset()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  **REMOVED**. Added to `FSA-defunct`. Partially addresses
  [\#65](https://github.com/fishR-Core-Team/FSA/issues/65).
- [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md):
  Modified. Changed a [`levels()`](https://rdrr.io/r/base/levels.html)
  in `iwsLitCheck()` to [`unique()`](https://rdrr.io/r/base/unique.html)
  because `species` is no longer a factor due to updating `WSlit` (i.e.,
  rdata file changed with new
  [`read.csv()`](https://rdrr.io/r/utils/read.table.html)).
- `WSlit`: Modified. Addresses
  [\#68](https://github.com/fishR-Core-Team/FSA/issues/68).
  - Corrected capitalization of “Cavedano Chub”, “European Chub”, “Lake
    Herring” (metric), and “Pursak Chub”.
  - Removed “not in Neumann et al. (2012)” notes.
  - Added African Sharptooth Catfish, Ankara Nase, Bighead and Silver
    Carp, Brook Trout (Appalachia), Fourbarbel Scraper, Horse Barbel,
    Nile Tilapia, Nipple-Lipped Scraper, Shoal Bass, South European
    Roach, Spotted Bass (Alabama subspecies) (AKA Alabama Bass).
  - Added Striped Bass (Hybrid) and Striped Bass x White Bass; though
    these are the same as the existing Palmetto Bass.
- [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md):
  Modified. Changed a [`levels()`](https://rdrr.io/r/base/levels.html)
  in `iwsLitCheck()` to [`unique()`](https://rdrr.io/r/base/unique.html)
  because `species` is no longer a factor due to updating `WSlit` (i.e.,
  rdata file changed with new
  [`read.csv()`](https://rdrr.io/r/utils/read.table.html)).

## FSA 0.8.32

CRAN release: 2021-01-15

- Removed Travis-CI and appveyor.
- No longer using coveralls for coverage statistics. Changed to
  codecov.io.
- Added GitHub Action for CI/CD (used
  `usethis::use_github_action_check_standard()`).
- Added GitHub Action for pkgdown (used
  `usethis::use_github_action("pkgdown")`).
- Added GitHub Action for code coverage with codecov.io.
- Added a code of conduct for contributors.
- Moved a bunch of plotting examples in the documentation to
  `tests\plottests\` to speed up testing. The `tests\plottests\` was
  added to `.Rbuildignore` .
- [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md):
  Modified. Fixed bug with y-axes when `freq=FALSE` is used (fixes
  [\#62](https://github.com/fishR-Core-Team/FSA/issues/62); thanks to
  [@carlostorrescubila](https://github.com/carlostorrescubila)).
- [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Fixed bugs with handling models that used character rather
  than factor variables.
- [`plotBinResp()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  REMOVED. Removed as a user-facing function, but made as an internal
  function for continued use in
  [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  while
  [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  is deprecated.
- [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md):
  Modified. Fixed bug relate to species that were `NA` (fixes
  [\#64](https://github.com/fishR-Core-Team/FSA/issues/64); thanks to
  Dan Shoup). Added more tests and fixed some typos in the
  documentation.
- [`psdPlot()`](https://fishr-core-team.github.io/FSA/reference/psdPlot.md):
  Modified. Fixed bug with box around the plot when `add.psd=FALSE`.
  Added 5% expansion to top of y-axis so that bars did not run into the
  box.
- [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Fixed bugs with handling models that used character rather
  than factor variables.

## FSA 0.8.31

CRAN release: 2020-11-08

- Now using roxygen v7.1.1.
- Added `tibble` to suggests (see comment about
  [`headtail()`](https://fishr-core-team.github.io/FSA/reference/headtail.md)
  below).
- Cleaned up the documentation of parameters for
  [`RichardsFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md)
  (documentation did not change when parameter letters were changed for
  the Age and Growth book).
- Changed example in
  [`headtail()`](https://fishr-core-team.github.io/FSA/reference/headtail.md)
  to use
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  from `tibble` package rather than
  [`tbl_df()`](https://dplyr.tidyverse.org/reference/tbl_df.html) from
  `dplyr` package. Required adding `tibble` to suggests.
- [`nlsTracePlot()`](https://fishr-core-team.github.io/FSA/reference/nlsTracePlot.md):
  Modified. Created a conditional catch depending on the version of R as
  the results of `nls(*,trace=TRUE)` are changing in v4.1.0 (per e-mail
  from Martin Maechler on 2-Nov-20).

## FSA 0.8.30

CRAN release: 2020-03-09

- **Date:** 9-Mar-20
- Started using `rhub::check_for_cran()` for checking before sending to
  CRAN.
- Updated tests for
  [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md)
  and
  [`ksTest()`](https://fishr-core-team.github.io/FSA/reference/ksTest.md)
  that used [`data.frame()`](https://rdrr.io/r/base/data.frame.html).
  This should have been done with v0.8.28.
- Fixed errors for tests in
  [`ksTest()`](https://fishr-core-team.github.io/FSA/reference/ksTest.md)
  that were identified using R-hub.
- Removed all links to documentation in non-dependent or non-suggested
  packages. This removes a note from R-hub.
- [`fishR()`](https://fishr-core-team.github.io/FSA/reference/fishR.md):
  Modified. Changed base URL to `https:` (from `http:`). Added `open=`,
  primarily to allow not opening a browser during testing.

## FSA 0.8.29

- **Date:** 8-Mar-20
- Removed dependency on `epitools` package as it may soon be orphaned.
  See changes to
  [`binCI()`](https://fishr-core-team.github.io/FSA/reference/binCI.md)
  and
  [`poiCI()`](https://fishr-core-team.github.io/FSA/reference/poiCI.md)
  outlined below.
- [`binCI()`](https://fishr-core-team.github.io/FSA/reference/binCI.md):
  Modified. Added internal functions that are based on (but not
  identical to) functions in the `epitools` package which will possibly
  be deprecated soon (per note from CRAN on 7-Mar-20).
- [`poiCI()`](https://fishr-core-team.github.io/FSA/reference/poiCI.md):
  Modified. Added internal functions that are based on (but not
  identical to) functions in the `epitools` package which will possibly
  be deprecated soon (per note from CRAN on 7-Mar-20).

## FSA 0.8.28

- **Date:** 28-Feb-20
- [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed so that lines are plotted after the points in the
  IVR versions.
- [`ksTest()`](https://fishr-core-team.github.io/FSA/reference/ksTest.md):
  Modified. Changed documentation examples to handle R’s new way of
  handling `stringsAsFactors=` (per request from CRAN on 27-Feb-20).
- [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md):
  Modified. Changed testing to handle R’s new way of handling
  `stringsAsFactors=` (per request from CRAN on 27-Feb-20).

## FSA 0.8.27

CRAN release: 2020-02-03

- Now using ROxygen2 7.0.2.
- Removed dependency on `gplots` package as it is now orphaned. Required
  adding `iRichColors()` internal function.
- [`lwCompPreds()`](https://fishr-core-team.github.io/FSA/reference/lwCompPreds.md):
  Removed `\dots` from arguments as it was not in usage (per request
  from CRAN on 3-Feb-20).
- [`repeatedRows2Keep()`](https://fishr-core-team.github.io/FSA/reference/repeatedRows2Keep.md):
  Modified. Now makes comparisons as if `NA`s are regular values.

## FSA 0.8.26

CRAN release: 2019-11-22

- Changed to depending on `R >=3.5.0`, because that is the latest
  version required by a package (i.e., `car`) that FSA imports or
  suggests. Used the “check_r_versions_of_package_dependencies” shiny
  app by “ateucher” (on Github) to help determine this.
- Removed `asbio` package from suggests as it hung up Travis-CI build
  (because of the need for the TCLTK package).
- [`capFirst()`](https://fishr-core-team.github.io/FSA/reference/capFirst.md):
  Modified. Fixed bug related to an `NA` item.
- [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md):
  Modified. Changed `spec=` to `species=` to be consistent with
  [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md)
  and
  [`psdPlot()`](https://fishr-core-team.github.io/FSA/reference/psdPlot.md).
- [`peek()`](https://fishr-core-team.github.io/FSA/reference/peek.md):
  Added.
- [`repeatedRows2Keep()`](https://fishr-core-team.github.io/FSA/reference/repeatedRows2Keep.md):
  Modified. Added a catch if the data.frame only contains one row (it
  then returns `TRUE` so that that row is kept).

## FSA 0.8.25 24-Jul-19

CRAN release: 2019-07-24

- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Changed so that PE2 and CV2 use the median in the entire
  calculation rather than just in the denominator.
- `iHndlColsUseIgnore()`: Modified. Changed so that a 0 indice returns
  an error.
- [`repeatedRows2Keep()`](https://fishr-core-team.github.io/FSA/reference/repeatedRows2Keep.md):
  Added.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Corrected bug related to point transparenty when `plot=TRUE`
  and there is no age/length combination repeats (i.e., all age/length
  combinations are unique). Corrected bug of `col.main=` being ignored
  when `plot=TRUE`.

## FSA 0.8.24 17-May-19

CRAN release: 2019-05-21

- Corrected misuses of `\concept` in Rd files per CRAN request.

## FSA 0.8.23 1-May-19

CRAN release: 2019-05-02

- Reorganized `testthat` folder as suggested in `testthat` release
  notes.
- Removed all uses of
  [`Subset()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  (replaced with
  [`filterD()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)).
- [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Fixed bug related to y-axis limits not extending to contain
  the data, confidence bands, or prediction bands (in `fitPlot.slr()`).
  This addresses [\#3](https://github.com/droglenc/NCStats/issues/3)
  listed for `NCStats`).
- [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md):
  Modified. Fixed bug related to subsequent calls after a call that used
  `iaxs=FALSE`. This addresses
  [\#46](https://github.com/fishR-Core-Team/FSA/issues/46).
- [`iLegendHelp()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Added a catch if a proper keyword is not supplied.
- [`nlsTracePlot()`](https://fishr-core-team.github.io/FSA/reference/nlsTracePlot.md):
  Modified. Moved error catching for improper keyword for legend
  placement forward.
- `SchnuteRichards()`: Added. This addresses
  [\#54](https://github.com/fishR-Core-Team/FSA/issues/54).

## FSA 0.8.22

CRAN release: 2018-11-22

- Corrected CITATION file.
- Updated tests for changes in the `fishmethods` package (`vblrt()`
  replaced with `growthlrt()` and `T=` replaced with `TC=` in
  `M.empirical()`) per CRAN request.

## FSA 0.8.21

CRAN release: 2018-11-03

- Added a webpage. Setup Travis-CI to handle updates.
- Added a hex sticker logo.
- Added `withr` to Imports (see usages below).
- Added `Encoding: UTF-8` to DESCRIPTION.
- Added Powell Wheeler as an author for their work adding
  `method="Burnham"` to
  [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md).
- Added Alexis Dinno as an author for their providing the base
  functionality of
  [`dunnTest()`](https://fishr-core-team.github.io/FSA/reference/dunnTest.md).
  Should have done this long ago.
- Removed all [`data()`](https://rdrr.io/r/utils/data.html) in examples
  that referred to data from this package. Included the `package=`
  argument in [`data()`](https://rdrr.io/r/utils/data.html) that loaded
  from other packages.
- Added `seealso`, with links to which functions use the data for
  examples, to docmentation for all data.frames.
- Removed `\dontrun()`s from the `bootCase` related examples now that
  `car` package is updated. This addresses
  [\#45](https://github.com/fishR-Core-Team/FSA/issues/45).
- [`addZeroCatch()`](https://fishr-core-team.github.io/FSA/reference/addZeroCatch.md):
  Modified. Added a catch that turns a “tibble” into a regular
  data.frame (which obviates some errors that occur with tibbles). Minor
  changes to documentation and comments in the code.
- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Added intermediate and summary calculations for median and
  modal age; average absolute deviation and standard deviation; APE and
  CV with the median rather than the mean as the divisor; and index of
  precision (D). Added `show.prec2=`. Updated tests and examples. Other
  minor modifications to the function code. Addresses
  [\#41](https://github.com/fishR-Core-Team/FSA/issues/41) and
  [\#49](https://github.com/fishR-Core-Team/FSA/issues/49).
- [`alkIndivAge()`](https://fishr-core-team.github.io/FSA/reference/alkIndivAge.md):
  Modified. Replaced an `options(warn=-1)` with
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html).
- [`alkPlot()`](https://fishr-core-team.github.io/FSA/reference/alkPlot.md):
  Modified. Changed to using
  [`withr::local_par()`](https://withr.r-lib.org/reference/with_par.html)
  (partially addresses
  [\#38](https://github.com/fishR-Core-Team/FSA/issues/38)). Replaced an
  `options(warn=-1)` with
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html).
- `alkSummaries()`: Modified. Replaced an `options(warn=-1)` with
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html).
- Bootstrapping functions: Modified. Changed to using
  [`withr::local_par()`](https://withr.r-lib.org/reference/with_par.html)
  (partially addresses
  [\#38](https://github.com/fishR-Core-Team/FSA/issues/38)).
- [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md):
  Modified. Changed to using
  [`withr::local_par()`](https://withr.r-lib.org/reference/with_par.html)
  (partially addresses
  [\#38](https://github.com/fishR-Core-Team/FSA/issues/38)).
- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified. Changed to using
  [`withr::local_par()`](https://withr.r-lib.org/reference/with_par.html)
  (partially addresses
  [\#38](https://github.com/fishR-Core-Team/FSA/issues/38)).
- [`fishR()`](https://fishr-core-team.github.io/FSA/reference/fishR.md):
  Modified. Changed examples in documentation to not be run (so as not
  to open an external webpage).
- `FSAnews()`: Modified. Changed examples in documentation to not be run
  (so as not to open an external webpage).
- Growth models: Modified. Changed to using
  [`withr::local_par()`](https://withr.r-lib.org/reference/with_par.html)
  (partially addresses
  [\#38](https://github.com/fishR-Core-Team/FSA/issues/38)).
- [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md):
  Modified. Changed to using
  [`withr::local_par()`](https://withr.r-lib.org/reference/with_par.html)
  (partially addresses
  [\#38](https://github.com/fishR-Core-Team/FSA/issues/38)).
- [`lwCompPreds()`](https://fishr-core-team.github.io/FSA/reference/lwCompPreds.md):
  Modified. Changed to using
  [`withr::local_par()`](https://withr.r-lib.org/reference/with_par.html)
  (partially addresses
  [\#38](https://github.com/fishR-Core-Team/FSA/issues/38)).
- [`plotAB()`](https://fishr-core-team.github.io/FSA/reference/plotAB.md):
  Modified. Now in its own documentation file (rather than with
  [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md)).
- [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md):
  Modified. Better handled the situation where the user asks for
  summaries with some fish greater than stock size but no fish greater
  than quality size (addresses
  [\#50](https://github.com/fishR-Core-Team/FSA/issues/50); thanks to
  Timothy Spier for the bug report).
- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Modified. Added `method="Burhnam"` via the
  [\#51](https://github.com/fishR-Core-Team/FSA/pull/51) from Powell
  Wheeler.
- [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed to using
  [`withr::local_par()`](https://withr.r-lib.org/reference/with_par.html)
  (partially addresses
  [\#38](https://github.com/fishR-Core-Team/FSA/issues/38)).
- `SMBassWB`: Modified. Fixed minor data entry error in row 383.
- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Added `Francis3` to the list of models.

## FSA 0.8.20

CRAN release: 2018-05-18

- Added `asbio`, `DescTools`, `nlme`, and `psych` packages to Suggests
  because they are used in tests (and as will soon be required by CRAN …
  per an e-mail from CRAN on 17-May-18).
- Fixed a bunch of bad links to other packages in the documentation.
- Removed the “Date” field from the Description file.
- `addRadCap()`: Removed. Moved to `RFishBC` package.
- `bcFuns()`: Removed. Moved to `RFishBC` package.
- `gConvert()`: Removed. Moved to `RFishBC` package.
- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified. Fixed a bug that was related to
  [`poiCI()`](https://fishr-core-team.github.io/FSA/reference/poiCI.md)
  returning results from all four types. Now
  [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md)
  will use only one type. Thanks to Timothy Spiers for pointing out this
  bug.
- `SMBassWB`: Modified. Fixed minor data entry error in row 404. Changed
  link in documentation from `alr3` to `alr4` package.

## FSA 0.8.19

CRAN release: 2018-04-08

- [`addZeroCatch()`](https://fishr-core-team.github.io/FSA/reference/addZeroCatch.md):
  Modified. Changed two `1:nrow()` structures to `seq_len(nrow())`
  (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Changed all `1:` structures to
  [`seq_len()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Changed all `1:` structures to
  [`seq_len()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- `bcFuns()`: Modified. Changed three `1:length()` structures to
  [`seq_along()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`bootCase()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  methods: Modified. Changed all `1:` structures to
  [`seq_len()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`capHistConvert()`](https://fishr-core-team.github.io/FSA/reference/capHistConvert.md):
  Modified. Changed all `1:` structures to
  [`seq_len()`](https://rdrr.io/r/base/seq.html) or
  [`seq_along()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md):
  Modified. Changed all `1:` structures to
  [`seq_len()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`chooseColors()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed all `1:` structures to
  [`seq_len()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`compSlopes()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed all `1:` structures to
  [`seq_len()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`compIntercepts()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed all `1:` structures to
  [`seq_len()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`extraSS()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Changed all `1:` structures to
  [`seq_len()`](https://rdrr.io/r/base/seq.html) or
  [`seq_along()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`headtail()`](https://fishr-core-team.github.io/FSA/reference/headtail.md):
  Modified. Changed all `1:` structures to
  [`seq_len()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md)
  methods: Modified. Changed all `1:` structures to
  [`seq_len()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`htest.boot()`](https://fishr-core-team.github.io/FSA/reference/boot.md):
  Removed (added last version) until I can test more.
- [`iHndlFormula()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Now categorizes a character variable as a factor variable.
  This addresses
  [\#35](https://github.com/fishR-Core-Team/FSA/issues/35)) for
  [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md)
  and
  [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md).
- [`lrt()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Changed all `1:` structures to
  [`seq_len()`](https://rdrr.io/r/base/seq.html) or
  [`seq_along()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`lwCompPreds()`](https://fishr-core-team.github.io/FSA/reference/lwCompPreds.md):
  Modified. Changed all `1:` structures to
  [`seq_len()`](https://rdrr.io/r/base/seq.html) or
  [`seq_along()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified. Changed two `1:length()` structures to
  [`seq_along()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md)
  methods: Modified. Changed all `1:` structures to
  [`seq_len()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`plot.boot()`](https://fishr-core-team.github.io/FSA/reference/boot.md):
  Removed (added last version) until I can test more.
- [`predict.boot()`](https://fishr-core-team.github.io/FSA/reference/boot.md):
  Removed (added last version) until I can test more.
- [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md):
  Modified. Changed three `1:` structures to
  [`seq_len()`](https://rdrr.io/r/base/seq.html) or
  [`seq_along()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed three `1:length()` structures to
  [`seq_along()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md):
  Modified. Changed one `1:length()` structure to
  [`seq_along()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).
- [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md):
  Modified. Changed three `1:` structures to
  [`seq_len()`](https://rdrr.io/r/base/seq.html) or
  [`seq_along()`](https://rdrr.io/r/base/seq.html) (partially addressing
  [\#36](https://github.com/fishR-Core-Team/FSA/issues/36)).

## FSA 0.8.18

- **Date:** 31-Mar-18
- Changed to depending on `R >=3.2.0`, because that is the latest
  version required by a package (i.e., `car`) that FSA imports or
  suggests. Used the “check_r_versions_of_package_dependencies” shiny
  app by “ateucher” (on Github) to help determine this.
- Using latest `testthat` package.
- [`bootCase()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Added. This was added because
  [`bootCase()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  will soon be removed from the `car` package. It was added so that the
  code in the Introductory Fisheries Analyses with R book will still
  work. It is largely a wrapper to
  [`Boot()`](https://rdrr.io/pkg/car/man/Boot.html) in `car` with
  `method="case"`. The documentation was updated somewhat.
- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified. Changed the weighted regression method so that negative
  weights are set to zero rather than the minimum of the positive values
  (brought to my attention by Vaskar Nepal KC). Also added an
  [`rSquared()`](https://fishr-core-team.github.io/FSA/reference/rSquared.md)
  method (per request by Vaskar Nepal KC).
- [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  Modified. Added an
  [`rSquared()`](https://fishr-core-team.github.io/FSA/reference/rSquared.md)
  method.
- [`expandCounts()`](https://fishr-core-team.github.io/FSA/reference/expandCounts.md):
  Modified. Minor changes to documentation.
- `hTest.boot()`: Added.
- [`plot.boot()`](https://fishr-core-team.github.io/FSA/reference/boot.md):
  Added.
- [`plotAB()`](https://fishr-core-team.github.io/FSA/reference/plotAB.md):
  Modified. Added `col.numbers=` to allow users to modify the color of
  the numbers when `what="numbers"` is used (addresses
  [\#34](https://github.com/fishR-Core-Team/FSA/issues/34)).
- [`predict.boot()`](https://fishr-core-team.github.io/FSA/reference/boot.md):
  Added.
- [`psdPlot()`](https://fishr-core-team.github.io/FSA/reference/psdPlot.md):
  Modified. Minor changes to documentation and look of the function
  code.
- [`rSquared()`](https://fishr-core-team.github.io/FSA/reference/rSquared.md):
  Added from `NCStats`, but including a generic method so that it can be
  used for other models (e.g.,
  [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md)).
- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Switched `Fabens` and `Fabens2` parameterizations to better
  match `Wang` (i.e., increment model first). Added `Francis2`
  parameterization for tag-recapture data.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Fixed some spacing issues with the warnings when starting
  values for Linf was poorly estimated. Added an argument to
  `ivbStarts.LinfK()` to suppress checking the value of Linf. This
  argument reduces the change of double-printing the warning message
  when there are bad estimates of starting values for Linf and K.

## FSA 0.8.17

CRAN release: 2017-10-29

- [`dunnTest()`](https://fishr-core-team.github.io/FSA/reference/dunnTest.md):
  Modified. Adjusted code to handle the addition of `altp=` to and
  modified output from `dunn.test()` in `dunn.test`. Added additional
  tests and corrected some issues in the documentation.
- [`GompertzFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Fixed error in message (i.e., `msg=TRUE`) for
  `param="Ricker2"`.

## FSA 0.8.16

CRAN release: 2017-09-07

- Need to resubmit v0.8.15 to CRAN, so bumped the version.
- [`growthFunShow()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Fixed error in expression for `type="Logistic"` and
  `param="CampanaJones1"`.

## FSA 0.8.15

- **Date:** 6-Sep-17
- Added a script to the `helpers` directory that will test that all
  required packages are installed.
- `iAddOutlierTestResults()`: Modified. Fixed bug related to point
  labels in
  [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  when the data.frame for the original model had `NA` values.
- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Modified document by merging pull request
  [\#33](https://github.com/fishR-Core-Team/FSA/pull/33).
- [`srStarts()`](https://fishr-core-team.github.io/FSA/reference/srStarts.md):
  Modified. Added `fixed=`. Added some catches for poor starting values.
  Added relevant tests. Addresses
  [\#30](https://github.com/fishR-Core-Team/FSA/issues/30).

## FSA 0.8.14

CRAN release: 2017-07-27

- Moved `dunn.test` and `lmtest` to `imports` to help with portability
  for workshops.
- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Fixed bug in
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) so that the
  tick marks on the marginal histograms match the tick marks on the main
  plot. Changed the default `hist.panel.size=` in
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) so that it
  more reliably prints the values on the axes of the marginal
  histograms.
- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Modified. Added “warnings” for when all catches are zeroes (an object
  is still returned with all `NA`s). Thanks to Daniel Hanks for pointing
  out this issue.
- [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md):
  Modified. Fixed bug when `percZero!="always"` and there are no valid
  values such that the calculated percent of zeroes is `NA`.

## FSA 0.8.13

CRAN release: 2017-04-29

- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. A complete rebuild of `plot`. Major changes are to add
  [`plotAB()`](https://fishr-core-team.github.io/FSA/reference/plotAB.md)
  which is primarily used to make the “legacy” age bias plots of
  Campana, removal of the “sunflower” plot option, new sets of defaults
  for many of the arguments that reflect my preferences for visualizing
  age comparisons (which includes defaulting to plotting differences in
  ages), addition of the ability to add marginal histograms (`xHist=`,
  `yHist=`, `col.hist=`, and `hist.panel.size=`), better handling of
  axis ticks and labels (primarily to show ticks at integers and make
  sure 0 is included for differences), and allowing the ability to add
  “summary layers” to the main plot (see `allowAdd=`). Many examples
  were added. Some functionality from previous versions will be broken.
- [`capFirst()`](https://fishr-core-team.github.io/FSA/reference/capFirst.md):
  Modified. Changed some `if()`s with
  [`class()`](https://rdrr.io/r/base/class.html)es to
  [`inherits()`](https://rdrr.io/r/base/class.html).
- [`compIntercepts()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Replaced two [`dim()`](https://rdrr.io/r/base/dim.html)
  calls with [`nrow()`](https://rdrr.io/r/base/nrow.html).
- [`fact2num()`](https://fishr-core-team.github.io/FSA/reference/fact2num.md):
  Modified. Changed some `if()`s with
  [`class()`](https://rdrr.io/r/base/class.html)es to
  [`inherits()`](https://rdrr.io/r/base/class.html).
- [`lagratio()`](https://fishr-core-team.github.io/FSA/reference/lagratio.md):
  Modified. Changed some `if()`s with
  [`class()`](https://rdrr.io/r/base/class.html)es to
  [`inherits()`](https://rdrr.io/r/base/class.html).
- [`iHndlCols2UseIgnore()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Changed some `if()`s with
  [`class()`](https://rdrr.io/r/base/class.html)es to
  [`inherits()`](https://rdrr.io/r/base/class.html).
- [`iLegendHelp()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Changed some `if()`s with
  [`class()`](https://rdrr.io/r/base/class.html)es to
  [`inherits()`](https://rdrr.io/r/base/class.html).
- `iPredictBoot()`: Modified. Changed some `if()`s with
  [`class()`](https://rdrr.io/r/base/class.html)es to
  [`inherits()`](https://rdrr.io/r/base/class.html).
- [`is.CapHist()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md):
  Added.
- [`iTypeoflm()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Added a catch for a linear model that has a character
  variable (now alerts the user with a warning).
- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified. Changed some `if()`s with
  [`class()`](https://rdrr.io/r/base/class.html)es to
  [`is.CapHist()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md).
- [`mrOpen()`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md):
  Modified. Changed some `if()`s with
  [`class()`](https://rdrr.io/r/base/class.html)es to
  [`is.CapHist()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md).
- [`nlsTracePlot()`](https://fishr-core-team.github.io/FSA/reference/nlsTracePlot.md):
  Modified. Changed some `if()`s with
  [`class()`](https://rdrr.io/r/base/class.html)es to
  [`inherits()`](https://rdrr.io/r/base/class.html).
- [`perc()`](https://fishr-core-team.github.io/FSA/reference/perc.md):
  Modified. Changed some `if()`s with
  [`class()`](https://rdrr.io/r/base/class.html)es to
  [`inherits()`](https://rdrr.io/r/base/class.html).
- [`plotAB()`](https://fishr-core-team.github.io/FSA/reference/plotAB.md):
  Added. See description above for
  [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md).
- [`plotBinResp()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed how default transparency level is calculated and set
  the maximum transparency to 50 (changed from 500). Fixed bug in how
  the width of the proportions windows were calculated by default. These
  changes will affect
  [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  for logistic regression models.
- [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md):
  Modified. Changed some `if()`s with
  [`class()`](https://rdrr.io/r/base/class.html)es to
  [`inherits()`](https://rdrr.io/r/base/class.html).
- [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed default for `loess=` from `TRUE` to `FALSE`. Changed
  some `if()`s with [`class()`](https://rdrr.io/r/base/class.html)es to
  [`inherits()`](https://rdrr.io/r/base/class.html).
- [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md):
  Modified. Changed some `if()`s with
  [`class()`](https://rdrr.io/r/base/class.html)es to
  [`inherits()`](https://rdrr.io/r/base/class.html).

## FSA 0.8.12

CRAN release: 2017-03-12

- Lots of spelling corrections after running `devtools::spell_check()`.
- Cleaned up some issues in the testing files that were caused by a new
  version of `fishmethods` and changes to R v3.4.0.
- [`metaM()`](https://fishr-core-team.github.io/FSA/reference/metaM.md):
  Modified. Changed `T=` to `Temp=` to reduce potential for conflicts
  with `TRUE` abbreviation.
- [`reproInfo()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md):
  Modified. Added `ind=` to select a CRAN mirror to help with a common
  problem I have when knitting.
- [`srStarts()`](https://fishr-core-team.github.io/FSA/reference/srStarts.md):
  Modified. Corrected mis-spelling in directive to `FSAsim` package.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Added a catch that Linf cannot be automatically estimated
  with fewer than three ages. Corrected mis-spelling in directive to
  `FSAsim` package.

## FSA 0.8.11

CRAN release: 2016-12-13

- Changed all [`stop()`](https://rdrr.io/r/base/stop.html)s to
  [`STOP()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md)s
  and all [`warning()`](https://rdrr.io/r/base/warning.html)s to
  [`WARN()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md).
  This modified nearly all functions.
- Changed all [`paste()`](https://rdrr.io/r/base/paste.html)s that used
  `sep=""` to [`paste0()`](https://rdrr.io/r/base/paste.html)s.
- Removed several `sep=""`s from
  [`message()`](https://rdrr.io/r/base/message.html)s.
- Removed `Hmisc` from, but added `epitools` to, imports. Removed all
  links to `Hmisc` to remove CRAN check warnings.
- Reorganized testing files. Added many tests.
- [`.onAttach()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Streamlined package startup message.
- [`addZeroCatch()`](https://fishr-core-team.github.io/FSA/reference/addZeroCatch.md):
  Modified. Added more “catches” for bad data types or arguments.
- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Changed all
  [`message()`](https://rdrr.io/r/base/message.html)s in
  [`summary()`](https://rdrr.io/r/base/summary.html) to
  [`cat()`](https://rdrr.io/r/base/cat.html)s.
- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Changed all
  [`message()`](https://rdrr.io/r/base/message.html)s in
  [`summary()`](https://rdrr.io/r/base/summary.html) to
  [`cat()`](https://rdrr.io/r/base/cat.html)s.
- [`binCI()`](https://fishr-core-team.github.io/FSA/reference/binCI.md):
  Modified. Changed from using `binconf()` in `Hmisc` to
  `binom.exact()`, `binom.wilson()`, and `binom.approx()` from
  `epitools` (this removes dependency on `Hmisc` which was causing
  problems). Allowed multiple `type`s to be chosen. Now only accepts
  whole numbers for `x` and `n`. Added `verbose=` so that the result can
  include all of the information returned from the `epitools` functions.
  Added a catch for bad `conf.level`s. Added some more tests.
- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified. Made sure that [`coef()`](https://rdrr.io/r/stats/coef.html)
  method returned a vector (addresses
  [\#19](https://github.com/fishR-Core-Team/FSA/issues/19)). Modified
  [`confint()`](https://rdrr.io/r/stats/confint.html) code for
  efficiency, made sure matrix is always returned.
- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified. Made sure that [`coef()`](https://rdrr.io/r/stats/coef.html)
  method returned a vector (addresses
  [\#19](https://github.com/fishR-Core-Team/FSA/issues/19)). Modified
  [`confint()`](https://rdrr.io/r/stats/confint.html) code for
  efficiency, made sure matrix is always returned.
- [`chooseColors()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Added `rev=` for returning reverse ordered (from default)
  colors.
- [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  Modified. Changed [`coef()`](https://rdrr.io/r/stats/coef.html) method
  so that it returned a named vector (addresses
  [\#19](https://github.com/fishR-Core-Team/FSA/issues/19)). Modified
  [`confint()`](https://rdrr.io/r/stats/confint.html) code for
  efficiency, made sure matrix is always returned. Removed `type=` to
  match other functions (incorporated that functionality into `parm=`).
  Removed `digits=` to match other functions.
- [`expandLenFreq()`](https://fishr-core-team.github.io/FSA/reference/expandLenFreq.md):
  Modified. Changed all
  [`message()`](https://rdrr.io/r/base/message.html)s to
  [`cat()`](https://rdrr.io/r/base/cat.html)s. Removed “names” from
  printed items for a cleaner look.
- [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Added `cex.leg=` and `box.lty.leg=` to IVR plots.
- [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md):
  Modified. Fixed a bug with adding the horizontal line at 0 when the
  user uses `plot=FALSE`, which occurs with `hist.bootCase()`.
- [`hyperCI()`](https://fishr-core-team.github.io/FSA/reference/hyperCI.md):
  Modified. Now only accepts whole numbers for `M`, `n`, or `m`. Added
  catch for bad `conf.level`s and multiple values of `M`, `n`, or `m`.
- [`iAddLoessLine()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Changed used of
  [`iMakeColor()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md)
  to
  [`col2rgbt()`](https://fishr-core-team.github.io/FSA/reference/col2rgbt.md).
- [`iGetDecimals()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Added warning for situations where `x` will be presented in
  exponential notation. Also returned a decimal of zero in this
  situation. Helps with a bug in
  [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md).
- `iHndlCols2use()`: Deleted. Changed to
  [`iHndlCols2UseIgnore()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md).
- [`iHndlCols2UseIgnore()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Added. Previously was `iHndlCols2use()`. Completely reworked to catch
  more problems including having both positive and negative indices
  (fixes [\#24](https://github.com/fishR-Core-Team/FSA/issues/24)) and
  choosing variable names that don’t exist (fixes
  [\#25](https://github.com/fishR-Core-Team/FSA/issues/25)).
- [`iHndlFormula()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Added code to deal with a formula that is a single
  “variable” sent in an array. Addresses
  [\#21](https://github.com/fishR-Core-Team/FSA/issues/21) for the
  simple situation of single “variable.”
- [`iHndlMultWhat()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Added `type=` to allow use with
  [`message()`](https://rdrr.io/r/base/message.html) or
  [`cat()`](https://rdrr.io/r/base/cat.html).
- [`iPlotExists()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Added. Helps with bug fix in
  [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md).
- [`is.wholenumber()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Added. Needed for changes to
  [`binCI()`](https://fishr-core-team.github.io/FSA/reference/binCI.md),
  [`hyperCI()`](https://fishr-core-team.github.io/FSA/reference/hyperCI.md),
  and
  [`poiCI()`](https://fishr-core-team.github.io/FSA/reference/poiCI.md).
- [`kCounts()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md):
  Modified. Fixed bug with `capitalize=` and `zero`. Streamlined code.
  Added tests.
- [`metaM()`](https://fishr-core-team.github.io/FSA/reference/metaM.md):
  Modified. Fixed bug with the way messages were output when multiple
  `methods` were provided and `justM=FALSE`. Added more tests.
- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified. Added `poi.type=` to handle new choices for Poisson
  confidence interals. Added some checks for non-vector uses of `M=` and
  `R=` (partially addresses
  [\#22](https://github.com/fishR-Core-Team/FSA/issues/22)). Fixed bug
  in how inputs for subgroups were output from
  [`summary()`](https://rdrr.io/r/base/summary.html) when
  `verbose=TRUE`.
- [`mrOpen()`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md):
  Modified. Changed all
  [`message()`](https://rdrr.io/r/base/message.html)s in
  [`summary()`](https://rdrr.io/r/base/summary.html) to
  [`cat()`](https://rdrr.io/r/base/cat.html)s. Fixed bug where returned
  value from [`summary()`](https://rdrr.io/r/base/summary.html) was not
  a data.frame if only one parameter was selected.
- [`nlsTracePlot()`](https://fishr-core-team.github.io/FSA/reference/nlsTracePlot.md):
  Added.
- [`plotBinResp()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed used of
  [`iMakeColor()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md)
  to
  [`col2rgbt()`](https://fishr-core-team.github.io/FSA/reference/col2rgbt.md).
- [`poiCI()`](https://fishr-core-team.github.io/FSA/reference/poiCI.md):
  Modified. Completely rebuilt to use the functions from `epitools`. Now
  only accepts whole numbers for `x`.
- `predict.bootCase()`: Modified. Modified so that situations where
  other than values of the dependent variable are in the dots argument
  (as would occur if making predictions for the Francis parameterization
  of the VBGF).
- [`predict.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):
  Modified. See note for `predict.bootCase()`.
- [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md):
  Modified. Fixed bug in output if more than two additional lengths were
  supplied.
- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Modified. Added a check and a returned error if `method="Schnute"` and
  the last of three catches is zero (addresses
  [\#26](https://github.com/fishR-Core-Team/FSA/issues/26)) Fixed bug
  related to sending catches in a one column data.frame. Fixed bug
  related to selecting only one `parm=` in
  [`confint()`](https://rdrr.io/r/stats/confint.html). Added tests.
- [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Added `cex.leg=` and `box.lty.leg=` to IVR plots. Removed
  extra spaces in main title if `main="MODEL"`. Added some tests.
- [`tictactoe()`](https://fishr-core-team.github.io/FSA/reference/tictactoe.md):
  Modified. Changed used of
  [`iMakeColor()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md)
  to
  [`col2rgbt()`](https://fishr-core-team.github.io/FSA/reference/col2rgbt.md).
- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Added `Ogle` to list of parameterizations. Changed order of
  `L0` and `K` parameters in returned function when `param="Original"`.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Added the `methLinf=` argument that allows the user to
  choose if Linf is estimated from a Walford plot (`methLinf="Walford"`;
  the default and old functionality), as the mean of fish in a certain
  number of old ages (`methLinf="oldAge"`), or as the mean of a certain
  number of the longest fish (`methLinf="longFish"`). The number of ages
  or long fish is given in `num4Linf=`. Added methods for `type="Ogle"`.

## FSA 0.8.10

CRAN release: 2016-09-24

- [`alkIndivAge()`](https://fishr-core-team.github.io/FSA/reference/alkIndivAge.md):
  Modified. Added `na.rm=TRUE` to the checks on the minimum and maximum
  length data.
- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified. Removed `type=` and blended that functionality into `parm=`
  for methods. Made `parm=` consistent across methods.
- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified. Added `axis.age=` argument that allows the user to choose
  which type of x-axis is displayed (see examples; this addresses
  [\#20](https://github.com/fishR-Core-Team/FSA/issues/20)) Also
  modified code that adds the axes so that they should “look better” in
  more instances. Added `na.rm=TRUE` to y-range calculation for the plot
  method. Added a [`coef()`](https://rdrr.io/r/stats/coef.html) method.
  Added a `parm=` argument to the
  [`confint()`](https://rdrr.io/r/stats/confint.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html) methods. Added
  tests.
- [`confint.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md),`confint.bootCase()`.
  Modified. Result is now a matrix even if only one parameter is chosen
  (previously it was an unnamed vector). The `parm=` now properly
  handles negative values. Streamlined plotting results. Added tests.
- [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  Modified. Checked for bad `conf.level=` in
  [`confint()`](https://rdrr.io/r/stats/confint.html) method.
- [`GompertzFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Fixed bug related to selecting `QuinnDeriso3`.
- [`htest.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md),`htest.bootCase()`.
  Modified. The `b0` now defaults to 0. Matrix of results now include
  the parameter as the rowname. Modified the internals of how the data
  are handled. Added tests.
- [`iAddLoessLine()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Suppressed warnings related to the loess line predictions.
- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified. Checked for bad `conf.level=` in
  [`confint()`](https://rdrr.io/r/stats/confint.html) method.
- [`mrOpen()`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md):
  Modified. Checked for bad `conf.level=` in
  [`confint()`](https://rdrr.io/r/stats/confint.html) method.
- [`predict.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md),
  `predict.bootCase()`. Modified. The `...` argument can now contain a
  vector of values such that predictions can be made for multiple values
  of the independent variable. Modified the output matrix to handle this
  modification. Removed `MARGIN` as it will always be `1` for `nlsBoot`
  and `bootCase` objects. Added checks for `FUN=`, `conf.level=`, and
  `digits=`. Added tests.
- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Modified. Checked for bad `conf.level=` in
  [`confint()`](https://rdrr.io/r/stats/confint.html) method. Changed
  internal functions from using a loop to using
  [`apply()`](https://rdrr.io/r/base/apply.html). Changed internal
  functions from using [`log()`](https://rdrr.io/r/base/Log.html) and
  [`choose()`](https://rdrr.io/r/base/Special.html) to using
  [`lchoose()`](https://rdrr.io/r/base/Special.html).
- [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md):
  Modified. Added `nvalid=` and `percZero` to only print the nvalid and
  percZero result if they are “interesting” (i.e., different than n or
  zero, respectively) by default (may be manually over-ridden). Modified
  tests.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Added `na.rm=TRUE` to checking of Linf values.

## FSA 0.8.9

CRAN release: 2016-08-23

- `ageComparison()`: Modified. Removed an internal call to
  [`fact2num()`](https://fishr-core-team.github.io/FSA/reference/fact2num.md)
  because of changes to
  [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md)
  below. Should not impact user experience.
- [`diags()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Added.
- `gompertzFuns()`: Modified. Fixed some spacing around the message when
  `msg=TRUE`.
- [`logisticFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Fixed some spacing around the message when `msg=TRUE`.
- [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md):
  Modified. Removed all uses where the main variable was a factor (this
  functionality was largely unneeded and unused, was inelegant and
  difficult to maintain). Removed pass-through to `summary`. Removed
  warnings about the RHS variables being converted to factors. Columns
  for “levels” of the RHS variables are now returned in their original
  model (i.e., if the variable was numeric in the original data.frame it
  is now numeric in the data.frame returned from this function) – this
  should reduce need for using
  [`fact2num()`](https://fishr-core-team.github.io/FSA/reference/fact2num.md)
  when using the results of this function for variables that were
  originally numeric. Added more examples and tests for the numeric
  data.

## FSA 0.8.8

CRAN release: 2016-07-18

- [`growthFunShow()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Added Pauly et al. (1992) seasonal cessation function. Added
  `case=` for use with Schnute model.
- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Added Pauly et al. (1992) seasonal cessation function.
  Slightly modified messages for “Typical” and “Original”
  parameterizations.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Added `fixed=` so that the user can define some of the
  starting values. Added Pauly et al. (1992) seasonal cessation
  function. Added tests for `fixed=`.

## FSA 0.8.7

CRAN release: 2016-05-08

- Compiled under R v3.3.0.
- Removed `relax` from `Suggests`. See
  [`srStarts()`](https://fishr-core-team.github.io/FSA/reference/srStarts.md)
  and
  [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md)
  notes below. This addresses
  [\#17](https://github.com/fishR-Core-Team/FSA/issues/17).
- Removed `gdata` from `Imports`. See
  [`filterD()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  and
  [`Subset()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  notes below. This addresses
  [\#5](https://github.com/fishR-Core-Team/FSA/issues/5).
- Added no coverage blocks to `ageKeyPlot()`,
  [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md),
  [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md),
  [`histFromSum()`](https://fishr-core-team.github.io/FSA/reference/histFromSum.md),
  [`lwCompPreds()`](https://fishr-core-team.github.io/FSA/reference/lwCompPreds.md),
  `plot.agebias()`, `plot.CatchCurve()`, `plot.ChapmanRobson()`,
  `plot.Depletion()`,
  [`plotBinResp()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md),
  `print.compSlopes()`, `print.compIntercepts()`, `print.metaM()`,
  [`psdPlot()`](https://fishr-core-team.github.io/FSA/reference/psdPlot.md),[`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md),
  `srModels()`,
  [`srStarts()`](https://fishr-core-team.github.io/FSA/reference/srStarts.md),
  and
  [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md).
- `ageKey()`: Removed. Deprecated since 0.4.24. Use
  [`alkIndivAge()`](https://fishr-core-team.github.io/FSA/reference/alkIndivAge.md).
- `ageKeyPlot()`: Removed. Deprecated since 0.4.24. Use
  [`alkPlot()`](https://fishr-core-team.github.io/FSA/reference/alkPlot.md).
- `bcFuns()`: Modified. Changed `msg=` to `verbose=`.
- [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md):
  Added tests.
- [`filterD()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed to use
  [`droplevels()`](https://rdrr.io/r/base/droplevels.html) from `base`
  rather than `drop.levels()` from `gdata`. Added `except=`.
- [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed the way colors, plotting characters, and line types
  were handled for most of the models. Should make their use more
  flexible. Fixed errors that occurred in IVR models when the factor
  variable preceded the covariate in the model (fixes
  [\#18](https://github.com/fishR-Core-Team/FSA/issues/18)). Started to
  add tests for error and warning messages.
- [`GompertzFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Changed `type=` to `param=`.
- `GompertzModels()`: Removed. Replaced with
  [`growthFunShow()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md).
- [`iGetDecimals()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Fixed a bug that occured when an integer was provided.
- `lenFreqExpand()`: Removed. Deprecated since 0.4.32. Use
  [`expandLenFreq()`](https://fishr-core-team.github.io/FSA/reference/expandLenFreq.md).
- [`logisticFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Changed `type=` to `param=`.
- `LogisticModels()`: Removed. Replaced with
  [`growthFunShow()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md).
- [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed the way colors, plotting characters, and line types
  were handled for most of the models. Should make their use more
  flexible. Now matches coding in
  [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md).
  Fixed bug with main titling, but now asks user to decide if they want
  the model call or not. Started to add tests for error and warning
  messages.
- [`RichardsFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Changed `type=` to `param=`.
- `RichardsModels()`: Removed. Replaced with
  [`growthFunShow()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md).
- [`growthFunShow()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Added.
- [`srFunShow()`](https://fishr-core-team.github.io/FSA/reference/srFuns.md):
  Added.
- `srModels()`: Removed. Replaced with
  [`srFunShow()`](https://fishr-core-team.github.io/FSA/reference/srFuns.md).
- [`srStarts()`](https://fishr-core-team.github.io/FSA/reference/srStarts.md):
  Modified. Removed `dynamicPlot=TRUE` option. Moved it to `FSAsim`
  package. Modified plot when `plot=TRUE` by adding “STARTING VALUES” to
  title and moving starting values to within the plot. Added `cex.main=`
  and `col.main=`.
- [`Subset()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  MOdified. Changed to use
  [`droplevels()`](https://rdrr.io/r/base/droplevels.html) from `base`
  rather than `drop.levels()` from `gdata`.
- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Changed `type=` to `param=`.
- `vbModels()`: Removed. Replaced with
  [`growthFunShow()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md).
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Removed `dynamicPlot=TRUE` option. Moved it to `FSAsim`
  package. Added `param=` to match other `vbXXX()` (works as does
  `type=`). Modified plot when `plot=TRUE` by adding “STARTING VALUES”
  to title and moving starting values to within the plot. Added and
  `col.main=`. Made warnings and error tests more explicit.

## FSA 0.8.6

CRAN release: 2016-03-25

- Fixed problems with tests, and made the tests more explicit, related
  to PSD and Wr functions. Suppressed some warnings related to
  [`sumTable()`](https://fishr-core-team.github.io/FSA/reference/sumTable.md)
  in ALK related tests and
  [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md)
  in age comparisons tests. Prompted by forthcoming changes to
  `testthat`.
- Removed `News.md` from `.Rbuildignore` (apparently now supported by
  CRAN).
- [`alkPlot()`](https://fishr-core-team.github.io/FSA/reference/alkPlot.md):
  Modified. Changed so that `xlim=` and `ylim=` would work when
  `type="area"` and `type="bar"`. This fixes
  [\#10](https://github.com/fishR-Core-Team/FSA/issues/10) (Thanks to
  Joseph Feldhaus).
- [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md):
  Modified. Added the `breaks=` argument (mostly a pass-through) and the
  `w=` argument that allows the user to just set the width of the bins
  without having to set each
  [`break`](https://rdrr.io/r/base/Control.html) value. This should
  complete [\#15](https://github.com/fishR-Core-Team/FSA/issues/15).
- `iCheckStartCatW()`: Modified. Now use
  [`iGetDecimals()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md)
  to extract the number of decimals in `startcat` and `w`.
- `iCheckStartcat()`: Added.
- `iCheckW()`: Added.
- [`iGetDecimals()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Added.
- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  Modified. Changed order of `startcat=` and `breaks=`. Slight
  modifications to documentation.
- [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md):
  Modified. Minor changes to documentation.
- [`psdPlot()`](https://fishr-core-team.github.io/FSA/reference/psdPlot.md):
  Modified. Fixed bug related to PSD values being printed when only
  PSD-Q existed (needed to add `drop0Est=FALSE` to the
  [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md)
  call; this fixes
  [\#13](https://github.com/fishR-Core-Team/FSA/issues/13)). Made the
  histogram bars flush with the x-axis rather than hovering above it
  (added `yaxis="i"` to
  [`hist()`](https://rdrr.io/r/graphics/hist.html); this fixes
  [\#12](https://github.com/fishR-Core-Team/FSA/issues/12)). Minor
  changes to documentation.
- [`psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md):
  Modified. Minor changes to documentation.
- [`purl2()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md):
  Modified. Added `delHeader=` argument and functionality.

## FSA 0.8.5

CRAN release: 2016-02-14

- Added URL for fishR webpage in DESCRIPTION per CRAN request. Removed
  it from the URL field in DESCRIPTION.

- Updated all references to Ogle (2016) in documentation.

- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Minor corrections to the documentation.

- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Fixed bug related to computations of percent agreement when
  `NA` values were present. There was an inconsistency between when
  `what="precision"` and `what="difference"` was used in
  [`summary()`](https://rdrr.io/r/base/summary.html). The bug fix now
  properly divides by the “valid sample size” for `what="precision"`.
  This fixes [\#9](https://github.com/fishR-Core-Team/FSA/issues/9)
  (Thanks to Joseph Feldhaus). Now returns `validn`. Modifications to
  the documentation.

- [`histFromSum()`](https://fishr-core-team.github.io/FSA/reference/histFromSum.md):
  Added. Addresses
  [\#4](https://github.com/fishR-Core-Team/FSA/issues/4).

- [`metaM()`](https://fishr-core-team.github.io/FSA/reference/metaM.md):
  Modified. Changed order of methods in `methods=`. Minor corrections
  and additions to documentation.

- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified. Now sends warning if an `NA` appears in the first position
  of `m`, the first position of `M`, or the last position of `R` and
  converts these to 0 so that the procedure can continue. Each of these
  positions is ignored in the calculations. This fixes
  [\#8](https://github.com/fishR-Core-Team/FSA/issues/8) (Thanks to Joe
  Mrnak).

- [`reproInfo()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md):
  Modified. Made changes to `iGetAllDependencis()` based on forthcoming
  changes to `package.dependencies()` (as notified by CRAN).

- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Fixed bug when `dynamicPlot=TRUE` was used.

## FSA 0.8.4

CRAN release: 2015-12-21

- Now using Roxygen2 v5.0.1.
- Removed some `requireNamespaces()` from some functions and moved those
  packages from `Suggests` to `Imports` so that those functions would
  work better with other packages. The only `requireNamespaces()` that
  remain are related to functions that require the `relax` package (so
  tcltk is not installed until needed) and `knitr`, `dunn.test`, and
  `lmtest` as these are unlikely to be used by other packages and will
  keep the packages that are loaded with `FSA` to a minimum. Packages
  moved from `Suggests` to `Depends` are `Hmisc` (for use in `binCI`),
  `gdata` (for use in
  [`filterD()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  and
  [`Subset()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)),
  `dplyr` (for use in
  [`filterD()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)),
  `sciplot` (for use in
  [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)),
  `car` (for use in
  [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)),
  and `gplots` (for use with colors).
- [`addZeroCatch()`](https://fishr-core-team.github.io/FSA/reference/addZeroCatch.md):
  Modified tests (to reduce warnings that were not part of tests).
- [`geomean()`](https://fishr-core-team.github.io/FSA/reference/geomean.md):
  Added.
- [`geosd()`](https://fishr-core-team.github.io/FSA/reference/geomean.md):
  Added.
- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  Modified. Fixed a bug related to using a `tbl_df` object.
- [`sumTable()`](https://fishr-core-team.github.io/FSA/reference/sumTable.md):
  Modified tests (but with
  [`dimnames()`](https://rdrr.io/r/base/dimnames.html)).

## FSA 0.8.3

CRAN release: 2015-10-23

- Removed vignetteBuilder from DESCRIPTION (remnant from a vignette I
  built and then removed) at request of CRAN.

## FSA 0.8.2

- **Date:** 22-Oct-15
- Converted all files in `data-raw` to CSV files.
- Removed all `\href{}{}` and `\url{}` codes to websites that I don’t
  control. The addresses are now “naked” such that the user will need to
  copy-and-paste them into a browser to view the web page rather than
  clicking on a hyper link. Hopefully this will eliminate problems with
  R CMD CHECK.
- `ChinookArg`: Updated help documentation.
- `Ecoli`: Added a Topics section.
- `Mirex`: Added a Topics section.
- `PikeNYPartial1`: Updated help documentation.
- `SpotVA1`: Updated help documentation.

## FSA 0.8.1

CRAN release: 2015-10-10

- [`col2rgbt()`](https://fishr-core-team.github.io/FSA/reference/col2rgbt.md):
  Added.
- [`compIntercepts()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Added.
- [`compSlopes()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Added.

------------------------------------------------------------------------

## FSA 0.8.0 8

CRAN release: 2015-10-08

- Added suggests for `dunn.test` for use in
  [`dunnTest()`](https://fishr-core-team.github.io/FSA/reference/dunnTest.md)
  (see below).
- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Changed [`combn()`](https://rdrr.io/r/utils/combn.html) to
  [`utils::combn()`](https://rdrr.io/r/utils/combn.html) and
  [`sd()`](https://rdrr.io/r/stats/sd.html) to `utils::sd()` (within an
  [`apply()`](https://rdrr.io/r/base/apply.html)).
- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified. Changed
  [`na.exclude()`](https://rdrr.io/r/stats/na.fail.html) to
  [`stats::na.exclude()`](https://rdrr.io/r/stats/na.fail.html).
- [`dunnTest()`](https://fishr-core-team.github.io/FSA/reference/dunnTest.md):
  Modified. Changed to more throughly use `dunn.test()` from
  `dunn.test`. Added the `two.sided=` argument to
  [`dunnTest()`](https://fishr-core-team.github.io/FSA/reference/dunnTest.md)
  and `dunn.test.results=` to
  [`print.dunnTest()`](https://fishr-core-team.github.io/FSA/reference/dunnTest.md).
- [`expandLenFreq()`](https://fishr-core-team.github.io/FSA/reference/expandLenFreq.md):
  Modified. Changed [`runif()`](https://rdrr.io/r/stats/Uniform.html) to
  [`stats::runif()`](https://rdrr.io/r/stats/Uniform.html).
- [`extraSS()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Changed [`anova()`](https://rdrr.io/r/stats/anova.html) to
  [`stats::anova()`](https://rdrr.io/r/stats/anova.html) in an
  [`lapply()`](https://rdrr.io/r/base/lapply.html).
- [`fishR()`](https://fishr-core-team.github.io/FSA/reference/fishR.md):
  Modified. Changed
  [`browseURL()`](https://rdrr.io/r/utils/browseURL.html) to
  [`utils::browseURL()`](https://rdrr.io/r/utils/browseURL.html).
- [`fsaNews()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed
  [`browseURL()`](https://rdrr.io/r/utils/browseURL.html) to
  [`utils::browseURL()`](https://rdrr.io/r/utils/browseURL.html).
- [`headtail()`](https://fishr-core-team.github.io/FSA/reference/headtail.md):
  Modified. Changed [`head()`](https://rdrr.io/r/utils/head.html) to
  [`utils::head()`](https://rdrr.io/r/utils/head.html) and
  [`tail()`](https://rdrr.io/r/utils/head.html) to
  [`utils::tail()`](https://rdrr.io/r/utils/head.html).
- `hist.bootCase()`: Modified. Changed
  [`hist()`](https://rdrr.io/r/graphics/hist.html) to
  [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md).
- `iAgeBiasPlot()`: Modified. Changed
  [`grconvertY()`](https://rdrr.io/r/graphics/convertXY.html) to
  [`graphics::grconvertY()`](https://rdrr.io/r/graphics/convertXY.html).
- `iALKMean.QD()`: Modified. Changed
  [`var()`](https://rdrr.io/r/stats/cor.html) to
  [`stats::var()`](https://rdrr.io/r/stats/cor.html) (within
  [`sumTable()`](https://fishr-core-team.github.io/FSA/reference/sumTable.md)).
- `iBubblesAdd()`: Modified. Changed
  [`rgb()`](https://rdrr.io/r/grDevices/rgb.html) to `grdevices::rgb()`.
- `iChkComplexModel()`: Modified. Changed
  [`df.residual()`](https://rdrr.io/r/stats/df.residual.html) to
  [`stats::df.residual()`](https://rdrr.io/r/stats/df.residual.html).
- `iCIboot()`: Modified. Changed
  [`hist()`](https://rdrr.io/r/graphics/hist.html) to
  [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md).
- `iEvent2Indiv()`: Modified. Changed
  [`unstack()`](https://rdrr.io/r/utils/stack.html) to
  [`utils::unstack()`](https://rdrr.io/r/utils/stack.html).
- `iGetAllDependencies()`: Modified. Changed
  [`installed.packages()`](https://rdrr.io/r/utils/installed.packages.html)
  to
  [`utils::installed.packages()`](https://rdrr.io/r/utils/installed.packages.html).
- `iHistResids()`: Modified. Removed
  [`graphics::hist()`](https://rdrr.io/r/graphics/hist.html) and changed
  to
  [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md).
- [`iHndlFormula()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Changed [`terms()`](https://rdrr.io/r/stats/terms.html) to
  [`stats::terms()`](https://rdrr.io/r/stats/terms.html).
- `iHndlResidType()`: Modified. Changed
  [`rstandard()`](https://rdrr.io/r/stats/influence.measures.html) to
  [`stats::rstandard()`](https://rdrr.io/r/stats/influence.measures.html)
  and [`rstudent()`](https://rdrr.io/r/stats/influence.measures.html) to
  [`stats::rstudent()`](https://rdrr.io/r/stats/influence.measures.html).
- `iHtestBoot()`: Modified. Removed
  [`graphics::hist()`](https://rdrr.io/r/graphics/hist.html) and changed
  to
  [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md).
- [`iMakeColor()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Changed [`rgb()`](https://rdrr.io/r/grDevices/rgb.html) to
  `grdevices::rgb()`.
- `iMakeModelHeading()`: Modified. Changed
  [`formula()`](https://rdrr.io/r/stats/formula.html) to
  [`stats::formula()`](https://rdrr.io/r/stats/formula.html) (within an
  [`lapply()`](https://rdrr.io/r/base/lapply.html)).
- `iMoran()`: Modified. Changed
  [`optimize()`](https://rdrr.io/r/stats/optimize.html) to
  `grdevices::optimize()`.
- `iProcessSessionInfo()`: Modified. Changed `sessioninfo()` to
  `utils::sessioninfo()`.
- `iSchnute()`: Modified. Changed
  [`optimize()`](https://rdrr.io/r/stats/optimize.html) to
  `grdevices::optimize()`.
- [`iTypeoflm()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Changed [`formula()`](https://rdrr.io/r/stats/formula.html)
  to [`stats::formula()`](https://rdrr.io/r/stats/formula.html).
- `plot.AgeBias()`: Modified. Changed
  [`rgb()`](https://rdrr.io/r/grDevices/rgb.html) to `grdevices::rgb()`.
- `plot.CatchCurve()`: Modified. Changed
  [`predict()`](https://rdrr.io/r/stats/predict.html) to
  [`stats::predict()`](https://rdrr.io/r/stats/predict.html).
- [`print.extraTest()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Changed `printCoefMat()` to `stats::printCoefMat()`.
- [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md):
  Modified. Changed [`data()`](https://rdrr.io/r/utils/data.html) to
  [`utils::data()`](https://rdrr.io/r/utils/data.html).
- [`psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md):
  Modified. Changed [`data()`](https://rdrr.io/r/utils/data.html) to
  [`utils::data()`](https://rdrr.io/r/utils/data.html).
- [`reproInfo()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md):
  Modified. Changed
  [`graphics.off()`](https://rdrr.io/r/grDevices/dev.html) to
  `grdevices::graphics.off()`.
- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Modified. Removed some of the examples from the help page to reduce
  the elapsed time for CRAN.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Changed [`rgb()`](https://rdrr.io/r/grDevices/rgb.html) to
  `grdevices::rgb()`.
- [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md):
  Modified. Changed [`data()`](https://rdrr.io/r/utils/data.html) to
  [`utils::data()`](https://rdrr.io/r/utils/data.html).
- [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md):
  Modified. Changed [`data()`](https://rdrr.io/r/utils/data.html) to
  [`utils::data()`](https://rdrr.io/r/utils/data.html).
- `test_AgeLengthKey`: Modified. Altered tests that had used `==` to use
  `expect_equivalent()` which uses
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html) with
  `check.attributes=FALSE`.
- `test_PSD`: Modified. Altered tests that had used `==` to use
  `expect_equivalent()` which uses
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html) with
  `check.attributes=FALSE`.

## FSA 0.7.11

- **Date:** Oct15
- Converted all `.txt` files to `.Rda` files. Original `.txt` files are
  in the `data-raw` directory which was added to `.Rbuildignore`.

## FSA 0.7.10

- **Date:** Oct15
- [`purl2()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md):
  Added `newname=` to allow the output file to have a name other than
  the same as the intput file.
- [`reproInfo()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md):
  Added `markdown` to the `out=` types.

## FSA 0.7.9

- **Date:** Sep15
- Updated `README.md` and `DESCRIPTION` for new websites.
- Changed all references to the WordPress site to the new website.
  Removed links to specific IFAR chapters. Changed my e-mail address.
  Created link in references to IFAR book page.
- [`fishR()`](https://fishr-core-team.github.io/FSA/reference/fishR.md):
  Modified. Updated for the new websites.

## FSA 0.7.8

- **Date:** Sep15
- `ageComparison()`: Modified. Changed `what="McNemars"` and
  `what="Bowkers"` to `what="McNemar"` and `what="Bowker"`. Fixed bug if
  all ages are `NA`.
- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified. Fixed bug related to `NA` values in the catch vector.
- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified. Fixed bug related to `NA` values in the catch vector.
- [`validn()`](https://fishr-core-team.github.io/FSA/reference/validn.md):
  Modified. Fixed bug related to when a 1-dimensional numeric vector was
  not recognized as a vector.

## FSA 0.7.7

- **Date:** Aug15
- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Changed default for `pch.mean=` to 95 (from 175). If `what=`
  has only one item, then results will now be invisibly returned so that
  results can be saved to an object.
- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Added `trunc.diff=`. If `what=` has only one item, then
  results will now be invisibly returned so that results can be saved to
  an object.
- [`mapvalues()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Corrected to export properly.
- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Modified. Minor edits to labels if `verbose=TRUE`. Added some more
  tests.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Made `yngAge` the default for `meth0=`. Fixed bug that
  occured when `meth0='yngAge'` and sample sizes at all ages were 1.

## FSA 0.7.6

- **Date:** Aug15
- [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md):
  Modified. Converted to using
  [`iHndlFormula()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md).
  Changed output for quantitative data (`validn` is always returned,
  `NAs` is never returned). Changed output for two-way factor data (not
  returned as a character from
  [`formatC()`](https://rdrr.io/r/base/formatc.html)). Removed `...`
  from code in several places as it was buggy and not used. Added more
  checks and modified check messages. Fixed bug from when a 1-d matrix
  of characters was sent. Added tests.
- [`sumTable()`](https://fishr-core-team.github.io/FSA/reference/sumTable.md):
  Modified. Converted to using
  [`iHndlFormula()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md).
  Added tests.

## FSA 0.7.5

- **Date:** Aug15
- `addRadCap()`: Modified. Streamlined code. Changed default `in.pre=`
  to `NULL` (from `inc`). Added some tests for returned data.
- `BluegillLM`: Removed. Moved to `FSAdata`.
- `gConvert()`: Modified. Streamlined code. Changed `type=` to
  `out.type=`. Changed default `in.pre=` and `in.var=` to `NULL` (from
  missing). Changed code to handle changes in `in.pre=` and `in.var=`.
  Added some tests for returned data.
- `gReshape()`: Removed. Moved to `FSAmisc`.

## FSA 0.7.4

- **Date:** Aug15
- [`binCI()`](https://fishr-core-team.github.io/FSA/reference/binCI.md):
  Modified. Check for`Hmisc` with `requireNamespaces()` before
  processing body of function. This allowed moving `Hmisc` into
  `Suggests` declarations rather than `Imports`.
- [`chooseColors()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Check for`gplots` with `requireNamespaces()` before
  processing body of function. This allowed moving `gplots` into
  `Suggests` declarations rather than `Imports`.
- [`filterD()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Check for `dplyr` and `gdata` with `requireNamespaces()`
  before processing body of function. This allowed moving `dplyr` and
  `gdata` into `Suggests` declarations rather than `Imports`.
- [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Check for`sciplot` with `requireNamespaces()` before adding
  intervals tot he plot. This allowed moving `sciplot` into `Suggests`
  declarations rather than `Imports`.
- [`lrt()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Check for`lmtest` with `requireNamespaces()` before
  processing body of function. This allowed moving `lmtest` into
  `Suggests` declarations rather than `Imports`.
- [`mapvalues()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed so that it is a direct `importFrom` and `export`
  without creating a help file in `FSA.`
- [`purl2()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md):
  Modified. Check for`knitr` with `requireNamespaces()` before
  processing body of function. This allowed moving `knitr` into
  `Suggests` declarations rather than `Imports`.
- [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Check for`car` with `requireNamespaces()` before
  highlighting outliers on the plot. This allowed moving `sciplot` into
  `Suggests` declarations rather than `Imports`.
- [`srStarts()`](https://fishr-core-team.github.io/FSA/reference/srStarts.md):
  Modified. Check for`relax` with `requireNamespaces()` before
  constructing the dynamic plot. This allowed moving `relax` into
  `Suggests` declarations rather than `Imports`.
- [`Subset()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Check for`gdata` with `requireNamespaces()` before
  processing body of function. This allowed moving `gdata` into
  `Suggests` declarations rather than `Imports`.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Check for`relax` with `requireNamespaces()` before
  constructing the dynamic plot. This allowed moving `relax` into
  `Suggests` declarations rather than `Imports`.

## FSA 0.7.3

- **Date:** Aug15
- Removed all `importFrom()` directives and went to hard-wiring to
  packages with `::`. Added `imports()` directives for `stats`,
  `graphics`, `tools`, and `grDevices`. Removed `imports()` directive
  for `multcomp()`.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Changed default methos for `methEV=`. Changed order of
  starting values for `type="Mooij"` in order to match that from
  [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md).
  This also fixed a bug when `dynamicPlot=TRUE` was used with
  `type="Mooij"`. Added tests to determine if parameter order is the
  same between
  [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md)
  and
  [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md)
  for all parameterizations.

## FSA 0.7.2

- **Date:** Jul15
- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Corrected bug with labeling of x-axis on age-bias plot when
  `ref.lab=` and `nref.lab=` were not given by the user. Changed default
  for `nYpos=` from `1.1` to `1.03`. Added `cex.n=` to allow control of
  the size of the sample size labels.
- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Changed `what="detail"` to `what="details"`. Note that
  `what="detail"` still works.
- [`dunnTest()`](https://fishr-core-team.github.io/FSA/reference/dunnTest.md):
  Modified. Added a note to the help file about the use of complete
  cases. Suggested from Paule Bodson-Clermont.
- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Added `Original` and `Typical` to the `type=` options. This
  allows both a capitalized and uncapitalized version for these two
  parameterizations.
- `vbModels()`: Modified. Changed order of `Original` and `Typical`
  (`Typical` is now shown first). Fixed in error in how the equation for
  the `Weisberg` parameterization was displayed.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Added `cex.main=` as an argument and defaulted to `0.75`.
  Added `raw=TRUE` to [`poly()`](https://rdrr.io/r/stats/poly.html)
  which is used when `meth0="poly"`. Added `Original` and `Typical` to
  the `type=` options. This allows both a capitalized and uncapitalized
  version for these two parameterizations.

## FSA 0.7.1

- **Date:** Jul15
- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Moved into a single file with
  [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md).
  Cleaned-up help file. No change in behavior.
- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Moved into a single file with
  [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md).
  Cleaned-up help file. No change in behavior.
- [`alkAgeDist()`](https://fishr-core-team.github.io/FSA/reference/alkAgeDist.md):
  Modified. Moved into a single file with
  [`alkMeanVar()`](https://fishr-core-team.github.io/FSA/reference/alkMeanVar.md).
  Cleaned-up help file. Added some error/warning tests. No change in
  behavior.
- [`alkIndivAge()`](https://fishr-core-team.github.io/FSA/reference/alkIndivAge.md):
  Modified. Clean-up help file. Added a stronger test for `type="SR"`
  method. No change in behavior.
- [`alkMeanVar()`](https://fishr-core-team.github.io/FSA/reference/alkMeanVar.md):
  Modified. Moved into a single file with
  [`alkAgeDist()`](https://fishr-core-team.github.io/FSA/reference/alkAgeDist.md).
  Cleaned-up help file. No change in behavior.
- [`alkPlot()`](https://fishr-core-team.github.io/FSA/reference/alkPlot.md):
  Cleaned-up help file and tests. No change in behavior.
- `confint.bootCase()`: Modified. Created a common internal function for
  use with
  [`confint.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md).
  Added the ability to plot histograms with confidence intervals
  superimposed (similar to what was in
  [`confint.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md)).
- [`confint.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):
  Modified. Created a common internal function for use with
  `confint.bootCase()`.
- [`GompertzFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Moved into `growthModels` file. Did not change behavior.
- `gompertzModels()`: Modified. Changed `type=` to `family=` to avoid
  confusion in the help file with `type=` in `gompertzFuns()`. Moved
  into `growthModels` file.
- `growthRadPlot()`: Deleted. Moved to `FSAmisc` package.
- [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md):
  Modified. Fixed so that `ymax=` also sets the y-axis limit when only
  one histogram is made (it was previously ignored).
- `htest.bootCase()`: Modified. Created a common internal function for
  use with
  [`htest.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md).
- [`htest.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):
  Modified. Created a common internal function for use with
  `htest.bootCase()`.
- [`iHndlFormula()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Added `na.action=NULL` to
  [`model.frame()`](https://rdrr.io/r/stats/model.frame.html) so that NA
  values will not be omitted.
- [`logisticFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Moved into `growthModels` file. Did not change behavior.
- `logisticModels()`: Modified. Changed `type=` to `family=` to avoid
  confusion in the help file with `type=` in
  [`logisticFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md).
  Moved into `growthModels` file.
- `predict.bootCase()`: Modified. Created a common internal function for
  use with
  [`predict.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md).
- [`predict.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):
  Modified. Created a common internal function for use with
  `predict.bootCase()`.
- [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md):
  Modified. Fixed bug that never tested if all lengths were `NA`.
  Required change to `iHndlFormula`.
- [`RichardsFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Moved into `growthModels` file. Changed `param=` to `type=` to better
  match similar functions.
- `RichardsModels()`: Moved into `growthModels` file. Did not change
  behavior.
- [`Schnute()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Moved into `growthModels` file. Changed `param=` to `type=` to better
  match similar functions.
- `SchnuteModels()`: Moved into `growthModels` file. Did not change
  behavior.
- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Moved into `growthModels` file. Did not change behavior.
- `vbModels()`: Modified. Changed `type=` to `family=` to avoid
  confusion in the help file with `type=` in
  [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md).
  Moved into `growthModels` file.

## FSA 0.7.0

- **Date:** Jul15
- Fixed description to be in title case.
- Fixed reference to fishR page in description file.
- Fixed several URL references, deleted others that have changed and are
  no longer available.
- Updated CITATION file (to remove CRAN note).
- [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md):
  Modified. Rebuilt to use
  [`iHndlFormula()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md).
  Modified how `xlab=` is used (result is the same). Added some tests.
- [`ksTest()`](https://fishr-core-team.github.io/FSA/reference/ksTest.md):
  Modified. Rebuilt to use
  [`iHndlFormula()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md).
  Added some tests for messages and to make sure results matched
  [`ks.test()`](https://rdrr.io/r/stats/ks.test.html).

## FSA 0.6.25

- **Date:** Jul15
- `alkPrep()`: Deleted. Moved to `FSAmisc` package (on GitHub).
- `changesPos()`: Deleted. Moved to `FSAmisc` package (on GitHub).
- `chapmanPlot()`: Deleted. Moved to `FSAmisc` package (on GitHub).
- `dietOverlap()`: Deleted. Moved to `FSAmisc` package (on GitHub).
- `Garvey1`: Deleted. Moved to `FSAmisc` package (on GitHub).
- `Garvey4a`: Deleted. Moved to `FSAmisc` package (on GitHub).
- `KS2D_NR`: Deleted. Moved to `FSAmisc` package (on GitHub).
- `ks2d1()`: Deleted. Moved to `FSAmisc` package (on GitHub).
- `ks2d1p()`: Deleted. Moved to `FSAmisc` package (on GitHub).
- `ks2d2()`: Deleted. Moved to `FSAmisc` package (on GitHub).
- `ks2d2p()`: Deleted. Moved to `FSAmisc` package (on GitHub).
- `popSizesPlot()`: Deleted. Moved to `FSAmisc` package (on GitHub).
- `posadj()`: Deleted. Moved to `FSAmisc` package (on GitHub).
- `TroutDietSL`: Deleted. Moved to `FSAmisc` package (on GitHub).
- `walfordPlot()`: Deleted. Moved to `FSAmisc` package (on GitHub).

## FSA 0.6.24

- **Date:** Jun15
- [`alkIndivAge()`](https://fishr-core-team.github.io/FSA/reference/alkIndivAge.md):
  Modified. Switched to using
  [`iHndlFormula()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md)
  at the beginning. Added more checks and tests.
- [`expandLenFreq()`](https://fishr-core-team.github.io/FSA/reference/expandLenFreq.md):
  Modified. Added more checks. Added some tests.
- [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md):
  Modified. Added more tests.

## FSA 0.6.23

- **Date:** Jun15
- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Fixed bugs related to axes on numbers plot and sunflower
  plot.
- [`filterD()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Added `reorder=FALSE` to `drop.levels()` so that the order
  of levels is not changed when levels are dropped.
- `residPlot.nlme()`: Added.

## FSA 0.6.22

- **Date:** Jun15
- [`extraSS()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Added `sim.name=` to allow for a common typing mistake.
- [`logbtcf()`](https://fishr-core-team.github.io/FSA/reference/logbtcf.md):
  Modified. Slight change to handle a check of `lm` class.
- [`lrt()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Added `sim.name=` to allow for a common typing mistake.
- [`pcumsum()`](https://fishr-core-team.github.io/FSA/reference/rcumsum.md):
  Modified. Modified to handle `table`, `matrix`, and `data.frame`
  classes as long as they are 1-dimensional.
- [`rcumsum()`](https://fishr-core-team.github.io/FSA/reference/rcumsum.md):
  Modified. Modified to handle `table`, `matrix`, and `data.frame`
  classes as long as they are 1-dimensional.
- [`srStarts()`](https://fishr-core-team.github.io/FSA/reference/srStarts.md):
  Modified. Corrected some bugs related to checks. Added more tests.

## FSA 0.6.21

- **Date:** Jun15
- `addRadCap()`: Modified. Modified so that `in.pre=` string must be at
  the start of the variable names. Added a check for when the `in.pre=`
  string does not exist at the start of any variable names. Added a
  check for whether all `in.var=` variables exist. Added some simple
  tests (need more).
- `bcFuns()`: Modified. Removed `type=`; `BCM=` can now be either
  numeric or a string. Allowed string to be in any case (will be
  converted to the required all upper-case). Corrected some errors for
  when `msg=TRUE`. Added some simple tests.
- `gConvert()`: Modified. Modified so that `in.pre=` string must be at
  the start of the variable names. Added a check for when the `in.pre=`
  string does not exist at the start of any variable names. Added some
  simple tests (need more).
- `gReshape()`: Modified. Modified so that `in.pre=` string must be at
  the start of the variable names. Added a check for when the `in.pre=`
  string does not exist at the start of any variable names. Added some
  simple tests (need more).

## FSA 0.6.20

- **Date:** Jun15
- `gompFuns()`: Deleted.
- `gompModels()`: Deleted.
- [`GompertzFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Added. Replaced `gompFuns()`. Added `type="Troynikov1"` and
  `type="Troynikov1"`.
- `GompertzModels()`: Added. Replaced `gompModels()`. Added `cex=` and
  `type=`.
- [`logisticFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Changed `type="Richards"` to `type="Karkach"`. Added
  `type=HaddonI`.
- `logisticModels()`: Modified. Added “Karkach” model. Added `cex=` and
  `type=`.
- [`RichardsFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Added two more parameterizations from Tjorve and Tjorve
  (2010).
- `schnute()`: Deleted.
- `schnuteModels()`: Deleteed.
- [`Schnute()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Added. Replaced `schnute()`. Fixed bugs with the way `t1=` and `t3=`
  are handled.
- `SchnuteModels()`: Added. Replaced `schnuteModels()`. Added `cex=`.
- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Added `type="Polacheck"` which is equivalent to
  `type="Laslett"`. Added a new reference in the help file.
- `vbModels()`: Modified. Added `cex=` and `type=`.

## FSA 0.6.19

- **Date:** Jun15
- [`RichardsFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Added.
- `RichardsModels()`: Added.

## FSA 0.6.18

- **Date:** Jun15
- Changed nearly all “messages” using
  [`cat()`](https://rdrr.io/r/base/cat.html) to using
  [`message()`](https://rdrr.io/r/base/message.html) so that they can be
  suppressed with `suppressMessage()` or `message=FALSE` in knitr. See
  “One comment on messages” at <http://yihui.name/knitr/demo/output/>.
  Specific functions modified are listed below.
- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Changed all [`cat()`](https://rdrr.io/r/base/cat.html) to
  [`message()`](https://rdrr.io/r/base/message.html)s. Changed so that
  messages (result headers) are only printed if `what=` contains more
  than one item.
- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Changed all [`cat()`](https://rdrr.io/r/base/cat.html) to
  [`message()`](https://rdrr.io/r/base/message.html)s. Changed so that
  messages (result headers) are only printed if `what=` contains more
  than one item.
- `bcFuns()`: Modified. Changed all
  [`cat()`](https://rdrr.io/r/base/cat.html) to
  [`message()`](https://rdrr.io/r/base/message.html)s.
- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified. Changed all [`cat()`](https://rdrr.io/r/base/cat.html) to
  [`message()`](https://rdrr.io/r/base/message.html)s. Deleted
  “Estimates with Standard Errors” message.
- [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  Modified. Changed all [`cat()`](https://rdrr.io/r/base/cat.html) to
  [`message()`](https://rdrr.io/r/base/message.html)s.
- `dietOverlap()`: Modified. Changed all
  [`cat()`](https://rdrr.io/r/base/cat.html) to
  [`message()`](https://rdrr.io/r/base/message.html)s. Slightly modified
  messages.
- [`expandLenFreq()`](https://fishr-core-team.github.io/FSA/reference/expandLenFreq.md):
  Modified. Changed all [`cat()`](https://rdrr.io/r/base/cat.html) to
  [`message()`](https://rdrr.io/r/base/message.html)s. Fixed bug with
  one of the messages.
- `ks2d1()`: Modified. Rewrote the
  [`print()`](https://rdrr.io/r/base/print.html) method. This removed a
  number of [`cat()`](https://rdrr.io/r/base/cat.html)s.
- `ks2d1p()`: Modified. Rewrote the
  [`print()`](https://rdrr.io/r/base/print.html) method. This removed a
  number of [`cat()`](https://rdrr.io/r/base/cat.html)s.
- `ks2d2()`: Modified. Rewrote the
  [`print()`](https://rdrr.io/r/base/print.html) method. This removed a
  number of [`cat()`](https://rdrr.io/r/base/cat.html)s.
- `ks2d2p()`: Modified. Rewrote the
  [`print()`](https://rdrr.io/r/base/print.html) method. This removed a
  number of [`cat()`](https://rdrr.io/r/base/cat.html)s.
- [`metaM()`](https://fishr-core-team.github.io/FSA/reference/metaM.md):
  Modified. Changed all [`cat()`](https://rdrr.io/r/base/cat.html) to
  [`message()`](https://rdrr.io/r/base/message.html)s.
- [`mrOpen()`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md):
  Modified. Changed all [`cat()`](https://rdrr.io/r/base/cat.html) to
  [`message()`](https://rdrr.io/r/base/message.html)s.
- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Modified. Changed all [`cat()`](https://rdrr.io/r/base/cat.html) to
  [`message()`](https://rdrr.io/r/base/message.html)s.
- [`srFuns()`](https://fishr-core-team.github.io/FSA/reference/srFuns.md):
  Modified. Changed all [`cat()`](https://rdrr.io/r/base/cat.html) to
  [`message()`](https://rdrr.io/r/base/message.html)s. Created some
  tests.

## FSA 0.6.17

- **Date:** Jun15
- [`extraSS()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Added more message tests and some calculational tests
  (compared to [`anova()`](https://rdrr.io/r/stats/anova.html) results).
- `gompFuns()`: Modified. Changed all
  [`cat()`](https://rdrr.io/r/base/cat.html)s to
  [`message()`](https://rdrr.io/r/base/message.html)s and slightly
  modified the messages. Fixed minor bugs in some created functions.
  Created some tests.
- [`logisticFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Changed all [`cat()`](https://rdrr.io/r/base/cat.html)s to
  [`message()`](https://rdrr.io/r/base/message.html)s and slightly
  modified the messages. Fixed minor bugs in some created functions.
  Created some tests.
- [`lrt()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Added more message tests and some calculational tests
  (compared to `lrtest()` from `lmtest` package results).
- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Changed all [`cat()`](https://rdrr.io/r/base/cat.html)s to
  [`message()`](https://rdrr.io/r/base/message.html)s and slightly
  modified the messages. Fixed minor bugs in some created functions.
  Created some tests.

## FSA 0.6.16

- **Date:** Jun15
- [`extraSS()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Added `sim_names=` and `com_name=` so that simple
  descriptive names could be given to the model and printed in the
  heading of the output. Added checks for whether the complex model
  appears more complex or not. Added tests for warning and error
  messages.
- [`fishR()`](https://fishr-core-team.github.io/FSA/reference/fishR.md):
  Modfiied. Fixed bug with `where="news"`. Added tests.
- [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Added ability to modify y-axis limits for the nonlinear
  regression model. Thanks to Gabriela N. for asking for this.
- [`hoCoef()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed `lmobj=` to `object=`, added degrees-of-freedom to
  the output matrix, streamlined the code, added some checks, and added
  some tests.
- [`lrt()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Added `sim_names=` and `com_name=` so that simple
  descriptive names could be given to the model and printed in the
  heading of the output. Added checks for whether the complex model
  appears more complex or not. Added tests for warning and error
  messages.

## FSA 0.6.15

- **Date:** Jun15
- [`addZeroCatch()`](https://fishr-core-team.github.io/FSA/reference/addZeroCatch.md):
  Modified. Deleted extraneous
  [`print()`](https://rdrr.io/r/base/print.html) statement.
- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  Modified. Major re-write to make it easier to trouble-shoot. Fixed bug
  related to empty category on end when `as.fact=TRUE` and
  `use.names=TRUE`. Added more tests.
- [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md):
  Modified. Removed extra open-ended category (e.g., PSD-T-) for PSD
  intervals.

## FSA 0.6.14

- **Date:** May15
- Added travis-ci integration.
- Added coveralls integration.
- Added `importFrom` for
  [`mapvalues()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  from `plyr`.
- `changesPos()`: Modified. Added some checks with error messages. Added
  suite of tests.
- [`chooseColors()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Added some checks with error messages. Added suite of tests.
- [`confint.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):Modified.
  Fixed bug if maximum number in `parm=` was greater than the number of
  parameters in the model.
- [`dunnTest()`](https://fishr-core-team.github.io/FSA/reference/dunnTest.md):
  Modified. Change class type from `DunnTest` to `dunnTest` to eliminate
  conflict with `DunnTest()` in `DescTools` package. Thanks to Sal
  Mangiafico for pointing out this conflict.
- [`fact2num()`](https://fishr-core-team.github.io/FSA/reference/fact2num.md):
  Modified. Added some checks with error messages. Added suite of tests.
- [`filterD()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed to using `drop.levels` from `gdata` rather than
  `droplevels`. Added a warning if the resultant data.frame has zero
  rows (same as in `Subset`). Added some checks with error messages.
  Added suite of tests.
- [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Replaced the use of
  [`nobs()`](https://rdrr.io/r/stats/nobs.html) from `gdata` in the
  internal function `iCIfp1()` with
  [`validn()`](https://fishr-core-team.github.io/FSA/reference/validn.md).
  This removed one dependency on `gdata`.
- [`headtail()`](https://fishr-core-team.github.io/FSA/reference/headtail.md):
  Modified. Internally remove `tbl_df` class (from `dplyr`) if it
  exists. Added some checks with error messages. Added suite of tests.
- [`lagratio()`](https://fishr-core-team.github.io/FSA/reference/lagratio.md):
  Modified. Corrected incorrect explanation of `differences=` in help
  file. Added `recursion=` and `direction=`. Added suite of tests.
- [`mapvalues()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Deleted. Deleted the function from `FSA` but imported it from `plyr`
  and then exported it from `FSA` so that it would be available to
  fisheries users without having to load `plyr`.
- `oddeven()`: Modified. Added some checks with error messages. Added
  suite of tests.
- [`perc()`](https://fishr-core-team.github.io/FSA/reference/perc.md):
  Modified. Added ability to use “and equals” or not to the items in
  `dir=` (i.e., there are now four items in `dir=`). Fixed a bug related
  to using `na.rm=FALSE` and a “less than” situation. Added some checks
  with error messages. Added suite of tests.
- [`pcumsum()`](https://fishr-core-team.github.io/FSA/reference/rcumsum.md):
  Modified. Added some checks with error messages. Added suite of tests.
- [`rcumsum()`](https://fishr-core-team.github.io/FSA/reference/rcumsum.md):
  Modified. Added some checks with error messages. Added suite of tests.
- [`se()`](https://fishr-core-team.github.io/FSA/reference/se.md):
  Modified. Added some checks with error messages. Added suite of tests.
- [`Subset()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Added some checks with error messages. Added suite of tests.
- [`validn()`](https://fishr-core-team.github.io/FSA/reference/validn.md):
  Modified. Added some checks with error messages. Added suite of tests.

## FSA 0.6.13

- **Date:** May15
- Some miscellaneous reorganizations of files.
- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Corrected bugs with `show.pts=TRUE` and “sunflower plot”
  that came from changes made in version 0.5.1.
- [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Deleted `student=`. Added `resid.type=` which allows used of
  standardized (internally studentized) and (externally) studentized
  residuals for linear models (along with raw residuals). Added code
  following
  [`nlsResiduals()`](https://rdrr.io/pkg/nlstools/man/nlsResiduals.html)
  from `nlstools` for standardized residuals for nonlinear models.

## FSA 0.6.12

- **Date:** May15
- `gompFuns()`: Added.
- `gompModels()`: Added.
- [`logisticFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Added.
- `logisticModels()`: Added.
- [`reproInfo()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md):
  Modified. Added the `out=` argument to allow the output to be straight
  R or LaTeX. Removed the `listFiles=` argument. Changed the output to
  be more succinct. Streamlined the code.
- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Fixed a bug with the Laslett model.

## FSA 0.6.11

- **Date:** Apr15
- [`kCounts()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md):
  Added. Was `swvCounts()`.
- [`kPvalue()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md):
  Added. Was `swvPvalue()`.
- [`purl2()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md):
  Added. Was `swvCode()`. Added `timestamp=` argument for adding a
  timestamp to the created script.
- [`reproInfo()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md):
  Added. Was `swvFinish()`.
- `swvANOVA()`: Deleted. Moved to `NCStats`.
- `swvCode()`: Deleted. Changed to
  [`purl2()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md).
- `swvCounts()`: Deleted. Changed to
  [`kCounts()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md).
- `swvFinish()`: Deleted. Changed to
  [`reproInfo()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md).
- `swvGLHT()`: Deleted. Moved to `NCStats`.
- `swvHtest()`: Deleted. Moved to `NCStats`.
- `swvPvalue()`: Deleted. Changed to
  [`kPvalue()`](https://fishr-core-team.github.io/FSA/reference/knitUtil.md).
- `swvREG()`: Deleted. Moved to `NCStats`.

## FSA 0.6.10

- **Date:** Apr15
- Compiling under R 3.2.0.
- Added some cross-reference links to help files.
- Remove fishR vignette section and added IFAR Chapter section to help
  files.
- [`fishR()`](https://fishr-core-team.github.io/FSA/reference/fishR.md):
  Modified. Added `IFAR` as an option. Updated code to be more simple.

## FSA 0.6.5

- **Date:** Apr15
- Last version for submission of first draft of Introductory Fisheries
  Analyses with R.
- [`capHistConvert()`](https://fishr-core-team.github.io/FSA/reference/capHistConvert.md):
  Modified. Added a warning section and an example of problems that can
  occur if the data are in event format but the event variable contains
  unused levels as may occur following subsetting. Thanks to Joseph
  Feldhaus for pointing out this problem.
- [`extraSS()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Changed algorithm to determine if the models were of the
  same class or not. The modification allows a model to have multiple
  classes.
- `iHndlCols2Use` (Intrnal Function): Modified. Fixed bug with how the
  columns were selected. Added a suite of tests for this function. This
  will fix bugs in
  [`capHistConvert()`](https://fishr-core-team.github.io/FSA/reference/capHistConvert.md)
  and
  [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md).
  Thanks to Joseph Feldhaus for pointing out this egregious error.
- [`lrt()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Changed algorithm to determine if the models were of the
  same class or not. The modification allows a model to have multiple
  classes.

## FSA 0.6.4

- **Date:** Apr15
- Changed to using `LazyData: true`.
- [`se()`](https://fishr-core-team.github.io/FSA/reference/se.md):
  Added. Removed `importFrom` of
  [`se()`](https://fishr-core-team.github.io/FSA/reference/se.md) from
  `sciplot`.

## FSA 0.6.3

- **Date:** Apr15
- Some modifications to tests.
- `plot.capHist()`: Modified. Changed default plot look which can now be
  controlled with `pch=`, `cex.pch=`, and `lwd=`. Modified the two
  y-axis scales to use `plotmath` characters.

## FSA 0.6.2

- **Date:** Mar15
- [`capHistConvert()`](https://fishr-core-team.github.io/FSA/reference/capHistConvert.md):
  Modified. Streamlined code around creating `var.lbls`. Made `event`
  the default value for `var.lbls.pre=`. Added some checks to
  `var.lbls.pre=` if it starts with a number or has too many values.
  Added `cols2use=` and modified use of `cols2ignore=` via
  `iHndlCols2use()`.
- [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md):
  Modified.
- `iHndlCol2use()`: Added. Added this internal function to handle
  `cols2use=` and `cols2ignore=` in
  [`capHistConvert()`](https://fishr-core-team.github.io/FSA/reference/capHistConvert.md)
  and
  [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md).

## FSA 0.6.1

- **Date:** Mar15
- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified. Changed how `ages2use=` was handled so that negative values
  can be used to exclude some ages. Will also now send an error if a mix
  of positive and negative ages are sent in `ages2use=`. Better handled
  the situation where `ages2use=` had more ages than the `age` variable.
  Checked for non-positive weights if `weighted=TRUE` and returned a
  warning and changed the non-positive weights to the minimum of the
  positive weights.
- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified. Changed how `ages2use=` was handled so that negative values
  can be used to exclude some ages. Will also now send an error if a mix
  of positive and negative ages are sent in `ages2use=`. Better handled
  the situation where `ages2use=` had more ages than the `age` variable.
- [`expandCounts()`](https://fishr-core-team.github.io/FSA/reference/expandCounts.md):
  Modified. Changed so that \`\`message()’’s are printed at the end
  instead of along the way. This reduces confusion of what appear to be
  messages of success followed by an error. Thanks to Dan Oele bringing
  this confusion to my attention.
- [`plotBinResp()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed the way the breaks were calculated (uses
  [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md)
  now).

## FSA 0.6.0

- **Date:** Mar15
- updated DESCRIPTION file (following this –
  [http://r-pkgs.had.co.nz/description.html](http://r-pkgs.had.co.nz/description.md)
- [`srFuns()`](https://fishr-core-team.github.io/FSA/reference/srFuns.md):
  Modified. Changed function returned when `simplify=FALSE` so that if
  the parameters are named that the name is dropped. Thus, when the
  function is used, the returned result will not be annoyingly named as
  the first parameter. Added functionality for the
  “density-independence” model.
- [`srStarts()`](https://fishr-core-team.github.io/FSA/reference/srStarts.md):
  Modified. Added functionality for the “density-independence” model.
- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Changed function returned when `simplify=FALSE` so that if
  the parameters are named that the name is dropped. Thus, when the
  function is used, the returned result will not be annoyingly named as
  the first parameter.

## FSA 0.5.3

- **Date:** Mar15
- `growthModelSim()`: Deleted. The simulation functionality was moved to
  the `FSAsim` package. The functionality related to finding starting
  values for the von Bertalanffy modesl was moved to
  [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md).
- [`srFuns()`](https://fishr-core-team.github.io/FSA/reference/srFuns.md):
  Modified. A complete rebuild to make similar to
  [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md).
  Added `simple=`. Added `type='Shepherd'` for the Shepherd (1982) three
  parameter model and `type='SailaLorda'` for the “Saila-Lorda” three
  parameter model from Iles (1994). Added tests for error messages.
- `srModels()`: Modified. A complete rebuild to make similar to
  [`growthModels()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md).
  Added “Shepherd” and “Saila-Lorda” models.
- `srSims()`: Deleted. The simulation functionality was moved to the
  `FSAsim` package. The functionality related to finding starting values
  was moved to
  [`srStarts()`](https://fishr-core-team.github.io/FSA/reference/srStarts.md).
- [`srStarts()`](https://fishr-core-team.github.io/FSA/reference/srStarts.md):
  Modified. A complete rebuild to streamline. Removed default method
  (i.e., a formula must be used now). Added “Shepherd” and “Saila-Lorda”
  models. Modified plotting routine, including adding `col.mdl=`,
  `lwd.mdl=`, and `lty.mdl=`. Moved the dynamic modeling aspects of
  `srSim()` into this function and is called with the new argument
  `dynamicPlot=TRUE`. Also added `minmax.ratio=` and `delta.prop=` for
  use with the dynamic plots.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. A complete rebuild to streamline and fix some bugs that had
  not been found. Modified plotting routine, including adding
  `col.mdl=`, `lwd.mdl=`, and `lty.mdl=`. Also added all of the von
  Bertalanffy parameterizations in `growthModelSim()` into this function
  and is called with the new argument `dynamicPlot=TRUE`. Added dynamics
  plots for the “Francis” and “Schnute” parameterizations.

## FSA 0.5.2

- **Date:** Mar15
- [`psdPlot()`](https://fishr-core-team.github.io/FSA/reference/psdPlot.md):
  Modified. Fixed bug related to `NA`s in `max.brks` variable.

## FSA 0.5.1

- **Date:** Mar15
- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Reversed the order of the formula … it is now
  `nrefvar~refvar`. This more closely matches other R functions where
  the tilde may be interpreted as the word “by”. In other words, the
  formula now reads as “nonreference variable by reference variable”
  (i.e., Y by X). Thanks for Richard McBride for the suggestion.
  Modified the age-bias plot extensively … added `sfrac=` and defaulted
  to 0 to remove ends of the confidence intervals, added `cex.mean=` to
  control the size of the symbol for the mean point, added `lwd=` that
  will controland set all of the `lwd` defaults to 1.
- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Changed all “CV” results to “ACV”.

## FSA 0.4.51

- **Date:** Mar15
- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified. Updated the help file regarding `zmethod="Smithetal"`.

## FSA 0.4.50

- **Date:** Mar15
- [`capFirst()`](https://fishr-core-team.github.io/FSA/reference/capFirst.md):
  Modified. Added a check to make sure the inputted object was either a
  character or factor class. Added code to return the object as the same
  class of the original object.
- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  Modified. Added a catch for bad choices of arguments. Added a catch to
  send a warning if the vector contains all `NA` values (this happens
  when
  [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md)
  is used within a loop or as part of
  [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md)).
  Added tests for error and warning messages. Changed how the formula
  was handled in the formula method.
- [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md):
  Modified. Fixed a bug with names when using labels. Added `verbose=`.
  Added catches and sent messages if `verbose=TRUE` for when no
  Gabelhouse lengths are know for a species and if the lengths for a
  species are all missing (see note for
  [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md)
  above).
- `PSDlit`: Modified. Fixed the trophy length for White Bass (from 15 to
  18). This solved a bug related to non-unique breaks.

## FSA 0.4.49

- **Date:** Mar15
- [`expandCounts()`](https://fishr-core-team.github.io/FSA/reference/expandCounts.md):
  Modified. Made message regarding rows with zero counts more useful.
  Added missing counts to the catch of zero counts. Made changes to
  handle more “odd” data entries (see “details” in the help file). Made
  some tests. Added some tests.

## FSA 0.4.48

- **Date:** Mar15
- [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md):
  Modified. Corrected “bug” with `units=`. Also modified warning message
  when no “stock” fish were present in the data.frame to further note
  what `units=` were used (i.e., this problem is likely to happen if the
  data is inches but the user uses the default `units='mm'`). Thanks
  to S. Mather for inspring this fix.

## FSA 0.4.47

- **Date:** Feb15
- [`dunnTest()`](https://fishr-core-team.github.io/FSA/reference/dunnTest.md):
  Modified. Corrected “bug” in the order that the groups are subtracted
  (i.e., they were flipped).

## FSA 0.4.46

- **Date:** Feb15
- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified. Changed default for `pos.est=` to `topright`. Added
  `cex.pos=` (and set default to slightly smaller value).
- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified. Changed default for `pos.est=` to `topright`. Added
  `cex.pos=` (and set default to slightly smaller value).

## FSA 0.4.45

- **Date:** Feb15
- [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md):
  Modified. Changed use of
  [`par()`](https://rdrr.io/r/graphics/par.html) to eliminate
  modifications to the gridding of plots after the function is complete.
  Also removed the setting of `mar=` and `mgp=` in
  [`par()`](https://rdrr.io/r/graphics/par.html).
- [`mrOpen()`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md):
  Modified. Removed pretty printing for
  [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`confint()`](https://rdrr.io/r/stats/confint.html) methods. These got
  in the way of being able to
  [`cbind()`](https://rdrr.io/r/base/cbind.html) the results together
  for a succinct display.
- [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed use of
  [`par()`](https://rdrr.io/r/graphics/par.html) to eliminate
  modifications to the gridding of plots after the function is complete.

## FSA 0.4.44

- **Date:** Feb15
- [`.onAttach()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. Centered the message and adjusted for different lengths of
  version numbers.
- [`alkPlot()`](https://fishr-core-team.github.io/FSA/reference/alkPlot.md):
  Modified. Fixed bug when using `add=TRUE` with `type="bubble"`.
- [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md):
  Modified. Changed to return
  [`par()`](https://rdrr.io/r/graphics/par.html) options to what they
  were before the function was called.
- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified. Changed
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) to default to
  slighly lighter colored dots and a black line.
- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified. Changed
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) to default to
  slighly lighter colored dots. Changed to return
  [`par()`](https://rdrr.io/r/graphics/par.html) options to what they
  were before the function was called.
- `growthModelSim()`: Modified. Changed to return
  [`par()`](https://rdrr.io/r/graphics/par.html) options to what they
  were before the function was called.
- `growthRadPlot()`: Modified. Changed to return
  [`par()`](https://rdrr.io/r/graphics/par.html) options to what they
  were before the function was called.
- [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md):
  Modified. Changed to return
  [`par()`](https://rdrr.io/r/graphics/par.html) options to what they
  were before the function was called.
- [`lwCompPreds()`](https://fishr-core-team.github.io/FSA/reference/lwCompPreds.md):
  Modified. Changed `quant.lens=` to `qlens=`. Changed default `qlens=`
  to have the 5th and 95th percentiles rather than the minimum and
  maximum values. Added `qpens.dec=` so that the user could control the
  number of decimals for the lengths derived from `qlens=`.
- `srSim()`: Modified. Changed to return
  [`par()`](https://rdrr.io/r/graphics/par.html) options to what they
  were before the function was called.

## FSA 0.4.43

- **Date:** Feb15
- [`mrOpen()`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md):
  Modified. Changed [`summary()`](https://rdrr.io/r/base/summary.html)
  and [`confint()`](https://rdrr.io/r/stats/confint.html) methods to
  allow single, multiple, or all choices of parameters to return results
  for. Also added code to print the results more prettily.
- `swvCode()`: Modified. Fixed bug related to `blanks='extra'`.

## FSA 0.4.42

- **Date:** Feb15
- [`filterD()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Added.

## FSA 0.4.41

- **Date:** Jan15
- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified. Removed the use of larger points in the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).
- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified. Removed the use of larger points in the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).
- [`metaM()`](https://fishr-core-team.github.io/FSA/reference/metaM.md):
  Modified. Deleted `group=` (and created `method="ZhangMegreyD"` and
  `method="ZhangMegreyP"`). Added geometric mean regresson methods for
  Hoenig. Changed default for `justM=` to `TRUE`. Fixed several minor
  bugs from the original implementation. Added some checks for
  reasonableness of some arguments. Created tests for several methods to
  see if the results matched those from Kenchington (2014). Added code
  to compute with several methods at once.
- [`Mmethods()`](https://fishr-core-team.github.io/FSA/reference/metaM.md):
  Added. Added as a function and removed as a vector.

## FSA 0.4.40

- **Date:** Jan15
- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  Modified. Fixed a bug that occurred if `breaks=` were given but the
  vector contained `NA`s. Thanks to Ben Neely for pointing this out.

## FSA 0.4.39

- **Date:** Jan15
- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified. Changed `use.weights=` to `weighted=`. Added some checks for
  the formula in the formula version and for the variables in the
  default version. Add unit tests for warnings and errors and tow tests
  for values.
- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified. Added the `method="Smithetal"` methodology for estimating
  the SE of Z (and made it the default). Added some checks for the
  formula in the formula version and for the variables in the default
  version. Added `verbose=` to
  [`summary()`](https://rdrr.io/r/base/summary.html). Add unit tests for
  warnings and errors and two tests for values.

## FSA 0.4.38

- **Date:** Jan15
- [`alkPlot()`](https://fishr-core-team.github.io/FSA/reference/alkPlot.md):
  Modified. Changed behavior for adding a legend to alleviate a bug.
- [`metaM()`](https://fishr-core-team.github.io/FSA/reference/metaM.md):
  Added.

## FSA 0.4.37

- **Date:** Jan15
- [`confint.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):
  Modified. Changed default for `err.col=` to `black` from `red`. Fixed
  example due to changes in `nlsBoot` package.
- [`extraSS()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Added a catch to make sure all models are of the same type.
  Added a catch to note that the function does not work with other that
  [`lm()`](https://rdrr.io/r/stats/lm.html) or
  [`nls()`](https://rdrr.io/r/stats/nls.html) models. Fixed a bug
  related to the labels for results from
  [`anova()`](https://rdrr.io/r/stats/anova.html) being different
  depending on whether [`lm()`](https://rdrr.io/r/stats/lm.html) or
  [`nls()`](https://rdrr.io/r/stats/nls.html) models were given. Added
  some examples.
- [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md):
  Modified. Fixed bug (originated in last version) that nothing was
  returned when only one histogram was constructed.
- [`lrt()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Changed call to `lrtest()` to a call to `lrtest.default()`.
  Added a catch to make sure all models are of the same type. Note that
  degrees-of-freedom from `lrtest()` are not error df; thus, modified to
  report error df to match
  [`extraSS()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md).
  Added some examples.

## FSA 0.4.36

- **Date:** Jan15
- [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md):
  Modifiied. Added `iaxs=`, which when set to the default value of
  `TRUE` will use `xaxs="i"` and `yaxs="i"` to remove the “floating”
  x-axis produced by [`hist()`](https://rdrr.io/r/graphics/hist.html) in
  base R.
- [`lwCompPreds()`](https://fishr-core-team.github.io/FSA/reference/lwCompPreds.md):
  Modified. Added the `yaxs=` argument.
- [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md):
  Modified. Added `showIntermediate=` to allow showing intermediate
  values in the calculation of the PSD indices. Added `justAdds=` to
  allow the user to return just those results that pertain to the values
  in `addLens=`. Added ability to use a named vector in `addLens=` and
  then not use `addNames=`. Changed `digits=1` to `digits=0`. Thanks to
  Ben Neely for the suggestions.
- [`psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md):
  Modified. Added ability to use a named vector in `addLens=` and then
  not use `addNames=`. The original functionality is still there. Added
  a check that one of the Gabelhouse lengths is not also one of the
  `addLens=` values. Deleted the `addLens=` value if it was (the user
  might have sent a name with this value and will want that name to
  appear in the results).
- [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modiifed. Added `xpd=TRUE` to the loess line routine so that the curve
  and polygon would stay within the plotting region.
- [`tictactoe()`](https://fishr-core-team.github.io/FSA/reference/tictactoe.md):
  Modified. Add the ability to handle differences between when
  `xaxs="r"` and `yaxs="r"` are used and when `xaxs="i"` and `yaxs="i"`
  are used.

## FSA 0.4.35

- **Date:** Jan15
- [`dunnTest()`](https://fishr-core-team.github.io/FSA/reference/dunnTest.md):
  Added.

## FSA 0.4.34

- **Date:** Dec14
- [`addZeroCatch()`](https://fishr-core-team.github.io/FSA/reference/addZeroCatch.md):
  Modified. Removed `idvar=`, forced the `eventvar=` and `speciesvar=`
  variables in the returned data.frame to be numeric if they were
  numeric in the original data.frame, allowed `speciesvar=` to have more
  than one variable, and added `na.rm=`. Multiple values for `specvar=`
  will allow the user to add zeros based on a combination of variables
  (e.g., species and size category). The `na.rm=` argument allows the
  user to remove “missing” species, which are common if some sampling
  events did not capture any fish.

## FSA 0.4.33

- **Date:** Dec14
- `growthModelSim()`: Modified. Changed all “K0” objects to “t50”.
- [`headtail()`](https://fishr-core-team.github.io/FSA/reference/headtail.md):
  Added.
- [`logbtcf()`](https://fishr-core-team.github.io/FSA/reference/logbtcf.md):
  Added.
- [`lwCompPreds()`](https://fishr-core-team.github.io/FSA/reference/lwCompPreds.md):
  Modified. Added `base=` to allow the function to work with logarithms
  to a different base. The original function was hard-wired to only use
  natural logarithms. Updated the examples and the tests.
- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Changed all “K0” objects to “t50”.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Changed all “K0” objects to “t50”.

## FSA 0.4.32

- **Date:** Nov14
- `expandcounts()`: Added (from `fishWiDNR` package).
- [`expandLenFreq()`](https://fishr-core-team.github.io/FSA/reference/expandLenFreq.md):
  Added. Same as `lenFreqExpand()` but thought that this name fits
  better with
  [`expandCounts()`](https://fishr-core-team.github.io/FSA/reference/expandCounts.md).
- [`pcumsum()`](https://fishr-core-team.github.io/FSA/reference/rcumsum.md):
  Added.
- [`rcumsum()`](https://fishr-core-team.github.io/FSA/reference/rcumsum.md):
  Modified. Completely new code (much simpler).
- [`validn()`](https://fishr-core-team.github.io/FSA/reference/validn.md):
  Added.

## FSA 0.4.31

- **Date:** Nov14
- Removed the suggests for `plyr`.
- [`addZeroCatch()`](https://fishr-core-team.github.io/FSA/reference/addZeroCatch.md):
  Modified. Slight modifications to help file. Fixed bug related to
  error checking the number of variables. Added some tests.
- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md)
  Modified. Added `droplevels=` and kept `drop.levels=` as I could not
  consistently remember what the name of the argument was – i.e., the
  user can use either one, but `droplevels=` is preferred.
- [`mapvalues()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Added. This is the exact same function from the `plyr` package.
  Included here to minimize conflicts between functions in `dplyr` and
  `plyr` that have the same name (i.e., don’t have to install `plyr`
  just for
  [`mapvalues()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  when also using `dplyr`).
- [`perc()`](https://fishr-core-team.github.io/FSA/reference/perc.md):
  Added.
- [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md):
  Modified. Minor change related to `droplevels=` in
  [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md).
- [`psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md):
  Modified. Changed name for the “zero” group to “substock.”
- `swvCode()`: Modified. Removed ability to Stangle the code and thus
  removed `method=`. Modified code to allow usage of .Rmd files in
  addition to .Rnw files.

## FSA 0.4.30

- **Date:** Oct14
- Added a suggests for `plyr`, for examples using
  [`mapvalues()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md).
- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  Modified. Changed `as.fact=` to default to same as `use.names=`. This
  will result in the same behavior as before. However, it also allows
  the user to set `use.names=TRUE` and `as.fact=FALSE` to return a
  character vector (that is not a factor).
- [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md):
  Modified. Added `addSpec=` and `addLens=` so that the user can have
  non-Gabelhouse lengths for individual species.
- `PSDlit`: Modified. Changed “Walleye x Sauger” to “Saugeye” and “White
  Bass x Striped Bass” to “Palmetto Bass”. Updated the Palmetto Bass
  values based on Dumont and Neely (2011), but kept old values as
  “Palmetto Bass (original)”. Deleted redundant entries for some
  species.
- `recodeF()`: Deleted. Functionality is in
  [`mapvalues()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  from `plyr`. Ease come easy go (i.e., added in last version).

## FSA 0.4.29

- **Date:** Oct14
- Added a suggests for `dplyr`.
- Added an external file in inst/extdata for testing PSD and Wr
  calculations.
- [`capFirst()`](https://fishr-core-team.github.io/FSA/reference/capFirst.md):
  Modified. Changed `words=` to `which=`.
- [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md):
  Modified. Added a default and a formula version to allow efficiency
  with `dplyr`. Added examples. Updated tests.
- `recodeF()`: Added.
- `recodeSpecies()`: Deleted. Functionality replaced by `recodeF()` in
  combination with
  [`capFirst()`](https://fishr-core-team.github.io/FSA/reference/capFirst.md).
- [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md):
  Modified. Added a default and a formula version to allow efficiency
  with `dplyr`. Added examples. Updated tests.
- `WSlit`: Modified. Added results for Sardine.

## FSA 0.4.28

- **Date:** Sep14
- [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md):
  Added.
- `psdDataPrep()`: Deleted. Functionality replaced by
  [`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md).
- `recodeSpecies()`: Modified. Completely re-written but with the same
  basic functionality. This new version returns a vector that can then
  be appended to an existing data.frame rather than the old function
  that returned a whole data.frame. This function should allow ease of
  use with
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) from
  `dplyr`. Added more catches for bad `formuala=`s. Added some tests.
- [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md):
  Modified. Completely re-written with completely new functionality.
  This new version returns a vector that can then be appended to an
  existing data.frame rather than the old function that returned a whole
  data.frame. This function should allow ease of use with
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) from
  `dplyr`. Added more catches for bad `formuala=`s. Added some tests.
- `wrDataPrep()`: Deleted. Functionality replaced by new
  [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md).

## FSA 0.4.27

- **Date:** Sep14
- [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md):
  Modified. Slight modifications to warning messages.
- [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md):
  Modified. Slight modification to warning messages.
- [`tictactoe()`](https://fishr-core-team.github.io/FSA/reference/tictactoe.md):
  Modified. Changed `predbal=` to `predobj=`, `preybal=` to `predbal=`,
  `xlab=` to `predlab=`, `ylab=` to `preylab=`, `bal.col=` to
  `obj.col=`, and `bal.trans=` to `obj.trans=`.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Fixed a bug related to `plot=TRUE` when `type="Francis"` or
  `type="Schnute"`.
- [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md):
  Modified. Modified how quadratic functions and the handling of fish
  less than the minimum applicable length were handled because of
  changes to
  [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md).
- `wsLit`: Modified. Changed order of variables, changed hybrid species
  names to match that of Neumann et al. (2012), update comments to
  related to Neumman et al. (2012) rather than Blackwell et al. (2012),
  and added information for the Riffle Dace.
- [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md):
  Modified. Changed the names of the `min.len` and `max.len` variables
  to be either `min.TL` and `max.TL` or `min.FL` and `max.TL` as
  appropriate. Suppressed the return of `max.len` and `quad` if they did
  not exist and suppressed return of `comment` if it was `none`. Added a
  catch if more than one species was given in `species=`. Created some
  tests.

## FSA 0.4.26

- **Date:** Sep14
- [`capFirst()`](https://fishr-core-team.github.io/FSA/reference/capFirst.md)
  Modified. Added an option to handle a vector of strings rather than
  just a single string.
- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  Modified. Fixed bug with category names when `use.names=TRUE`. Moved
  all internal functions outside of
  [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md)
  environment (and renamed them). Cleaned up code.
- [`psdCI()`](https://fishr-core-team.github.io/FSA/reference/psdCI.md):
  Modified. Added more catches for calls with mistakes. Create some
  internal functions to modularize the computations. Added tests.
- [`psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md):
  Modified. Added more catches for calls with mistakes. Added tests.
- [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md):
  Modified. Completely redone. Changed default to use multinomial rather
  than binomial method for confidence intervals (added `method=`
  argument to control CI type). Changed to throw an error of a species
  is not given in \`\`species=’’. Added tests.
- `psdDataPrep()`: Modified. Changed `use.catnames=` to `use.names` and
  `psdname=` to `vname=` to be consistent with
  [`psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md).
  Removed duplicitous [`factor()`](https://rdrr.io/r/base/factor.html)
  calls for the length category and species name variables in the
  returned data.framed.
- [`psdPlot()`](https://fishr-core-team.github.io/FSA/reference/psdPlot.md):
  Modified. Completely redone (fixed several bugs and overall sloppy
  code). Added `psd.add=`. Changed `legend.pos=` and `legend.cex=` to
  `psd.pos=` and `psd.cex=`.
- `recodeSpecies()`: Modified. Made changes to reflect new
  [`capFirst()`](https://fishr-core-team.github.io/FSA/reference/capFirst.md)
  functionality.
- [`tictactoe()`](https://fishr-core-team.github.io/FSA/reference/tictactoe.md):
  Modfied.
- `tictactoeAdd()`: Removed. Directed user to use
  [`plotCI()`](https://plotrix.github.io/plotrix/reference/plotCI.html)
  from `plotrix` instead.

## FSA 0.4.25

- **Date:** Sep14
- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified. Better handled a given value of `R=`.
- [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md)
  Modified. Fixed a bug that appeared when no “zero” fish were present
  in the data. Moved all internal functions outside of
  [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md)
  environment (and renamed them).
- [`psdCI()`](https://fishr-core-team.github.io/FSA/reference/psdCI.md):
  Added.
- [`psdPlot()`](https://fishr-core-team.github.io/FSA/reference/psdPlot.md):
  Modified. Fixed a bug that appeared when no “zero” fish were present
  in the data. Used
  [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md)
  to compute the PSD values. Moved default legend position to `topleft`.
- `swvCode()`: Modified. Fixed bug when attempting to use this function
  from outside of the directory where the .Rnw file exists. Added
  functionality to add a “note” to the first line(s) of the output file.
  Added code to remove the first line of the output file if it was going
  to be blank.
- `swvFinish()`: Modified. Updated code because `iGetFilePrefix()` was
  deleted.

## FSA 0.4.24

- **Date:** Aug14
- `ageKey()`: Deprecated. See `alkIndAge()`.
- `ageKeyPlot()`: Deprecated. See
  [`alkPlot()`](https://fishr-core-team.github.io/FSA/reference/alkPlot.md).
- `ageKeyPrep()`: Deprecated. See `alkPrep()`.
- [`alkAgeDist()`](https://fishr-core-team.github.io/FSA/reference/alkAgeDist.md):
  Added.
- `alkIndAge()`: Added. Was `ageKey()`. Added `seed=` to help with
  reproducibility. Modified code to better handle when an age-length key
  has a whole row of missing data (as would happen if `as.fact=TRUE` and
  `drop.levels=FALSE` in
  [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md)).
  Added some checks for the age-length key structure. Moved all internal
  functions outside of `alkIndAge()` environment (and renamed them).
- [`alkMeanVar()`](https://fishr-core-team.github.io/FSA/reference/alkMeanVar.md):
  Added.
- [`alkPlot()`](https://fishr-core-team.github.io/FSA/reference/alkPlot.md):
  Added. Was `ageKeyPlot()`. Fixed bug with colors when adding legend to
  bar and area plots. Allowed legend to be removed from area plot. Added
  ability to add a legend to the lines and splines plot. Added `pal=` to
  allow choice of color palette for areas in bar and area plot and lines
  in lines and splines plots. Allowed an area plot when one row of
  age-length key sums to zero (previously did not allow this). Create
  internal functions for each plot type. Moved all internal functions
  outside of
  [`alkPlot()`](https://fishr-core-team.github.io/FSA/reference/alkPlot.md)
  environment (and renamed them). Added some checks on the age-length
  key structure.
- `alkPrep()`: Added. Was `ageKeyPrep()`. Added some checks on the
  age-length key structure.
- `iCheck ALK()`: Added as an internal function (used to test the
  structure of the age-length keys in several other functions).
- [`summary.mrOpen()`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md):
  Modified. Removed “Estimates” heading if `verbose=FALSE`.
- [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md):
  Modified. Moved all internal functions outside of
  [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md)
  environment (and renamed them).

## FSA 0.4.23

- **Date:** Aug14
- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Modified. Completely modified the code so that the examples with
  [`apply()`](https://rdrr.io/r/base/apply.html) and
  [`lapply()`](https://rdrr.io/r/base/lapply.html) would also provide
  confidence intervals. Also changed the code to reflect that
  $\sum_{i = 1}^{k - 1}T_{i}$ from Schnute (1983) is the same as $X$
  from Carle and Strub (1978), the $\sum_{i = 1}^{k - 1}T_{i} - C_{1}$
  in Schnute (1983) is the same as $X - (k - 1)C_{1}$, and $q$ in
  Schnute (1983) is $p$ in most other resources. These changes allowed
  some efficiencies and connected the theory behind the methods more
  firmly. Removed the check for character data. Kept the check for
  whether catch was a vector or not but if catch is a one row or one
  column matrix or data.frame then it will be converted to a vector to
  continue. The latter change allows one to extract one row from a
  data.frame to send to
  [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md)
  without having to use
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html). Modified and
  added examples of the use of
  [`apply()`](https://rdrr.io/r/base/apply.html) and
  [`lapply()`](https://rdrr.io/r/base/lapply.html).

## FSA 0.4.22

- **Date:** Aug14
- `ageKey()`: Modified. Changed to using
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html) to check if the
  ALK has rows that don’t sum to 1. This was an attempt to minimize the
  number of “false negatives” caused by [R FAQ
  7.31](https://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f).
  Changed the check of whether the longest fish in the length sample is
  greater than the longest length bin in the ALK to whether the longest
  fish in the length sample is greater than the longest length bin in
  ALK PLUS the minimum width of length categories. This last change is
  an attempt to minimize the number of warnings that occur when the
  longest fish in the length sample would be in the last length category
  ALK but because the length categories are labelled by their minimum
  length it looks like it is not. The minimum width is used to still
  allow unevent length categories and, thus, this check may still
  produce some “false negatives.”
- `ageKeyPlot()`: Modified. Removed `bubble.ylab=`. Modified `ylab=` to
  handle what `bubble.ylab=` used to handle.
- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Modified. Added options to perform Moran (1951) and Schnute (1983)
  removal methods. Added examples of the new functionality. Updated the
  tests for the new functionality.

## FSA 0.4.21

- **Date:** Jul14
- [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  Modified. Changed `type=` to `method=` and added `DeLury` as an option
  to `method=` (and left `Delury`). Changed `ricker.mod=` to
  `Ricker.mod=`. Added some checking for bad arguments. Created internal
  functions specific to the Leslie and DeLury methods (for isolation).
  Modified some clunky code. Added references to specific sections in
  Seber (2002) for SE equations. Updated examples. Added tests and error
  checking.
- [`coef.depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  Modified. Added `digits=`.
- [`confint.depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  Modified. Added `digits=`. Modified the `parm=` list to be more
  efficient.
- [`plot.depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  Modified. Removed internal
  [`par()`](https://rdrr.io/r/graphics/par.html) settings.
- [`summary.depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  Modified. Added `verbose=` and `digits=`.

## FSA 0.4.20

- **Date:** Jul14
- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Modified. Made `"CarleStrub"` the default method. Changed `type=` to
  `method=`. Changed internal `meth` object to `lbl`. Moved all internal
  functions outside of
  [`mrOpen()`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md)
  environment and added other internal functions to isolate all
  intermediate calculations. Added a `verbose=` and `parm=` to
  [`summary()`](https://rdrr.io/r/base/summary.html). Streamlined clunky
  code in [`confint()`](https://rdrr.io/r/stats/confint.html) including
  removing the `all` and `both` options from `parm=`. Added more checks
  for abd inputs, notes in the code as to sources for the fomulae, and
  tests.

## FSA 0.4.19

- **Date:** Jul14
- Modified some tests to check whether the suggested package was
  installed.
- [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md):
  Modified. Changed column and row labels for `$methodB.top` and column
  labels for `$methodB.bot`. Added a m-array object for when more than
  two sampling events are present. Added calculations for the number of
  fish first seen on event i (ui), the number of fish last seen on event
  i (vi), and the number of fish seen i times (fi) to `$sum`.
- [`jolly()`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md):
  Added. Same as
  [`mrOpen()`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md),
  added only for convenience.
- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified. Fixed bugs around printing of CI type with Schnabel and the
  ignoring of `conf.level=` with Schnabel.
- `mrOpen`: Modified. Changed `ci.type=` to `type=` and `phi.type=` to
  `phi.full=`. Removed `type=` from
  [`summary()`](https://rdrr.io/r/base/summary.html) and added a
  `verbose=` which will print only the estimates if `FALSE` or both
  observables and estimates if `TRUE`. Added a `verbose=` to
  [`confint()`](https://rdrr.io/r/stats/confint.html) to control whether
  the message about the type of confidence interval is printed or not.
  Moved all internal functions outside of
  [`mrOpen()`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md)
  environment and added other internal functions to isolate all
  intermediate calculations. Changes to row and column labels in
  [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md)
  resulted in changes to row lables for
  [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`confint()`](https://rdrr.io/r/stats/confint.html) results.
  Streamlined some clunky code. Added checks for misformed `mb.top=` and
  `mb.bot=`. Added tests and notes in the code as to sources for the
  fomulae.
- `plot.CapHistSum()`: Added.
- `plot.mrClosed()`: Modified. Changed axis labels as the expressions
  did not print with some fonts and devices.

## FSA 0.4.18

- **Date:** Jul14
- Moved to compiling under R 3.1.1.
- Added a Suggests for `marked` for the example in
  [`capHistConvert()`](https://fishr-core-team.github.io/FSA/reference/capHistConvert.md).
- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Changed default value of `min.n.CI=` from 5 to 3. Added an
  `na.rm=TRUE` to the [`min()`](https://rdrr.io/r/base/Extremes.html)
  and [`max()`](https://rdrr.io/r/base/Extremes.html) that produced the
  age ranges for the age agreement table.
- `BluegillJL`: Modified. Corrected lake name and added a citation.
- [`capHistConvert()`](https://fishr-core-team.github.io/FSA/reference/capHistConvert.md):
  Modified. This should probably be considered as a new function if
  updating from the old version. Modifications included simplifying the
  structure allowed for the input data.frames (they can have only an id
  or a freq column and then columns related to the capture history …
  this makes the function less flexible but simplifies its use for those
  that are most likely to use it), moved to a series of internal
  functions, created a common intermediate data format (which
  streamlined the code considerably), changed the name of the `FSA`
  format to `individual` and the `Rcapture` format to `frequency`, added
  an `out.type='event'` format, added `in.type='RMark'` and
  `in.type='marked'`formats, fixed the bug with outputting `RMark`
  format, changed the default for new frequency variables from `Freq` to
  `freq`, removed the `mch=` and `event=` arguments, replaced `cols=`
  with `cols2ignore=`, added the `include.id=` argument, changed the
  `in.type=` default, coded some “catches” for common mistakes in use,
  coded to keep the unique fish identifier in `id=` or event name given
  in the variable names as much as possible, fixed a bug with
  `event.ord=`. Added several new examples.
- [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md):
  Modified. Change `cols=` argument to `cols2use=`. Moved all internal
  functions outside of
  [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md)
  environment.
- `CutthroatAL`: Modified. Updated from a new source to include many
  more years of samples.
- [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed `trans.pt=` to `transparency=`.
- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified. Completely re-built the internal file structure. Changed
  `incl.inputs=` to `verbose=`. Added the ability to construct a CI for
  the overall PE when multiple groups are used in a Petersen family
  method (thus, added a `incl.all=` to
  [`confint()`](https://rdrr.io/r/stats/confint.html)). Changed default
  for `incl.all=` from `FALSE` to `TRUE`. Modified the messages when
  `verbose=TRUE`.
- `plot.AgeBias()`. Modified. Fixed bug that produced a warning if all
  of the bias t-tests were either significant or not significant.
  Changed `col.err=` to `col.CI=`, `lwd.err=` to `lwd.CI=`,
  `col.err.sig=` to `col.CIsig=`, `col.ref=` to `col.agree=`, `lwd.ref=`
  to `lwd.agree=`, `lty.ref=` to `lty.agree=`, `show.rng=` to
  `show.range=`, `col.rng=` to `col.range=`, `lwd.rng=` to `lwd.range=`.
  Removed `col.lab=` and `row.lab=` which were deprecated several minor
  versions ago. Changed default values for `lwd.rng=` and `lwd.CI=` from
  2 to 1. Added a `cex.numbers=` argument for controlling the size of
  the numbers in the “numbers plot” (defaults to 0.9).
- [`plotBinResp()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed `trans.pt=` to `transparency=`.

## FSA 0.4.17

- **Date:** Jul14
- `confint.mrClosed()`: Modified. Moved all internal functions outside
  of `confint.mrClosed()` environment (see `iCI.MRCMultiple()` and
  `iCI.MRCSingle()`). Changed `ci.type=` to just `type=`. Streamlined
  binomial method for single census. Used `iMRCSingleSE()` to get SE for
  when `type="normal"` for Chapman, Bailey, and Ricker methods.
- [`extraSS()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Slight change to row labels in output table.
- `iMRCMultiple()`: Added. Was `mrc2()` internal function inside of
  [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md)
  environment.
- `iMRCSingle()`: Added. Was `mrc1()` internal function inside of
  [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md)
  environment.
- `iMRCSingleSE()`: Added. Moved functionality out of
  `summary.mrClosed()`. Checked and documented all formulas with sources
  (in code and in Rd file).
- [`lrt()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Modified. Slight change to row labels in output table.
- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified. Moved all internal functions outside of
  [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md)
  environment (see `iMRCMultiple()` and `iMRCSingle()`). Changed `type=`
  argument to `method=`. Added more catches for argument problems
  (required setting `n=`, `m=`, `M=` and `R=` to `NULL`). Streamlined
  warning message for when `incl.SE=TRUE` is used with Schnabel or
  Schumacher-Eschmeyer method. Added tests and reported results in the
  help file for population size, SE, and CI estimates for each method.
- `plot.mrClosed()`: Modified. Removed setting of
  [`par()`](https://rdrr.io/r/graphics/par.html). Changed from using
  [`lowess()`](https://rdrr.io/r/stats/lowess.html) to using
  [`loess()`](https://rdrr.io/r/stats/loess.html) and set better default
  values. Added descriptive text to help file.
- `summary.mrClosed()`: Modified. Moved SE calculations into an internal
  function (see `iMRCSingleSE()`).

## FSA 0.4.16

- **Date:** Jul14
- `BluegillLM`: Modified. Added a seealso.
- [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed the loess-related methods to use
  [`loess()`](https://rdrr.io/r/stats/loess.html), to put an approximate
  confident band with the line, the line and band are “under” the
  points, the line is lighter. Put the horizontal reference line at zero
  under the points. Made `loess=TRUE` the default.
- [`iAddLoessLine()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. See
  [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md).
- [`iHndlFormula()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. COrrected the positioning of the explanatory variables when
  the model has a response variable.
- `iMakeBaseResidPlot()`: Added as an internal function to
  [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  to simplify some coding.
- [`iMakeColor()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Modified. More intelligently handles values that are greater than 1
  (converts them to decimals by inverting.)
- `lwPredsComp()`: Modified. Changed `mdl=` to `object=`. Added use of
  internal
  [`iHndlFormula()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md)
  and moved two internal functions outside the main function. Changed
  default for intervals from `both` to `confidence` and changed so that
  if only the confidence or prediction intervals are plotted they will
  be black with `lwd=` width (if both are plotted the CI is now black
  and the PI is now blue). Added a `show.preds` argument. Changed
  `connect.means=` to `connect.preds=`. Changed default `lwd=` value and
  how it is used for CIs, PIs, and the connection lines. Added
  `col.connect=` argument. Removed `mar` and `mgp` from
  [`par()`](https://rdrr.io/r/graphics/par.html) call (left `mfrow`).
  Added more examples. Added tests for error messages.
- [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Added `inclHist=` argument. Corrected a bug around the use
  of
  [`thigmophobe()`](https://plotrix.github.io/plotrix/reference/thigmophobe.html)
  in `iAddOutlierTest()`. Changed default for `student=` to `FALSE`.
  Modified and added more examples.
- `SMBassWB`: Modified. Added a seealso.

## FSA 0.4.15

- **Date:** Jun14
- lots of roxygen2 Rd cleaning.
- `addLoessLine()`: Deleted. Moved functionality to
  [`iAddLoessLine()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md)
  and moved code to
  [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  file..
- `addOutlierTestResults()`: Deleted. Moved functionality to
  `iAddOutlierTestResults()` and moved code to
  [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  file.
- [`capHistConvert()`](https://fishr-core-team.github.io/FSA/reference/capHistConvert.md):
  Added an [`interactive()`](https://rdrr.io/r/base/interactive.html) to
  the `Rcapture` example in the help file.
- `checkStartcatW()`: Deleted. Moved functionality to
  [`iCheckStartcatW()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md).
- `ci.fp()`: Deleted. Moved functionality to `iCIfp()` and moved code to
  [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  file.
- `ci.fp.1()`: Deleted. Moved functionality to `iCIfp1()` and moved code
  to
  [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  file.
- `ciLabel()`: Deleted. Moved functionality to
  [`iCILabel()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md).
- `getAllDependencies()`: Deleted. Moved functionality to
  `iGetAllDependencies()` and moved code to `swvUtils` file.
- `getFilePrefix()`: Deleted. Moved functionality to `iGetFilePrefix()`
  and moved code to `swvUtils` file.
- `getMainTitle()`: Deleted. Moved functionality to `iGetMainTitle()`
  and moved code to
  [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  file.
- `getVarFromFormula()`: Deleted. Moved functionality to
  [`iGetVarFromFormula()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md).
- `hndlFormula()`: Deleted. Moved functionality to
  [`iHndlFormula()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md).
- `hndlMultWhat()`: Deleted. Moved functionality to
  [`iHndlMultWhat()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md).
- [`iAddLoessLine()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Added. Was `addLoessLine()`.
- `iAddOutlierTestResults()`: Added. Was `addOutlierTestResults()`.
- [`iCheckStartcatW()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Added. Was `checkStartcatW()`.
- `iCIfp()`: Added. Was `ci.fp()`.
- `iCIfp1()`: Added. Was `ci.fp.1()`.
- [`iCILabel()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Added. Was `ciLabel()`.
- `iGetAllDependencies()`: Added. Was `getAllDependencies()`.
- `iGetFilePrefix()`: Added. Was `getFilePrefix()`.
- `iGetMainTitle()`: Added. Was `getMainTitle()`.
- [`iGetVarFromFormula()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Added. Was `getVarFromFormula()`.
- [`iHndlFormula()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Added. Was `hndlFormula()`.
- [`iHndlMultWhat()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Added. Was `hndlMultWhat()`.
- [`iLegendHelp()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Added. Was `legendHelp()`.
- [`iMakeColor()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Added. Was `makeColor()`.
- `iMakeFilename()`: Added. Was `makeFilename()`.
- `iMakeItemsToRemove()`: Added. Was `makeItemsToRemove()`.
- `iProcessSessionInfo()`: Added. Was `processSessionInfo()`.
- `iPSDLitCheck()`: Added. Was `psdLitCheck()`.
- [`is.even()`](https://fishr-core-team.github.io/FSA/reference/is.odd.md):
  Added.
- [`is.odd()`](https://fishr-core-team.github.io/FSA/reference/is.odd.md):
  Added. Was `odd()`.
- [`iTypeoflm()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Added. Was `typeoflm()`.
- `iwsLitCheck()`: Added. Was `wsLitCheck()`
- `legendHelp()`: Deleted. Moved functionality to
  [`iLegendHelp()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md).
- `listSpecies()`: Deleted. Moved functionality to
  [`iListSpecies()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md).
- `makeColor()`: Deleted. Moved functionality to
  [`iMakeColor()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md).
- `makeFilename()`: Deleted. Moved functionality to `iMakeFilename()`
  and moved code to `swvUtils` file.
- `makeItemsToRemove()`: Deleted. Moved functionality to
  `iMakeItemsToRemove()` and moved code to `swvUtils` file.
- `odd()`: Deleted. Moved functionality to
  [`is.odd()`](https://fishr-core-team.github.io/FSA/reference/is.odd.md).
- [`predict.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):
  Added an [`interactive()`](https://rdrr.io/r/base/interactive.html) to
  the `nlstools` example in the help file.
- `printProgressMsg()`: Deleted. Not used anywhere.
- `processSessionInfo()`: Deleted. Moved functionality
  `iProcessSessionInfo()` and moved code to `swvUtils` file.
- `PSDLitCheck()`: Deleted. Moved functionality to `iPSDLitCheck()` and
  moved code to `psdVals()` file.
- `pssCalc()`: Deleted. Was deprecated several versions ago. See
  [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md).
- `pssDataPrep()`: Deleted. Was deprecated several versions ago. See
  `psdDataPrep()`.
- `pssPlot()`: Deleted. Was deprecated several versions ago. See
  [`psdPlot()`](https://fishr-core-team.github.io/FSA/reference/psdPlot.md).
- `pssVal()`: Deleted. Was deprecated several versions ago. See
  [`psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md).
- `typeoflm()`: Deleted. Moved functionality to
  [`iTypeoflm()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md).
- `wsLitCheck()`: Deleted. Moved functionality to `iwsLitCheck()` and
  moved code to `wsVals()` file.

## FSA 0.4.14

- **Date:** Jun14
- added tests (in `test_VonB2b.R`) to assure that group comparisons of
  von Bertalanffy parameters equal those in Kimura (1980) and `vblrt()`
  in `fishmethods`.
- added importsFrom for `lmtest` for
  [`lrt()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md).
  Also used in testing (`test_VonB2b.R`).
- [`confint.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):
  Modified. Modified the plotting to use
  [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md),
  removed `par(mar=)` definitions, and added `err.col=` and `lwd.col=`
  to control the color and line width of the confidence interval line on
  the plot.
- [`extraSS()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Added.
- [`growthModels()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Added Weisberg parameterization. Changed `vbGallucciQuinn`
  to `vbGQ`.
- `growthModelSim()`: Modified. Added Weisberg parameterization. Added
  `vbGQ` abbreviation (synonymous with `vbGallucciQuinn`).
- [`lrt()`](https://fishr-core-team.github.io/FSA/reference/extraTests.md):
  Added.
- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Added Weisberg parameterization. Added `vbGQ` abbreviation
  (synonymous with `vbGallucciQuinn`). Simplified the functions for when
  `simple=FALSE` (no error checking now).
- `vbModels()`: Modified. Added Weisberg parameterization. Changed
  `vbGallucciQuinn` to `vbGQ`.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Added Weisberg parameterization. Added `vbGQ` abbreviation
  (synonymous with `vbGallucciQuinn`). Added an internal function for
  checking whther the starting values for K and Linf made sense.

## FSA 0.4.13

- **Date:** Jun14

- added testthat files for error checking of `chapmanPlot()`,
  [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md),
  [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md),
  and `walfordPlot()`. Added a testthat file for checking that the von
  Bertalanffy fitting using
  [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md)
  and
  [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md)
  matches other sources.

- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Deprecated `col.lab=` and `row.lab=` and replaced with
  `ref.lab=` and `nref.lab=`. Moved all functions that were internal to
  main functions to being internal to the package. In the process, I
  changed the names of the internal functions slightly, made explicit
  the argument passing, and added internal descriptions of the internal
  files. Changed several if else strings in the plot method to a
  [`switch()`](https://rdrr.io/r/base/switch.html).

- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Changed some messages so they were not as wide.

- `chapmanPlot()`: Modified. Removed S3 functions so that
  [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md)
  has to use a formula. Added some checking related to the formula.

- [`growthModels()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Created an internal function that eliminates repetitiveness
  between this and `vbModels()`. Changed the `GompX` types to
  `GompertzX`.

- `growthModelSim()`: Modified. Removed S3 functions so that
  `growthModelSim()` has to use a formula. Added some checking related
  to the formula. Changed the order of the arguments so that `formula=`
  and `data=` come before `type=`. This allows a similar interface with
  [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md).
  Included a hack that still allows the user to enter a type as the
  first argument (and thus not have to type `type=` if any
  parameterization besides the `vbTypical` is being used). Corrected
  spelling of Gallucci for Gallucci and Quinn model.

- `hndlFormula()`: Modified. Fixed bug with expected number of response
  variables value in return list.

- `SpotVA1`: Modified. Updated reference.

- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Changed `schnute` parameterization to use L3 instead of L2
  and t3 instead of t2.

- `vbModels()`: Modified. Created an internal function that eliminates
  repetitiveness between this and
  [`growthModels()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md).

- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Removed S3 functions so that
  [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md)
  has to use a formula. Added some checking related to the formula.
  Changed `tFrancis=` to `ages2use=`. Changed the Schnute method to use
  the ages in `ages2use=` rather than being hard-wired to use the
  minimum and maximum observed age. Both the Schnute and Francis methods
  will use the minimum and maximum observed ages if `ages2use=NULL`.
  Added a catch for if `ages2use=` are in descending order (should be in
  ascending order). Changed `Schnute` parameterization to use L3 instead
  of L2.

- `walfordPlot()`: Modified. Removed S3 functions so that
  [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md)
  has to use a formula. Added some checking related to the formula.

## FSA 0.4.12

- **Date:** May14

- added Suggests for `testthat`, `fishmethods`, `FSAdata` for testing
  and `popbio` for an example that was made “interactive” from “dont
  run”(see below).

- added testthat files for
  [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md)
  and
  [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md).

- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Removed unit testings from examples and put in the testing
  file.

- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Removed deprecated `what="agreement"`.

- [`confint.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):
  Modified. Changed example from “dont run” to “interactive.”

- [`fact2num()`](https://fishr-core-team.github.io/FSA/reference/fact2num.md):
  Modified. Changed example from “dont run” to “interactive.”

- [`fishR()`](https://fishr-core-team.github.io/FSA/reference/fishR.md):
  Modified. Removed `news` and added `posts` to the `where=` argument.
  Cleaned up the Rd file. Changed example from “dont run” to
  “interactive.”

- [`FSA()`](https://fishr-core-team.github.io/FSA/reference/FSA.md):
  Modified. Cleaned up the Rd file.

- `FSANews()`,
  [`fsaNews()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Cleaned up and fixed the Usage section in the Rd file.
  Changed example from “dont run” to “interactive.”

- `growthRadPlot()`: Modified. Changed example from “dont run” to
  “interactive.”

- [`htest.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):
  Modified. Changed example from “dont run” to “interactive.”

- [`lagratio()`](https://fishr-core-team.github.io/FSA/reference/lagratio.md):
  Modified. Changed example from “dont run” to “interactive.”

- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  Modified. Changed Rd file for deletion of
  [`view()`](https://tibble.tidyverse.org/reference/view.html).

- `popSizesPlot()`: Modified. Changed example from “dont run” to
  “interactive.”

- `TroutDietSL`: Modified. Changed Rd file for deletion of
  [`view()`](https://tibble.tidyverse.org/reference/view.html).

- [`view()`](https://tibble.tidyverse.org/reference/view.html): Deleted.
  Moved to NCStats package.

- `wrDataPrep()`: Modified. Changed Rd file for deletion of
  [`view()`](https://tibble.tidyverse.org/reference/view.html).

## FSA 0.4.11

- **Date:** May14

- Removed Roxygen directives in DESCRIPTION (with changes to roxygen2
  4.0.1).

- Changed `@S3method` and `@method` to `@export` in the following files
  according to changes in ROxygen2 as described at
  stackoverflow.com/questions/7198758/, among several other places:
  `ageBias`, `agePrecision`, `bootCase`, `catchCurve`, `chapmanRobson`,
  `confint.nlsboot`, `depletion`, `dietOverlap`, `fitPlot`,
  `hist.formula`, `htest.nlsBoot`, `ks2d1`, `ks2d1p`, `ks2d2`, `ks2d2p`,
  `ksTest`, `lencat`, `mrClosed`, `mrOpen`, `plotBinResp`,
  `predict.nlsBoot`, `removal`, `residPlot`, `srStarts`, `Subset`,
  `Summarize`, `sumTable`, `vbStarts`, and `walfordChapmanPlot`.

- [`addZeroCatch()`](https://fishr-core-team.github.io/FSA/reference/addZeroCatch.md):
  Modified. Added a catch for the situation where no zeros need to be
  added to the data.frame. Cleaned-up the help file, modified the
  examples, and added another example. Thanks to Ben Neely for bringing
  this bug (handling where zeros are not needed) to my attention.

- [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md):
  Modified. Cleaned up the code (no changes in functionality).

- `catchCurveSim()`: Deleted. Moved to FSAsim package.

- `checkstartcatw()`: Modified. Changed the catch for whether the
  starting category value was greater than the minimum observed value to
  correct for a pathological case where they were equal but not with
  machine rounding.

- `lenFreqExpand()`: Modified. Slightly changed the examples in the help
  file.

- `lwPredsComp()`: Modified. Streamlined the code (no changes to
  functionality).

- [`mrOpen()`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md):
  Modified. Streamlined the code (no changes to functionality). Removed
  all explicity partial matching options in
  [`switch()`](https://rdrr.io/r/base/switch.html)es as these were
  already caught with previous
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html)s.

## FSA 0.4.10

- **Date:** May14

- Added Roxygen directives to DESCRIPTION.

- Updated to Roxygen2 4.0.0 which modified several help files.

- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Cleaned-up the help file.

- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Cleaned-up the help file.

- `ageKey()`: Modified. Cleaned-up the help file and modified the
  example.

- `ageKeyPlot()`: Modified. Added more description and cleaned-up the
  help file.

- `ageKeyPrep()`: Modified. Added more description and cleaned-up the
  help file.

- `lenFreqExpand()`: Modified. Corrected `total=` to use `length(x)`
  rather than `nrow(df)`, which was left over from a previous change.
  Cleaned-up the help file.

- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified. Increased the readability of the code (added comments, used
  [`with()`](https://rdrr.io/r/base/with.html) for some long
  calculations, added spacing). Added specific citations to equations in
  the help file. Changed the degrees-of-freedom in the confidence
  interval calculation for the Schnabel methods from number of samples
  minus 2 to number of samples minus 1 (following Krebs).

- [`poiCI()`](https://fishr-core-team.github.io/FSA/reference/poiCI.md):
  Modified. Cleaned-up the help file.

- `psdDataPrep()`: Modified. Fixed error around `use.catnames=`.

- `swvCounts()`: Modified. Fixed error in output.

## FSA 0.4.9

- **Date:** May14

- Removed nlme dependencies (with removal of `vbDataGen()`).

- `ageComp()`: Deleted. Fully deprecated. Use
  [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md)
  and
  [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md)
  instead.

- `cohortSim()`: Deleted. Moved to FSAsim package.

- [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  Modified. Remove link to `leslieSim()`.

- `lengthWeightSim()`: Deleted. Moved to FSAsim package.

- `leslieSim()`: Deleted. Moved to FSAsim package.

- `lwModelSim()`: Deleted. Moved to FSAsim package.

- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified. Remove link to `mrClosed1Sim()`.

- `mrClosed1Sim()`: Deleted. Moved to FSAsim package.

- `srCobWeb()`: Deleted. Moved to FSAsim package.

- `vbComp()`: Deleted. Moved to FSAsim package.

- `vbDataGen()`: Deleted. Moved to FSAsim package.

- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Remove link to `vbComp()`.

- `VBGMlit()`: Deleted. Moved to FSAsim package.

## FSA 0.4.8

- **Date:** May14
- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Added the ability to use multiple `what=` arguments with
  [`c()`](https://rdrr.io/r/base/c.html). Added `what="n"` to get the
  sample size on the age-agreement table. Added `nYpos=` to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) to allow
  control of the position of the sample size values on the plot. Changed
  the order of the printing of results when `what="symmetry"` is used in
  [`summary()`](https://rdrr.io/r/base/summary.html). The order more
  closely follows the “level of complexity” of the tests. Added unit
  test examples to the help file.
- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Added the ability to use multiple `what=` arguments with
  [`c()`](https://rdrr.io/r/base/c.html).
- `hndlMultWhat()`: Added. An internal file to help
  [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md)
  and `agePrecision` handle multiple `what=` arguments.

## FSA 0.4.7

- **Date:** Apr14

- Removed all of the functions related to constructing and validating
  standard weight equations. These are now in the [FSAWs
  package](https://github.com/droglenc/FSAWs). This is the start of an
  effort to streamline the FSA package.

- Removed importFrom quantreg (only used for standard weight methods).

- `ChinookArg`: Added (from FSAdata).

- `emp()`: Removed.

- `FroesWs()`: Removed.

- `lencatOLD()`: Removed (from FSA-internals).

- `lwPredsComp()`: Modified. Changed example to using `ChinookArg`
  rather than `RuffeWs` because `RuffeWs` was moved to the FSAWs
  package.

- `LMBassWs`: Removed.

- `rlp()`: Removed.

- `RuffeWs`: Removed.

- `WalleyeGerowLW`: Removed.

- `wsValidate()`: Removed.

- `WalleyeGerowLW`: Removed.

## FSA 0.4.6

- **Date:** Apr14

- Changed to compiling under R 3.1.0

- Imported
  [`stackpoly()`](https://plotrix.github.io/plotrix/reference/stackpoly.html)
  from plotrix for use in `ageKeyPlot()`.

- Added concepts (that largely match those in the FSAdata pacakge) to
  most of the data files.

- Made some grammatical changes and added author sections to Rd files.

- `ageKeyPlot()`: Added.

- `dietOverlap()`: Modified. Changed examples in help file to reflect
  changes to
  [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md).

- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  Modified. Added generic functions.
  [`lencat.default()`](https://fishr-core-team.github.io/FSA/reference/lencat.md)
  accepts a vector as its first argument and returns a single vector.
  [`lencat.formula()`](https://fishr-core-team.github.io/FSA/reference/lencat.md)
  accepts a formula as its first argument and the `data=` argument. The
  [`lencat.formula()`](https://fishr-core-team.github.io/FSA/reference/lencat.md)
  is the same as the old
  [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md)
  and
  [`lencat.default()`](https://fishr-core-team.github.io/FSA/reference/lencat.md)
  provides new functionality. Additionally, the default for `startcat=`
  is now `NULL` and a value for `startcat=` is found automatically
  (though a value can still be supplied by the user). The
  `use.catnames=` was changed to `use.names=`. Other changes were made
  to simplify the code.

- `lenFreqExpand()`: Modified. Removed the `df=` and `cl=` arguments and
  replaced with `x=`, which is simply a vector of length measurements.
  Changed to `startcat=NULL` so that that the starting category value
  can be determined automatically (or can still be set by the user).

## FSA 0.4.5

- **Date:** Apr14

- Converted to using github as a repository.

- Changed NEWS to NEWS.md

- Added ImportFrom for relax package (see below).

- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Modified. Added a plot that shows the number of observations at each
  combined age. Changed the coding slightly around Bowker’s test (added
  an internal function) and implemented Evans and Hoenig’s and McNemar’s
  test. These changes resulting in adding a “table” choice to `what=`
  that will print just the age-agreement table. When `what="symmetry"`
  is chosen all three ob Bowker’s, McNemar’s, and Evans-Hoenig results
  will be output as a table. The age-agreement table is no longer
  printed when `what="symmetry"`. In addition, `what="Bowkers"`,
  `what="EvansHoenig"`, and `what="McNemars"` can be used to see the
  Bowker’s, Evans and Hoenig, and McNemars test results, respectfully.
  Added a `cont.corr=` argument for use with McNemars test.

- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Added the ability to show raw (vs. absolute value)
  differences between structures. This resulted in the removal of
  `what="agreement"` (though it is deprecated, with a message, for now)
  and the addition of `what="difference"` and
  `what="absolute difference"`.

- [`fishR()`](https://fishr-core-team.github.io/FSA/reference/fishR.md):
  Modified. Changed to point to the github NEWS.md when `where="news"`.

- [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed the logistic regression code to handle the changes
  to
  [`plotBinResp()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  (see below). In addition, a temporary fix was added so that the size
  of the y-axis labels could be modified with an external call to
  [`par()`](https://rdrr.io/r/graphics/par.html). This was a fix for
  Glen Sutton but will ultimately need to be handled more elegantly.

- [`fsaNews()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Changed to point to the github NEWS.md.

- `catchCurveSim()`: Added back from FSATeach (required adding
  ImportFrom for relax package).

- `cohortSim()`: Added back from FSATeach (required adding ImportFrom
  for relax package).

- `growthModelSim()`: Added back from FSATeach (required adding
  ImportFrom for relax package).

- `lengthWeightSim()`: Added back (was `lwModelSim()`) from FSATeach
  (required adding ImportFrom for relax package).

- `leslieSim()`: Added back from FSATeach (required adding ImportFrom
  for relax package).

- `mrClosed1Sim()`: Added back from FSATeach (required adding ImportFrom
  for relax package).

- [`plotBinResp()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Added `yaxis1.ticks=` and `yaxis1.lbls=` arguments so that
  the user can control the tick-mark locations and labels for the left
  y-axis (the defaults are to show ticks every 0.1 units but only label
  0, 0.5, and 1). Added `yaxis2.show=` argument to allow the user to
  “turn-off” the right y-axis (defaults to being on) which is labeled
  with the level labels.

- `srSim()`: Added back from FSATeach (required adding ImportFrom for
  relax package).

## FSA 0.4.4

- **Date:** Apr14
- `ageKeyPrep()`: Added.
- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Modified. Fixed the bug where the APE and CV were over-estimated in
  situations where the multiple ages agreed at an age=0 (thanks to
  Richard McBride for pointing out this error).
- `wsLit`: Modified. Added Pursak chub information from Sulun et
  al. (2014).

## FSA 0.4.3

- **Date:** Mar14
- [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md):
  Added. Extracted the age-bias related material from `ageComp()`.
  Modified the code to remove unneeded code. From `ageComp()`, remove
  the `what=` argument related to differences and added a `difference=`
  argument. Also changed `what="bias.diff"` to `what="diff.bias"` to
  allow for a quicker partial matching (i.e. separate more from
  `what="bias"`). Major modifications to how the axis limits are created
  if none are provided. Modified where the sample size is shown on the
  age-bias plot. Added the `min.n.CI=` argument. Added an example using
  `WhitefishLC` to be consistent with
  [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md).
- `ageComp()`: Modified. Split into
  [`ageBias()`](https://fishr-core-team.github.io/FSA/reference/ageBias.md)
  and
  [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md).
  Added a warning that this function is deprecated and will be removed
  in the future.
- `ageKey()`: Modified. Fixed a bug that occurred when a data frame that
  already contained an LCat variable was provided.
- [`agePrecision()`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md):
  Added. Extracted age precision related material from `ageComp()`.
  Modified the code to allow for calculations across more than two
  structures. Code was streamlined dramatically from what was in
  `ageComp()`. Added an example using WhitefishLC as it allows for
  demonstrating more than two age assignments.
- [`capFirst()`](https://fishr-core-team.github.io/FSA/reference/capFirst.md):
  Modified. Added functionality, controlled by the new words= parameter,
  to allow all words, rather than just the first word, to be
  capitalized.
- [`capHistConvert()`](https://fishr-core-team.github.io/FSA/reference/capHistConvert.md):
  Modified the help file by commenting out the example that depends on
  the RCapture package. This is needed for the RForge site for the time
  being.
- [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified Rd. Added two polynomial regression examples.
- `fitPlot.IVR()`: Modified. Changed to use new `typeoflm()`, changed
  `interval=` argument, removed automatic main title, removed a bunch of
  unneeded code.
- `fitPlot.logreg()`: Modified. Removed automatic main title.
- `fitPlot.nls()`: Modified. Removed automatic main title.
- `fitPlot.ONEWAY()`: Modified. Changed to use new `typeoflm()`, removed
  automatic main title, removed one line of unneeded code.
- `fitPlot.SLR()`: Modified. Changed to use new `typeoflm()`, changed
  `interval=` argument, removed automatic main title.
- `fitPlot.TWOWAY()`: Modified. Changed to use new `typeoflm()` and
  removed automatic main title
- `gReshape()`: Modified. Added a `drop=` argument so that the user can
  drop some variables before reshaping. Also, added
  `new.row.names=1:100000` to the
  [`reshape()`](https://rdrr.io/r/stats/reshape.html) call to
  work-around issues with duplicate row names (which were particularly
  problematic if any of the `id.vars=` had missing values.)
- [`growthModels()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Corrected spelling of Gallucci for Gallucci and Quinn model.
- [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md):
  Modified. Add a `col=` argument that defaults to “gray90”.
- `hndlFormula()`: Added. An internal function to handle various
  assessments related to using formulas.
- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  Modified. Added the ability to add names if the vector sent in
  `breaks=` is named.
- `confint.mrClosed()`: Modified. Removed extra linespaces in printed
  output. Changed default for `incl.inputs=` to FALSE.
- `summary.mrClosed()`: Modified. Removed extra linespaces in printed
  output. Changed default for `incl.inputs=` to FALSE.
- [`predict.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):
  Modified the help file by commenting out the example that depends on
  the nlsBoot package. This is needed for the RForge site for the time
  being.
- [`psdCalc()`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md):
  Added (was `pssCalc()`).
- `psdDataPrep()`: Added (was `pssDataPrep()`) and modified. Deleted the
  code in this function that added category names as this functionality
  was added to
  [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md).
  See
  [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md)
  above.
- `PSDlit`: Added (was `PSSlit`) and modified. Changed all species names
  to have both words capitalized so as to follow the latest AFS
  guidelines.
- [`psdPlot()`](https://fishr-core-team.github.io/FSA/reference/psdPlot.md):
  Added (was `pssPlot()`).
- [`psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md):
  Added (was `pssVal()`).
- `rsdCalc()`: Deleted.
- `rsdVal()`: Deleted.
- `recodeSpecies()`: Modified. Changed `capFirst=` to `doCapFirst=` to
  minimize confusion with
  [`capFirst()`](https://fishr-core-team.github.io/FSA/reference/capFirst.md).
  Change `doCapFirst=` to a character that behaves like `words=` in
  [`capFirst()`](https://fishr-core-team.github.io/FSA/reference/capFirst.md),
  rather than as a logical.
- `SpotVA1`: Modified. Removed link to source documents because it
  caused a problem when making the PDF manual.
- `StripedBass1`: Deleted. Moved to FSAdata as no longer needed because
  some examples were changed to use `WhitefishLC`.
- [`Subset()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified. Added a `resetRownames=` argument.
- `swvCode()`: Modified. Added an `out.dir=` argument.
- `swvCounts()`: Modified. Added a `capitalize=` argument.
- `typeoflm()`: Modifed. Changed to use `hndlFormula()`. Made an
  internal function.
- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Corrected spelling of Gallucci for Gallucci and Quinn model.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Corrected spelling of Gallucci for Gallucci and Quinn model.
- `WhitefishLC`: Added (from FSAdata).
- `wsLit`: Modified. Changed all species names to have both words
  capitalized so as to follow the latest AFS guidelines.

## FSA 0.4.2

- **Date:** Dec13

- Changed to compiling under R 3.0.2.

- Removed dependency on reshape package (see changes for `emp()`,
  `gReshape()`, and `ssValidate()` below) and the relax, tcltk, and
  TeachingDemos packages (see changes for `catchCurveSim()`,
  `cohortSim()`, `growthModelSim()`, `leslieSim()`, `lwModelSim()`,
  `mrClosed1Sim()`, `simAgeBias()`, `simAges()`, `simLenFromAge()`,
  `simLenSelect()`, and `srSim()` below).

- .`onAttach()`: Modified. Added notes to use
  [`citation()`](https://rdrr.io/r/utils/citation.html).

- `bcFuns()`: Modified. Added “BPH” and “SPH” options to
  `type= argument` (same as “LBPH” and “LSPH”, respectively). Changed a
  catch using [`cat()`](https://rdrr.io/r/base/cat.html) to using
  [`message()`](https://rdrr.io/r/base/message.html). Added some
  specificity to the help file (more is needed).

- `catchCurveSim()`: Deleted. Moved to FSATeach package.

- `changesPos()`: Added.

- `cohortSim()`: Deleted. Moved to FSATeach package.

- `emp()`: Modified. Replaced use of `cast()` with
  [`aggregate()`](https://rdrr.io/r/stats/aggregate.html).

- `gReshape()`: Modified. Replaced use of `melt()` with
  [`reshape()`](https://rdrr.io/r/stats/reshape.html) from base package.
  Fixed bug if name of “increments” was not “inc” (now catches that
  `in.pre=` value is used). Fixed bug that `na.rm=` was ignored.
  Modified so that rownames are not created until after the NAs are
  moved or not. Changed the default name in `var.name=` from “age” to
  “prvAge” to reduce the highly possible chance that there might be
  another variable in the data frame named “age.”

- `growthModelSim()`: Deleted. Moved to FSATeach package.

- `growthRadPlot()`: Modified. Slightly changed the xlab= argument
  default.

- `leslieSim()`: Deleted. Moved to FSATeach package.

- `lwModelSim()`: Deleted. Moved to FSATeach package.

- `mrClosed1Sim()`: Deleted. Moved to FSATeach package.

- `simAgeBias()`: Deleted. Moved to FSATeach package.

- `simAges()`: Deleted. Moved to FSATeach package.

- `simLenFromAge()`: Deleted. Moved to FSATeach package.

- `simLenSelect()`: Deleted. Moved to FSATeach package.

- `srSim()`: Deleted. Moved to FSATeach package.

- `summary.ageComp()`: Modified. Added a `zero.print=` argument with a
  default of a single dash for use when printing an age-agreement table.
  Added `flip.table=` argument to allow ease of comparison between the
  age-agreement table and the age-bias plot. Changed so that if
  `what="prec.stats"` the summary percentages by absolute differences is
  also printed. Modified the print of several data frames (for
  `what="bias"`, `"symmetry"`, and `"prec.stats"`) so that row names
  (i.e., row numbers) are not printed.

- [`sumTable()`](https://fishr-core-team.github.io/FSA/reference/sumTable.md):
  Added. Brought over from NCStats.

- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Changed all non-simple growth model functions with checks
  for the number of model parameters and definitions sent. Changed the
  Francis parameterization model to take only two values of `t=` (i.e.,
  the intermediate value is not used and, thus, is not required); thus,
  the `t2=` argument was removed.

- `vbGen()`: Modified. Fixed bug that developed when changes to
  `gReshap()` were made. Added warning suppression related to
  “calculations” on NAs.

- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Changed tFrancis argument to use only two ages. Changed the
  default for `meth.EV=` to “poly”. Removed jittering and added a
  transparency to the plot. Removed the box around the legend and moved
  the legend to the “bottomright.” Fixed a typo in the plot heading.

- `wsValidate()`: Modified. Replaced use of `cast()` with
  [`aggregate()`](https://rdrr.io/r/stats/aggregate.html).

## FSA 0.4.1

- **Date:** Oct13

- Changed R dependency to \>3.0.0 (because gplots package has that
  dependency).

- Added importFrom for `cast()`, `is.formula()`, and `melt()` in reshape
  package.

- [`capHistConvert()`](https://fishr-core-team.github.io/FSA/reference/capHistConvert.md):
  Corrected the formatting of the documentation.

- [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md):
  Corrected the documentation. Added a second example.

- `dietOverlap()`: Modified. Changed the “Morista” option to “Morisita”
  to be consistent with the correct spelling of the name.

- `Garvey1`: Added. Used in examples in `ks2d1()`.

- `Garvey4a`: Added. Used in examples in `ks2d1()`.

- [`histStack()`](https://plotrix.github.io/plotrix/reference/histStack.html):
  Deleted, moved to plotrix package. Arguments were changed there.

- `ks2d()`: Deleted, changed to `ks2d2()`.

- `ks2d1()`: Added.

- `ks2d2()`: Added, was `ks2d()`.

- `ks2dp()`: Deleted, changed to `ks2d2p()`.

- `ks2d2p()`: Added, was `ks2dp()`.

- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified. Changed all “messages” using
  [`cat()`](https://rdrr.io/r/base/cat.html) to using
  [`message()`](https://rdrr.io/r/base/message.html) so that they can be
  suppressed with `suppressMessage()` or `message=FALSE` in knitr. See
  “One comment on messages” at <http://yihui.name/knitr/demo/output/>.

- `pkolgomorov1x()`: Added to FSAinternals (from `ks2d()`).

- [`plotH()`](https://plotrix.github.io/plotrix/reference/plotH.html):
  Deleted, moved to plotrix package.

- `quad_dens()`: Added to FSAinternals (from `ks2d()`).

## FSA 0.4.0

- **Date:** Jun13

- Corrected all pointers to fishR vignettes (because of new webpage).

- Removed importFrom color.scale from plotrix because of changes to
  `discharge()` and `wetPerim()`.

- removed importFrom %nin% from Hmisc. See multiple changes because of
  this below.

- [`.onAttach()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md):
  Added, was `.onLoad()`.

- `.onLoad()`: Deleted, now
  [`.onAttach()`](https://fishr-core-team.github.io/FSA/reference/FSA-internals.md).

- `addMargins()`: Deleted, moved back to NCStats.

- `addSigLetters()`: Deleted, moved back to NCStats.

- [`addZeroCatch()`](https://fishr-core-team.github.io/FSA/reference/addZeroCatch.md):
  Modified. Changed the looping structure for finding the sampling event
  and species combinations that need zeros. This should speed things up
  substantially. Also, modified to allow no `idvar=` variables. Finally,
  the returned data frame has the variables (columns) in the same order
  as the original data frame (rather than having the order modified).

- `ageComp()`: Modified some of the code to adjust for name changes in
  [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md).
  Modified to use a formula notation.

- `ageKey()`: Modified to using a formula notation. This removed the
  `dl=`, `cl=`, and `ca=` arguments. Made minor adjustments to the help
  pages (in addition to changes related to the argument modifications).

- `bcFuns()`: Removed use of %nin%.

- [`capFirst()`](https://fishr-core-team.github.io/FSA/reference/capFirst.md):
  Modified so that ONLY the first letter is capitalized (previous
  version would de-capitalize the first letter in the second word but
  leave the rest of the letters capitalized).

- [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md):
  Modified to correct an error that occurred when computing the Method B
  table when a capture history occurred only once or not at all.

- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified by adding the Hoenig et al. (1983) bias correction formula
  for the estimate of Z as the default option.

- [`confint.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):
  Removed use of %nin%.

- `discharge()`: Deleted, moved to NCStats (to reduce overhead here).

- [`histStack()`](https://plotrix.github.io/plotrix/reference/histStack.html):
  Modified by adding a formula method (`histStack.formula()`) which
  required adding a default method (`histStack.default()`).

- [`htest.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):
  Removed use of %nin%.

- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  Modified by changing to using a formula notation and a `data=`
  argument. This means that the `df=` and `cl=` arguments are no longer
  used. In addition, the warning about fish larger than the larger
  category has been turned off. The method to handle this was not
  changed, the warning was just turned off.

- `lencatOLD()`: Added as an internal file to temporarily allow me not
  to change all functions that were affected by the changes to
  [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md).
  The functions that required this are `emp()` and `wsValidate()`.

- `lenFreqExpand()`: Modified to deal with
  [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md)
  change.

- `limnoProfilePlot()`: Deleted, moved to NCStats (to reduce overhead
  here).

- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Removed use of %nin%.

- [`plotBinResp()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  Modified by moving `makeColor()` internal function to FSA-internals so
  that it can also be used by
  [`tictactoe()`](https://fishr-core-team.github.io/FSA/reference/tictactoe.md).

- `predict.bootCase()`: Added.

- `PSSLit`: added from RSDLit. Added from Ogle and Winfield (2009) for
  ruffe, Bonvechio et al. (2010) for Suwannee bass, and from Phelps and
  Willis (2013) for several “carp” species.

- `PSSLitCheck()`: Added this internal file. Modified `pssVal()`,
  `pssCalc()`, and `pssPlot()` accordingly.

- [`psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md):
  Deprecated, will delete, became `pssVal()`.

- `pssCalc()`: Added, was `rsdCalc()`. Modified to using a formula
  notation and a `data=` argument.

- `pssDataPrep()`: Added.

- `pssPlot()`: Added, was `rsdPlot()`. Modified to using a formula
  notation and a `data=` argument, to handle the default change for
  `incl.zero=` in `pssVal()`, and changed the default `pss.lty=`
  settings.

- `pssVal()`: Added, was `rsdVal()`. Changed `incl.zero=TRUE` to be the
  default.

- `recodeSpecies()`: Added.

- `rsdCalc()`: Deleted, became `pssCalc()`.

- `rsdLit()`: Deleted, became `PSSLit()`.

- `rsdPlot()`: Delted, became `pssPlot()`.

- `rsdVal()`: Deprecated, will delete, became `pssVal()`.

- `sigLetters()`: Deleted, `cld()` in multcomp has been modified to
  deprecate this.

- `simLenSelect()`: Modified to deal with
  [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md)
  change.

- [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md):
  Modified by calculating the percentage of zeros for quantitative data.
  Also changed the names in the returned vectors or data frames to
  reduce capitalization, spaces, and punctuation. Removed use of %nin%.

- [`tictactoe()`](https://fishr-core-team.github.io/FSA/reference/tictactoe.md):
  Modified by changing the way the “in balance” regions are depicted.
  This resulted in the addition of the `bal.trans=` argument.

- `tictactoeAdd()`: Modified by changing PSD labels to PSS.

- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Removed use of %nin%.

- `wetPerim()`: Deleted, moved to NCStats (to reduce overhead here).

- [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md):
  Modified. Major modifications to account for changes to `WSlit`. Added
  the
  [`capFirst()`](https://fishr-core-team.github.io/FSA/reference/capFirst.md)
  check for species name. Changed `subNA=` to `remove.submin=` to make
  consistent with `wrDataPrep()`.

- `wrDataPrep()`: Added.

- `wrVal()`: Deleted.

- `WSlit`: Modified. Completely rebuilt so that quadratic equation using
  EmP could be incorporated into the database. Also added equations for
  several new species.

- `WSLitCheck()`: Added this internal file. Modified
  [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md),
  `wrVal()`, and
  [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)
  accordingly.

- [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md):
  Modified. A major modification to account for the major changes to
  `WSLit`.

- `wsValidate()`: Removed use of %nin%.

## FSA 0.3.4

- **Date:** Jan13

- added special “fishR Vignette” sections with links to several help
  files.

- [`binCI()`](https://fishr-core-team.github.io/FSA/reference/binCI.md):
  Modified so that result is a matrix rather than sometimes (when only
  one set of CIs were computed) being a vector.

- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified by minorly adjusting how
  [`confint()`](https://rdrr.io/r/stats/confint.html) produced CIs.
  Also, disallowed using `parm=` when the user asks for CIs for the
  linear model. This allowed using
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html) as a check for
  appropriate `parm=` values. Modified the examples in the help file
  slightly and added an example of using the weighted regression method.

- [`plot.catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified so that log(catch) values less than 0 will be plotted.

- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified by minorly adjusting how
  [`confint()`](https://rdrr.io/r/stats/confint.html) produced CIs.

- [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  Modified by minorly adjusting how
  [`confint()`](https://rdrr.io/r/stats/confint.html) produced CIs and
  added a [`cat()`](https://rdrr.io/r/base/cat.html)ted output to the
  [`summary()`](https://rdrr.io/r/base/summary.html) method describing
  whether the Leslie or DeLury method was used.

- `growthModelSim()`: Modified. Streamlined the code (removed some
  “junk” and unneeded redundancies). Also corrected the error where the
  fourth parameter in the vbSchnute and Schnute were not observed to be
  connected to sliders. Also changed a few default slider values. Also
  set the minimum age (`t.min`) to 0 and cannot be over-ridden (was
  previously controlled by a slider). Thus, removed the minimum age
  slider. Also moved the maximum age slider to the bottom of the
  sliders. Changed the calls for the Gompertz models to use the full
  name (i.e., `Gompertz1` instead of `Gomp1`). Changed model= to type=
  to be more consistent with other similar functions.

- [`hyperCI()`](https://fishr-core-team.github.io/FSA/reference/hyperCI.md):
  Modified so that the result is a matrix rather than a vector.

- `leslieSim()`: Modified by adding `hscale=1.5` to resampling version.

- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified to handle the changes in
  [`hyperCI()`](https://fishr-core-team.github.io/FSA/reference/hyperCI.md)
  and
  [`binCI()`](https://fishr-core-team.github.io/FSA/reference/binCI.md).
  Also modified messages in
  [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`confint()`](https://rdrr.io/r/stats/confint.html) (to streamline).

- [`predict.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):
  Added.

- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Modified by minorly adjusting how
  [`confint()`](https://rdrr.io/r/stats/confint.html) produced CIs and
  removed a [`cat()`](https://rdrr.io/r/base/cat.html)ted line from the
  summary() method. Also, modified the “catches” for the 2- and 3-pass
  specific methods to disallow using anything but a vector with either 2
  or 3 samples.

- `srCobWeb()`: Added.

- `srSim()`: Modified. Streamlined the code (lots of “junk” code that
  did not do anything and some unneeded redundancies) were removed.
  Modified the default values and the axis labels so as to produce
  generally more interesting simulations. Modified the graphic to show
  the peak recruitment level and, if a Ricker model, the stock size
  where the peak recruitment occurs. Changed a long series of if-else
  for the different parametrizations to a
  [`switch()`](https://rdrr.io/r/base/switch.html). Changed `model=` to
  `type=` to be consistent with other srXXX functions.

- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified slightly the messages if `msg=TRUE`. Added a message for the
  Wang2 model and corrected an error for the Somers2 model.

- [`view()`](https://tibble.tidyverse.org/reference/view.html): Modified
  to remove the ability to print to a window (use method built into
  RStudio instead). Also generalized to use for both a matrix or a
  data.frame (eliminates some warning messages).

## FSA 0.3.3

- **Date:** 21Dec12

- Added ImportFrom for `slider()` and `gslider()` from the relax
  package. Deleted the ImportFrom for `slider()` from the
  `TeachingDemos` package. These functions were the same but it was
  being deprecated from `TeachingDemos`.

- General: added `call.=FALSE` to several
  [`stop()`](https://rdrr.io/r/base/stop.html)s and
  [`warning()`](https://rdrr.io/r/base/warning.html)s.

- General: replaced [`paste()`](https://rdrr.io/r/base/paste.html)
  inside of several [`cat()`](https://rdrr.io/r/base/cat.html)s.

- `ageKey()`: Modified to use
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html) with type=.

- [`binCI()`](https://fishr-core-team.github.io/FSA/reference/binCI.md):
  Modified to use `ciLabel()` (see below).

- `catchCurveSim()`: Modified in a variety of ways. First, moved the
  ability to control the recruitment age and the steadiness of the Z and
  N\*
  `changes to function arguments rather than slider controls. Second, streamlined the internal functions. Third, converted to using`gslider()`instead of`slider()\`.
  Fourth, made minor cosmetic changes to the plot. Fifth, I edited the
  help file somewhat.

- `checkStartcatW()`: Added this internal function.

- `ciLabel()`: Added this internal function.

- `cohortSim()`: Modified in a variety of ways. First, streamlined the
  internal functions so that the plot can be created individually.
  Second, converted to using `gslider()` instead of `slider()`.

- `confint.bootCase()`: Modified to use `ciLabel()`.

- [`confint.catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified to use `ciLabel()`.

- [`confint.chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified to use `ciLabel()`.

- [`confint.depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  Modified to use `ciLabel()`.

- `confint.mrClosed()`: Modified to use `ciLabel()`.

- [`confint.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):
  Modified to use `ciLabel()`.

- [`confint.removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Modified to use `ciLabel()`.

- `dietOverlap()`: Added.

- `fsa.news(), FSA.news()`: Deleted, renamed to
  [`fsaNews()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md)
  and `FSANews()`.

- `fsaNews(), FSANews()`: Renamed versions of `fsa.news()` and
  `FSA.news()`.

- `FSAsims()`: Deleted. Rarely used and not supported in non-windows and
  RStudio.

- `growthModelSim()`: Modified in a variety of ways. First, streamlined
  the internal functions so that the plot can be created individually.
  Second, converted to using `gslider()` instead of `slider()`.

- [`hyperCI()`](https://fishr-core-team.github.io/FSA/reference/hyperCI.md):
  Modified to use `ciLabel()`.

- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  Modified by using the new `checkStartcatW()` internal function.

- `lenFreqExpand()`: Modified by adding show.summary= argument and using
  the new `checkStartcatW()` internal function.

- `leslieSim()`: Modified in a variety of ways. First, combined the code
  from `leslieSim2()` into this function. This required deleting the
  use.rand= argument and adding a `type=` argument. In addition, the
  `leslieRandRun()` internal function was moved to this R document (from
  FSA-internals). Second, the functions were all streamlined with new
  internal functions. Third, converted to using `gslider()` instead of
  `slider()`. Fourth, made minor cosmetic changes to each plot
  (including adding a small legend to the old `leslieSim2())`.

- `leslieSim2()`: Deleted. See `leslieSim()`.

- `lwModelSim()`: Modified in a variety of ways. First, streamlined the
  internal functions so that the plot can be created individually (will
  ultimately allow use of the manipulate package). Second, converted to
  using `gslider()` instead of `slider()`.

- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified by removing `numdigs=` argument.

- `mrClosed1Sim()`: Modified in a variety of ways. First, streamlined
  the internal functions so that the plot can be created individually.
  Second, converted to using `gslider()` instead of `slider()`.

- [`poiCI()`](https://fishr-core-team.github.io/FSA/reference/poiCI.md):
  Modified to use `ciLabel()`.

- `rlp()`: Modified by replacing `decimals=` argument with digits=
  argument.

- `srSim()`: Modified in a variety of ways. First, streamlined the
  internal functions so that the plot can be created individually.
  Second, converted to using `gslider()` instead of `slider()`. Third,
  removed the S3methods.

- [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md):
  Modified by removing `numdigs` argument.

- `TroutDietSL`: Added for use with `dietOverlap()`.

- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified by including a catch for negative starting values of K or
  starting values of Linf that are 50% smaller or larger than the
  observed maximum length in the data set.

## FSA 0.3.2

- **Date:** 1Dec12

- Changed R dependency to \>2.14.0.

- Added a ImportsFrom for knitr (purl() in swvCode() added below).

- Moved gdata to an ImportsFrom from Suggests. Needed for nobs() in
  ci.fp1() which is used in fitPlot.ONEWAY and drop.levels() used in the
  example in RuffeWs.

- Deleted dependency on FSAdata.

- Added the following data files from FSAdata: BluegillJL, BluegillLM,
  BrookTroutTH, CodNorwegian, CutthroatAL, Ecoli, KS2D_NR, LMBassWs,
  Mirex, PikeNY, PikeNYPartial1, RSDlit, RuffeWs, SMBassLS, SMBassWB,
  SpotVA1, StripedBass1, VBGMlit, WalleyeGerowLW, WR79, WSlit. This
  allowed removing the depending on FSAdata.

- `.onLoad()`: modified slightly with a suggestion from Simon Urbanek to
  eliminate a warning on RCMD Check (that showed up on rforge.net, but
  not locally).

- `addMargins()`: added from NCStats.

- `addSigLetters()`: added from NCStats. Modified to allow the use of a
  result from sigLetters() in lets=.

- \`bootCase methods: added from NCStats. Needed to import bootCase from
  car.

- [`hist.formula()`](https://fishr-core-team.github.io/FSA/reference/hist.formula.md):
  added from NCStats.

- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  made some warning messages more concise.

- `lsmean()`: deleted. Functionality is replaced by lsmeans() in the
  lsmeans package.

- `psdVal(), rsdCalc(), rsdVal(), rsdPlot()`: added code to eliminate
  “global bindings” note when performing RCMD Check. Solutions came from
  Adrian Alexa’s response to this question:
  <https://groups.google.com/forum/?fromgroups=#!topic/cambridge-r-user-group/c7vf8o3QwDo>

- `sigLetters()`: added. Hopefully this will eventually be replaced by
  changed to cld() in the multcomp package.

- [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md):
  made some warning messages more concise.

- `swvCounts(), swvPvalue(), swvANOVA(), swvGLHT(), swvREG(), swvHtest(), swvCode(), swvFinish()`:
  added from miscOgle.

- [`view()`](https://tibble.tidyverse.org/reference/view.html): added
  from NCStats.

- `wsVal(), wrAdd()`: added code to eliminate “global bindings” note
  when performing RCMD Check. Solutions came from Adrian Alexa’s
  response to this question:
  <https://groups.google.com/forum/?fromgroups=#!topic/cambridge-r-user-group/c7vf8o3QwDo>

## FSA 0.3.1

- **Date:** 25Nov12

- Switched to using the Project mode in RStudio.

- Switched to using semantic versioning for the version number (which
  means that the hyphen before the last number has been replaced by a
  period).

- Switched to using roxygen to construct help files.

- Set some values =NULL to eliminate “global bindings” warning when
  performing the RCMD check – emp(), pos2adj(), psdVal(), simAgeBias(),
  srStarts(), vbStarts(), and wsValidate(). This did not work for the
  WSlit and RSDlit problems in rsdCalc(), rsdPlot(), rsdVal() and
  wsVal().

- Added an importFrom for lineplot.CI()) and se() from sciplot (used in
  fitPlot()).

- Added an importFrom for outlierTest() from car for use in residPlot().

- Deleted importFrom for alias() from stats (was used in wlgm()).

- Deleted importFrom for boxcox() from MASS (was used in wlgm()).

- Deleted depends on NCStats (moved many functions here (see below) and
  then made NCStats depend on FSA).

- Deleted suggests for exactRankTests (apparently no longer needed).

- Moved nlstools from depends to suggests (only needed for an example in
  confint.nlsboot that is not run because of the time required).

- Moved plotrix from depends to importsFrom for color.scale(), plotCI(),
  and thigmophobe().

- Moved quantreg from depends to importsFrom for rq() (used in emp()).

- Attempted to move reshape to importsFrom but had problems with missing
  is.formula() from plyr.

- `ageComp()`: modified class name to “ageComp” from “AgeComp”.

- [`capFirst()`](https://fishr-core-team.github.io/FSA/reference/capFirst.md):
  added.

- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  modified class name to “catchCurve” from “CC”.

- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  modified class name to “chapmanRobson” from “CR”.

- `coefPlot()`: deleted (Weisberg’s LGM is now out-dated).

- [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  modifed class name to “depletion” from “Depletion”.

- `discharge()`: modified class name to “discharge” from “StrmDschrg”.

- `emp()`: modified class names to “empXX” from “EMPxx”.

- [`fitPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  added from NCStats.

- `FroeseWs()`: modified class name to “FroeseWs” from “FROESE”.

- [`histStack()`](https://plotrix.github.io/plotrix/reference/histStack.html):
  added.

- [`hoCoef()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  added from NCStats.

- `ks2d()`: modified class name to “ks2d” from “ks2d”.

- `ks2dp()`: modified class name to “ks2dp” from “ks2dp”.

- `legendHelp()`: added (internal) from NCStats.

- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  modified by moving the two internal functions – mrc1() and mrc2() – to
  inside mrClosed, moving the two internal functions – ci.mrc1() and
  ci.mrc2() – to inside confint.mrClosed(), removed the “MRC1” and
  “MRC2” classes, changed the “MRC” class name to “mrClosed”, and added
  a catch to plot.mrClosed() to stop if the user tries to plot with
  single-census data.

- [`mrOpen()`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md):
  modified class name to “mrOpen” from “MRO”.

- [`plotBinResp()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  added from NCStats.

- [`plotH()`](https://plotrix.github.io/plotrix/reference/plotH.html):
  minor modifications to the Rd file.

- `pos2adj()`: modified the labels for the positions by including full
  names for all directions, eliminating the single letters for the four
  main directions, but also leaving the four “off” directions as
  abbreviations.

- `psdVal(), rsdVal(), rsdCalc(), rsdPlot()`: modified to use capFirst
  so that the user does not need to focus on capitalization of the
  species name.

- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  modified class name to “removal” from “Removal”.

- [`residPlot()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  added from NCStats.

- `rlp()`: modified class name to “rlp” from “RLP”.

- [`Summarize()`](https://fishr-core-team.github.io/FSA/reference/Summarize.md):
  added from NCStats.

- `typeoflm()`: added from NCStats.

- `wetPerim()`: modified class name to “wetPerim” from “WetPerim”.

- `wlgm()`: deleted (Weisberg’s LGM is now out-dated).

- `wsValidate()`: modified the classnames to “willis” from “WILLIS” and
  “empq” from “EMPQ”. Also made minor modification because of class name
  change in FroeseWs()

- `ycPlot()`: deleted (Weisberg’s LGM is now out-dated).

## FSA 0.3-0

- **Date:** 8-Nov-12

- Moved several functions from NCStats that are used quite often for
  fisheries analyses. Ultimately, I want to remove the dependency to
  NCStats.

- Deleted an importFrom for gtools, created an internal odd() instead.

- Added an importFrom for gplots, to get rich.colors() for
  chooseColors().

- Added an importFrom and removed an import for NCStats.

- `ageComp()`: modified to use internal odd(), rather than odd()
  imported from gtools.

- [`binCI()`](https://fishr-core-team.github.io/FSA/reference/binCI.md):
  moved from NCStats.

- [`chooseColors()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  copied from NCStats (i.e., same function still in NCStats).

- [`confint.nlsBoot()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):
  moved from NCStats.

- [`fact2num()`](https://fishr-core-team.github.io/FSA/reference/fact2num.md):
  moved from NCStats.

- [`htest()`](https://fishr-core-team.github.io/FSA/reference/nlsBoot.md):
  copied from NCStats (i.e., same function still in NCStats).

- `htest.nlsBOot()`: moved from NCStats.

- [`hyperCI()`](https://fishr-core-team.github.io/FSA/reference/hyperCI.md):
  moved from NCStats.

- `ks2d()`: moved from NCStats.

- `ks2dp()`: moved from NCStats.

- [`ksTest()`](https://fishr-core-team.github.io/FSA/reference/ksTest.md):
  moved from NCStats.

- [`lagratio()`](https://fishr-core-team.github.io/FSA/reference/lagratio.md):
  moved from NCStats.

- \`lsmean(), and related internals: moved from NCStats.

- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  modified so as not to depend on ci.t() from NCStats.

- [`plotH()`](https://plotrix.github.io/plotrix/reference/plotH.html):
  moved from NCStats.

- [`poiCI()`](https://fishr-core-team.github.io/FSA/reference/poiCI.md):
  moved from NCStats.

- `popSizesPlot()`: moved from NCStats.

- `pos2adj()`: moved from NCStats.

- [`rcumsum()`](https://fishr-core-team.github.io/FSA/reference/rcumsum.md):
  moved from NCStats.

- `rsdPlot()`: modified to handle situations where substock fish are not
  present in the data. Thanks to Max Wolter for pointing out this issue.

- [`Subset()`](https://fishr-core-team.github.io/FSA/reference/FSA-defunct.md):
  copied from NCStats (i.e., same function still in NCStats).

## FSA 0.2-8

- **Date:** 21Jun12

- Switched to compiling under R version 2.14.1 (64-bit).

- Changed license specification from “GPL version 2 or newer” to “GPL
  (\>= 2)” to avoid warning on check.

- Added a suggestion for gdata to fix warning with capHistConver.rd (see
  below).

- \`capHistConvert.rd: Modified the examples to not use “gdata::combine”
  by adding a “require(gdata)” in the examples and suggesting gdata in
  the description file.

- [`fishR()`](https://fishr-core-team.github.io/FSA/reference/fishR.md):
  Added.

- `simAgeBias()`: changed width to widths in layout() to avoid warning
  on check.

- `simLenSelectM()`: changed width to widths in layout() to avoid
  warning on check.

## FSA 0.2-7

- **Date:** 2Mar12
- `.onLoad()`: Modified. Moved the startup message into
  packageStartupMessage() in hopes of eliminating the warning when
  checking the package.
- `catchCurveSim()`: Modified. Changed max.age= to 15 (from 10).
  Slightly changed the labels related to ‘Z Steady’ and ‘N\* \`Steady’.
- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified. Corrected a bug for when the ages2use= argument contains
  ages that are not found in the data. Thanks to Eric Berglund for
  finding this bug.
- [`psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md):
  Modified. As described for rsdVal().
- `rsdCalc()`: Added.
- `rsdPlot()`: Modified. Modified to reflect changes in rsdVal().
- `rsdVal()`: Modified. Removed the metric= and mm= arguments in favor
  of a new argument, units=, where the user chooses the units as a
  string. This streamlines, for example, the selection of mm. The
  modifications also resulted in mm being the default. Also, removed the
  appended units names from the names attribute – i.e., “stock” rather
  than “stock.mm” or “stock.in”.
- [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md):
  Added.
- `wrVal()`: Modified. As described for wsVal().
- [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md):
  Modified. Removed the justcoef= argument. Added the ab= and comment=
  arguments. Also, removed the appended units names from the names
  attribute – i.e., “int” rather than “int.E” or “int.mm”.

## FSA 0.2-6

- **Date:** 1Oct11

- Switched to compiling under R version 2.13.1 (32-bit).

- Removed importFroms that were required for updateFSA().

- Removed splines package from imports list (not needed).

- [`capHistConvert()`](https://fishr-core-team.github.io/FSA/reference/capHistConvert.md):
  Modified. Modifications to handle changes to capHistSum().

- [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md):
  Modified. Changed the returned list structure. First, caphist.sum is
  now caphist. Second, if only two samples are given, then only caphist
  and sum, where sum is a data frame of the required summaries for the
  Petersen method, are returned. If more than two samples are given,
  then caphist, sum, methodB.top, and methodB.bot are returned. Note
  that there is n\* \`longer an item labeled as schnabel.sum returned.

- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  Modified. Modifications to handle the changes to capHistSum(). Also
  modified so that if only two samples were summarized in a CapHist
  object and that object is supplied as the first argument to mrClosed()
  then the Petersen method will find the data it needs from the CapHist
  object.

- `rsdPlot()`: Modified. Modified calls to min() and max() to include
  na.rm=TRUE. This fixes bug related to vectors with missing values.

- `updateFSA()`: Removed.

- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Added ‘Somers2’ option to type= argument.

- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified. Added ‘Somers2’ option to type= argument.

## FSA 0.2-5

- **Date:** 19Aug11

- Modified description file to show my e-mail address.

- Added `cnvrt.coords()` as an ImportFrom TeachingDemos. Needed for
  `simAgeBias()` and `simLenSelectM()`.

- `ageKey()`: Modified. Length categories in the length sample, if none
  are provided in len.breaks=, are constructed from the length
  categories present in the age-length key rather than guessing at a
  starting value and width and creating evenly spaced categories. This
  should fix the bug that occurred when an age-length key was originally
  created with even length categories but the key is so sparse that the
  length categories with actual data are uneven. Also, changed the error
  catching so that the routine is stopped if a length in the length
  sample is smaller than the smallest length category in the age length
  key but will only elicit a warning if the largest length is greater
  than the largest length category in the age-length key.

- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Modified. Changed to have a .default and .formula method.

- [`chapmanRobson.default()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Added.

- [`chapmanRobson.formula()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  Added.

- `FSAsims()`: Modified. Corrected calls to growthModelSim() for von
  Bertalanffy models.

- `growthModelSim()`: Modified. Changed from modeling “size” to modeling
  “length” (or “weight” for just “vbTypicalW” and “vbOriginalW”).
  Changes required adding two new model options – “vbTypicalW” and
  “vbOriginalW” – for modeling weights and leaving all of the original
  model options as models for length. Added a max.wt= argument for use
  when modeling weights. Removed “vbBevertonHolt” as a model option
  because it is covered by “vbTypical” and was not actually implemented.
  Changed order of models so that “vbTypical” rather than “vbOriginal”
  is the default model used. Made slight cosmetic changes to slider bar
  options (e.g., “to” became “t_0”). Made changes and some corrections
  to the .Rd file.

- `rsdPlot()`: Added. Still needs more thorough proofing.

- `simAgeBias()`: Added.

- `simAges()`: Added.

- `simApplyAgeBias()`: Added.

- `simLenFromAge()`: Added.

- `simLenSelectM()`: Added.

- `simLenSelectP()`: Added.

- `vbComp()`: Modified. Streamlined the code. Changed the t= argument to
  ages= to remove any possible confusion with t(). Removed the option to
  model over ages provided in the (previous) t= argument. Instead the
  ages= argument can be used to represent the maximum age to model to.
  The ages= argument can be a vector such that each simulation can have
  a different set of ages over which the model is evaluated. This allows
  for more realistinc modeling.

## FSA 0.2-4

- **Date:** 15Jun11

- Switched to compiling under R version 2.13.0.

- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Modified Wang’s formulas to be for length increments. Added
  a length increments version to Faben’s method (“Fabens2”).

## FSA 0.2-3

- **Date:** 18Apr11

- Updated citation file.

- Added importFrom for tools and utils packages.

- `ageKey()`: Modified. Added a len.breaks= argument so that an
  age-length key with variable widths for the length categories can be
  used. Added an example to the Rd file to illustrate the use.

- `confint.MRC()`: Modified. Replaced numdigs= argument with digits=
  argument. Retained numdigs= for backwards compatability.

- \`lwPredsComp.Rd: Modified. Replaced use of lgrep() with grepl()
  because of change in NCStats.

- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Modified. Changed order of items printed in the returned list. In
  addition, if the type is one of Zippin, CarleStrub, or Seber3 then a
  set of intermediate values (k, T, and X) is also included in the
  returned list. The first change is cosmetic, the second change was
  made to help with some troubleshooting. Added an argument to allow
  choosing the method of contructing SE for the CarleStrub method.
  Created an internal function for computing the Zippin SE method to
  allow easier use with the other methods. The help file was changed to
  make note of the non-estimable SE when No=T in the CarleStrub method
  under certain circumstances. These changes result in a different SE
  being reported if the CarleStrub method is used and CS.se=“Zippin”
  (the default) is used. The “old” results can be obtained by using
  CS.se=“Alternative”. I have yet to find a solid references for this
  SE.

- `summary.MRC()`: Modified. Replaced numdigs= argument with digits=
  argument. Retained numdigs= for backwards compatability.

- `tictactoeAdd()`: Modified. Added capability of labeling points.

- `updateFSA()`: Added. Had to add an importFrom from the tools package.

- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified. Added Wang and Wang2 functions.

## FSA 0.2-2

- **Date:** 3Mar11

- moved to compling under 2.12.1 (32-bit)

- changed dependency to \>2.11.1

- `ageComp()`: modified dramatically. Primarily added the ability to
  test for bias by comparing the mean of the y-structure to the value of
  the x-structure with t-tests adjusted for multiple comparisons.
  Modified the code to allow this to happen more efficiently and to
  output results in the plot() and summary() methods. Also modified the
  plot() method so that the default is to just show the confidence
  intervals rather than showing the CIs and the range of the data (use
  show.rng=TRUE to see the old plot). Also changed the CI colors so that
  significant differences are shown in red (default) and non-significant
  differences are shown in blue (default) (set both col.err= and
  col.err.sig= to the same color to get the old plot).

- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  modified so that vname=NULL is the default. This will default to using
  “LCat” as the variable name (as in the previous version). However,
  modified the way the vname is appended to the new data frame so that
  if vname already exists in the data frame a new name will be used
  (vname plus some number).

- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  added just.ests= argument and changed the ests part of the returned
  value to be a vector rather than a matrix. Both changes allowed for
  better use of lapply() for computing the removal estimates on more
  than one group. Changed from an error to a warning for situations
  where the method could not compute population estimates (i.e., because
  the population was not depleted). In addition, NAs are returned in
  situations where population estimates can not be made. An example of
  computing the removal estimate for more than one group was added to
  the .rd file. Thanks to Jon Bolland for asking the question that
  motivated these changes.

## FSA 0.2-1

- **Date:** 31-Jan-11
- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  Modified by adding a formula method. This required moving the original
  code into a default method and changing the age= argument to x=.
- `lenFreqExpand()`: Modified by adding the additional= argument (which
  required modifying the total= argument and adding an error check for
  the situation where the total fish to assign lengths is not greater
  than the number of fish in the measured subsample).
- `.onLoad()`: modified. Changed to include version number of loaded
  version.
- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Modified by adding simple= argument. Added a ‘Somers’ seasonal growth
  oscillations model and ‘Fabens’ model for tag-recapture data. Also
  added, but did not check, a ‘Laslett’ ‘double von Bertalanffy’ model.
- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Modified by setting a catch to return a single root for st0 or sL0 if
  the polynomial root found a double root. Thanks to Giacom\*
  \`Tavecchia for identifying this error. Added a ‘Somers’ seasonal
  growth oscillations model.

## FSA 0.2-0

- **Date:** 23-Sep-10
- `bcFuns()`: Added. Still needs to be thoroughly proofed.
- `FSAsims()`: Modified to reflect srSim() change described below.
- `listSpecies()`: Moved internal function out of being within RSDval()
  and WSval() and then added an argument for the data frame containing
  the species names. The hope was that this would correct the “n\*
  \`visible binding” warnings when performing RCMD check but it did not.
- `srModels()`: Renamed from stockRecruitModels() to be more consistent
  with the rest of the stock-recruitment functions.
- `srSim()`: Renamed from stockRecruitSim() to be more consistent with
  the rest of the stock-recruitment functions.
- `vbDataGen()`: Modified use of minAge argument – will now always
  back-calculate to age-1 but minAge denotes the minimum age-at-capture
  that will be modeled. Deleted use of cfAge variable in code.
- `vbModels()`: Added.

## FSA 0.1-6

- **Date:** 23-Aug-10
- completed changing naming convention to “camel” type – e.g.,
  `stockRecruitModels()` rather than `stock.recruit.models()`.
- `ageComp()`: renamed from age.comp().
- `ageKey()`: renamed from age.key().
- [`capHistConvert()`](https://fishr-core-team.github.io/FSA/reference/capHistConvert.md):
  renamed from caphist.convert().
- [`capHistSum()`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md):
  renames from caphist.sum().
- [`catchCurve()`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md):
  renamed from catch.curve().
- `catchCurveSim()`: renamed from cc.sim().
- [`chapmanRobson()`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md):
  renamed from chapman.robson().
- `coefPlot()`: renamed from coefplot().
- `cohortSim()`: renamed from cohort.sim().
- `emp()`: modified for name changes in NCStats.
- `FroeseWs()`: modified for name changes in NCStats.
- `FSASims()`: modified by updating to new names of simulation
  functions.
- `gConvert()`: renamed from g.convert().
- `gReshape()`: renamed from g.reshape().
- `growthRadPlot()`: renamed from growrad.plot().
- `lenFreqExpand()`: renamed from len.freq.expand().
- `leslieRandRun()`: renamed from leslie.rand.run(). This is an internal
  function.
- `leslieSim()`: renamed from leslie.sim().
- `leslieSim2()`: renamed from leslie.sim2().
- `limnoProfilePlot()`: renamed from limnoprofile.plot().
- `lwModelSim()`: renamed from lwmodel.sim().
- `lwPredsComp()`: renamed from comp.lwpreds().
- [`mrClosed()`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md):
  renamed from mr.closed(). Modified for name changes in NCStats.
- `mrClosed1Sim()`: renamed from mr.closed1.sim().
- [`mrOpen()`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md):
  renamed from mr.open().
- [`psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md):
  renamed from PSDval().
- [`rcumsum()`](https://fishr-core-team.github.io/FSA/reference/rcumsum.md):
  deleted. Moved to NCStats package.
- `rpl()`: modified for name changes in NCStats.
- `rsdVal()`: renamed from RSDval().
- `tictactoeAdd()`: renamed from tictactoe.add(). Modified for name
  changes in NCStats.
- `vbComp()`: renamed from vb.comp().
- `wetPerim()`: renamed from wetperim().
- `wlgm()`: modified for name changes in NCStats.
- `wrVal()`: renamed from WRval().
- [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md):
  renamed from WSval().
- `wsValidate()`: renamed from validateWs(). Also modified for name
  changes in NCStats.
- `ycPlot()`: renamed from ycplot().

## FSA 0.1-5

- **Date:** 20Aug10

- moved to compiling under 2.11.1.

- started changing my naming convention to “camel” type – e.g.,
  `stockRecruitModels()` rather than `stock.recruit.models()`. In this
  version, I am only changing the functions that I am working on. I will
  change the rest in the next version.

- added an importFrom for `nlme` as
  [`groupedData()`](https://rdrr.io/pkg/nlme/man/groupedData.html) was
  needed for `vbDataGen()`.

- `age.key()`: Modified the way that the length categories in the
  age-length key is determined. Previously I just used the rownames
  found in the key, but this allows lengths with a row of all NA or
  zeros to be considered as a length found in the age length key. Now
  the row sums are found and the sums with NaN or 0 are removed. In
  addition, I added a warning message if the row sums d\* \`not sum to
  1.

- `caphist.convert()`: Modified such that an “RMark” type can be output.
  0 chapmanPlot()\`: Added.

- `growmodel.sim()`: Deleted. Changed to growthModelSim(). See below. 0
  growthModelSim()\`: Added. Initially a renaming of growmodel.sim().
  However, the model names were changed to be more consistent with other
  functions and a method for the Mooij et al. paramaterization was
  added.

- [`growthModels()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Added.

- [`srFuns()`](https://fishr-core-team.github.io/FSA/reference/srFuns.md):
  Added.

- [`srStarts()`](https://fishr-core-team.github.io/FSA/reference/srStarts.md):
  Added.

- `stock.recruit()`: Deleted, along with all related generics.

- `stock.recruit.sim()`: Deleted. Changed to stockRecruitSim(). See
  below.

- `stockRecruitModels()`: Added.

- `stockRecruitSim()`: Initially a renaming of stock.recruit.sim.
  However, added a “formula” method which required adding generic and
  default methods. Changed the order of the S and R arguments.
  Re-ordered, modified, and added models in accordance with the
  vignette. Updated the Rd file to reflect these changes and made a very
  slight modification to the examples and added an example to illustrate
  the use of the formula. Found decent default values for simulations.

- `stockRecruitSim.default()`: Added. See above.

- `stockRecruitSim.formula()`: Added. See above.

- `vbDataGen()`: Added.

- [`vbFuns()`](https://fishr-core-team.github.io/FSA/reference/growthModels.md):
  Added.

- [`vbStarts()`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md):
  Added.

- `walfordPlot()`: Added.

## FSA 0.1-4

- **Date:** 6Jun10
- `growmodel.sim()`: added an option to fit the “original” von
  Bertalanffy function. Also added more “mis-spelling” options to the
  other model names.

## FSA 0.1-2

- **Date:** 17Dec09

- moved to compiling under 2.10.1.

- \`added a dependency to tcltk so that simulators would work properly
  upon load of FSA.

- `age.comp()`: added xlim= and ylim= arguments so user can control x-
  and y-axis limits if desired. Changed code so that better choices for
  axis limits are selected automatically if xlim and ylim are both NULL.
  Changed code so that the “extra” vertical space added when show.n=TRUE
  AND ylim is NLL is 10 percent of the y-axis range rather than just an
  extra one unit. Allowed function to work better with xaxt=“n” and
  yaxt=“n” in case the user wants to create their own axes. Removed a
  par() setting within the plotting function. Thanks to David A. Hewitt
  for pointing out the deficiences with the axis labeling.

- `age.key()`: corrected how the age column is labeled if the column did
  not already exist in the data frame. Was also indirectly modified with
  lencat() modification. Also modified to stop and warn the user if the
  length sample has fish whose lengths are not present in the length-age
  key (previously there was a warning, but then ultimately there was an
  error).

- `catch.curve()`: added a use.weights= argument to allow using weights
  in the catch curve regression as proposed by Maceina and Bettoli
  (1998).

- `chapman.robson()`: changed S result from a proportion to a percentage
  (i.e., \* \`100).

- `comp.lwpreds()`: added center.value= argument to allow centering in
  the regressions. Added an example to the .rd file.

- `fsa.news()`: added to show user the NEWS file.

- `FSA-package()`: updated.

- `growmodel.sim()`: added the ability to use a formula and data=
  argument. Made the model argument not have a default value. Corrected
  an error when both x and y were NULL. Corrected errors in the Rd file.
  Thanks to Jacek Szlakowski for pointing out these problems.

- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  modified so that an “extra” last length category (with no fish in it)
  was not included when as.fact=TRUE and drop.levels=FALSE is used. This
  should correct the “problem” of an extra all-NA row in the age-length
  keys.

- `tictactoe.add()`: added to the namespace export list. Changed order
  of items listed in the ci.type= argument to match that of bin.ci()
  from NCStats.

## FSA 0.1-1

- **Date:** 15Apr09
- added a namespace
- removed dependencies and changed to imports … left plotrix and
  quantreg as dependencies (they do not have a namespaces). left reshape
  as a dependency because of it’s dependency on plyr.
- `.FirstLib()`: removed (changed to .onLoad() because of namespace).
- `age.comp()`: modified by removing reference to “valid.n” (which is no
  longer used because of changes to Summarize() in NCStats). Modified to
  only attempt to compute SE if n\>1 and st. dev \> 0.
- `comp.lwpreds()`: added. Exported in namespace.
- `g.convert()`: fixed major error in how the function converted
  increments to radii.
- `growrad.plot()`: added. Exported in namespace.
- `mr.closed()`: modified by changing library(Rcapture) to
  require(Rcapture) in help page.
- `plot.EMPQ()`: modified by changing object$probtox$prob.
- \`emp.rd: fixed an incorrect use of Summary() (changed to summary()).
- `validateWs()`: converted sign.slope variable in the Willis method to
  a factor to deal with situations where all results were positive or
  negative.
- \`wlgm.rd: fixed the summarization example (cast() did not work with
  Summarize().

## FSA 0.0-14

- **Date:** 20Dec08
- `age.comp()`: streamlined code (put bias and difference plots in same
  function, used grconvertY for show.n, used plotCI for range intervals,
  caught and corrected CI problems when n=1 or SD=0). N\*
  \`functionality difference, just improved code.
- `growmodel.sim()`: modified by determining some of the slider values
  from the data when x= and y= arguments are not null. This makes the
  graph more useful for determining starting values in nls() modeling.

## FSA 0.0-13

- **Date:** 6Dec08

- added a dependency to quantreg (for `rq()` in `emp()`).

- added CITATION file.

- `age.comp()`: modified the plot() function by adding a ‘difference’
  method to the what= argument. This allows creation of an
  “age-difference” plot as used in Muir et al. (2008).

- `caphist.convert()`: modified by adding an event.ord= argument to
  allow the user to identify the order of the event names when
  converting from a capture-by-event type. This is particulary useful if
  the event names are things like ‘first’, ‘second’, ‘third’, ‘fourth’
  because R orders these alphabetically which adversely effects the
  correctness of the capture histories.

- `compute.Ws()`: moved this internal function out of validateWs() to be
  a stand-alone internal function. This allows usage with animation
  routines.

- `discharge(), summary.StrmDschrg(), plot.StrmDschrg()`: added.

- `emp()`: added probs= argument result to return list. Corrected ylab
  in plotting methods. Added a predict method. Added a method= argument
  that allows choice of using linear regression or quantile regression
  to find the Ws equation. Modified objects in the return list (added
  rawdata component) and added the back-transformed Wq value in regdata
  (for comparison with Gerow’s Excel tool). Changed code for finding
  summarized dataframes inside the function by using cast() from the
  reshape package – this resulted in a 3x reduction in system.time().

- `mr.closed()`: modified by correcting error in the multiple census
  methods if M, n, and m (but not R) were supplied. Also corrected an
  error in the examples.

- `rlp()`: added probs= argument result to return list. Corrected ylab
  in plotting methods. Added a predict method.

- `PSDval(),RSDval()`: added a check for missing species name so that
  the user can just type PSDval() to get the list of possible species
  names. Also added a check to see if RSDlit was already loaded.

- `validateWs()`: added probs= argument result to return list. Corrected
  ylab in plotting methods. Added a predict method. Modified EmpQ()
  internal function to use predict() methods for emp and rlp objects.
  Streamlined some of the code by including a compute.Ws() internal
  function and using the update() function. Changed code for finding
  summarized dataframes inside the function by using cast() from the
  reshape package – this resulted in a 1.5x reduction in system.time().

- `wetperim(),summary.WetPerim(),plot.WetPerim()`: added.

- `wlgm()`: major changes included moving some internal functions
  outside of wlgm(), adding the ability to use the data= argument, and
  adding the ability to fit weighted regressions on the summary
  statistics. Other minor changes were also made. Updated the .Rd file.

- `WSval(),WRval()`: added a check for missing species name so that the
  user can just type WSval() to get the list of possible species names.
  Also added a check to see if WSlit was already loaded.

## FSA 0.0-12

- **Date:** 15Jul08
- `.First.lib`: Added
- `add.zerocatch()`: added this function to add zeros to catch records
  where a species of fish was not caught.
- `limnoprofile.plot()`: added this function to simplify constructing
  plots of depth versus limnological measure with the depth decreasing
  from top to bottom.
- `rlp()`: changed default qtype= to 8 (from 7). Added a probs= argument
  to allow other than 75th percentile calculations.
- `emp()`: updated the help page. Added a logical for if p.n.low does
  not exist when using cutoff.tail. Renamed items in the output list.
  Added a table of number of individuals per length category to output
  list. Added a probs= argument to allow other than 75th percentile
  calculations. Added its own generics – rather than relying on the
  rlp() generics.
- `FroeseWs()`: added this function, and its generics, to perform the
  standard weight equation calculation as proposed by Froese (2006).
- `validateWs()`: added this function, and its generics, to perform the
  Willis and EmpQ methods for assessing length bias in the standard
  weight equations. Added a probs= argument to allow other than 75th
  percentile calculations. Added a mean= argument to allow use of means
  rather than quantiles. Modified to accept an object of class FROESE.

## FSA 0.0-11

- **Date:** 15May08

- Moved to RForge.net.

- changed to R2.7.0.

- added a dependency to `Rcapture` (for the example in
  `caphist.convert()`).

- `anova.RLP()`: added this function to produce the anova table for the
  standard weight equation.

- `caphist.convert()`: added this function convert between various
  capture history formats (FSA,event,MARK,Rcapture).

- `emp()`: added this function, and its generics, to perform Gerow’s EmP
  method for obtaining a standard weight equation.

- `fit.plot.RLP()`: added this function.

- `plot.RLP()`: modified so that color palette with a gradient rather
  than only a solid color can be used for the populations. In addition,
  added order.pop= argument that will order the populations from
  smallest to largest predicted with in the first length interval. When
  used with the color gradients this will make it easier to see which
  populations cross over other populations.  

- `rlp()`: modified function so that the user can choose to use any-mm
  length intervals rather than having 10-mm hardwired. Modified output
  in list somewhat to more closely match the output of emp().

## FSA 0.0-10

- **Date:** 1May08
- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  Modified by adding an as.fact= argument that allows the user to decide
  if the resulting variable should be returned as a factor variable or
  not. The default is set to return as a factor variable. This allows
  tables of the new variable to include zeros for levels of the new
  variable that contain no individuals. This makes some RSD/PSD (and
  likely age-length key) calculations simpler. Also added a drop.levels=
  argument to allow the user to drop unused levels if so desired.
- `mr.closed()`: This function is a combination of the old mr.closed1()
  and mr.closed2(). It also allows the user to compute single census
  estimates with multiple sub-groups in the data (i.e., length- or
  age-classes). The function also allows the user to compute an overall
  population esitmate of multiple sub-groups are present and an overall
  SE if the incl.SE=TRUE is used. It also corrects the SE computations
  implemented in version 0.0-9. This change caused the construction of
  our internal functions – mrc1, mrc2, ci.mrc1, and ci.mrc2.
- `mr.closed1()`: removed this function. Use mr.closed() instead.
- `mr.closed2()`: removed this function. Use mr.closed() instead.
- `PSDval()`: Added mm= argument so that metric result can be returned
  in mm. Also added incl.zero= argument that will include a zer\*
  \`value in the first position in the vector; this is useful for when
  creating PSD/RSD values.
- [`rcumsum()`](https://fishr-core-team.github.io/FSA/reference/rcumsum.md):
  Added this function (from NCStats).
- `RSDval()`: See PSDval description.

## FSA 0.0-9

- **Date:** unknown
- `age.comp()`: Corrected SE calculation used to construct the CIs.
  Changed the CI plotting routine to use plotCI in plotrix package –
  this puts lines rather than points on the ends of the CIs. Added a
  check for computing SDs and CIs for when n=1 or when all measurements
  are the same. This reduces (eliminates?) the number of warnings that
  are given.
- `catch.curve()`: added na.rm=TRUE arguments to min() and max() in
  plot.CC(). Changed type= argument so that “params” is the default
  rather than “lm”. This makes it more consistent with other simulation
  programs.
- `cc.sim()`: Put in catch for situations where the CV for No and Z were
  equal to zero. Originally, the program attempted to computed a random
  number from a normal distribution with a standard deviation of zero.
  This corrected the problem of n\* \`lines appearing unless the CVs
  were greater than zero.
- `ch.convert()`: STARTED A FUNCTION to CONVERT B/W CAPTURE HISTORY
  FORMATS. N\* \`RD FILE YET.
- [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  Moved type= argument to third position. Will more easily allow
  type=“Leslie” as a default (i.e., can just enter catch and effort
  vector.
- `FSAsims()`: Added a “mark-recap” menu section. Added a chapman
  1-sample M-R item to the menu.
- `leslie.sim()`: corrected the conditionals on p.surv and r.prop so
  that it asks if any not the first value is less than 1. This corrects
  the problem of R returning a large number of warnings.
- `leslie.sim2()`: corrected the call to depletion() so that the type of
  model (“Leslie”) was the third rather than the first argument. This
  was caused by a change in the usage of depletion in previous version
  changes.
- `mr.closed1()`: Modified output list to include an estimate of the
  variance as described in Ricker(1975).
- `summary.MRC1()`: Modified output so that (1) the given information is
  a little easier to read, (2) the population estimate is returned in a
  matrix, (3) the SE from Ricker(1975) can be included in the outputm,
  and (4) a label can be placed on row for the matrix output. The
  purpose of these changes was to allow the SE to be computed and to
  allow future functions to more flexibly use the output.

## FSA 0.0-8

- **Date:** unknown

- changed some to in RD files. Changed most hard-wired quotes to or in
  RD files. Changed some text-based equations to more latex-based
  equations in or markups. This fixed the Latex compilation problems
  that I was having when using RCMD check.

- `age.comp()`: Removed single-letter values from the what= argument.
  Will rely on partial matching.

- `age.key()`: Changed default name for new column with ages from “Age”
  to “age”. Added example.

- `coefplot.WLGM()`: Changed to use plotCI() from the plotrix package.
  This removed the for loop that I had programmed. This also added the
  sfrac= and gap= arguments. Updated the RD.

- [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  Removed single-letter and lower-case values from the type= argument.
  Will rely on partial matching.

- [`lencat()`](https://fishr-core-team.github.io/FSA/reference/lencat.md):
  Changed d argument to df.

- `lenfreq.expand()`: Changed d argument to df.

- `mr.open()`: Added match.arg functionality to the ci.type and phi.type
  arguments.

- `plot.RLP()`: Added “object \<- x” to allow x to be used inside the
  curve() function without confusion.

- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  Removed abbreviated values from the type= argument. Will rely on
  partial matching.

- `rlp()`: Added examples from Murphy et al. (1990)

- `stock.recruit()`: Removed abbreviated values from the type= argument
  – will rely on partial matching. Changed sumtype argument to what.

- `stock.recruit.sim()`: Changed model argument to type and added param
  argument (to make compatible with stock.recruit). Moved R and S
  arguments to beginning of argument string (more compatible with
  stock.recruit). STILL NEED to ADD SECOND RICKER MODEL.

- `vb.comp()`: Changed d argument to df.

- `wlgm.RD()`: Added example code. Added some details.

## FSA 0.0-7

- **Date:** unknown

- changed to compiling under R 2.6.1.

- added FSA.R file that loads the required librarys.

- now depends on `MASS` package because of the creation of the
  `boxcox.WLGM()` function and on the `plotrix` package for elements of
  `ycplot()`.

- `add.radcap()`: Created this function to add the radius-at-capture to
  a one-fish-per-line data frame of increments.

- `age.comp()`: Added a
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html) call for the
  what argument.

- `age.key()`: Corrected `=T` or `=F` to `=TRUE` or `=FALSE`.

- `cc.sim()`: Corrected the use of `z.param` with correct use of
  `Z.param`.

- `coefplot()`: A new generic function for plotting coefficients from a
  Weisberg Linear Growth Model analysis.

- `g.convert()`: Changed arguments from data and measure.var to df and
  in.var. Included
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html) for the type
  argument. Moved type argument as it now has a default. Added the
  in.pre argument that allows the user to identify all input variables
  by a common prefix rather than having to list them out in in.var.
  Added the out.pre argument that allows the user to control the prefix
  for the newly created variables in the output data frame. Updated help
  file.

- `g.reshape()`: Changed arguments from data and prefix to df and
  in.pre. Deleted the measure.var argument. Moved new in.pre argument
  forward and na.rm argument backward in the argument list. Used in.pre
  to identify the measure.var variables to send to the melt function. If
  id.var is left blank (and in.pre is not) then id.var is the remaining
  variables not identified by in.pre. The val.name argument was set
  equal to in.pre as the default.

- `leslie.sim()`: Corrected call to old `leslie()` function with a
  correct call to
  [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md)
  function with `type="Leslie"`.

- `leslie.sim2()`: Corrected two calls to old `leslie()` function with
  correct calls to
  [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md)
  function with `type="Leslie"`.

- `leslie.rand.run()`: Moved R Documentation alias to FSA-internal.RD.

- `mr.closed2()`: Forced the function to use the
  [`lowess()`](https://rdrr.io/r/stats/lowess.html) function in the
  stats rather than the gplots package (which is loaded from `NCStats`).
  Also changed the `loess.f` argument to f and added the iter argument
  for sending to [`lowess()`](https://rdrr.io/r/stats/lowess.html).

- `confint.MRO()`: Put in a logical catch when printing the confidence
  intervals because if `ci.type="Manly"` then CIs for B are not
  computed.

- `summary.MRO()`: Put in a logical catch when printing the estimates
  because if `ci.type="Manly"` then SEs are not computed.

- `plot.RLP()`: Corrected `=T` or `=F` to `=TRUE` or `=FALSE`.

- `wlgm()`: Created a method with a number of generics (alias, anova,
  boxcox, coef, coefplot, confint, ycplot) for performing the Weisberg
  Linear Growth Model.

- `ycplot()`: A new generic function for creating a year-class plot for
  the Weisberg Linear Growth Model analysis.

## FSA 0.0-6

- **Date:** unknown
- `agebias.plot()`: deleted and replaced with agecomp and plot.AgeComp
  functions.
- `agesunflower.plot()`: deleted and replaced with agecomp and
  plot.AgeComp functions.
- `agecomp()`: a new function that, along with its extractor functions,
  combines all of the functionality of the old `age.tests()`,
  `age.symmetry()`, `agebias.plot()`, and `agesunflower.plot()`. Allows
  for a more seamless comparison of ageing reads.
- `plot.AgeComp()`: an extractor function for objects saved from the
  `agecomp()` function. This replaces the old `agebias.plot()` and
  `agesunflower.plot()` functions.
- `summary.Agecomp()`: an extractor function for objects saved from the
  `agecomp()` function. This replaces the old `age.tests()` and
  `age.symmetry()` functions.
- `age.symmetry()`: deleted and replaced with `agecomp()` and
  `summary.AgeComp()` functions.
- `age.tests()`: deleted and replaced with `agecomp()` and
  `summary.AgeComp()` functions.
- `cc.sim()`: modified graphic to (1) include assumption violation
  labels on top of the graph and (2) label y-axis with “log(Catch)”
  rather than “ln(Catch)”.
- `delury()`: deleted and replaced with
  [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md)
  function. See notes for
  [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md)
  function.
- [`depletion()`](https://fishr-core-team.github.io/FSA/reference/depletion.md):
  created a new function that performs the Leslie or Delury method as
  determined by a user-defined argument to type. This function replaces
  the old `leslie()` and `delury()` functions. All extractor functions
  for the class “LeslieDelury” have now been changed to class
  “Depletion”.
- `plot.Depletion()`: correct xlab so that it defaults to “Cumulative
  Effort” for the Delury method.
- `leslie()`: deleted and replaced with depletion function. See notes
  for depletion function.
- `leslie.sim()`: modified graphic to include assumption violation
  labels on top of the graph.
- `mr.open()`: modified `est.N()` internal function so that N is set
  equal to n if N\<n. In other words, if the sample size at time i is
  larger than the estimated population size at time i then the estimated
  population size is set equal to the observed sample size. This
  corrects the problem of the `N.se` calculation attempting to take the
  square root of a negative number causing the `mr.open()` function to
  shut down.
- `pass.removal()`: changed name to removal.
- [`removal()`](https://fishr-core-team.github.io/FSA/reference/removal.md):
  new name for old pass.removal function.
- `rlp()`: new name for old (upper-case) RLP.
- `RLP()`: changed name to (lower-case) rlp.
