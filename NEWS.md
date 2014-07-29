# FSA 0.4.21 ongoing
* `depletion()`: Modified.  Changed `type=` to `method=` and added `DeLury` as an option to `method=` (and left `Delury`).   Changed `ricker.mod=` to `Ricker.mod=`.  Added some checking for bad arguments.  Created internal functions specific to the Leslie and DeLury methods (for isolation).  Modified some clunky code.  Added references to specific sections in Seber (2002) for SE equations.  Updated examples.  Added tests and error checking.
* `coef.depletion()`: Modified.  Added `digits=`.
* `confint.depletion()`: Modified.  Added `digits=`.  Modified the `parm=` list to be more efficient.
* `plot.depletion()`: Modified.  Removed internal `par()` settings.
* `summary.depletion()`: Modified.  Added `verbose=` and `digits=`.

# FSA 0.4.20 Jul14
* `removal()`: Modified.  Made `"CarleStrub"` the default method.  Changed `type=` to `method=`.  Changed internal `meth` object to `lbl`.  Moved all internal functions outside of `mrOpen()` environment and added other internal functions to isolate all intermediate calculations.  Added a `verbose=` and `parm=` to `summary()`.  Streamlined clunky code in `confint()` including removing the `all` and `both` options from `parm=`.  Added more checks for abd inputs, notes in the code as to sources for the fomulae, and tests.

# FSA 0.4.19 Jul14
* Modified some tests to check whether the suggested package was installed.
* `capHistSum()`: Modified.  Changed column and row labels for `$methodB.top` and column labels for `$methodB.bot`.  Added a m-array object for when more than two sampling events are present.  Added calculations for the number of fish first seen on event i (ui), the number of fish last seen on event i (vi), and the number of fish seen i times (fi) to `$sum`.
* `jolly()`: Added.  Same as `mrOpen()`, added only for convenience.
* `mrClosed()`:  Modified.  Fixed bugs around printing of CI type with Schnabel and the ignoring of `conf.level=` with Schnabel.
* `mrOpen`: Modified.  Changed `ci.type=` to `type=` and `phi.type=` to `phi.full=`.  Removed `type=` from `summary()` and added a `verbose=` which will print only the estimates if `FALSE` or both observables and estimates if `TRUE`.  Added a `verbose=` to `confint()` to control whether the message about the type of confidence interval is printed or not.  Moved all internal functions outside of `mrOpen()` environment and added other internal functions to isolate all intermediate calculations.  Changes to row and column labels in `capHistSum()` resulted in changes to row lables for `summary()` and `confint()` results.  Streamlined some clunky code.  Added checks for misformed `mb.top=` and `mb.bot=`.  Added tests and notes in the code as to sources for the fomulae.
* `plot.CapHistSum()`:  Added.
* `plot.mrClosed()`:  Modified.  Changed axis labels as the expressions did not print with some fonts and devices.

# FSA 0.4.18 Jul14
* Moved to compiling under R 3.1.1.
* Added a Suggests for `marked` for the example in `capHistConvert()`.
* `ageBias()`: Modified.  Changed default value of `min.n.CI=` from 5 to 3.  Added an `na.rm=TRUE` to the `min()` and `max()` that produced the age ranges for the age agreement table.
* `BluegillJL`:  Modified.  Corrected lake name and added a citation.
* `capHistConvert()`:  Modified.  This should probably be considered as a new function if updating from the old version.  Modifications included simplifying the structure allowed for the input data.frames (they can have only an id or a freq column and then columns related to the capture history ... this makes the function less flexible but simplifies its use for those that are most likely to use it), moved to a series of internal functions, created a common intermediate data format (which streamlined the code considerably), changed the name of the `FSA` format to `individual` and the `Rcapture` format to `frequency`, added an `out.type='event'` format, added `in.type='RMark'` and `in.type='marked'`formats, fixed the bug with outputting `RMark` format, changed the default for new frequency variables from `Freq` to `freq`, removed the `mch=` and `event=` arguments, replaced `cols=` with `cols2ignore=`, added the `include.id=` argument, changed the `in.type=` default, coded some "catches" for common mistakes in use, coded to keep the unique fish identifier in `id=` or event name given in the variable names as much as possible, fixed a bug with `event.ord=`.  Added several new examples.
* `capHistSum()`: Modified.  Change `cols=` argument to `cols2use=`.  Moved all internal functions outside of `capHistSum()` environment.
* `CutthroatAL`:  Modified.  Updated from a new source to include many more years of samples.
* `fitPlot()`:  Modified.  Changed `trans.pt=` to `transparency=`.
* `mrClosed()`:  Modified.  Completely re-built the internal file structure.  Changed `incl.inputs=` to `verbose=`.  Added the ability to construct a CI for the overall PE when multiple groups are used in a Petersen family method (thus, added a `incl.all=` to `confint()`).  Changed default for `incl.all=` from `FALSE` to `TRUE`.  Modified the messages when `verbose=TRUE`.
* `plot.AgeBias()`.  Modified.  Fixed bug that produced a warning if all of the bias t-tests were either significant or not significant.  Changed `col.err=` to `col.CI=`, `lwd.err=` to `lwd.CI=`, `col.err.sig=` to `col.CIsig=`, `col.ref=` to `col.agree=`, `lwd.ref=` to `lwd.agree=`, `lty.ref=` to `lty.agree=`, `show.rng=` to `show.range=`, `col.rng=` to `col.range=`, `lwd.rng=` to `lwd.range=`.  Removed `col.lab=` and `row.lab=` which were deprecated several minor versions ago.  Changed default values for `lwd.rng=` and `lwd.CI=` from 2 to 1.  Added a `cex.numbers=` argument for controlling the size of the numbers in the "numbers plot" (defaults to 0.9).
* `plotBinResp()`:  Modified.  Changed `trans.pt=` to `transparency=`.

# FSA 0.4.17 Jul14
* `confint.mrClosed()`: Modified.  Moved all internal functions outside of `confint.mrClosed()` environment (see `iCI.MRCMultiple()` and `iCI.MRCSingle()`).  Changed `ci.type=` to just `type=`.  Streamlined binomial method for single census.  Used `iMRCSingleSE()` to get SE for when `type="normal"` for Chapman, Bailey, and Ricker methods.
* `extraSS()`: Modified.  Slight change to row labels in output table.
* `iMRCMultiple()`:  Added.  Was `mrc2()` internal function inside of `mrClosed()` environment.
* `iMRCSingle()`:  Added.  Was `mrc1()` internal function inside of `mrClosed()` environment.
* `iMRCSingleSE()`: Added.  Moved functionality out of `summary.mrClosed()`.  Checked and documented all formulas with sources (in code and in Rd file).
* `lrt()`: Modified.  Slight change to row labels in output table.
* `mrClosed()`: Modified.  Moved all internal functions outside of `mrClosed()` environment (see `iMRCMultiple()` and `iMRCSingle()`).  Changed `type=` argument to `method=`.  Added more catches for argument problems (required setting `n=`, `m=`, `M=` and `R=` to `NULL`).  Streamlined warning message for when `incl.SE=TRUE` is used with Schnabel or Schumacher-Eschmeyer method.  Added tests and reported results in the help file for population size, SE, and CI estimates for each method.
* `plot.mrClosed()`: Modified.  Removed setting of `par()`.  Changed from using `lowess()` to using `loess()` and set better default values.  Added descriptive text to help file.
* `summary.mrClosed()`: Modified.  Moved SE calculations into an internal function (see `iMRCSingleSE()`).

# FSA 0.4.16 Jul14
* `BluegillLM`: Modified.  Added a seealso.
* `residPlot()`: Modified.  Changed the loess-related methods to use `loess()`, to put an approximate confident band with the line, the line and band are "under" the points, the line is lighter.  Put the horizontal reference line at zero under the points.  Made `loess=TRUE` the default.
* `iAddLoessLine()`: Modified.  See `residPlot()`.
* `iHndlFormula()`: Modified.  COrrected the positioning of the explanatory variables when the model has a response variable.
* `iMakeBaseResidPlot()`:  Added as an internal function to `residPlot()` to simplify some coding.
* `iMakeColor()`: Modified.  More intelligently handles values that are greater than 1 (converts them to decimals by inverting.)
* `lwPredsComp()`: Modified.  Changed `mdl=` to `object=`.  Added use of internal `iHndlFormula()` and moved two internal functions outside the main function.  Changed default for intervals from `both` to `confidence` and changed so that if only the confidence or prediction intervals are plotted they will be black with `lwd=` width (if both are plotted the CI is now black and the PI is now blue).  Added a `show.preds` argument.  Changed `connect.means=` to `connect.preds=`.  Changed default `lwd=` value and how it is used for CIs, PIs, and the connection lines.  Added `col.connect=` argument.  Removed `mar` and `mgp` from `par()` call (left `mfrow`).  Added more examples.  Added tests for error messages.
* `residPlot()`: Modified.  Added `inclHist=` argument.  Corrected a bug around the use of `thigmophobe()` in `iAddOutlierTest()`.  Changed default for `student=` to `FALSE`.  Modified and added more examples.
* `SMBassWB`: Modified.  Added a seealso.


# FSA 0.4.15 Jun14
* lots of roxygen2 Rd cleaning.
* `addLoessLine()`: Deleted.  Moved functionality to `iAddLoessLine()` and moved code to `residPlot()` file..
* `addOutlierTestResults()`: Deleted.  Moved functionality to `iAddOutlierTestResults()` and moved code to `residPlot()` file.
* `capHistConvert()`: Added an `interactive()` to the `Rcapture` example in the help file.
* `checkStartcatW()`: Deleted.  Moved functionality to `iCheckStartcatW()`.
* `ci.fp()`: Deleted.  Moved functionality to `iCIfp()` and moved code to `fitPlot()` file.
* `ci.fp.1()`: Deleted.  Moved functionality to `iCIfp1()` and moved code to `fitPlot()` file.
* `ciLabel()`: Deleted.  Moved functionality to `iCILabel()`.
* `getAllDependencies()`: Deleted.  Moved functionality to `iGetAllDependencies()` and moved code to `swvUtils` file.
* `getFilePrefix()`: Deleted.  Moved functionality to `iGetFilePrefix()` and moved code to `swvUtils` file.
* `getMainTitle()`: Deleted.  Moved functionality to `iGetMainTitle()` and moved code to `residPlot()` file.
* `getVarFromFormula()`: Deleted.  Moved functionality to `iGetVarFromFormula()`.
* `hndlFormula()`: Deleted.  Moved functionality to `iHndlFormula()`.
* `hndlMultWhat()`: Deleted.  Moved functionality to `iHndlMultWhat()`.
* `iAddLoessLine()`: Added.  Was `addLoessLine()`.
* `iAddOutlierTestResults()`: Added.  Was `addOutlierTestResults()`.
* `iCheckStartcatW()`: Added.  Was `checkStartcatW()`.
* `iCIfp()`: Added.  Was `ci.fp()`.
* `iCIfp1()`: Added.  Was `ci.fp.1()`.
* `iCILabel()`: Added.  Was `ciLabel()`.
* `iGetAllDependencies()`: Added.  Was `getAllDependencies()`.
* `iGetFilePrefix()`:  Added.  Was `getFilePrefix()`.
* `iGetMainTitle()`: Added.  Was `getMainTitle()`.
* `iGetVarFromFormula()`: Added.  Was `getVarFromFormula()`.
* `iHndlFormula()`: Added.  Was `hndlFormula()`.
* `iHndlMultWhat()`: Added.  Was `hndlMultWhat()`.
* `iLegendHelp()`: Added.  Was `legendHelp()`.
* `iMakeColor()`: Added.  Was `makeColor()`.
* `iMakeFilename()`: Added.  Was `makeFilename()`.
* `iMakeItemsToRemove()`:  Added.  Was `makeItemsToRemove()`.
* `iProcessSessionInfo()`: Added.  Was `processSessionInfo()`.
* `iPSDLitCheck()`: Added.  Was `psdLitCheck()`.
* `is.even()`: Added.
* `is.odd()`: Added.  Was `odd()`.
* `iTypeoflm()`: Added.  Was `typeoflm()`.
* `iwsLitCheck()`: Added.  Was `wsLitCheck()`
* `legendHelp()`: Deleted.  Moved functionality to `iLegendHelp()`.
* `listSpecies()`: Deleted.  Moved functionality to `iListSpecies()`.
* `makeColor()`: Deleted.  Moved functionality to `iMakeColor()`.
* `makeFilename()`: Deleted.  Moved functionality to `iMakeFilename()` and moved code to `swvUtils` file.
* `makeItemsToRemove()`: Deleted.  Moved functionality to `iMakeItemsToRemove()` and moved code to `swvUtils` file.
* `odd()`: Deleted.  Moved functionality to `is.odd()`.
* `predict.nlsBoot()`:  Added an `interactive()` to the `nlstools` example in the help file.
* `printProgressMsg()`:  Deleted.  Not used anywhere.
* `processSessionInfo()`: Deleted.  Moved functionality `iProcessSessionInfo()` and moved code to `swvUtils` file.
* `PSDLitCheck()`: Deleted.  Moved functionality to `iPSDLitCheck()` and moved code to `psdVals()` file.
* `pssCalc()`: Deleted.  Was deprecated several versions ago. See `psdCalc()`.
* `pssDataPrep()`: Deleted.  Was deprecated several versions ago. See `psdDataPrep()`.
* `pssPlot()`: Deleted.  Was deprecated several versions ago. See `psdPlot()`.
* `pssVal()`: Deleted.  Was deprecated several versions ago. See `psdVal()`.
* `typeoflm()`: Deleted.  Moved functionality to `iTypeoflm()`.
* `wsLitCheck()`: Deleted.  Moved functionality to `iwsLitCheck()` and moved code to `wsVals()` file.

# FSA 0.4.14 Jun14

* added tests (in `test_VonB2b.R`) to assure that group comparisons of von Bertalanffy parameters equal those in Kimura (1980) and `vblrt()` in `fishmethods`.
* added importsFrom for `lmtest` for `lrt()`.  Also used in testing (`test_VonB2b.R`).
* `confint.nlsBoot()`: Modified.  Modified the plotting to use `hist.formula()`, removed `par(mar=)` definitions, and added `err.col=` and `lwd.col=` to control the color and line width of the confidence interval line on the plot.
* `extraSS()`: Added.
* `growthModels()`: Modified.  Added Weisberg parameterization.  Changed `vbGallucciQuinn` to `vbGQ`.
* `growthModelSim()`: Modified.  Added Weisberg parameterization.  Added `vbGQ` abbreviation (synonymous with `vbGallucciQuinn`).
* `lrt()`: Added.
* `vbFuns()`: Modified.  Added Weisberg parameterization.  Added `vbGQ` abbreviation (synonymous with `vbGallucciQuinn`).  Simplified the functions for when `simple=FALSE` (no error checking now).
* `vbModels()`: Modified.  Added Weisberg parameterization.  Changed `vbGallucciQuinn` to `vbGQ`.
* `vbStarts()`: Modified.  Added Weisberg parameterization.  Added `vbGQ` abbreviation (synonymous with `vbGallucciQuinn`).  Added an internal function for checking whther the starting values for K and Linf made sense.


# FSA 0.4.13 Jun14

* added testthat files for error checking of `chapmanPlot()`, `vbFuns()`, `vbStarts()`, and `walfordPlot()`.  Added a testthat file for checking that the von Bertalanffy fitting using `vbFuns()` and `vbStarts()` matches other sources.
* `ageBias()`: Modified.  Deprecated `col.lab=` and `row.lab=` and replaced with `ref.lab=` and `nref.lab=`.  Moved all functions that were internal to main functions to being internal to the package.  In the process, I changed the names of the internal functions slightly, made explicit the argument passing, and added internal descriptions of the internal files.  Changed several if else strings in the plot method to a `switch()`.
* `agePrecision()`: Modified.  Changed some messages so they were not as wide.
* `chapmanPlot()`: Modified.  Removed S3 functions so that `vbStarts()` has to use a formula.  Added some checking related to the formula. 
* `growthModels()`: Modified.  Created an internal function that eliminates repetitiveness between this and `vbModels()`.  Changed the `GompX` types to `GompertzX`.
* `growthModelSim()`: Modified.  Removed S3 functions so that `growthModelSim()` has to use a formula.  Added some checking related to the formula.  Changed the order of the arguments so that `formula=` and `data=` come before `type=`.  This allows a similar interface with `vbStarts()`.  Included a hack that still allows the user to enter a type as the first argument (and thus not have to type `type=` if any parameterization besides the `vbTypical` is being used).  Corrected spelling of Gallucci for Gallucci and  Quinn model.
* `hndlFormula()`: Modified.  Fixed bug with expected number of response variables value in return list.
* `SpotVA1`: Modified.  Updated reference.
* `vbFuns()`: Modified.  Changed `schnute` parameterization to use L3 instead of L2 and t3 instead of t2. 
* `vbModels()`: Modified.  Created an internal function that eliminates repetitiveness between this and `growthModels()`.
* `vbStarts()`: Modified.  Removed S3 functions so that `vbStarts()` has to use a formula.  Added some checking related to the formula.  Changed `tFrancis=` to `ages2use=`.  Changed the Schnute method to use the ages in `ages2use=` rather than being hard-wired to use the minimum and maximum observed age.  Both the Schnute and Francis methods will use the minimum and maximum observed ages if `ages2use=NULL`.  Added a catch for if `ages2use=` are in descending order (should be in ascending order).  Changed `Schnute` parameterization to use L3 instead of L2.
* `walfordPlot()`: Modified.  Removed S3 functions so that `vbStarts()` has to use a formula.  Added some checking related to the formula.  


# FSA 0.4.12 May14

* added Suggests for `testthat`, `fishmethods`, `FSAdata` for testing and `popbio` for an example that was made "interactive" from "dont run"(see below).
* added testthat files for `ageBias()` and `agePrecision()`.
* `ageBias()`: Modified.  Removed unit testings from examples and put in the testing file.
* `agePrecision()`: Modified.  Removed deprecated `what="agreement"`.
* `confint.nlsBoot()`: Modified.  Changed example from "dont run" to "interactive."
* `fact2num()`: Modified.  Changed example from "dont run" to "interactive."
* `fishR()`: Modified.  Removed `news` and added `posts` to the `where=` argument.  Cleaned up the Rd file.  Changed example from "dont run" to "interactive."
* `FSA()`: Modified.  Cleaned up the Rd file.
* `FSANews()`, `fsaNews()`: Modified.  Cleaned up and fixed the Usage section in the Rd file.  Changed example from "dont run" to "interactive."
* `growthRadPlot()`: Modified.  Changed example from "dont run" to "interactive."
* `htest.nlsBoot()`: Modified.  Changed example from "dont run" to "interactive."
* `lagratio()`: Modified.  Changed example from "dont run" to "interactive."
* `lencat()`: Modified.  Changed Rd file for deletion of `view()`.
* `popSizesPlot()`: Modified.  Changed example from "dont run" to "interactive."
* `TroutDietSL`: Modified.  Changed Rd file for deletion of `view()`.
* `view()`: Deleted.  Moved to NCStats package.
* `wrDataPrep()`: Modified.  Changed Rd file for deletion of `view()`.

# FSA 0.4.11 May14

* Removed Roxygen directives in DESCRIPTION (with changes to roxygen2 4.0.1).
* Changed `@S3method` and `@method` to `@export` in the following files according to changes in ROxygen2 as [described here](http://stackoverflow.com/questions/7198758/roxygen2-how-to-properly-document-s3-methods/7199577#7199577), among several other places: `ageBias`, `agePrecision`, `bootCase`, `catchCurve`, `chapmanRobson`, `confint.nlsboot`, `depletion`, `dietOverlap`, `fitPlot`, `hist.formula`, `htest.nlsBoot`, `ks2d1`, `ks2d1p`, `ks2d2`, `ks2d2p`, `ksTest`, `lencat`, `mrClosed`, `mrOpen`, `plotBinResp`, `predict.nlsBoot`, `removal`, `residPlot`, `srStarts`, `Subset`, `Summarize`, `sumTable`, `vbStarts`, and `walfordChapmanPlot`.

* `addZeroCatch()`: Modified.  Added a catch for the situation where no zeroes need to be added to the data.frame.  Cleaned-up the help file, modified the examples, and added another example.  Thanks to Ben Neely for bringing this bug (handling where zeroes are not needed) to my attention.
* `capHistSum()`: Modified.  Cleaned up the code (no changes in functionality).
* `catchCurveSim()`: Deleted.  Moved to FSAsim package.
* `checkstartcatw()`: Modified.  Changed the catch for whether the starting category value was greater than the minimum observed value to correct for a pathological case where they were equal but not with machine rounding.
* `lenFreqExpand()`: Modified.  Slightly changed the examples in the help file.
* `lwPredsComp()`: Modified.  Streamlined the code (no changes to functionality).
* `mrOpen()`: Modified.  Streamlined the code (no changes to functionality).  Removed all explicity partial matching options in `switch()`es as these were already caught with previous `match.arg()`s.

# FSA 0.4.10 May14

* Added Roxygen directives to DESCRIPTION.
* Updated to Roxygen2 4.0.0 which modified several help files.
* `ageBias()`: Modified.  Cleaned-up the help file.
* `agePrecision()`: Modified.  Cleaned-up the help file.
* `ageKey()`: Modified.  Cleaned-up the help file and modified the example.
* `ageKeyPlot()`: Modified.  Added more description and cleaned-up the help file.
* `ageKeyPrep()`: Modified.  Added more description and cleaned-up the help file.
* `lenFreqExpand()`: Modified.  Corrected `total=` to use `length(x)` rather than `nrow(df)`, which was left over from a previous change.  Cleaned-up the help file.
* `mrClosed()`: Modified.  Increased the readability of the code (added comments, used `with()` for some long calculations, added spacing).  Added specific citations to equations in the help file.  Changed the degrees-of-freedom in the confidence interval calculation for the Schnabel methods from number of samples minus 2 to number of samples minus 1 (following Krebs).
* `poiCI()`:  Modified.  Cleaned-up the help file.
* `psdDataPrep()`: Modified.  Fixed error around `use.catnames=`.
* `swvCounts()`: Modified.  Fixed error in output.


# FSA 0.4.9 May14

* Removed nlme dependencies (with removal of `vbDataGen()`).
* `ageComp()`: Deleted.  Fully deprecated.  Use `ageBias()` and `agePrecision()` instead.
* `cohortSim()`: Deleted.  Moved to FSAsim package.
* `depletion()`: Modified.  Remove link to `leslieSim()`.
* `lengthWeightSim()`: Deleted.  Moved to FSAsim package.
* `leslieSim()`: Deleted.  Moved to FSAsim package.
* `lwModelSim()`: Deleted.  Moved to FSAsim package.
* `mrClosed()`: Modified.  Remove link to `mrClosed1Sim()`.
* `mrClosed1Sim()`: Deleted.  Moved to FSAsim package.
* `srCobWeb()`: Deleted.  Moved to FSAsim package.
* `vbComp()`: Deleted.  Moved to FSAsim package.
* `vbDataGen()`: Deleted.  Moved to FSAsim package.
* `vbFuns()`: Modified.  Remove link to `vbComp()`.
* `VBGMlit()`: Deleted.  Moved to FSAsim package.

# FSA 0.4.8 May14

* `ageBias()`: Modified.  Added the ability to use multiple `what=` arguments with `c()`.  Added `what="n"` to get the sample size on the age-agreement table.  Added `nYpos=` to `plot()` to allow control of the position of the sample size values on the plot.  Changed the order of the printing of results when `what="symmetry"` is used in `summary()`.  The order more closely follows the "level of complexity" of the tests.  Added unit test examples to the help file.
* `agePrecision()`: Modified.  Added the ability to use multiple `what=` arguments with `c()`.
* `hndlMultWhat()`: Added.  An internal file to help `ageBias()` and `agePrecision` handle multiple `what=` arguments.

# FSA 0.4.7 Apr14

* Removed all of the functions related to constructing and validating standard weight equations.  These are now in the [FSAWs package](https://github.com/droglenc/FSAWs).  This is the start of an effort to streamline the FSA package.
* Removed importFrom quantreg (only used for standard weight methods).

* `ChinookArg`: Added (from FSAdata).
* `emp()`: Removed.
* `FroesWs()`: Removed.
* `lencatOLD()`: Removed (from FSA-internals).
* `lwPredsComp()`: Modified.  Changed example to using `ChinookArg` rather than `RuffeWs` because `RuffeWs` was moved to the FSAWs package.
* `LMBassWs`: Removed.
* `rlp()`: Removed.
* `RuffeWs`: Removed.
* `WalleyeGerowLW`: Removed.
* `wsValidate()`: Removed.
* `WalleyeGerowLW`: Removed.

# FSA 0.4.6 Apr14

* Changed to compiling under R 3.1.0
* Imported `stackpoly()` from plotrix for use in `ageKeyPlot()`.
* Added concepts (that largely match those in the FSAdata pacakge) to most of the data files.
* Made some grammatical changes and added author sections to Rd files.
* `ageKeyPlot()`: Added.
* `dietOverlap()`: Modified.  Changed examples in help file to reflect changes to `lencat()`.
* `lencat()`: Modified.  Added generic functions.  `lencat.default()` accepts a vector as its first argument and returns a single vector.  `lencat.formula()` accepts a formula as its first argument and the `data=` argument.  The `lencat.formula()` is the same as the old `lencat()` and `lencat.default()` provides new functionality.  Additionally, the default for `startcat=` is now `NULL` and a value for `startcat=` is found automatically (though a value can still be supplied by the user).  The `use.catnames=` was changed to `use.names=`.  Other changes were made to simplify the code.
* `lenFreqExpand()`: Modified.  Removed the `df=` and `cl=` arguments and replaced with `x=`, which is simply a vector of length measurements.  Changed to `startcat=NULL` so that that the starting category value can be determined automatically (or can still be set by the user).


# FSA 0.4.5 Apr14 

* Converted to using github as a repository.
* Changed NEWS to NEWS.md
* Added ImportFrom for relax package (see below).
* `ageBias()`: Modified.  Added a plot that shows the number of observations at each combined age.  Changed the coding slightly around Bowker's test (added an internal function) and implemented Evans and Hoenig's and McNemar's test.  These changes resulting in adding a "table" choice to `what=` that will print just the age-agreement table.  When `what="symmetry"` is chosen all three ob Bowker's, McNemar's, and Evans-Hoenig results will be output as a table.  The age-agreement table is no longer printed when `what="symmetry"`.  In addition, `what="Bowkers"`, `what="EvansHoenig"`, and `what="McNemars"` can be used to see the Bowker's, Evans and Hoenig, and McNemars test results, respectfully.  Added a `cont.corr=` argument for use with McNemars test.
* `agePrecision()`:  Modified.  Added the ability to show raw (vs. absolute value) differences between structures.  This resulted in the removal of `what="agreement"` (though it is deprecated, with a message, for now) and the addition of `what="difference"` and `what="absolute difference"`.
* `fishR()`: Modified.  Changed to point to the github NEWS.md when `where="news"`.
* `fitPlot()`: Modified.  Changed the logistic regression code to handle the changes to `plotBinResp()` (see below).  In addition, a temporary fix was added so that the size of the y-axis labels could be modified with an external call to `par()`.  This was a fix for Glen Sutton but will ultimately need to be handled more elegantly.
* `fsaNews()`: Modified.  Changed to point to the github NEWS.md.
* `catchCurveSim()`: Added back from FSATeach (required adding ImportFrom for relax package).
* `cohortSim()`: Added back from FSATeach (required adding ImportFrom for relax package).
* `growthModelSim()`: Added back from FSATeach (required adding ImportFrom for relax package).
* `lengthWeightSim()`: Added back (was `lwModelSim()`) from FSATeach (required adding ImportFrom for relax package).
* `leslieSim()`: Added back from FSATeach (required adding ImportFrom for relax package).
* `mrClosed1Sim()`: Added back from FSATeach (required adding ImportFrom for relax package).
* `plotBinResp()`: Modified.  Added `yaxis1.ticks=` and `yaxis1.lbls=` arguments so that the user can control the tick-mark locations and labels for the left y-axis (the defaults are to show ticks every 0.1 units but only label 0, 0.5, and 1).  Added `yaxis2.show=` argument to allow the user to "turn-off" the right y-axis (defaults to being on) which is labeled with the level labels.
* `srSim()`: Added back from FSATeach (required adding ImportFrom for relax package).


# FSA 0.4.4 Apr14

* `ageKeyPrep()`: Added.
* `agePrecision()`: Modified.  Fixed the bug where the APE and CV were over-estimated in situations where the multiple ages agreed at an age=0 (thanks to Richard McBride for pointing out this error).
* `wsLit`: Modified.  Added Pursak chub information from Sulun et al. (2014).


# FSA 0.4.3 Mar14

* `ageBias()`: Added.  Extracted the age-bias related material from `ageComp()`.  Modified the code to remove unneeded code.  From `ageComp()`, remove the  `what=` argument related to differences and added a `difference=` argument.  Also changed `what="bias.diff"` to `what="diff.bias"` to allow for a quicker partial matching (i.e. separate more from `what="bias"`).  Major modifications to how the axis limits are created if none are provided.  Modified where the sample size is shown on the age-bias plot.  Added the `min.n.CI=` argument.  Added an example using `WhitefishLC` to be consistent with `agePrecision()`.
* `ageComp()`: Modified.  Split into `ageBias()` and `agePrecision()`.  Added a warning that this function is deprecated and will be removed in the future.
* `ageKey()`: Modified.  Fixed a bug that occurred when a data frame that already contained an LCat variable was provided.  
* `agePrecision()`:  Added.  Extracted age precision related material from `ageComp()`.  Modified the code to allow for calculations across more than two structures.  Code was streamlined dramatically from what was in `ageComp()`.  Added an example using WhitefishLC as it allows for demonstrating more than two age assignments.
* `capFirst()`: Modified.  Added functionality, controlled by the new words= parameter, to allow all words, rather than just the first word, to be capitalized.
* `capHistConvert()`:  Modified the help file by commenting out the example that depends on the RCapture package.  This is needed for the RForge site for the time being.
* `fitPlot()`: Modified Rd.  Added two polynomial regression examples.
* `fitPlot.IVR()`: Modified.  Changed to use new `typeoflm()`, changed `interval=` argument, removed automatic main title, removed a bunch of unneeded code.
* `fitPlot.logreg()`:  Modified.  Removed automatic main title.
* `fitPlot.nls()`: Modified.  Removed automatic main title.
* `fitPlot.ONEWAY()`:  Modified.  Changed to use new `typeoflm()`, removed automatic main title, removed one line of unneeded code.
* `fitPlot.SLR()`: Modified.  Changed to use new `typeoflm()`, changed `interval=` argument, removed automatic main title.
* `fitPlot.TWOWAY()`:  Modified.  Changed to use new `typeoflm()` and removed automatic main title
* `gReshape()`: Modified.  Added a `drop=` argument so that the user can drop some variables before reshaping.  Also, added `new.row.names=1:100000` to the `reshape()` call to work-around issues with duplicate row names (which were particularly problematic if any of the `id.vars=` had missing values.)
* `growthModels()`: Modified.  Corrected spelling of Gallucci for Gallucci and  Quinn model.
* `hist.formula()`: Modified.  Add a `col=` argument that defaults to "gray90".
* `hndlFormula()`: Added.  An internal function to handle various assessments related to using formulas.
* `lencat()`: Modified.  Added the ability to add names if the vector sent in `breaks=` is named.
* `confint.mrClosed()`: Modified.  Removed extra linespaces in printed output.  Changed default for `incl.inputs=` to FALSE.
* `summary.mrClosed()`: Modified.  Removed extra linespaces in printed output.  Changed default for `incl.inputs=` to FALSE.
* `predict.nlsBoot()`:  Modified the help file by commenting out the example that depends on the nlsBoot package.  This is needed for the RForge site for the time being.
* `psdCalc()`: Added (was `pssCalc()`).
* `psdDataPrep()`: Added (was `pssDataPrep()`) and modified.  Deleted the code in this function that added category names as this functionality was added to `lencat()`.  See `lencat()` above.
* `PSDlit`: Added (was `PSSlit`) and modified.  Changed all species names to have both words capitalized so as to follow the latest AFS guidelines.
* `psdPlot()`: Added (was `pssPlot()`).
* `psdVal()`: Added (was `pssVal()`).
* `rsdCalc()`: Deleted.
* `rsdVal()`: Deleted.
* `recodeSpecies()`: Modified.  Changed `capFirst=` to `doCapFirst=` to minimize confusion with `capFirst()`.  Change `doCapFirst=` to a character that behaves like `words=` in `capFirst()`, rather than as a logical.
* `SpotVA1`:  Modified.  Removed link to source documents because it caused a problem when making the PDF manual.
* `StripedBass1`:  Deleted.  Moved to FSAdata as no longer needed because some examples were changed to use `WhitefishLC`.
* `Subset()`:  Modified.  Added a `resetRownames=` argument.
* `swvCode()`: Modified.  Added an `out.dir=` argument.
* `swvCounts()`: Modified.  Added a `capitalize=` argument.
* `typeoflm()`: Modifed.  Changed to use `hndlFormula()`.  Made an internal function.
* `vbFuns()`: Modified.  Corrected spelling of Gallucci for Gallucci and Quinn model.
* `vbStarts()`: Modified.  Corrected spelling of Gallucci for Gallucci and Quinn model.
* `WhitefishLC`: Added (from FSAdata).
* `wsLit`: Modified.  Changed all species names to have both words capitalized so as to follow the latest AFS guidelines.

# FSA 0.4.2 Dec13

* Changed to compiling under R 3.0.2.
* Removed dependency on reshape package (see changes for `emp()`, `gReshape()`, and `ssValidate()` below) and the relax, tcltk, and TeachingDemos packages (see changes for `catchCurveSim()`, `cohortSim()`, `growthModelSim()`, `leslieSim()`, `lwModelSim()`, `mrClosed1Sim()`, `simAgeBias()`, `simAges()`, `simLenFromAge()`, `simLenSelect()`, and `srSim()` below).

* .`onAttach()`:  Modified.  Added notes to use `citation()`.
* `bcFuns()`: Modified.  Added "BPH" and "SPH" options to `type= argument` (same as "LBPH" and "LSPH", respectively).  Changed a catch using `cat()` to using `message()`.  Added some specificity to the help file (more is needed).
* `catchCurveSim()`: Deleted.  Moved to FSATeach package.
* `changesPos()`: Added.
* `cohortSim()`: Deleted.  Moved to FSATeach package.
* `emp()`: Modified.  Replaced use of `cast()` with `aggregate()`.
* `gReshape()`: Modified.  Replaced use of `melt()` with `reshape()` from base package.  Fixed bug if name of "increments" was not "inc" (now catches that `in.pre=` value is used).  Fixed bug that `na.rm=` was ignored.  Modified so that rownames are not created until after the NAs are moved or not.  Changed the default name in `var.name=` from "age" to "prvAge" to reduce the highly possible chance that there might be another variable in the data frame named "age."
* `growthModelSim()`: Deleted.  Moved to FSATeach package.
* `growthRadPlot()`:  Modified.  Slightly changed the xlab= argument default.
* `leslieSim()`: Deleted.  Moved to FSATeach package.
* `lwModelSim()`: Deleted.  Moved to FSATeach package.
* `mrClosed1Sim()`: Deleted.  Moved to FSATeach package.
* `simAgeBias()`: Deleted.  Moved to FSATeach package.
* `simAges()`: Deleted.  Moved to FSATeach package.
* `simLenFromAge()`: Deleted.  Moved to FSATeach package.
* `simLenSelect()`: Deleted.  Moved to FSATeach package.
* `srSim()`: Deleted.  Moved to FSATeach package.
* `summary.ageComp()`: Modified.  Added a `zero.print=` argument with a default of a single dash for use when printing an age-agreement table.  Added `flip.table=` argument to allow ease of comparison between the age-agreement table and the age-bias plot.  Changed so that if `what="prec.stats"` the summary percentages by absolute differences is also printed.  Modified the print of several data frames (for `what="bias"`, `"symmetry"`, and `"prec.stats"`) so that row names (i.e., row numbers) are not printed.
* `sumTable()`: Added.  Brought over from NCStats.
* `vbFuns()`: Modified.  Changed all non-simple growth model functions with checks for the number of model parameters and definitions sent.  Changed the Francis parameterization model to take only two values of `t=` (i.e., the intermediate value is not used and, thus, is not required); thus, the `t2=` argument was removed.
* `vbGen()`: Modified.  Fixed bug that developed when changes to `gReshap()` were made.  Added warning suppression related to "calculations" on NAs.
* `vbStarts()`:  Modified.  Changed tFrancis argument to use only two ages.  Changed the default for `meth.EV=` to "poly".  Removed jittering and added a transparency to the plot.  Removed the box around the legend and moved the legend to the "bottomright."  Fixed a typo in the plot heading.
* `wsValidate()`: Modified.  Replaced use of `cast()` with `aggregate()`.

# FSA 0.4.1 Oct13

* Changed R dependency to >3.0.0 (because gplots package has that dependency).
* Added importFrom for `cast()`, `is.formula()`, and `melt()` in reshape package.

* `capHistConvert()`: Corrected the formatting of the documentation.
* `capHistSum()`: Corrected the documentation.  Added a second example.
* `dietOverlap()`: Modified.  Changed the "Morista" option to "Morisita" to be consistent with the correct spelling of the name.
* `Garvey1`: Added.  Used in examples in `ks2d1()`.
* `Garvey4a`: Added.  Used in examples in `ks2d1()`.
* `histStack()`: Deleted, moved to plotrix package.  Arguments were changed there.
* `ks2d()`: Deleted, changed to `ks2d2()`.
* `ks2d1()`: Added.
* `ks2d2()`: Added, was `ks2d()`.
* `ks2dp()`: Deleted, changed to `ks2d2p()`.
* `ks2d2p()`: Added, was `ks2dp()`.
* `mrClosed()`: Modified.  Changed all "messages" using `cat()` to using `message()` so that they can be suppressed with `suppressMessage()` or `message=FALSE` in knitr.  See  "One comment on messages" at http://yihui.name/knitr/demo/output/.
* `pkolgomorov1x()`: Added to FSAinternals (from `ks2d()`).
* `plotH()`: Deleted, moved to plotrix package.
* `quad_dens()`: Added to FSAinternals (from `ks2d()`).

# FSA 0.4.0 Jun13

* Corrected all pointers to fishR vignettes (because of new webpage).
* Removed importFrom color.scale from plotrix because of changes to `discharge()` and `wetPerim()`.
* removed importFrom &#37;nin&#37; from Hmisc.  See multiple changes because of this below.

* `.onAttach()`: Added, was `.onLoad()`.
* `.onLoad()`: Deleted, now `.onAttach()`.
* `addMargins()`: Deleted, moved back to NCStats.
* `addSigLetters()`: Deleted, moved back to NCStats.
* `addZeroCatch()`: Modified.  Changed the looping structure for finding the sampling event and species combinations that need zeroes.  This should speed things up substantially.  Also, modified to allow no `idvar=` variables.  Finally, the returned data frame has the variables (columns) in the same order as the original data frame (rather than having the order modified).
* `ageComp()`: Modified some of the code to adjust for name changes in `Summarize()`.    Modified to use a formula notation.
* `ageKey()`: Modified to using a formula notation.  This removed the `dl=`, `cl=`, and `ca=` arguments.  Made minor adjustments to the help pages (in addition to changes related to the argument modifications).
* `bcFuns()`: Removed use of &#37;nin&#37;.
* `capFirst()`: Modified so that ONLY the first letter is capitalized (previous version would de-capitalize the first letter in the second word but leave the rest of the letters capitalized).
* `capHistSum()`: Modified to correct an error that occurred when computing the Method B table when a capture history occurred only once or not at all.
* `chapmanRobson()`: Modified by adding the Hoenig et al. (1983) bias correction formula for the estimate of Z as the default option.
* `confint.nlsBoot()`: Removed use of &#37;nin&#37;.
* `discharge()`: Deleted, moved to NCStats (to reduce overhead here).
* `histStack()`: Modified by adding a formula method (`histStack.formula()`) which required adding a default method (`histStack.default()`).
* `htest.nlsBoot()`: Removed use of &#37;nin&#37;.
* `lencat()`: Modified by changing to using a formula notation and a `data=` argument.  This means that the `df=` and `cl=` arguments are no longer used.  In addition, the warning about fish larger than the larger category has been turned off.  The method to handle this was not changed, the warning was just turned off.
* `lencatOLD()`: Added as an internal file to temporarily allow me not to change all functions that were affected by the changes to `lencat()`.  The functions that required this are `emp()` and `wsValidate()`.
* `lenFreqExpand()`: Modified to deal with `lencat()` change.
* `limnoProfilePlot()`: Deleted, moved to NCStats (to reduce overhead here).
* `mrClosed()`: Removed use of &#37;nin&#37;.
* `plotBinResp()`: Modified by moving `makeColor()` internal function to FSA-internals so that it can also be used by `tictactoe()`.
* `predict.bootCase()`: Added.
* `PSSLit`: added from RSDLit.  Added from Ogle and Winfield (2009) for ruffe, Bonvechio et al. (2010) for Suwannee bass, and from Phelps and Willis (2013) for several "carp" species.
* `PSSLitCheck()`: Added this internal file.  Modified `pssVal()`, `pssCalc()`, and `pssPlot()` accordingly.
* `psdVal()`: Deprecated, will delete, became `pssVal()`.
* `pssCalc()`: Added, was `rsdCalc()`.  Modified to using a formula notation and a `data=` argument.
* `pssDataPrep()`: Added.
* `pssPlot()`: Added, was `rsdPlot()`.  Modified to using a formula notation and a `data=` argument, to handle the default change for `incl.zero=` in `pssVal()`, and changed the default `pss.lty=` settings.
* `pssVal()`: Added, was `rsdVal()`.  Changed `incl.zero=TRUE` to be the default.
* `recodeSpecies()`: Added.
* `rsdCalc()`: Deleted, became `pssCalc()`.
* `rsdLit()`: Deleted, became `PSSLit()`.
* `rsdPlot()`: Delted, became `pssPlot()`.
* `rsdVal()`: Deprecated, will delete, became `pssVal()`.
* `sigLetters()`: Deleted, `cld()` in multcomp has been modified to deprecate this.
* `simLenSelect()`: Modified to deal with `lencat()` change.
* `Summarize()`: Modified by calculating the percentage of zeroes for quantitative data.  Also changed the names in the returned vectors or data frames to reduce capitalization, spaces, and punctuation.  Removed use of &#37;nin&#37;.
* `tictactoe()`: Modified by changing the way the "in balance" regions are depicted.  This resulted in the addition of the `bal.trans=` argument.
* `tictactoeAdd()`: Modified by changing PSD labels to PSS.
* `vbStarts()`: Removed use of &#37;nin&#37;.
* `wetPerim()`: Deleted, moved to NCStats (to reduce overhead here).
* `wrAdd()`: Modified.  Major modifications to account for changes to `WSlit`.  Added the `capFirst()` check for species name.  Changed `subNA=` to `remove.submin=` to make consistent with `wrDataPrep()`.
* `wrDataPrep()`: Added.
* `wrVal()`: Deleted.
* `WSlit`: Modified.  Completely rebuilt so that quadratic equation using EmP could be incorporated into the database.  Also added equations for several new species.
* `WSLitCheck()`: Added this internal file.  Modified `wsVal()`, `wrVal()`, and `wrAdd()` accordingly.
* `wsVal()`: Modified.  A major modification to account for the major changes to `WSLit`.
* `wsValidate()`: Removed use of &#37;nin&#37;.


# FSA 0.3.4 Jan13

* added special "fishR Vignette" sections with links to several help files.

* binCI(): Modified so that result is a matrix rather than sometimes (when only
    one set of CIs were computed) being a vector.
* catchCurve(): Modified by minorly adjusting how confint() produced CIs.  Also,
    disallowed using parm= when the user asks for CIs for the linear model.  This
    allowed using match.arg() as a check for appropriate parm= values.  Modified
    the examples in the help file slightly and added an example of using the 
    weighted regression method.
* plot.catchCurve(): Modified so that log(catch) values less than 0 will be plotted.
* chapmanRobson(): Modified by minorly adjusting how confint() produced CIs.
* depletion(): Modified by minorly adjusting how confint() produced CIs and added
    a cat()ted output to the summary() method describing whether the Leslie or
    DeLury method was used.
* growthModelSim(): Modified.  Streamlined the code (removed some "junk" and
    unneeded redundancies).  Also corrected the error where the fourth parameter
    in the vbSchnute and Schnute were not observed to be connected to sliders.
    Also changed a few default slider values.  Also set the minimum age (t.min)
    to 0 and cannot be over-ridden (was previously controlled by a slider).  Thus,
    removed the minimum age slider.  Also moved the maximum age slider to the
    bottom of the sliders.  Changed the calls for the Gompertz models to use the
    full name (i.e., Gompertz1 instead of Gomp1).  Changed model= to type= to be
    more consistent with other similar functions.
* hyperCI(): Modified so that the result is a matrix rather than a vector.
* leslieSim(): Modified by adding hscale=1.5 to resampling version.
* mrClosed(): Modified to handle the changes in hyperCI() and binCI().  Also modified
    messages in summary() and confint() (to streamline).
* predict.nlsBoot(): Added.
* removal(): Modified by minorly adjusting how confint() produced CIs and removed
    a cat()ted line from the summary() method.  Also, modified the "catches" for
    the 2- and 3-pass specific methods to disallow using anything but a vector
    with either 2 or 3 samples.
* srCobWeb(): Added.
* srSim(): Modified.  Streamlined the code (lots of "junk" code that did not do
    anything and some unneeded redundancies) were removed.  Modified the default
    values and the axis labels so as to produce generally more interesting
    simulations.  Modified the graphic to show the peak recruitment level and, if
    a Ricker model, the stock size where the peak recruitment occurs.  Changed a
    long series of if-else for the different parametrizations to a switch().
    Changed model= to type= to be consistent with other srXXX functions.
* vbFuns(): Modified slightly the messages if msg=TRUE.  Added a message for the
    Wang2 model and corrected an error for the Somers2 model.
* view(): Modified to remove the ability to print to a window (use method built
    into RStudio instead).  Also generalized to use for both a matrix or a
    data.frame (eliminates some warning messages).

# FSA 0.3.3 21Dec12

* Added ImportFrom for slider() and gslider() from the relax package.  Deleted the
    ImportFrom for slider() from the TeachingDemos package.  These functions were
    the same but it was being deprecated from TeachingDemos.
* General: added call.=FALSE to several stop()s and warning()s.
* General: replaced paste() inside of several cat()s.

* ageKey(): Modified to use match.arg() with type=.
* binCI(): Modified to use ciLabel() (see below).
* catchCurveSim(): Modified in a variety of ways.  First, moved the ability to 
    control the recruitment age and the steadiness of the Z and N* changes to
    function arguments rather than slider controls.  Second, streamlined the internal
    functions.  Third, converted to using gslider() instead of slider().  Fourth,
    made minor cosmetic changes to the plot.  Fifth, I edited the help file somewhat.
* checkStartcatW(): Added this internal function.
* ciLabel(): Added this internal function.
* cohortSim(): Modified in a variety of ways.  First, streamlined the internal
    functions so that the plot can be created individually.  Second, converted to
    using gslider() instead of slider().
* confint.bootCase(): Modified to use ciLabel().
* confint.catchCurve(): Modified to use ciLabel().
* confint.chapmanRobson(): Modified to use ciLabel().
* confint.depletion(): Modified to use ciLabel().
* confint.mrClosed(): Modified to use ciLabel().
* confint.nlsBoot(): Modified to use ciLabel().
* confint.removal(): Modified to use ciLabel().
* dietOverlap(): Added.
* fsa.news(), FSA.news(): Deleted, renamed to fsaNews() and FSANews().
* fsaNews(), FSANews(): Renamed versions of fsa.news() and FSA.news().
* FSAsims(): Deleted.  Rarely used and not supported in non-windows and RStudio.
* growthModelSim(): Modified in a variety of ways.  First, streamlined the internal
    functions so that the plot can be created individually.  Second, converted to
    using gslider() instead of slider().
* hyperCI(): Modified to use ciLabel().
* lencat(): Modified by using the new checkStartcatW() internal function.
* lenFreqExpand(): Modified by adding show.summary= argument and using the new
    checkStartcatW() internal function.
* leslieSim(): Modified in a variety of ways.  First, combined the code from 
    leslieSim2() into this function.  This required deleting the use.rand=
    argument and adding a type= argument.  In addition, the leslieRandRun() internal
    function was moved to this R document (from FSA-internals).  Second, the functions
    were all streamlined with new internal functions.  Third, converted to using
    gslider() instead of slider().  Fourth, made minor cosmetic changes to each
    plot (including adding a small legend to the old leslieSim2()).
* leslieSim2(): Deleted.  See leslieSim().
* lwModelSim(): Modified in a variety of ways.  First, streamlined the internal
    functions so that the plot can be created individually (will ultimately allow
    use of the manipulate package).  Second, converted to using gslider() instead
    of slider().
* mrClosed(): Modified by removing numdigs= argument.
* mrClosed1Sim(): Modified in a variety of ways.  First, streamlined the internal
    functions so that the plot can be created individually.  Second, converted to
    using gslider() instead of slider().
* poiCI(): Modified to use ciLabel().
* rlp(): Modified by replacing decimals= argument with digits= argument.
* srSim(): Modified in a variety of ways.  First, streamlined the internal
    functions so that the plot can be created individually.  Second, converted to
    using gslider() instead of slider().  Third, removed the S3methods.
* Summarize() Modified by removing numdigs= argument.
* TroutDietSL: Added for use with dietOverlap().
* vbStarts(): Modified by including a catch for negative starting values of K or
    starting values of Linf that are 50% smallr or larger than the observed maximum
    length in the data set.

# FSA 0.3.2 1Dec12

* Changed R dependency to >2.14.0.
* Added a ImportsFrom for knitr (purl() in swvCode() added below).
* Moved gdata to an ImportsFrom from Suggests.  Needed for nobs() in ci.fp1() which
    is used in fitPlot.ONEWAY and drop.levels() used in the example in RuffeWs.
* Deleted dependency on FSAdata.

* Added the following data files from FSAdata: BluegillJL, BluegillLM, BrookTroutTH,
    CodNorwegian, CutthroatAL, Ecoli, KS2D_NR, LMBassWs, Mirex, PikeNY, PikeNYPartial1,
    RSDlit, RuffeWs, SMBassLS, SMBassWB, SpotVA1, StripedBass1, VBGMlit, WalleyeGerowLW,
    WR79, WSlit.  This allowed removing the depending on FSAdata.
* .onLoad(): modified slightly with a suggestion from Simon Urbanek to eliminate
    a warning on RCMD Check (that showed up on rforge.net, but not locally).
* addMargins(): added from NCStats.
* addSigLetters(): added from NCStats.  Modified to allow the use of a result from
    sigLetters() in lets=.
* bootCase methods: added from NCStats.  Needed to import bootCase from car.
* hist.formula(): added from NCStats.
* lencat(): made some warning messages more concise.
* lsmean(): deleted.  Functionality is replaced by lsmeans() in the lsmeans package.
* psdVal(), rsdCalc(), rsdVal(), rsdPlot(): added code to eliminate "global bindings"
    note when performing RCMD Check.  Solutions came from Adrian Alexa's response
    to this question: https://groups.google.com/forum/?fromgroups=#!topic/cambridge-r-user-group/c7vf8o3QwDo
* sigLetters(): added.  Hopefully this will eventually be replaced by changed to
    cld() in the multcomp package.
* Summarize(): made some warning messages more concise.
* swvCounts(), swvPvalue(), swvANOVA(), swvGLHT(), swvREG(), swvHtest(), swvCode(),
    swvFinish(): added from miscOgle.
* view(): added from NCStats.
* wsVal(), wrAdd(): added code to eliminate "global bindings" note when performing
    RCMD Check.  Solutions came from Adrian Alexa's response to this question:
    https://groups.google.com/forum/?fromgroups=#!topic/cambridge-r-user-group/c7vf8o3QwDo

# FSA 0.3.1 25Nov12

* Switched to using the Project mode in RStudio.
* Switched to using semantic versioning for the version number (which means that
    the hyphen before the last number has been replaced by a period).
* Switched to using roxygen to construct help files.
* Set some values =NULL to eliminate "global bindings" warning when performing
    the RCMD check -- emp(), pos2adj(), psdVal(), simAgeBias(), srStarts(), 
    vbStarts(), and wsValidate().  This did not work for the WSlit and RSDlit
    problems in rsdCalc(), rsdPlot(), rsdVal() and wsVal().
* Added an importFrom for lineplot.CI()) and se() from sciplot (used in fitPlot()).
* Added an importFrom for outlierTest() from car for use in residPlot().
* Deleted importFrom for alias() from stats (was used in wlgm()).
* Deleted importFrom for boxcox() from MASS (was used in wlgm()).
* Deleted depends on NCStats (moved many functions here (see below) and then 
    made NCStats depend on FSA).
* Deleted suggests for exactRankTests (apparently no longer needed).
* Moved nlstools from depends to suggests (only needed for an example in 
    confint.nlsboot that is not run because of the time required).
* Moved plotrix from depends to importsFrom for color.scale(), plotCI(), and
    thigmophobe().
* Moved quantreg from depends to importsFrom for rq() (used in emp()).
* Attempted to move reshape to importsFrom but had problems with missing
    is.formula() from plyr.

* ageComp(): modified class name to "ageComp" from "AgeComp".
* capFirst(): added.
* catchCurve(): modified class name to "catchCurve" from "CC".
* chapmanRobson(): modified class name to "chapmanRobson" from "CR".
* coefPlot(): deleted (Weisberg's LGM is now out-dated).
* depletion(): modifed class name to "depletion" from "Depletion".
* discharge(): modified class name to "discharge" from "StrmDschrg".
* emp(): modified class names to "empXX" from "EMPxx".
* fitPlot(): added from NCStats.
* FroeseWs(): modified class name to "FroeseWs" from "FROESE".
* histStack(): added.
* hoCoef(): added from NCStats.
* ks2d(): modified class name to "ks2d" from "ks2d".
* ks2dp(): modified class name to "ks2dp" from "ks2dp".
* legendHelp(): added (internal) from NCStats.
* mrClosed(): modified by moving the two internal functions -- mrc1() and mrc2() --
    to inside mrClosed, moving the two internal functions -- ci.mrc1() and
    ci.mrc2() -- to inside confint.mrClosed(), removed the "MRC1" and "MRC2"
    classes, changed the "MRC" class name to "mrClosed", and added a catch to
    plot.mrClosed() to stop if the user tries to plot with single-census data.
* mrOpen(): modified class name to "mrOpen" from "MRO".
* plotBinResp(): added from NCStats.
* plotH(): minor modifications to the Rd file.
* pos2adj(): modified the labels for the positions by including full names for 
    all directions, eliminating the single letters for the four main directions,
    but also leaving the four "off" directions as abbreviations.
* psdVal(), rsdVal(), rsdCalc(), rsdPlot(): modified to use capFirst so that the
    user does not need to focus on capitalization of the species name.
* removal(): modified class name to "removal" from "Removal".
* residPlot(): added from NCStats.
* rlp(): modified class name to "rlp" from "RLP".
* Summarize(): added from NCStats.
* typeoflm(): added from NCStats.
* wetPerim(): modified class name to "wetPerim" from "WetPerim".
* wlgm(): deleted (Weisberg's LGM is now out-dated).
* wsValidate(): modified the classnames to "willis" from "WILLIS" and "empq" from
    "EMPQ".  Also made minor modification because of class name change in FroeseWs()
* ycPlot(): deleted (Weisberg's LGM is now out-dated).

# FSA 0.3-0 8-Nov-12

* Moved several functions from NCStats that are used quite often for fisheries
    analyses.  Ultimately, I want to remove the dependency to NCStats.
* Deleted an importFrom for gtools, created an internal odd() instead.
* Added an importFrom for gplots, to get rich.colors() for chooseColors().
* Added an importFrom and removed an import for NCStats.

* ageComp(): modified to use internal odd(), rather than odd() imported from gtools.
* binCI(): moved from NCStats.
* chooseColors(): copied from NCStats (i.e., same function still in NCStats).
* confint.nlsBoot(): moved from NCStats.
* fact2num(): moved from NCStats.
* htest(): copied from NCStats (i.e., same function still in NCStats).
* htest.nlsBOot(): moved from NCStats.
* hyperCI(): moved from NCStats.
* ks2d(): moved from NCStats.
* ks2dp(): moved from NCStats.
* ksTest(): moved from NCStats.
* lagratio(): moved from NCStats.
* lsmean(), and related internals: moved from NCStats.
* mrClosed(): modified so as not to depend on ci.t() from NCStats.
* plotH(): moved from NCStats.
* poiCI(): moved from NCStats.
* popSizesPlot(): moved from NCStats.
* pos2adj(): moved from NCStats.
* rcumsum(): moved from NCStats.
* rsdPlot(): modified to handle situations where substock fish are not present in
    the data.  Thanks to Max Wolter for pointing out this issue.
* Subset(): copied from NCStats (i.e., same function still in NCStats).

# FSA 0.2-8 21Jun12

* Switched to compiling under R version 2.14.1 (64-bit).
* Changed license specification from "GPL version 2 or newer" to "GPL (>= 2)" to
    avoid warning on check.
* Added a suggestion for gdata to fix warning with capHistConver.rd (see below).

* capHistConvert.rd: Modified the examples to not use "gdata::combine" by adding
    a "require(gdata)" in the examples and suggesting gdata in the description file.
* fishR(): Added.
* simAgeBias(): changed width to widths in layout() to avoid warning on check.
* simLenSelectM(): changed width to widths in layout() to avoid warning on check.

# FSA 0.2-7 2Mar12

* .onLoad(): Modified.  Moved the startup message into packageStartupMessage()
    in hopes of eliminating the warning when checking the package.
* catchCurveSim(): Modified.  Changed max.age= to 15 (from 10).  Slightly changed
    the labels related to 'Z Steady' and 'N* Steady'.
* chapmanRobson(): Modified.  Corrected a bug for when the ages2use= argument
    contains ages that are not found in the data.  Thanks to Eric Berglund for
    finding this bug.
* psdVal(): Modified.  As described for rsdVal().
* rsdCalc(): Added.
* rsdPlot(): Modified.  Modified to reflect changes in rsdVal().
* rsdVal(): Modified.  Removed the metric= and mm= arguments in favor of a new
    argument, units=, where the user chooses the units as a string.  This
    streamlines, for example, the selection of mm.  The modifications also
    resulted in mm being the default.  Also, removed the appended units names
    from the names attribute -- i.e., "stock" rather than "stock.mm" or "stock.in".
* wrAdd(): Added.
* wrVal(): Modified.  As described for wsVal().
* wsVal(): Modified.  Removed the justcoef= argument.  Added the ab= and comment=
    arguments.  Also, removed the appended units names from the names attribute
    -- i.e., "int" rather than "int.E" or "int.mm".
    
# FSA 0.2-6 1Oct11

* Switched to compiling under R version 2.13.1 (32-bit).
* Removed importFroms that were required for updateFSA().
* Removed splines package from imports list (not needed).

* capHistConvert(): Modified.  Modifications to handle changes to capHistSum().
* capHistSum(): Modified.  Changed the returned list structure.  First, caphist.sum
    is now caphist.  Second, if only two samples are given, then only caphist and
    sum, where sum is a data frame of the required summaries for the Petersen
    method, are returned.  If more than two samples are given, then caphist, sum, 
    methodB.top, and methodB.bot are returned.  Note that there is n* longer an
    item labeled as schnabel.sum returned.
* mrClosed():  Modified.  Modifications to handle the changes to capHistSum().
    Also modified so that if only two samples were summarized in a CapHist object
    and that object is supplied as the first argument to mrClosed() then the
    Petersen method will find the data it needs from the CapHist object.
* rsdPlot(): Modified.  Modified calls to min() and max() to include na.rm=TRUE.
    This fixes bug related to vectors with missing values.
* updateFSA(): Removed.
* vbFuns(): Modified.  Added 'Somers2' option to type= argument.
* vbStarts(): Modified.  Added 'Somers2' option to type= argument.

# FSA 0.2-5 19Aug11

* Modified description file to show my e-mail address.
* Added cnvrt.coords() as an ImportFrom TeachingDemos.  Needed for simAgeBias()
    and simLenSelectM().

* ageKey(): Modified.  Length categories in the length sample, if none are
    provided in len.breaks=, are constructed from the length categories present
    in the age-length key rather than guessing at a starting value and width and
    creating evenly spaced categories.  This should fix the bug that occurred
    when an age-length key was originally created with even length categories but
    the key is so sparse that the length categories with actual data are uneven.
    Also, changed the error catching so that the routine is stopped if a length
    in the length sample is smaller than the smallest length category in the age
    length key but will only elicit a warning if the largest length is greater
    than the largest length category in the age-length key.
* chapmanRobson(): Modified.  Changed to have a .default and .formula method.
* chapmanRobson.default(): Added.
* chapmanRobson.formula(): Added.
* FSAsims(): Modified.  Corrected calls to growthModelSim() for von Bertalanffy
    models.
* growthModelSim(): Modified.  Changed from modeling "size" to modeling "length"
   (or "weight" for just "vbTypicalW" and "vbOriginalW").  Changes required adding
   two new model options -- "vbTypicalW" and "vbOriginalW" -- for modeling weights
   and leaving all of the original model options as models for length.  The plots
   represent the different characteristics (length or weight) modeled as d* the
   slider bar options.  Added a max.wt= argument for use when modeling weights.
   Removed "vbBevertonHolt" as a model option because it is covered by "vbTypical"
   and was not actually implemented.  Changed order of models so that "vbTypical"
   rather than "vbOriginal" is the default model used.  Made slight cosmetic
   changes to slider bar options (e.g., "to" became "t_0").  Made changes and
   some corrections to the .Rd file.
* rsdPlot(): Added.  Still needs more thorough proofing.
* simAgeBias(): Added.
* simAges(): Added.
* simApplyAgeBias(): Added.
* simLenFromAge(): Added.
* simLenSelectM(): Added.
* simLenSelectP(): Added.
* vbComp(): Modified.  Streamlined the code.  Changed the t= argument to ages= to
    remove any possible confusion with t().  Removed the option to model over
    ages provided in the (previous) t= argument.  Instead the ages= argument can
    be used to represent the maximum age to model to.  The ages= argument can be
    a vector such that each simulation can have a different set of ages over
    which the model is evaluated.  This allows for more realistinc modeling.

# FSA 0.2-4 15Jun11

* Switched to compiling under R version 2.13.0.

* vbFuns(): Modified.  Modified Wang's formulas to be for length increments.
    Added a length increments version to Faben's method ("Fabens2").

# FSA 0.2-3 18Apr11

* Updated citation file.
* Added importFrom for tools and utils packages.

* ageKey(): Modified.  Added a len.breaks= argument so that an age-length key
    with variable widths for the length categories can be used.  Added an
    example to the Rd file to illustrate the use.
* confint.MRC(): Modified.  Replaced numdigs= argument with digits= argument.
    Retained numdigs= for backwards compatability.
* lwPredsComp.Rd: Modified.  Replaced use of lgrep() with grepl() because of 
    change in NCStats.
* removal():  Modified.  Changed order of items printed in the returned list.  
    In addition, if the type is one of Zippin, CarleStrub, or Seber3 then a set
    of intermediate values (k, T, and X) is also included in the returned list.
    The first change is cosmetic, the second change was made to help with some
    troubleshooting.  Added an argument to allow choosing the method of contructing
    SE for the CarleStrub method.  Created an internal function for computing the
    Zippin SE method to allow easier use with the other methods.  The help file
    was changed to make note of the non-estimable SE when No=T in the CarleStrub
    method under certain circumstances.  These changes result in a different SE
    being reported if the CarleStrub method is used and CS.se="Zippin" (the
    default) is used.  The "old" results can be obtained by using 
    CS.se="Alternative".  I have yet to find a solid references for this SE.
* summary.MRC(): Modified.  Replaced numdigs= argument with digits= argument.
    Retained numdigs= for backwards compatability.
* tictactoeAdd(): Modified.  Added capability of labeling points.
* updateFSA(): Added.  Had to add an importFrom from the tools package.
* vbFuns(): Modified.  Added Wang and Wang2 functions.

# FSA 0.2-2 3Mar11

* moved to compling under 2.12.1 (32-bit)
* changed dependency to >2.11.1

* ageComp(): modified dramatically.  Primarily added the ability to test for
    bias by comparing the mean of the y-structure to the value of the x-structure
    with t-tests adjusted for multiple comparisons.  Modified the code to allow
    this to happen more efficiently and to output results in the plot() and
    summary() methods.  Also modified the plot() method so that the default is 
    to just show the confidence intervals rather than showing the CIs and the
    range of the data (use show.rng=TRUE to see the old plot).  Also changed the
    CI colors so that significant differences are shown in red (default) and
    non-significant differences are shown in blue (default) (set both col.err=
    and col.err.sig= to the same color to get the old plot).
* lencat(): modified so that vname=NULL is the default.  This will default to 
    using "LCat" as the variable name (as in the previous version).  However,
    modified the way the vname is appended to the new data frame so that if
    vname already exists in the data frame a new name will be used (vname plus
    some number).
* removal(): added just.ests= argument and changed the ests part of the returned
    value to be a vector rather than a matrix.  Both changes allowed for better
    use of lapply() for computing the removal estimates on more than one group.
    Changed from an error to a warning for situations where the method could not
    compute population estimates (i.e., because the population was not depleted).
    In addition, NAs are returned in situations where population estimates can
    not be made.  An example of computing the removal estimate for more than one
    group was added to the .rd file.  Thanks to Jon Bolland for asking the
    question that motivated these changes.

# FSA 0.2-1 31-Jan-11

* catchCurve(): Modified by adding a formula method.  This required moving the
    original code into a default method and changing the age= argument to x=.
* lenFreqExpand(): Modified by adding the additional= argument (which required
    modifying the total= argument and adding an error check for the situation
    where the total fish to assign lengths is not greater than the number of 
    fish in the measured subsample).
* .onLoad(): modified.  Changed to include version number of loaded version.
* vbFuns(): Modified by adding simple= argument.  Added a 'Somers' seasonal
    growth oscillations model and 'Fabens' model for tag-recapture data.  Also
    added, but did not check, a 'Laslett' 'double von Bertalanffy' model.
* vbStarts(): Modified by setting a catch to return a single root for st0 or sL0
    if the polynomial root found a double root.  Thanks to Giacom* Tavecchia for
    identifying this error.  Added a 'Somers' seasonal growth oscillations model.   

# FSA 0.2-0 23-Sep-10

* bcFuns(): Added.  Still needs to be thoroughly proofed.
* FSAsims(): Modified to reflect srSim() change described below.
* listSpecies(): Moved internal function out of being within RSDval() and WSval()
    and then added an argument for the data frame containing the species names.
    The hope was that this would correct the "n* visible binding" warnings when
    performing RCMD check but it did not.
* srModels(): Renamed from stockRecruitModels() to be more consistent with the
    rest of the stock-recruitment functions.
* srSim(): Renamed from stockRecruitSim() to be more consistent with the rest of
    the stock-recruitment functions.
* vbDataGen(): Modified use of minAge argument -- will now always back-calculate to
    age-1 but minAge denotes the minimum age-at-capture that will be modeled.
    Deleted use of cfAge variable in code.
* vbModels(): Added.

# FSA 0.1-6 23-Aug-10

* completed changing naming convention to "camel" type -- e.g., stockRecruitModels()
    rather than stock.recruit.models().
    
* ageComp(): renamed from age.comp().
* ageKey(): renamed from age.key().
* capHistConvert(): renamed from caphist.convert().
* capHistSum(): renames from caphist.sum().
* catchCurve(): renamed from catch.curve().
* catchCurveSim(): renamed from cc.sim().
* chapmanRobson(): renamed from chapman.robson().
* coefPlot(): renamed from coefplot().
* cohortSim(): renamed from cohort.sim().
* emp(): modified for name changes in NCStats.
* FroeseWs(): modified for name changes in NCStats.
* FSASims(): modified by updating to new names of simulation functions.
* gConvert(): renamed from g.convert().
* gReshape(): renamed from g.reshape().
* growthRadPlot(): renamed from growrad.plot().
* lenFreqExpand(): renamed from len.freq.expand().
* leslieRandRun(): renamed from leslie.rand.run().  This is an internal function.
* leslieSim(): renamed from leslie.sim().
* leslieSim2(): renamed from leslie.sim2().
* limnoProfilePlot(): renamed from limnoprofile.plot().
* lwModelSim(): renamed from lwmodel.sim().
* lwPredsComp(): renamed from comp.lwpreds().
* mrClosed(): renamed from mr.closed().  Modified for name changes in NCStats.
* mrClosed1Sim(): renamed from mr.closed1.sim().
* mrOpen(): renamed from mr.open().
* psdVal(): renamed from PSDval().
* rcumsum(): deleted.  Moved to NCStats package.
* rpl(): modified for name changes in NCStats.
* rsdVal(): renamed from RSDval().
* tictactoeAdd(): renamed from tictactoe.add().  Modified for name changes in NCStats.
* vbComp(): renamed from vb.comp().
* wetPerim(): renamed from wetperim().
* wlgm(): modified for name changes in NCStats.
* wrVal(): renamed from WRval().
* wsVal(): renamed from WSval().
* wsValidate(): renamed from validateWs().  Also modified for name changes in NCStats.
* ycPlot(): renamed from ycplot().

# FSA 0.1-5 20Aug10

* moved to compiling under 2.11.1.
* started changing my naming convention to "camel" type -- e.g., stockRecruitModels()
    rather than stock.recruit.models().  In this version, I am only changing the
    functions that I am working on.  I will change the rest in the next version.
* added an importFrom for nlme as groupedData() was needed for vbDataGen().

* age.key(): Modified the way that the length categories in the age-length key
    is determined.  Previously I just used the rownames found in the key, but
    this allows lengths with a row of all NA or zeroes to be considered as a
    length found in the age length key.  Now the row sums are found and the
    sums with NaN or 0 are removed.  In addition, I added a warning message
    if the row sums d* not sum to 1.
* caphist.convert(): Modified such that an "RMark" type can be output.
0 chapmanPlot(): Added.
* growmodel.sim(): Deleted.  Changed to growthModelSim().  See below.
0 growthModelSim(): Added.  Initially a renaming of growmodel.sim().  However, the
    model names were changed to be more consistent with other functions and a
    method for the Mooij et al. paramaterization was added.
* growthModels(): Added.
* srFuns(): Added.
* srStarts(): Added.
* stock.recruit(): Deleted, along with all related generics.
* stock.recruit.sim(): Deleted.  Changed to stockRecruitSim().  See below.
* stockRecruitModels(): Added.
* stockRecruitSim():  Initially a renaming of stock.recruit.sim.  However, added
    a "formula" method which required adding generic and default methods.  Changed
    the order of the S and R arguments.  Re-ordered, modified, and added models
    in accordance with the vignette.  Updated the Rd file to reflect these changes
    and made a very slight modification to the examples and added an example to
    illustrate the use of the formula.  Found decent default values for simulations.
* stockRecruitSim.default(): Added.  See above.
* stockRecruitSim.formula(): Added.  See above.
* vbDataGen(): Added.
* vbFuns(): Added.
* vbStarts(): Added.
* walfordPlot(): Added.

# FSA 0.1-4 6Jun10

* growmodel.sim(): added an option to fit the "original" von Bertalanffy function.
    also added more "mis-spelling" options to the other model names.   

# FSA 0.1-2 17Dec09

* moved to compiling under 2.10.1.

* FSA-package(): updated.
* added a dependency to tcltk so that simulators would work properly upon load of FSA.
* age.comp(): added xlim= and ylim= arguments so user can control x- and y-axis limits
    if desired.  Changed code so that better choices for axis limits are selected
    automatically if xlim and ylim are both NULL.  Changed code so that the "extra"
    vertical space added when show.n=TRUE AND ylim is NLL is 10 percent of the y-axis
    range rather than just an extra one unit.  Allowed function to work better with
    xaxt="n" and yaxt="n" in case the user wants to create their own axes.  Removed
    a par() setting within the plotting function.  Thanks to David A. Hewitt for
    pointing out the deficiences with the axis labeling.
* age.key(): corrected how the age column is labeled if the column did not already
    exist in the data frame.  Was also indirectly modified with lencat() modification.
    Also modified to stop and warn the user if the length sample has fish whose
    lengths are not present in the length-age key (previously there was a warning,
    but then ultimately there was an error).
* catch.curve(): added a use.weights= argument to allow using weights in the catch
    curve regression as proposed by Maceina and Bettoli (1998).
* chapman.robson(): changed S result from a proportion to a percentage (i.e., * 100).
* comp.lwpreds(): added center.value= argument to allow centering in the regressions.
    Added an example to the .rd file.
* fsa.news(): added to show user the NEWS file.
* growmodel.sim(): added the ability to use a formula and data= argument.  Made the
    model argument not have a default value.  Corrected an error when both x and y
    were NULL.  Corrected errors in the Rd file.  Thanks to Jacek Szlakowski for
    pointing out these problems.
* lencat(): modified so that an "extra" last length category (with no fish in it) was
    not included when as.fact=TRUE and drop.levels=FALSE is used.  This should correct
    the "problem" of an extra all-NA row in the age-length keys.
* tictactoe.add(): added to the namespace export list.  Changed order of items listed
    in the ci.type= argument to match that of bin.ci() from NCStats.

# FSA 0.1-1 15Apr09

* added a namespace
* removed dependencies and changed to imports ...
    left plotrix and quantreg as dependencies (they d* not have a namespaces).
    left reshape as a dependency because of it's dependency on plyr.
    
* .FirstLib(): removed (changed to .onLoad() because of namespace).
* age.comp(): modified by removing reference to "valid.n" (which is no longer used
    because of changes to Summarize() in NCStats).  Modified to only attempt to
    compute SE if n>1 and st. dev > 0.
* comp.lwpreds(): added.  Exported in namespace.
* g.convert(): fixed major error in how the function converted increments to radii.
* growrad.plot(): added.  Exported in namespace.
* mr.closed(): modified by changing library(Rcapture) to require(Rcapture) in help page.
* plot.EMPQ(): modified by changing object$prob to x$prob.
* emp.rd: fixed an incorrect use of Summary() (changed to summary()).
* validateWs(): converted sign.slope variable in the Willis method to a factor to
    deal with situations where all results were positive or negative.
* wlgm.rd: fixed the summarization example (cast() did not work with Summarize().

# FSA 0.0-14 20Dec08

* age.comp(): streamlined code (put bias and difference plots in same function, 
    used grconvertY for show.n, used plotCI for range intervals, caught and 
    corrected CI problems when n=1 or SD=0).  N* functionality difference, just 
    improved code.
* growmodel.sim(): modified by determining some of the slider values from the 
    data when x= and y= arguments are not null.  This makes the graph more useful 
    for determining starting values in nls() modeling.

# FSA 0.0-13 6Dec08

* added a dependency to quantreg (for rq() in emp()).
* added CITATION file.

* age.comp(): modified the plot() function by adding a 'difference' method to the
    what= argument.  This allows creation of an "age-difference" plot as used in
    Muir et al. (2008).
* caphist.convert(): modified by adding an event.ord= argument to allow the user
    to identify the order of the event names when converting from a capture-by-event
    type.  This is particulary useful if the event names are things like 'first',
    'second', 'third', 'fourth' because R orders these alphabetically which
    adversely effects the correctness of the capture histories.
* compute.Ws(): moved this internal function out of validateWs() to be a stand-alone
    internal function.  This allows usage with animation routines.
* discharge(), summary.StrmDschrg(), plot.StrmDschrg(): added.
* emp(): added probs= argument result to return list.  Corrected ylab in plotting
    methods.  Added a predict method.  Added a method= argument that allows choice
    of using linear regression or quantile regression to find the Ws equation. 
    Modified objects in the return list (added rawdata component) and added the
    back-transformed Wq value in regdata (for comparison with Gerow's Excel tool).
    Changed code for finding summarized dataframes inside the function by using
    cast() from the reshape package -- this resulted in a 3x reduction in system.time().
* mr.closed(): modified by correcting error in the multiple census methods if M,
    n, and m (but not R) were supplied.  Also corrected an error in the examples.
* rlp(): added probs= argument result to return list.  Corrected ylab in plotting
    methods.  Added a predict method.
* PSDval(),RSDval(): added a check for missing species name so that the user can
    just type PSDval() to get the list of possible species names.  Also added a
    check to see if RSDlit was already loaded.
* validateWs(): added probs= argument result to return list.  Corrected ylab in
    plotting methods.  Added a predict method.  Modified EmpQ() internal function
    to use predict() methods for emp and rlp objects.  Streamlined some of the
    code by including a compute.Ws() internal function and using the update()
    function.  Changed code for finding summarized dataframes inside the function
    by using cast() from the reshape package -- this resulted in a 1.5x reduction
    in system.time().
* wetperim(),summary.WetPerim(),plot.WetPerim(): added.
* wlgm(): major changes included moving some internal functions outside of wlgm(),
    adding the ability to use the data= argument, and adding the ability to fit
    weighted regressions on the summary statistics.  Other  minor changes were
    also made.  Updated the .Rd file.
* WSval(),WRval(): added a check for missing species name so that the user can
    just type WSval() to get the list of possible species names.  Also added a
    check to see if WSlit was already loaded.

# FSA 0.0-12  15Jul08

* .First.lib: Added
* add.zerocatch(): added this function to add zeroes to catch records where a
    species of fish was not caught.
* limnoprofile.plot(): added this function to simplify constructing plots of
    depth versus limnological measure with the depth decreasing from top to bottom.
* rlp(): changed default qtype= to 8 (from 7).  Added a probs= argument to allow
    other than 75th percentile calculations.
* emp(): updated the help page.  Added a logical for if p.n.low does not exist
    when using cutoff.tail.  Renamed items in the output list.  Added a table of
    number of individuals per length category to output list.  Added a probs=
    argument to allow other than 75th percentile calculations.  Added its own
    generics -- rather than relying on the rlp() generics.
* FroeseWs(): added this function, and its generics, to perform the standard
    weight equation calculation as proposed by Froese (2006).
* validateWs(): added this function, and its generics, to perform the Willis and
    EmpQ methods for assessing length bias in the standard weight equations.
    Added a probs= argument to allow other than 75th percentile calculations.
    Added a mean= argument to allow use of means rather than quantiles.  Modified
    to accept an object of class FROESE.

# FSA 0.0-11  15May08

* Moved to RForge.net.
* changed to R2.7.0.
* added a dependency to Rcapture (for the example in caphist.convert).

* anova.RLP(): added this function to produce the anova table for the standard weight equation.
* caphist.convert(): added this function convert between various capture history
    formats (FSA,event,MARK,Rcapture).
* emp(): added this function, and its generics, to perform Gerow's EmP method for
    obtaining a standard weight equation.
* fit.plot.RLP(): added this function.
* plot.RLP(): modified so that color palette with a gradient rather than only a
    solid color can be used for the populations.  In addition, added order.pop=
    argument that will order the populations from smallest to largest predicted
    with in the first length interval.  When used with the color gradients this
    will  make it easier to see which populations cross over other populations.            
* rlp(): modified function so that the user can choose to use any-mm length
    intervals rather than having 10-mm hardwired.  Modified output in list
    somewhat to more closely match the output of emp().

# FSA 0.0-10  1May08

* lencat(): Modified by adding an as.fact= argument that allows the user to decide
    if the resulting variable should be returned as a factor variable or not.  The
    default is set to return as a factor variable.  This allows tables of the new
    variable to include zeroes for levels of the new variable that contain n* individuals.
    This makes some RSD/PSD (and likely age-length key) calculations simpler.  Also added
    a drop.levels= argument to allow the user to drop unused levels if so desired.
* mr.closed():  This function is a combination of the old mr.closed1() and mr.closed2().
    It also allows the user to compute single census estimates with multiple sub-groups
    in the data (i.e., length- or age-classes).  The function also allows the user
    to compute an overall population esitmate of multiple sub-groups are present
    and an overall SE if the incl.SE=TRUE is used.  It also corrects the SE computations
    implemented in version 0.0-9.  This change caused the construction of our internal
    functions -- mrc1, mrc2, ci.mrc1, and ci.mrc2.
* mr.closed1(): removed this function.  Use mr.closed() instead.
* mr.closed2(): removed this function.  Use mr.closed() instead.
* PSDval(): Added mm= argument so that metric result can be returned in mm.  Also
    added incl.zero= argument that will include a zer* value in the first position
    in the vector; this is useful for when creating PSD/RSD values.
* rcumsum(): Added this function (from NCStats).
* RSDval(): See PSDval description.

# FSA 0.0-9 unknown

* age.comp(): Corrected SE calculation used to construct the CIs.  Changed the CI
    plotting routine to use plotCI in plotrix package -- this puts lines rather
    than points on the ends of the CIs.  Added a check for computing SDs and CIs
    for when n=1 or when all measurements are the same.  This reduces (eliminates?)
    the number of warnings that are given.
* catch.curve(): added na.rm=TRUE arguments to min() and max() in plot.CC().
    Changed type= argument so that "params" is the default rather than "lm".
    This makes it more consistent with other simulation programs.
* cc.sim(): Put in catch for situations where the CV for No and Z were equal to
    zero.  Originally, the program attempted to computed a random number from a
    normal distribution with a standard deviation of zero.  This corrected the
    problem of n* lines appearing unless the CVs were greater than zero.
* ch.convert(): STARTED A FUNCTION to CONVERT B/W CAPTURE HISTORY FORMATS.
    N* RD FILE YET.
* depletion():  Moved type= argument to third position.  Will more easily allow
    type="Leslie" as a default (i.e., can just enter catch and effort vector.
* FSAsims(): Added a "mark-recap" menu section.  Added a chapman 1-sample M-R
    item to the menu.
* leslie.sim(): corrected the conditionals on p.surv and r.prop so that it asks
    if any not the first value is less than 1.  This corrects the problem of R
    returning a large number of warnings.
* leslie.sim2(): corrected the call to depletion() so that the type of model
    ("Leslie") was the third rather than the first argument.  This was caused by
    a change in the usage of depletion in previous version changes.
* mr.closed1(): Modified output list to include an estimate of the variance as
    described in Ricker(1975).
* summary.MRC1(): Modified output so that (1) the given information is a little
    easier to read, (2) the population estimate is returned in a matrix, (3) the
    SE from Ricker(1975) can be included in the outputm, and (4) a label can be
    placed on row for the matrix output.  The purpose of these changes was to allow
    the SE to be computed and to allow future functions to more flexibly use the output.

# FSA 0.0-8 unknown

* changed some \items to \tabular in RD files.  Changed most hard-wired quotes to
    \sQuote or \dQuote in RD files.  Changed some text-based equations to more
    latex-based equations in \eqn or \deqn markups.  This fixed the Latex compilation
    problems that I was having when using RCMD check.

* age.comp(): Removed single-letter values from the what= argument.  Will rely
    on partial matching.
* age.key(): Changed default name for new column with ages from "Age" to "age".
    Added example.  
* coefplot.WLGM(): Changed to use plotCI() from the plotrix package.  This removed
    the for loop that I had programmed.  This also added the sfrac= and gap= arguments.
    Updated the RD.
* depletion(): Removed single-letter and lower-case values from the type= argument.
    Will rely on partial matching.
* lencat(): Changed d argument to df.
* lenfreq.expand(): Changed d argument to df.
* mr.open(): Added match.arg functionality to the ci.type and phi.type arguments.
* plot.RLP(): Added "object <- x" to allow x to be used inside the curve() function
    without confusion.
* removal(): Removed abbreviated values from the type= argument.  Will rely on
    partial matching.
* rlp(): Added examples from Murphy et al. (1990)
* stock.recruit(): Removed abbreviated values from the type= argument -- will rely
    on partial matching.  Changed sumtype argument to what.
* stock.recruit.sim():  Changed model argument to type and added param argument
    (to make compatible with stock.recruit).  Moved R and S arguments to beginning
    of argument string (more compatible with stock.recruit).  STILL NEED to ADD
    SECOND RICKER MODEL.
* vb.comp(): Changed d argument to df.
* wlgm.RD(): Added example code.  Added some details.

# FSA 0.0-7 unknown

* changed to compiling under R 2.6.1.
* added FSA.R file that loads the required librarys.
* now depends on MASS package because of the creation of the boxcox.WLGM() function
    and on the plotrix package for elements of ycplot().

* add.radcap(): Created this function to add the radius-at-capture to a one-fish-per-line
    data frame of increments.
* age.comp(): Added a match.arg() call for the what argument.
* age.key(): Corrected =T or =F to =TRUE or =FALSE.
* cc.sim(): Corrected the use of z.param with correct use of Z.param.
* coefplot(): A new generic function for plotting coefficients from a Weisberg
    Linear Growth Model analysis.
* g.convert(): Changed arguments from data and measure.var to df and in.var.
    Included match.arg() for the type argument.  Moved type argument as it now
    has a default.  Added the in.pre argument that allows  the user to identify
    all input variables by a common prefix rather than having to list them out in 
    in.var.  Added the out.pre argument that allows the user to control the prefix
    for the newly created  variables in the output data frame.  Updated help file.
* g.reshape(): Changed arguments from data and prefix to df and in.pre.  Deleted
    the measure.var argument.  Moved new in.pre argument forward and na.rm argument
    backward in the argument list.  Used in.pre to identify the measure.var variables
    to send to the melt function.  If id.var is left blank (and in.pre is not) then
    id.var is the remaining variables not identified by in.pre.  The val.name
    argument was set equal to in.pre as the default.
* leslie.sim(): Corrected call to old leslie() function with a correct call to
    depletion() function with type="Leslie".
* leslie.sim2(): Corrected two calls to old leslie() function with correct calls
    to depletion() function with type="Leslie".
* leslie.rand.run(): Moved R Documentation alias to FSA-internal.RD.
* mr.closed2(): Forced the function to use the lowess() function in the stats
    rather than the gplots package (which is loaded from NCStats).  Also changed
    the loess.f argument to f and added the iter argument for sending to lowess().
* confint.MRO(): Put in a logical catch when printing the confidence intervals
    because if ci.type="Manly" then CIs for B are not computed.
* summary.MRO(): Put in a logical catch when printing the estimates because if
    ci.type="Manly" then SEs are not computed.
* plot.RLP(): Corrected =T or =F to =TRUE or =FALSE.
* wlgm(): Created a method with a number of generics (alias, anova, boxcox, coef,
    coefplot, confint, ycplot) for performing the Weisberg Linear Growth Model.
* ycplot(): A new generic function for creating a year-class plot for the Weisberg
    Linear Growth Model analysis.

# FSA 0.0-6 unknown

* agebias.plot(): deleted and replaced with agecomp and plot.AgeComp functions.
* agesunflower.plot(): deleted and replaced with agecomp and plot.AgeComp functions.
* agecomp(): a new function that, along with its extractor functions, combines all
    of the functionality of the old age.tests, age.symmetry, agebias.plot, and
    agesunflower.plot.  Allows for a more seamless comparison of ageing reads.
* plot.AgeComp(): an extractor function for objects saved from the agecomp function.
    This replaces the old agebias.plot and agesunflower.plot functions.
* summary.Agecomp(): an extractor function for objects saved from the agecomp function.
    This replaces the old age.tests and age.symmetry functions.
* age.symmetry(): deleted and replaced with agecomp and summary.AgeComp functions.
* age.tests(): deleted and replaced with agecomp and summary.AgeComp functions.
* cc.sim(): modified graphic to (1) include assumption violation labels on top
    of the graph and (2) label y-axis with "log(Catch)" rather than "ln(Catch)".
* delury(): deleted and replaced with depletion function.  See notes for depletion
    function.
* depletion(): created a new function that performs the Leslie or Delury method as
    determined by a user-defined argument to type.  This function replaces the old
    leslie and delury functions.  All extractor functions for the class "LeslieDelury"
    have now been changed to class "Depletion".
* plot.Depletion(): correct xlab so that it defaults to "Cumulative Effort" for
    the Delury method.
* leslie(): deleted and replaced with depletion function.  See notes for depletion function.
* leslie.sim(): modified graphic to include assumption violation labels on top of the graph.
* mr.open(): modified est.N() internal function so that N is set equal to n if N<n.
    In other words, if the sample size at time i is larger than the estimated
    population size at time i then the estimated population size is set equal to
    the observed sample size.  This corrects the problem of the N.se calculation
    attempting to take the square root of a negative number causing the mr.open()
    function to shut down.
* pass.removal(): changed name to removal.
* removal(): new name for old pass.removal function.
* rlp(): new name for old (upper-case) RLP.
* RLP(): changed name to (lower-case) rlp.