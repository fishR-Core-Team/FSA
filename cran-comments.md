This is a resubmission from 7-Oct-15.  Comments or changes (can likely be ignored) related to the previous submission are below.  This passed on winbuilder for both the current and development versions of R (I actually checked the logs this time ... I did not realize that an "OK" in the e-mail was not sufficient).

# Comments
* The "fishR" in the description file is the name of a website for which a link is provided in the URL field.  This is described as "...on the fishR website listed below."  Is this adequate description?

# Changes
* Corrected incorrect spelling of "peform" in the description file.
* Changed `rgb()` inside functions (not examples or tests) to `grDevices::rgb()`.
* Changed `graphics.off()` inside functions (not examples or tests) to `grDevices::graphics.off()`.
* Changed `grconvertY()` inside functions (not examples or tests) to `graphics::grconvertY()`.
* Changed several `hist()` inside functions (not examples or tests) to `hist.formula()` which is a function of this package.  Made sure that non-formula calls to `hist()` used `graphcis::hist()`.
* Changed `anova()` inside functions (not examples or tests) to `stats::anova()`.
* Changed `df.residual()` inside functions (not examples or tests) to `stats::df.residual()`.
* Changed `formula()` inside functions (not examples or tests) to `stats::formula()`.
* Changed `na.exclude()` inside functions (not examples or tests) to `stats::na.exclude()`.
* Changed `optimize()` inside functions (not examples or tests) to `stats::optimize()`.
* Changed `predict()` inside functions (not examples or tests) to `stats::predict()`.
* Changed `printCoefMat()` inside functions (not examples or tests) to `stats::printCoefMat()`.
* Changed `rstandard()` inside functions (not examples or tests) to `stats::rstandard()`.
* Changed `rstudent()` inside functions (not examples or tests) to `stats::df.rstudent()`.
* Changed `runif()` inside functions (not examples or tests) to `stats::runif()`.
* Changed `sd()` inside functions (not examples or tests) to `stats::sd()`.
* Changed `terms()` inside functions (not examples or tests) to `stats::terms()`.
* Changed `var()` inside functions (not examples or tests) to `stats::var()`.
* Changed `browseURL()` inside functions (not examples or tests) to `utils::browseURL()`.
* Changed `combn()` inside functions (not examples or tests) to `utils::combn()`.
* Changed `data()` inside functions (not examples or tests) to `utils::data()`.
* Changed `head()` inside functions (not examples or tests) to `utils::head()`.
* Changed `installed.packages()` inside functions (not examples or tests) to `utils::installed.packages()`.
* Changed `sessionInfo()` inside functions (not examples or tests) to `utils::sessionInfo()`.
* Changed `tail()` inside functions (not examples or tests) to `utils::tail()`.
* Changed `unstack()` inside functions (not examples or tests) to `utils::unstack()`.
* Removed some examples from `removal()` in order to reduce the elapsed time (<5 s) for CRAN.
