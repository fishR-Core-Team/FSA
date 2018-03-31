#' @title Compute and view possible differences between paired sets of ages.
#'
#' @description Constructs age-agreement tables, statistical tests to detect differences, and plots to visualize potential differences in paired age estimates. Ages may be from, for example, two readers of the same structure, one reader at two times, two structures (e.g., scales, spines, otoliths), or one structure and known ages.
#'
#' @param formula A formula of the form \code{nrefvar~refvar}, where \code{nrefvar} and \code{refvar} generically represent variables that contain the paired \dQuote{nonreference} and \dQuote{reference} age estimates, respectively. See details.
#' @param data A data.frame that minimally contains the paired age estimates given in \code{formula}.
#' @param ref.lab A string label for the reference age estimates.
#' @param nref.lab A string label for the nonreference age estimates.
#' @param method A string for which method to use to adjust p-values for multiple comparisons. See \code{?p.adjust.methods}.
#' @param sig.level A numeric value to determine whether a p-value indicates a significant result. The confidence level used in \code{plot} is 100*(1-\code{sig.level}).
#' @param min.n.CI A numeric value (default is 3) that is the smallest sample size for which a confidence interval will be computed.
#' @param what A string that indicates what type of summary to print or plot to construct. See details.
#' @param flip.table A logical that indicates whether the age-agreement table should be \sQuote{flipped} (i.e., rows are reversed so that younger ages are at the bottom of the table). This makes the table more directly comparable to the age bias plot.
#' @param zero.print A string for what should be printed in place of the zeros on an age-agreement table. The default is to print a single dash.
#' @param digits A numeric for the minimum number of digits to print when showing \code{what="bias"} or \code{what="diff.bias"} in \code{summary}.
#' @param cont.corr A string that indicates the continuity correction method to be used with (only) McNemar test. If \code{"none"} (default) then no continuity correction is used, if \code{"Yates"} then 0.5 is used, and if \code{"Edwards"} then 1 is used.
#' @param xvals A string for whether the x-axis values are reference ages or mean of the reference and nonreference ages.
#' @param x,object An object of class \code{ageBias}, usually a result from \code{ageBias}.
#' @param xlab,ylab A string label for the x-axis (reference) or y-axis (non-reference) age estimates, respectively.
#' @param xlim,ylim A numeric vector of length 2 that contains the limits of the x-axis (reference ages) or y-axis (non-reference ages), respectively.
#' @param xaxt,yaxt A string which specifies the x- and y-axis types. Specifying \dQuote{n} suppresses plotting of the axis. See \code{?par}.
#' @param col.agree A string or numeric for the color of the 1:1 or zero (if \code{difference=TRUE}) reference line.
#' @param lwd.agree A numeric for the line width of the 1:1 or zero (if \code{difference=TRUE}) reference line.
#' @param lty.agree A numeric for the line type of the 1:1 or zero (if \code{difference=TRUE}) reference line.
#' @param lwd A numeric that controls the separate \sQuote{lwd} argument (e.g., \code{lwd.CI} and \code{lwd.range}).
#' @param sfrac A numeric that controls the separate \sQuote{sfrac} arguments (e.g., \code{sfrac.CI} and \code{sfrac.range}). See \code{sfrac} in \code{\link[plotrix]{plotCI}} of \pkg{plotrix}.
#' @param show.pts A logical for whether or not the raw data points are plotted.
#' @param pch.pts A numeric for the plotting character of the raw data points.
#' @param col.pts A string or numeric for the color of the raw data points. The default is to use black with the transparency found in \code{transparency}.
#' @param cex.pts A character expansion value for the size of the symbols for \code{pch.pts}.
#' @param transparency A numeric (between 0 and 1) for the level of transparency of the raw data points. This number of points plotted on top of each other will represent the color in \code{col.pts}.
#' @param pch.mean A numeric for the plotting character used for the mean values when the means are considered insignificant.
#' @param pch.mean.sig A numeric for the plotting character for the mean values when the means are considered significant.
#' @param cex.mean A character expansion value for the size of the mean symbol in \code{pch.mean} and \code{pch.mean.sig}.
#' @param show.range A logical for whether or not vertical bars that represent the range of the data points are plotted.
#' @param col.range A string or numeric for the color of the range intervals.
#' @param lwd.range A numeric for the line width of the range intervals.
#' @param sfrac.range A numeric for the size of the ends of the range intervals. See \code{sfrac} in \code{\link[plotrix]{plotCI}} of \pkg{plotrix}.
#' @param show.CI A logical for whether confidence intervals should be plotted or not.
#' @param col.CI A string or numeric for the color of confidence interval bars that are considered non-significant.
#' @param col.CIsig A string or numeric for the color of confidence interval bars that are considered significant.
#' @param lwd.CI A numeric for the line width of the confidence interval bars.
#' @param sfrac.CI A numeric for the size of the ends of the confidence interval bars. See \code{sfrac} in \code{\link[plotrix]{plotCI}} of \pkg{plotrix}.
#' @param yHist A logical for whether a histogram of the y-axis variable should be added to the right margin of the age bias plot. See details.
#' @param xHist A logical for whether a histogram of the x-axis variable should be added to the top margin of the age bias plot. See details.
#' @param hist.panel.size A numeric between 0 and 1 that indicates the proportional size of histograms (relative to the entire plotting pane) in the plot margins (only used if \code{xHist=TRUE} or \code{yHist=TRUE}).
#' @param col.hist A string for the color of the bars in the marginal histograms (only used if \code{xHist=TRUE} or \code{yHist=TRUE}).
#' @param show.n A logical for whether the sample sizes for each level of the x-axis variable is shown (\code{=TRUE}, default) or not (\code{=FALSE}).
#' @param nYpos A numeric for the relative Y position of the sample size values when \code{show.n=TRUE}. For example, if \code{nYpos=1.03} then the sample size values will be centered at 3 percent above the top end of the y-axis.
#' @param cex.n A character expansion value for the size of the sample size values.
#' @param cex.numbers A character expansion value for the size of the numbers plotted when \code{what="numbers"} is used.
#' @param col.numbers A string for the color of the numbers plotted when \code{what="numbers"} is used.
#' @param allowAdd A logical that will allow the user to add items to the main (i.e., not the marginal histograms) plot panel (if \code{TRUE}). Defaults to \code{FALSE}.
#' @param \dots Additional arguments for methods.
#'
#' @details Generally, one of the two age estimates will be identified as the \dQuote{reference} set. In some cases this may be the true ages, the ages from the more experienced reader, the ages from the first reading, or the ages from the structure generally thought to provide the most accurate results. In other cases, such as when comparing two novice readers, the choice may be arbitrary. The reference ages will form the columns of the age-agreement table and will be the \dQuote{constant} age used in the t-tests and age bias plots (i.e., the x-axis). See further details below.
#' 
#' The age-agreement table is constructed with  \code{what="table"} in \code{summary}. The agreement table can be \dQuote{flipped} (i.e., the rows in descending rather than ascending order) with \code{flip.table=TRUE}. By default, the tables are shown with zeros replaced by dashes. This behavior can be changed with \code{zero.print}.
#'
#' Three statistical tests of symmetry for the age-agreement table can be computed with \code{what=} in \code{summary}. The \dQuote{unpooled} or Bowker test as described in Hoenig et al. (1995) is constructed with \code{what="Bowker"}, the \dQuote{semi-pooled} or Evans-Hoenig test as described in Evans and Hoenig (1998) is constructed with \code{what="EvansHoenig"}, and the \dQuote{pooled} or McNemar test as described in Evans and Hoenig (1998) is constructed with \code{what="McNemar"}. All three tests are computed with \code{what="symmetry"}.
#' 
#' Four types of plots for visualizing differences between sets of two age estimates may be created. Two of these plots may be constructed with \code{plotAB()}. In the \code{plotAB()} plots, the reference ages are plotted on the x-axis and the nonreference ages are on the y-axis. The 1:1 (45 degree) agreement line is shown for comparative purposes. The default \code{plotAB()} plot (using \code{what="bias"}) was inspired by the age bias plot introduced by Campana et al. (1995). The default settings for this age bias plot show the mean and confidence interval for the nonreference ages at each of the reference ages. The level of confidence is controlled by \code{sig.level=} given in the original \code{ageBias} call (i.e., confidence level is 100*(1-\code{sig.level})). Confidence intervals are only shown if the sample size is greater than the value in \code{min.n.CI=} (also from the original call to \code{ageBias}). Confidence intervals plotted in red with an open dot (by default; these can be changed with \code{col.CIsig} and \code{pch.mean.sig}, respectively) do not contain the reference age (see discussion of t-tests below). Sample sizes at each reference age are shown if \code{show.n=TRUE}. The position of the sample sizes is controlled with \code{nYpos=}, whereas their size is controlled with \code{cex.n}. Arguments may be used to nearly replicate the age bias plot as introduced by Campana et al. (1995) as shown in the examples.
#' 
#' The frequency of observations at each unique (x,y) coordinate are shown by using \code{what="numbers"} in \code{plotAB}. The size of the numbers is controlled with \code{cex.numbers}.
#' 
#' Muir et al. (2008) modified the age bias plot by plotting the difference between the two ages on the y-axis (still against a reference age on the x-axis). McBride (2015) introduced the Bland-Altman plot for comparing fish ages where the difference in ages is plotted on the y-axis versus the mean of the ages on the x-axis. Modifications of these plots are created with \code{plot} (rather than \code{plotAB}) using \code{xvals=} to control what is plotted on the x-axis (i.e., \code{xvals="reference"} uses the reference ages, whereas \code{xvals="mean"} uses the mean of the two ages for the x-axis). In both plots, a horizontal agreement line at a difference of zero is shown for comparative purposes. In addition, a histogram of the differences is shown in the right margin (can be excluded with \code{yHist=FALSE}.) A histogram of the reference ages is shown by default in the top margin for the modified age bias plot, but not for the modified Bland-Altman-like plot (the presence of this histogram is controlled with \code{xHist=}).
#' 
#' By default, the modified age bias plot shows the mean and range for the nonreference ages at each of the reference ages. Means shown with an open dot are mean age differences that are significantly different from zero. The ranges of differences in ages at each reference age can be removed with \code{show.range=FALSE}. A confidence interval for difference in ages can be added with \code{show.CI=FALSE}. The color and symbol coding for the means and CIs is the same as described above for \code{plotAB}. Individual points are plotted if \code{show.pts=TRUE}, where there color is controlled by \code{col.pts=} and \code{transparency=}. See examples for use of these arguments.
#' 
#' Both the main (i.e., not the marginal histograms) can be "added to" after the plot is constructed if \code{allowAdd=TRUE} is used. For example, the Bland-Altman-like plot can be augmented with a horizontal line at the mean difference in ages, a line for the regression between the differences and the means, or a generalized additive model that describes the relationship between the differences and the means. See the examples for use of \code{allowAdd=TRUE}. It is recommended to save the plotting parameters (e.g., \code{op <- par(no.readonly=TRUE)}) before using \code{plot} with \code{allowAdd=TRUE} so that the original parameters can be reset (e.g., \code{par(op)}); see the examples.
#'
#' Individual t-tests to determine if the mean age of the nonreference set at a particular age of the reference set is equal to the reference age (e.g., is the mean age of the nonreference set at age-3 of the reference set statistically different from 3?) are shown with \code{what="bias"} in \code{summary}. The results provide a column that indicates whether the difference is significant or not as determined by adjusted p-values from the t-tests and using the significance level provided in \code{sig.level} (defaults to 0.05). Similar results for the difference in ages (e.g., is the mean reference age minus nonreference age at a nonreference age of 3 different from 0?) are constructed with \code{what="diff.bias"} in \code{summary}.
#'
#' The sample size present in the age-agreement table is found with \code{what="n"}.
#'
#' @return \code{ageBias} returns a list with the following items:
#' \itemize{
#'   \item data A data.frame with the original paired age estimates and the difference between those estimates.
#'   \item agree The age-agreement table.
#'   \item bias A data.frame that contains the bias statistics.
#'   \item bias.diff A data.frame that contains the bias statistics for the differences.
#'   \item ref.lab A string that contains an optional label for the age estimates in the columns (reference) of the age-agreement table.
#'   \item nref.lab A string that contains an optional label for the age estimates in the rows (non-reference) of the age-agreement table.
#'}
#'
#' \code{summary} returns the result if \code{what=} contains one item, otherwise it returns nothing. Nothing is returned by \code{plot} or \code{plotAB}, but see details for a description of the plot that is produced.
#' 
#' @section Testing: Tested all symmetry test results against results in Evans and Hoenig (2008), the McNemar and Evans-Hoenig results against results from \code{\link[fishmethods]{compare2}} in \pkg{fishmethods}, and all results using the \code{\link[FSAdata]{AlewifeLH}} data set from \pkg{FSAdata} against results from the online resource at http://www.nefsc.noaa.gov/fbp/age-prec/.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso See \code{\link{agePrecision}} for measures of precision between pairs of age estimates. See \code{\link[fishmethods]{compare2}} in \pkg{fishmethods} for similar functionality.
#'
#' @section IFAR Chapter: 4-Age Comparisons. \bold{Note that \code{plot} has changed since IFAR was published. Some of the original functionality is in \code{plotAB}. See examples.}
#'
#' @references Ogle, D.H. 2016. \href{http://derekogle.com/IFAR}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Campana, S.E., M.C. Annand, and J.I. McMillan. 1995. Graphical and statistical methods for determining the consistency of age determinations. Transactions of the American Fisheries Society 124:131-138. [Was (is?) available from http://www.bio.gc.ca/otoliths/documents/Campana\%20et\%20al\%201995\%20TAFS.pdf.]
#'
#' Evans, G.T. and J.M. Hoenig. 1998. Testing and viewing symmetry in contingency tables, with application to readers of fish ages. Biometrics 54:620-629.
#'
#' Hoenig, J.M., M.J. Morgan, and C.A. Brown. 1995. Analysing differences between two age determination methods by tests of symmetry. Canadian Journal of Fisheries and Aquatic Sciences 52:364-368.
#' 
#' McBride, R.S. 2015. Diagnosis of paired age agreement: A simulation approach of accuracy and precision effects. ICES Journal of Marine Science 72:2149-2167.
#'
#' Muir, A.M., M.P. Ebener, J.X. He, and J.E. Johnson. 2008. A comparison of the scale and otolith methods of age estimation for lake whitefish in Lake Huron. North American Journal of Fisheries Management 28:625-635. [Was (is?) available from http://www.tandfonline.com/doi/abs/10.1577/M06-160.1]
#'
#' @aliases ageBias plot.ageBias summary.ageBias
#'
#' @keywords htest manip
#'
#' @examples
#' data(WhitefishLC)
#' ab1 <- ageBias(scaleC~otolithC,data=WhitefishLC,ref.lab="Otolith Age",nref.lab="Scale Age")
#' summary(ab1)
#' summary(ab1,what="symmetry")
#' summary(ab1,what="Bowker")
#' summary(ab1,what="EvansHoenig")
#' summary(ab1,what="McNemar")
#' summary(ab1,what="McNemar",cont.corr="Yates")
#' summary(ab1,what="bias")
#' summary(ab1,what="diff.bias")
#' summary(ab1,what="n")
#' summary(ab1,what=c("n","symmetry","table"))
#' # show the zeros (rather than dashes)
#' summary(ab1,what="table",zero.print="0")
#' # flip the table -- ease of comparison to age bias plot
#' summary(ab1,what="table",flip.table=TRUE)
#'
#' #############################################################
#' ## Traditional Age-Bias Plot (from Campana et al. (1995))
#' # Default
#' plotAB(ab1)
#' # very close to Campana et al. (2001)
#' plotAB(ab1,pch.mean.sig=19,col.CIsig="black",sfrac=0.01)
#' # Squared up the axes
#' plotAB(ab1,pch.mean.sig=19,col.CIsig="black",ylim=c(-1,23),xlim=c(-1,23))
#' ## Show sample sizes
#' plotAB(ab1,show.n=TRUE)
#' ## Move sample sizes (and change text size)
#' plotAB(ab1,show.n=TRUE,nYpos=0.02,cex.n=0.5)
#'
#' ## Traditional numbers plot
#' plotAB(ab1,what="numbers") 
#' 
#' #############################################################
#' ## Differences Plot (originally inspired by Muir et al. (2008))
#' # Default (ranges, open circles for sig diffs, marginal hists)
#' plot(ab1)
#' # Show CIs for means (with and without ranges)
#' plot(ab1,show.CI=TRUE)
#' plot(ab1,show.CI=TRUE,show.range=FALSE)
#' # show points (with and without CIs)
#' plot(ab1,show.CI=TRUE,show.range=FALSE,show.pts=TRUE)
#' plot(ab1,show.range=FALSE,show.pts=TRUE)
#' # Use same symbols for all means (with ranges)
#' plot(ab1,pch.mean.sig=19)
#' # Use same symbols/colors for all means/CIs (without ranges)
#' plot(ab1,show.range=FALSE,show.CI=TRUE,pch.mean.sig=19,col.CIsig="black")
#' # Remove histograms
#' plot(ab1,xHist=FALSE)
#' plot(ab1,yHist=FALSE)
#' plot(ab1,xHist=FALSE,yHist=FALSE)
#' ## Suppress confidence intervals for n < a certain value
#' ##   must set this in the original ageBias() call
#' ab2 <- ageBias(scaleC~otolithC,data=WhitefishLC,min.n.CI=8,
#'                ref.lab="Otolith Age",nref.lab="Scale Age")
#' plot(ab2,show.CI=TRUE,show.range=FALSE)
#'
#'  
#' #############################################################
#' ## Differences Plot (originally inspired by Bland-Altman plots
#' ##   in McBride (2015))
#' plot(ab1,xvals="mean")
#' ## Modify axis limits
#' plot(ab1,xvals="mean",xlim=c(1,17))
#' ## Add and remove histograms
#' plot(ab1,xvals="mean",xHist=TRUE)
#' plot(ab1,xvals="mean",xHist=TRUE,yHist=FALSE)
#' plot(ab1,xvals="mean",yHist=FALSE)
#' 
#' #############################################################
#' ## Adding post hoc analyses to the main plot
#' # get original graphing parameters to be reset at the end
#' op <- par(no.readonly=TRUE)
#' 
#' # get raw data
#' tmp <- ab1$d
#' head(tmp)
#' 
#' # Add mean difference (w/ approx. 95% CI)
#' bias <- mean(tmp$diff)+c(-1.96,0,1.96)*se(tmp$diff)
#' plot(ab1,xvals="mean",xlim=c(1,17),allowAdd=TRUE)
#' abline(h=bias,lty=2,col="red")
#' par(op)
#' 
#' # Same as above, but without margoinal histogram, and with
#' #   95% agreement lines as well (1.96SDs)
#' #   (this is nearly a replicate of a Bland-Altman plot)
#' bias <- mean(tmp$diff)+c(-1.96,0,1.96)*se(tmp$diff)
#' agline <- mean(tmp$diff)+c(-1.96,1.96)*sd(tmp$diff)
#' plot(ab1,xvals="mean",yHist=FALSE,allowAdd=TRUE)
#' abline(h=bias,lty=2,col="red")
#' abline(h=agline,lty=3,lwd=2,col="blue")
#' par(op)
#' 
#' # Add linear regression line of differences on means (w/ approx. 95% CI)
#' lm1 <- lm(diff~mean,data=tmp)
#' xval <- seq(0,19,0.1)
#' pred1 <- predict(lm1,data.frame(mean=xval),interval="confidence")
#' bias1 <- data.frame(xval,pred1)
#' head(bias1)
#' plot(ab1,xvals="mean",xlim=c(1,17),allowAdd=TRUE)
#' lines(lwr~xval,data=bias1,lty=2,col="red")
#' lines(upr~xval,data=bias1,lty=2,col="red")
#' lines(fit~xval,data=bias1,lty=2,col="red")
#' par(op)
#' 
#' # Add loess of differences on means (w/ approx. 95% CI as a polygon)
#' lo2 <- loess(diff~mean,data=tmp)
#' xval <- seq(min(tmp$mean),max(tmp$mean),0.1)
#' pred2 <- predict(lo2,data.frame(mean=xval),se=TRUE)
#' bias2 <- data.frame(xval,pred2)
#' bias2$lwr <- bias2$fit-1.96*bias2$se.fit
#' bias2$upr <- bias2$fit+1.96*bias2$se.fit
#' head(bias2)
#' plot(ab1,xvals="mean",xlim=c(1,17),allowAdd=TRUE)
#' with(bias2,polygon(c(xval,rev(xval)),c(lwr,rev(upr)),
#'                    col=col2rgbt("red",1/10),border=NA))
#' lines(fit~xval,data=bias2,lty=2,col="red")
#' par(op)
#'                   
#' # Same as above, but polygon and line behind the points
#' plot(ab1,xvals="mean",xlim=c(1,17),col.pts="white",allowAdd=TRUE)
#' with(bias2,polygon(c(xval,rev(xval)),c(lwr,rev(upr)),
#'                    col=col2rgbt("red",1/10),border=NA))
#' lines(fit~xval,data=bias2,lty=2,col="red")
#' points(diff~mean,data=tmp,pch=19,col=col2rgbt("black",1/8))
#' par(op)
#' 
#' # Can also be made with the reference ages on the x-axis
#' lo3 <- loess(diff~otolithC,data=tmp)
#' xval <- seq(min(tmp$otolithC),max(tmp$otolithC),0.1)
#' pred3 <- predict(lo3,data.frame(otolithC=xval),se=TRUE)
#' bias3 <- data.frame(xval,pred3)
#' bias3$lwr <- bias3$fit-1.96*bias3$se.fit
#' bias3$upr <- bias3$fit+1.96*bias3$se.fit
#' plot(ab1,show.range=FALSE,show.pts=TRUE,col.pts="white",allowAdd=TRUE)
#' with(bias3,polygon(c(xval,rev(xval)),c(lwr,rev(upr)),
#'                    col=col2rgbt("red",1/10),border=NA))
#' lines(fit~xval,data=bias3,lty=2,col="red")
#' points(diff~otolithC,data=tmp,pch=19,col=col2rgbt("black",1/8))
#' par(op)
#' 
#' @rdname ageBias
#' @export
ageBias <- function(formula,data,ref.lab=tmp$Enames,nref.lab=tmp$Rname,
                    method=stats::p.adjust.methods,sig.level=0.05,min.n.CI=3) {
  ## Perform some checks on the formula
  tmp <- iHndlFormula(formula,data,expNumR=1,expNumE=1)
  if (!tmp$metExpNumR) STOP("'ageBias' must have only one LHS variable.")
  if (!tmp$Rclass %in% c("numeric","integer")) STOP("LHS variable must be numeric.")
  if (!tmp$metExpNumE) STOP("'ageBias' must have only one RHS variable.")
  if (!tmp$Eclass %in% c("numeric","integer")) STOP("RHS variable must be numeric.")
  ## get variable names separately
  nref.name <- tmp$Rname
  ref.name <- tmp$Enames
  ## rename dataframe of just ages (for simplicity)
  d <- tmp$mf
  ## sample size
  n <- nrow(d)
  ## add differences data for each fish
  d$diff <- d[,nref.name]-d[,ref.name]
  ## add mean data for each fish
  d$mean <- rowMeans(d[,c(nref.name,ref.name)])
  
  ## Summarizations of nonref data by ref data (more 'true' structure)
  # turn off warnings for not using factor data (turn back on below)
  options(warn=-1)
  # Summary stats of cdata by ages of rdata
  bias.df <- iAgeBiasDF(Summarize(stats::as.formula(paste(nref.name,"~",ref.name)),data=d),
                        ref.name,FALSE,min.n.CI,sig.level)
  # Summary stats of diff by cdata
  bias2.df <- iAgeBiasDF(Summarize(stats::as.formula(paste("diff~",ref.name)),data=d),
                         ref.name,TRUE,min.n.CI,sig.level)
  options(warn=0)
  
  ## Agreement contingency table adjusted to be square  
  # finds overall range of ages
  ages <- min(d[,c(nref.name,ref.name)],na.rm=TRUE):max(d[,c(nref.name,ref.name)],na.rm=TRUE)
  # converts nr and r data to factor with ages levels
  nref.fact <- factor(d[,nref.name],levels=ages)
  ref.fact <- factor(d[,ref.name],levels=ages)
  # Agreement contingency table
  agree.table <- table(nref.fact,ref.fact,dnn=c(nref.lab,ref.lab))
  
  ## Put together an output list
  d <- list(data=d,agree=agree.table,bias=bias.df,bias.diff=bias2.df,
            ref.name=ref.name,ref.lab=ref.lab,
            nref.name=nref.name,nref.lab=nref.lab)
  class(d) <- "ageBias"
  d
}

#' @rdname ageBias
#' @export
summary.ageBias <- function(object,
                   what=c("table","symmetry","Bowker","EvansHoenig","McNemar","bias","diff.bias","n"),
                   flip.table=FALSE,zero.print="-",digits=3,cont.corr=c("none","Yates","Edwards"),
                   ...) {
  what <- match.arg(what,several.ok=TRUE)
  retres <- ifelse(length(what)==1,TRUE,FALSE)
  showmsg <- ifelse (length(what)>1,TRUE,FALSE)
  if ("n" %in% what) {
    res <- sum(object$agree)
    cat("Sample size in the age agreement table is ",res,".",sep="")
    if (length(what)>1) cat("\n")
    what <- iHndlMultWhat(what,"n","cat")
  }
  if ("bias" %in% what) {
    if (showmsg) cat("Summary of",object$nref.lab,"by",object$ref.lab,"\n")
    res <- object$bias[-ncol(object$bias)]
    print(res,row.names=FALSE,digits=digits)
    what <- iHndlMultWhat(what,"bias","cat")
  }
  if ("diff.bias" %in% what) {
    if (showmsg) cat("Summary of ",object$nref.lab,"-",object$ref.lab," by ",object$ref.lab,"\n",sep="")
    res <- object$bias.diff[-ncol(object$bias.diff)]
    print(res,row.names=FALSE,digits=digits)
    what <- iHndlMultWhat(what,"diff.bias","cat")
  }
  if ("table" %in% what) {
    # show the age-agreement table
    if (!flip.table) {
      if (showmsg) cat("Raw agreement table (square)\n")
      res <- object$agree
      print(res,zero.print=zero.print)
    } else {
      if (showmsg) cat("Raw agreement table (square & flipped)\n")
      # flip the rows
      res <- object$agree[nrow(object$agree):1,]
      # for printing purposes
      class(res) <- "table"
      print(res,zero.print=zero.print)
    }
    what <- iHndlMultWhat(what,"table","cat")
  }
  if (any(c("symmetry","Bowker","EvansHoenig","McNemar") %in% what)) {
    # always return the results
    retres <- TRUE
    symTest <- NULL # to avoid "global bindings" warning in rcmd check
    res <- iMcNemar(object,match.arg(cont.corr))
    res <- rbind(res,iEvansHoenig(object))
    res <- rbind(res,iBowker(object))
    # if what="symmetry" print all results, otherwise only print what is asked for
    
    if (showmsg) cat("Age agreement table symmetry test results\n")
    if (!"symmetry" %in% what) res <- Subset(res,grepl(what,symTest))
    print(res)
  }
  if (retres) invisible(res)
}

#=============================================================
# This internal function is used to created a data frame of
#   summarized data for the ageBias() function. 
#
# tmp -- the results from Summarize of ages in a column
#        variable by a row variable
# cname -- the name of the column variable
# diff -- a logical of whether differences are being used or not
# min.n.CI -- the minimum n for which CIs should be computed
#
# returns a data.frame
#=============================================================
iAgeBiasDF <- function(tmp,cname,diff,min.n.CI,sig.level) {
  # Ages of cdata strux
  x <-tmp[,1]
  # Is it appropriate to compute a CI
  canCI <- tmp$n>=min.n.CI & tmp$sd>0
  # Fill SEs, p-values calcs, and CIs with NA (will leave NA
  # for those where CI was inappropriate)
  tmp$SE <- tmp$t <- tmp$p.value <- tmp$LCI <- tmp$UCI <- NA
  # SE of 2nd strux
  tmp$SE[canCI] <- tmp$sd[canCI]/sqrt(tmp$n[canCI])
  # t test statistic that mean of 2nd strux equals value of 1st strux
  if (!diff) tmp$t[canCI] <- (tmp$mean[canCI]-x[canCI])/tmp$SE[canCI]
  else tmp$t[canCI] <- tmp$mean[canCI]/tmp$SE[canCI]
  # two-tailed p-value (adjusted for multiple comparisons)
  tmp$p.value <- stats::pt(abs(tmp$t),df=tmp$n-1,lower.tail=FALSE)*2
  tmp$adj.p <- round(stats::p.adjust(tmp$p.value,method=stats::p.adjust.methods),5)
  # Assign significant difference (fill with FALSE then change to TRUE if sig)
  tmp$sig <- rep(FALSE,nrow(tmp))
  tmp$sig[tmp$adj.p<sig.level] <- TRUE
  # CIs
  tmp$LCI[canCI] <- tmp$mean[canCI]+stats::qt(sig.level/2,tmp$n[canCI]-1)*tmp$SE[canCI]
  tmp$UCI[canCI] <- tmp$mean[canCI]+stats::qt(sig.level/2,tmp$n[canCI]-1,lower.tail=FALSE)*tmp$SE[canCI]
  # put together as a dataframe
  tmpdf <- data.frame(age=x,n=tmp$n,min=tmp$min,max=tmp$max,mean=tmp$mean,SE=tmp$SE,
                      t=tmp$t,adj.p=tmp$adj.p,sig=tmp$sig,
                      LCI=tmp$LCI,UCI=tmp$UCI,canCI=canCI)
  # label first column with col.data name
  names(tmpdf)[1] <- cname  
  tmpdf
} ## end iAgeBiasDF internal function

#=============================================================
# This internal function is used to handle the age-agreement
# table for symmetry tests. Specifically, it removes the main
# diagonal, finds the upper and lower triangles, and returns
# them all in a list. It is called by the iBowker, iMcNemar,
# and iEvansHoenig functions.
#=============================================================
iHandleAgreeTable <- function(obj) {
  # rename agreement table
  at <- obj$agree
  # Remove the values on the diagonal
  diag(at) <- NA
  # Find the values on the lower- and upper- triangles
  lo <- up <- at
  lo[upper.tri(lo)] <- NA
  up[lower.tri(up)] <- NA
  # return all of the parts
  list(at=at,lo=lo,up=up)
}

#=============================================================
# Internal function to compute Bowker Test of Symmetry.
#=============================================================
iBowker <- function(obj) {
  AAT <- iHandleAgreeTable(obj)
  # Chi-sq parts (Evans and Hoenig's eq 1, Hoenig et al.'s eq 3)
  rat <- ((AAT$lo-t(AAT$up))^2)/(AAT$lo+t(AAT$up))
  # Add chi-square parts
  chi.sq <- sum(rat,na.rm=TRUE)
  # Count number of chi-sq parts
  df <- sum(as.numeric(!is.na(rat)))
  # Find the p-value 
  p <- stats::pchisq(chi.sq,df,lower.tail=FALSE)
  # Return dataframe
  data.frame(symTest="Bowker",df=df,chi.sq=chi.sq,p=p)
} ## End internal Bowker Test function

#=============================================================
# Internal function to compute McNemar Test of Symmetry.
#=============================================================
iMcNemar <- function(obj,cont.cor) {
  # handle continuity correction
  if (cont.cor=="none") {
    title <- "McNemar"
    cc <- 0
  } else if (cont.cor=="Yates") {
    title <- "McNemar (Yates Correction)"
    cc <- 0.5
  } else if (cont.cor== "Edwards") {
    title <- "McNemar (Edwards Correction)"
    cc <- 1
  } else STOP("Continuity correction is incorrect")
  AAT <- iHandleAgreeTable(obj)
  # Chi-sq parts (Evans and Hoenig's eq 2, but include the correction factor)
  top <- (abs(sum((AAT$lo-t(AAT$up)),na.rm=TRUE))-cc)^2
  bot <- sum(AAT$lo+t(AAT$up),na.rm=TRUE)
  chi.sq <- top/bot
  df <- 1
  # Find the p-value 
  p <- stats::pchisq(chi.sq,df,lower.tail=FALSE)
  # Return list
  data.frame(symTest=title,df=df,chi.sq=chi.sq,p=p)
} ## End internal McNemar Test function

#=============================================================
# Internal function to compute EvansHoenig Test of Symmetry.
#=============================================================
iEvansHoenig <- function(obj) {
  AAT <- iHandleAgreeTable(obj)
  # Create matrix of differences in potential ages
  diffs <- AAT$at
  for (i in seq_len(nrow(AAT$at))) {
    for (j in seq_len(ncol(AAT$at))) {
      diffs[i,j] <- as.numeric(rownames(AAT$at)[j])-as.numeric(colnames(AAT$at)[i])
    }
  }
  # Find max diff
  max.diff <- max(diffs)
  # Find parts of Evans Hoenig calcualtions -- finds individual off-diagonals
  #   and then computes the ratio that forms the chi-square parts
  rat <- numeric(nrow(AAT$at)-1)
  for (i in seq_len(max.diff)) {
    above <- AAT$at[diffs==i]
    below <- AAT$at[diffs==-i]
    rat[i] <- (sum(above-below)^2)/sum(above+below)
  }
  # Remove those values that were na (i.e., these were diagonals w/ no obs)
  rat <- rat[!is.na(rat)]
  # Find degrees-of-freedom ... number of off-diagonals with observations
  df <- length(rat)
  # sum the chi-square parts to get a full chi-square value
  chi.sq <- sum(rat)
  p <- stats::pchisq(chi.sq,df,lower.tail=FALSE)
  # Return data.frame
  data.frame(symTest="EvansHoenig",df=df,chi.sq=chi.sq,p=p)
} ## End internal Evans Hoenig's Test function


#' @rdname ageBias
#' @export
plot.ageBias <- function(x,xvals=c("reference","mean"),
                         xlab=ifelse(xvals=="reference",x$ref.lab,"Mean Age"),
                         ylab=paste(x$nref.lab,"-",x$ref.lab),
                         xlim=NULL,ylim=NULL,
                         yaxt=graphics::par("yaxt"),xaxt=graphics::par("xaxt"),
                         col.agree="gray60",lwd.agree=lwd,lty.agree=2,
                         lwd=1,sfrac=0,
                         show.pts=NULL,pch.pts=20,cex.pts=ifelse(xHist|yHist,1.5,1),
                         col.pts="black",transparency=1/10,
                         show.CI=FALSE,col.CI="black",col.CIsig="red",
                         lwd.CI=lwd,sfrac.CI=sfrac,
                         show.range=NULL,col.range=ifelse(show.CI,"gray70","black"),
                         lwd.range=lwd,sfrac.range=sfrac,
                         pch.mean=19,pch.mean.sig=ifelse(show.CI|show.range,21,19),
                         cex.mean=lwd,
                         yHist=TRUE,xHist=NULL,hist.panel.size=1/7,col.hist="gray90",
                         allowAdd=FALSE,...) { # nocov start
  ## Handle some defaults
  xvals <- match.arg(xvals)
  if (is.null(show.pts)) show.pts <- ifelse(xvals=="reference",FALSE,TRUE)
  if (is.null(show.range)) show.range <- ifelse(xvals=="reference",TRUE,FALSE)
  if (is.null(xHist)) xHist <- ifelse(xvals=="reference",TRUE,FALSE)
  ## Plot preparations
  op <- graphics::par(no.readonly=TRUE)
  iABSetLayout(yHist,xHist,hist.panel.size)
  col.pts <- col2rgbt(col.pts,transparency)
  ## Route based on plot type
  if (xvals=="reference") {
    iDiffAB(x,xlab,ylab,xlim,ylim,yaxt,xaxt,
            col.agree,lwd.agree,lty.agree,
            show.pts,pch.pts,cex.pts,col.pts,
            show.CI,col.CI,col.CIsig,lwd.CI,sfrac.CI,
            show.range,col.range,lwd.range,sfrac.range,
            pch.mean,pch.mean.sig,cex.mean,
            yHist,xHist,col.hist,op,...)
  } else {
    iDiffBA(x,xlab,ylab,xlim,ylim,yaxt,xaxt,
            col.agree,lwd.agree,lty.agree,
            pch.pts,cex.pts,col.pts,yHist,xHist,col.hist,op,...)
  }
  if(!allowAdd) {
    graphics::layout(1,widths=1,heights=1)
    graphics::par(op)
  }
} # nocov end


#=============================================================
# This internal function is used to find appropriate axis
# limits for the age bias plot. 
#=============================================================
iABAxisLmts <- function(x,xlim,ylim,difference,show.range,show.pts,show.CI) { # nocov start
  # identify whether difference data should be used or not, put in a tmp data frame
  if (!difference) { d <- x$bias } else { d <- x$bias.diff }
  # If no xlim given then make xlim the range of x values
  # which are given in the first position of d (note that
  # d is the age bias statistics)
  if (!is.null(xlim)) xlmt <- xlim
  else xlmt <- range(d[,1],na.rm=TRUE)
  # If no ylim is given then make ylim. Making ylim depends
  # on whether differences are used and whether ranges are
  # shown or not.
  if (!is.null(ylim)) ylmt <- ylim
  else {
    # build up a vector that will ultimately find the range
    # to form the axes. Begin by filling with the means.
    tmp.min <- tmp.max <- d$mean
    # if show.cis then add in LCI and UCI. CIs will be shown
    # for all real age bias plots. This is primarily for use
    # with the numbers plot
    if (show.CI) {
      tmp.min <- c(tmp.min,d$LCI)
      tmp.max <- c(tmp.max,d$UCI)
    }
    # if show.range or show.pts then add in min and max
    if (show.range | show.pts) {
      tmp.min <- c(tmp.min,d$min)
      tmp.max <- c(tmp.max,d$max)
    }
    # if not differences then add in xlmts
    tmp.min <- c(tmp.min,xlmt)
    # Find floor of min and ceiling of max to make the limits
    ylmt <- c(floor(min(tmp.min,na.rm=TRUE)),ceiling(max(tmp.max,na.rm=TRUE)))
  }
  # return values
  list(xlim=xlmt,ylim=ylmt)
} # nocov end

#=============================================================
# This internal function is used to find "pretty" ticks and 
#   labels for the axes of age bias plots. 
#=============================================================
iABAxisLbls <- function(x) { # nocov start
  xlmt <- x$xlim
  ylmt <- x$ylim
  # create pretty axis tickes and labels (integers and will include zero)
  xlbls.lmt <- c(floor(xlmt[1]),ceiling(xlmt[2]))
  xlbl1 <- seq(xlbls.lmt[1],xlbls.lmt[2],1)
  xlbl2 <- pretty(xlbls.lmt)
  if(any(apply(matrix(xlbl2),1,iGetDecimals)>0)) xlbl2 <- xlbl1
  ylbls.lmt <- c(floor(ylmt[1]),ceiling(ylmt[2]))
  ylbl1 <- seq(ylbls.lmt[1],ylbls.lmt[2],1)
  ylbl2 <- pretty(ylbls.lmt)
  if(any(apply(matrix(ylbl2),1,iGetDecimals)>0)) ylbl2 <- ylbl1
  # return values
  list(xlim=xlmt,xticks=xlbl1,xlbls=xlbl2,
       ylim=ylmt,yticks=ylbl1,ylbls=ylbl2)
} # nocov end


#=============================================================
# This internal function is used to create an appropriate
#   layout if yHist=TRUE or xHist=TRUE. 
#=============================================================
iABSetLayout <- function(yHist,xHist,hist.size) { # nocov start
  if (yHist & xHist) { # Both marginal hists
    graphics::layout(matrix(c(1,0,3,2),ncol=2,byrow=TRUE),
                     widths=c(1-hist.size,hist.size),
                     heights=c(hist.size,1-hist.size))
  } else if (yHist) {  # Only marginal hist on Y-axis
    graphics::layout(matrix(c(2,1),ncol=2),widths=c(1-hist.size,hist.size),heights=1)
  } else if (xHist) {  # Only marginal hist on X-axis
    graphics::layout(matrix(c(1,2),ncol=1),widths=1,heights=c(hist.size,1-hist.size))
  } else {
    graphics::layout(1,widths=1,heights=1) # no marginal hists (return normal layout)
  }
} # nocov end


#=============================================================
# This internal function is used to create the histogram for
#   the x-axis margin if xHist=TRUE. 
#=============================================================
iABAddXHist <- function(xdat,col.hist,axlmts,op,yHist) { # nocov start
  # Set graphing parameters for this panel
  #   the bottom is set to zero to butt up against the age bias plot,
  #   the left and right are set to the same as the age bias plot which
  #   forces the width to be the same as the age bias plot, and the top
  #   is set to a small value (either 0.5 or what the user had it set at
  #   if that was less than 1, values greater than 1 are ugly).
  graphics::par(mar=c(0,op$mar[2],ifelse(op$mar[3]<1.25,op$mar[3],0.5),
                      ifelse(yHist,0,op$mar[4])))
  # Make a histogram (but don't plot) with breaks that are one unit wide
  #   to find the breaks and counts data for use in rect below
  startcat <- floor(min(xdat,na.rm=TRUE))
  breaks <- seq(startcat,max(xdat,na.rm=TRUE)+1,1)
  tmp <- graphics::hist(xdat,right=FALSE,plot=FALSE,
                        warn.unused=FALSE,breaks=breaks)
  # Find x and y-axis limits
  ylmts <- c(0,max(tmp$counts))
  xlmts <- axlmts$xlim
  # Make a blank plot to which rectangles will be added
  graphics::plot(NULL,type="n",yaxs="i",xaxt="n",yaxt="n",bty="n",
                 xlab="",ylab="n",ylim=ylmts,xlim=xlmts)
  # Add the rectangles
  graphics::rect(tmp$breaks[-length(tmp$breaks)]-0.5,0,
                 tmp$breaks[-1]-0.5,tmp$counts,col=col.hist)
  # Put a y-axis on the right, with only two values marked
  tcks <- tckslbl <- c(0,round(mean(ylmts),0),max(ylmts))
  graphics::axis(2,at=tcks,labels=NA)
  graphics::axis(2,at=tcks[2])
  graphics::axis(2,at=tcks[3],xpd=TRUE)
  # Extends line down from zero ... looks like a break for labels
  graphics::axis(2,at=0,labels=NA,tcl=-0.8*graphics::par()$mgp[1])
  # Add small (either 1/2 of default tcl or 0.2) x-axis ticks
  tmp <- graphics::par()$tcl/2
  graphics::axis(1,at=axlmts$xticks,labels=NA,tcl=ifelse(tmp<(-0.2),-0.2,tmp))
} # nocov end


#=============================================================
# This internal function is used to create the histogram for
#   the y-axis margin if yHist=TRUE. 
#=============================================================
iABAddYHist <- function(ydat,col.hist,axlmts,op,xHist) { # nocov start
  # Set graphing parameters for this panel
  #   the left is set to zero to butt up against the age bias plot,
  #   the bottom and top are set to the same as the age bias plot which
  #   forces the height to be the same as the age bias plot, and the right
  #   is set to a small value (either 0.5 or what the user had it set at
  #   if that was less than 1, values greater than 1 are ugly).
  graphics::par(mar=c(op$mar[1],0,ifelse(xHist,0,op$mar[3]),
                      ifelse(op$mar[4]<1.25,op$mar[4],0.5)))
  # Make a histogram (but don't plot) with breaks that are one unit wide
  #   to find the breaks and counts data for use in rect below
  startcat <- floor(min(ydat,na.rm=TRUE))
  breaks <- seq(startcat,max(ydat,na.rm=TRUE)+1,1)
  tmp <- graphics::hist(ydat,right=FALSE,plot=FALSE,
                        warn.unused=FALSE,breaks=breaks)
  # Find x and y-axis limits
  xlmts <- c(0,max(tmp$counts))
  ylmts <- axlmts$ylim
  # Make a blank plot to which rectangles will be added
  graphics::plot(NULL,type="n",xaxs="i",xaxt="n",yaxt="n",bty="n",
                 xlab="n",ylab="",ylim=ylmts,xlim=xlmts)
  # Add the rectangles
  graphics::rect(0,tmp$breaks[-length(tmp$breaks)]-0.5,
                 tmp$counts,tmp$breaks[-1]-0.5,col=col.hist)
  # Put a y-axis on the right, with only two values marked
  tcks <- tckslbl <- c(0,round(mean(xlmts),0),max(xlmts))
  graphics::axis(1,at=tcks,labels=NA)
  graphics::axis(1,at=tcks[2])
  graphics::axis(1,at=tcks[3],xpd=TRUE)
  # Extends line down from zero ... looks like a break for labels
  graphics::axis(1,at=0,labels=NA,tcl=-0.8*graphics::par()$mgp[1])
  # Add small (either 1/2 of default tcl or 0.2) x-axis ticks
  tmp <- graphics::par()$tcl/2
  graphics::axis(2,at=axlmts$yticks,labels=NA,tcl=ifelse(tmp<(-0.2),-0.2,tmp))
} # nocov end


#=============================================================
# Internal function to produce the age bias plot using differences.
#   This was inspired by Muir et al. (2008)
#=============================================================
iDiffAB <- function(x,xlab,ylab,xlim,ylim,yaxt,xaxt,
                    col.agree,lwd.agree,lty.agree,
                    show.pts,pch.pts,cex.pts,col.pts,
                    show.CI,col.CI,col.CIsig,lwd.CI,sfrac.CI,
                    show.range,col.range,lwd.range,sfrac.range,
                    pch.mean,pch.mean.sig,cex.mean,
                    yHist,xHist,col.hist,op,...) { # nocov start
  ## Get data
  sumd <- x$bias.diff
  rawd <- x$data[,c(x$ref.name,"diff")]
  ## Find useful axis limits
  axlmts <- iABAxisLmts(x,xlim,ylim,show.range,show.pts,show.CI,difference=TRUE)
  axlmts <- iABAxisLbls(axlmts)
  ## Add axis histograms if asked for
  if (xHist) iABAddXHist(rawd[,1],col.hist,axlmts,op,yHist)
  if (yHist) iABAddYHist(rawd[,2],col.hist,axlmts,op,xHist)
  ## Make Main Plot
  graphics::par(mar=c(op$mar[1],op$mar[2],
                      ifelse(xHist,0,op$mar[3]),ifelse(yHist,0,op$mar[4])))
  # Set base plot with Mean of 2nd vs. 1st age range
  graphics::plot(sumd$mean~sumd[,x$ref.name],xlim=axlmts$xlim,ylim=axlmts$ylim,
                 xlab=xlab,ylab=ylab,col="white",yaxt="n",xaxt="n",...)
  # Add axes (integer ticks, prettier labels (incl. 0))
  if (xaxt!="n") {
    graphics::axis(1,at=axlmts$xticks,labels=NA)
    graphics::axis(1,at=axlmts$xlbls)
  }
  if (yaxt!="n") {
    graphics::axis(2,at=axlmts$yticks,labels=NA)
    graphics::axis(2,at=axlmts$ylbls)
  }
  # agreement line -- horizontal for difference and 45 degree for bias plot
  graphics::abline(h=0,lwd=lwd.agree,lty=lty.agree,col=col.agree)
  # add individual points if asked for
  if (show.pts) {
    graphics::points(x$d[,x$ref.name],x$d[,"diff"],
                     col=col.pts,pch=pch.pts,cex=cex.pts)
  }
  # add range of individual points if asked for
  if (show.range) {
    plotrix::plotCI(x=sumd[,x$ref.name],y=sumd$mean,li=sumd$min,ui=sumd$max,
                    add=TRUE,slty=1,scol=col.range,pch=NULL,
                    lwd=lwd.range,gap=0,sfrac=sfrac.range)
  }
  # add CIs for mean if asked for
  if (show.CI) {
    if (any(sumd$sig)) {  #  for ages that are signficantly different
      plotrix::plotCI(x=sumd[,x$ref.name][sumd$sig],y=sumd$mean[sumd$sig],
                      li=sumd$LCI[sumd$sig],ui=sumd$UCI[sumd$sig],
                      add=TRUE,slty=1,scol=col.CIsig,pch=NULL,
                      lwd=lwd.CI,gap=0,sfrac=sfrac.CI)
    }
    if (any(!sumd$sig)) {#  for ages that are not significantly different
      plotrix::plotCI(x=sumd[,x$ref.name][!sumd$sig],y=sumd$mean[!sumd$sig],
                      li=sumd$LCI[!sumd$sig],ui=sumd$UCI[!sumd$sig],
                      add=TRUE,slty=1,scol=col.CI,pch=NULL,
                      lwd=lwd.CI,gap=0,sfrac=sfrac.CI)
    }
  }
  # Add means
  if (show.CI | show.range) {
    if (any(sumd$sig)) {  #  for ages that are signficantly different
      graphics::points(x=sumd[,x$ref.name][sumd$sig],y=sumd$mean[sumd$sig],
                       pch=pch.mean.sig,cex=cex.mean,
                       col=ifelse(show.CI,col.CIsig,col.CI),bg="white")
    }
    if (any(!sumd$sig)) {#  for ages that are not significantly different
      graphics::points(x=sumd[,x$ref.name][!sumd$sig],y=sumd$mean[!sumd$sig],
                       pch=pch.mean,cex=cex.mean,col=col.CI,bg="white")
    }
  }
} # nocov end


#=============================================================
# Internal function to produce the age bias plot using differences
#   but with mean ages on the x-axis. This was inspired by the
#   Bland-Altman plots in McBride (2015).
#=============================================================
iDiffBA <- function(x,xlab,ylab,xlim,ylim,yaxt,xaxt,
                    col.agree,lwd.agree,lty.agree,
                    pch.pts,cex.pts,col.pts,
                    yHist,xHist,col.hist,op,...) { # nocov start
  rawd <- x$data[,c("mean","diff")]
  if (is.null(xlim)) xlim <- range(rawd[,1])
  if (is.null(ylim)) ylim <- range(rawd[,2])
  axlmts <- list(xlim=xlim,ylim=ylim)
  axlmts <- iABAxisLbls(axlmts)
  ## Add axis histograms if asked for
  if (xHist) iABAddXHist(rawd[,1],col.hist,axlmts,op,yHist)
  if (yHist) iABAddYHist(rawd[,2],col.hist,axlmts,op,xHist)
  ## Make Main Plot
  graphics::par(mar=c(op$mar[1],op$mar[2],
                      ifelse(xHist,0,op$mar[3]),ifelse(yHist,0,op$mar[4])))
   # Set base plot with Mean of 2nd vs. 1st age range
  graphics::plot(rawd[,2]~rawd[,1],xlim=axlmts$xlim,ylim=axlmts$ylim,
                 xlab=xlab,ylab=ylab,col="white",yaxt="n",xaxt="n",...)
  # Add axes (integer ticks, prettier labels (incl. 0))
  if (xaxt!="n") {
    graphics::axis(1,at=axlmts$xticks,labels=NA)
    graphics::axis(1,at=axlmts$xlbls)
  }
  if (yaxt!="n") {
    graphics::axis(2,at=axlmts$yticks,labels=NA)
    graphics::axis(2,at=axlmts$ylbls)
  }
  # agreement line -- horizontal for difference and 45 degree for bias plot
  graphics::abline(h=0,lwd=lwd.agree,lty=lty.agree,col=col.agree)
  # add individual points if asked for
  graphics::points(rawd[,2]~rawd[,1],col=col.pts,pch=pch.pts,cex=cex.pts)
} # nocov end


#' @rdname ageBias
#' @export
plotAB <- function(x,what=c("bias","Campana","numbers"),
                   xlab=x$ref.lab,ylab=x$nref.lab,xlim=NULL,ylim=NULL,
                   yaxt=graphics::par("yaxt"),xaxt=graphics::par("xaxt"),
                   col.agree="gray60",lwd.agree=lwd,lty.agree=2,lwd=1,sfrac=0,
                   pch.mean=19,pch.mean.sig=21,cex.mean=lwd,
                   col.CI="black",col.CIsig="red",lwd.CI=lwd,sfrac.CI=sfrac,
                   show.n=FALSE,nYpos=1.03,cex.n=0.75,
                   cex.numbers=0.75,col.numbers="black",...) { # nocov start
  what <- match.arg(what)
  axlmts <- iABAxisLmts(x,xlim,ylim,difference=FALSE,show.range=FALSE,
                        show.pts=FALSE,show.CI=TRUE)
  if (what!="numbers") {
    iPlotABCamp(x,xlab,ylab,axlmts,yaxt,xaxt,
                col.agree,lwd.agree,lty.agree,lwd,sfrac,
                pch.mean,pch.mean.sig,cex.mean,col.CI,
                col.CIsig,lwd.CI,sfrac.CI,show.n,nYpos,cex.n,...)
  } else {
    iPlotABNum(x,xlab,ylab,axlmts,yaxt,xaxt,
               lwd.agree,lty.agree,col.agree,cex.numbers,col.numbers,...)
  } 
} # nocov end


#=============================================================
# Internal function to produce the age bias plot (like Campana).
#=============================================================
iPlotABCamp <- function(x,xlab,ylab,axlmts,yaxt,xaxt,
                        col.agree,lwd.agree,lty.agree,lwd,sfrac,
                        pch.mean,pch.mean.sig,cex.mean,col.CI,
                        col.CIsig,lwd.CI,sfrac.CI,
                        show.n,nYpos,cex.n,...) { # nocov start
  sumd <- x$bias
  # Plot more tick marks    
  graphics::par(lab=c(length(sumd[,x$ref.name]),length(sumd$mean),7))    
  # Set base plot with Mean of 2nd vs. 1st age range
  graphics::plot(sumd$mean~sumd[,x$ref.name],xlim=axlmts$xlim,ylim=axlmts$ylim,
                 xlab=xlab,ylab=ylab,col="white",yaxt="n",...)
  # Helps keep y-axis as integers (needed for difference plot)
  if (yaxt!="n") {graphics::axis(2,seq(axlmts$ylim[1],axlmts$ylim[2],1))}
  # agreement line -- horizontal for difference and 45 degree for bias plot
  graphics::abline(a=0,b=1,lwd=lwd.agree,lty=lty.agree,col=col.agree)
  # add CIs for mean
  if (any(sumd$sig)) {  #  for ages that are signficantly different
    plotrix::plotCI(x=sumd[,x$ref.name][sumd$sig],y=sumd$mean[sumd$sig],
                    li=sumd$LCI[sumd$sig],ui=sumd$UCI[sumd$sig],
                    add=TRUE,slty=1,scol=col.CIsig,pch=NULL,
                    lwd=lwd.CI,gap=0,sfrac=sfrac.CI)
  }
  if (any(!sumd$sig)) {#  for ages that are not significantly different
    plotrix::plotCI(x=sumd[,x$ref.name][!sumd$sig],y=sumd$mean[!sumd$sig],
                    li=sumd$LCI[!sumd$sig],ui=sumd$UCI[!sumd$sig],
                    add=TRUE,slty=1,scol=col.CI,pch=NULL,
                    lwd=lwd.CI,gap=0,sfrac=sfrac.CI)
  }
  # Add means
  if (any(sumd$sig)) {  #  for ages that are signficantly different
    graphics::points(x=sumd[,x$ref.name][sumd$sig],y=sumd$mean[sumd$sig],
                     pch=pch.mean.sig,cex=cex.mean,col=col.CIsig,bg="white")
  }
  if (any(!sumd$sig)) {#  for ages that are not significantly different
    graphics::points(x=sumd[,x$ref.name][!sumd$sig],y=sumd$mean[!sumd$sig],
                     pch=pch.mean,cex=cex.mean,col=col.CI,bg="white")
  }
  # show the sample sizes at the top if asked for
  if (show.n) graphics::text(sumd[,x$ref.name],graphics::grconvertY(nYpos,"npc"),
                             sumd$n,cex=cex.n,xpd=TRUE)
}  # nocov end

#=============================================================
# Internal function to produce the age bias numbers plot.
#=============================================================
iPlotABNum <- function(obj,xlab,ylab,axlmts,yaxt,xaxt,
                       lwd.agree,lty.agree,col.agree,
                       cex.numbers,col.numbers,...) { # nocov start
  # convert age-agreement table into a data frame with all zeros removed
  # y,x in d[,1] and d[,2], respectively
  # lables in d[,3]
  d <- as.data.frame(obj$agree)
  d[,1] <- fact2num(d[,1])
  d[,2] <- fact2num(d[,2])
  d <- d[d[,3]>0,]
  # make an empty plot
  graphics::plot(d[,2],d[,1],type="n",xlab=xlab,ylab=ylab,
                 xlim=axlmts$xlim,ylim=axlmts$ylim,yaxt="n",xaxt="n",...)
  # Helps keep axes as integers
  if (yaxt!="n") {graphics::axis(2,seq(axlmts$ylim[1],axlmts$ylim[2],1))}
  if (xaxt!="n") {graphics::axis(1,seq(axlmts$xlim[1],axlmts$xlim[2],1))}
  # add the one-to-one line
  graphics::lines(axlmts$xlim,axlmts$xlim,lwd=lwd.agree,lty=lty.agree,col=col.agree)
  # add the numbers at each point
  graphics::text(d[,2],d[,1],labels=d[,3],cex=cex.numbers,col=col.numbers)
} # nocov end



#' @title Compute measures of precision among sets of ages.
#'
#' @description Computes overall measures of precision for multiple age estimates made on the same individuals. Ages may be from two or more readers of the same structure, one reader at two or more times, or two or more structures (e.g., scales, spines, otoliths). Measures of precision include ACV (Average Coefficient of Variation), APE (Average Percent Error), and various percentage difference values.
#'
#' @param formula A formula of the form \code{~var1+var2+var3+...} or, alternatively, \code{var1~var2+var3+...}, where the \code{varX} generically represent the variables that contain the age estimates. The alternative formula allows for similar code as used in \code{\link{ageBias}} and can have only one variable on the left-hand side.
#' @param data A data.frame that minimally contains the variables in \code{formula}.
#' @param object An object of class \code{agePrec}, usually from \code{agePrecision}.
#' @param what A string (or vector of strings) that indicates what type of summary to print. See details.
#' @param percent A logical that indicates whether the difference table (see details) should be represented as percentages (\code{TRUE}; default) or frequency (\code{FALSE}) of fish.
#' @param trunc.diff A single integer that identifies the age for which all values that age and greater are combined into one category. See the examples.
#' @param digits A single numeric that indicates the minimum number of digits to print when using \code{summary}.
#' @param \dots Additional arguments for methods.
#'
#' @details If \code{what="precision"} in \code{summary} then a summary table that contains the following items will be printed:
#' \itemize{
#'   \item n Number of fish in \code{data}.
#'   \item validn Number of fish in \code{data} that have non-\code{NA} data for all R age estimates.
#'   \item R Number of age estimates given in \code{formula}.
#'   \item ACV The mean coefficient of variation. See the \href{http://derekogle.com/IFAR}{IFAR chapter} for calculation details.
#'   \item APE The mean average percent error. See the \href{http://derekogle.com/IFAR}{IFAR chapter} for calculation details.
#'   \item PercAgree The percentage of fish for which all age estimates perfectly agree.
#' }
#'
#' If \code{what="difference"} is used in \code{summary}, then a table that describes either the percentage (if \code{percent=TRUE}, default) or frequency of fish by the difference in paired age estimates. This table has one row for each possible pair of age estimates.
#'
#' If \code{what="absolute difference"} is used in \code{summary}, then a table that describes either the percentage (if \code{percent=TRUE}, default) or frequency of fish by the absolute value of the difference in paired age estimates. This table has one row for each possible pair of age estimates. The \dQuote{1} column, for example, represents age estimates that disagree by one year (in either direction).
#'
#' If \code{what="detail"} is used in \code{summary}, then a data frame of the original \code{data} along with the intermediate calculations of the average age, standard deviation of age, APE, and ACV for each individual will be printed. These details are generally only used to check or to understand calculations.
#' 
#' All percentage calculations above use the \code{validn} value in the denominator.
#' 
#' @return The main function returns a list with the following items:
#' \itemize{
#'   \item detail A data.frame with all data given in \code{data} and intermediate calculations for each fish. See details
#'   \item rawdiff A frequency table of fish by differences for each pair of ages.
#'   \item absdiff A frequency table of fish by absolute differences for each pair of ages.
#'   \item APE The mean average percent error.
#'   \item ACV The mean coefficient of variation.
#'   \item n Number of fish in \code{data}.
#'   \item validn Number of fish in \code{data} that have non-\code{NA} data for all R age estimates.
#'   \item R Number of age estimates for each fish given in \code{formula}.
#' }
#'
#' The \code{summary} returns the result if \code{what=} contains one item, otherwise it returns nothing. See details for what is printed.
#' 
#' @section Testing: Tested all precision results against published results in Herbst and Marsden (2011) for the \code{\link{WhitefishLC}} data and the results for the \code{\link[FSAdata]{AlewifeLH}} data set from \pkg{FSAdata} against results from the online resource at http://www.nefsc.noaa.gov/fbp/age-prec/.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @section IFAR Chapter: 4-Age Comparisons.
#'
#' @seealso See \code{\link{ageBias}} for computation of the full age agreement table, along with tests and plots of age bias.
#' 
#' @references Ogle, D.H. 2016. \href{http://derekogle.com/IFAR}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Beamish, R.J. and D.A. Fournier. 1981. A method for comparing the precision of a set of age determinations. Canadian Journal of Fisheries and Aquatic Sciences 38:982-983. [Was (is?) available from http://www.pac.dfo-mpo.gc.ca/science/people-gens/beamish/PDF_files/compareagecjfas1981.pdf.]
#'
#'Campana, S.E. 1982. Accuracy, precision and quality control in age determination, including a review of the use and abuse of age validation methods. Journal of Fish Biology 59:197-242. [Was (is?) available from http://www.denix.osd.mil/nr/crid/Coral_Reef_Iniative_Database/References_for_Reef_Assessment_files/Campana,\%202001.pdf.]
#'
#'Campana, S.E., M.C. Annand, and J.I. McMillan. 1995. Graphical and statistical methods for determining the consistency of age determinations. Transactions of the American Fisheries Society 124:131-138. [Was (is?) available from http://www.bio.gc.ca/otoliths/documents/Campana\%20et\%20al\%201995\%20TAFS.pdf.]
#'
#'Chang, W.Y.B. 1982. A statistical method for evaluating the reproducibility of age determination. Canadian Journal of Fisheries and Aquatic Sciences 39:1208-1210. [Was (is?) available from http://www.nrcresearchpress.com/doi/abs/10.1139/f82-158.]
#' 
#' McBride, R.S. 2015. Diagnosis of paired age agreement: A simulation approach of accuracy and precision effects. ICES Journal of Marine Science, 72:2149-2167.
#'
#' @aliases agePrecision plot.agePrec summary.agePrec
#'
#' @keywords htest manip
#' 
#' @examples
#' ## Example with just two age estimates
#' data(WhitefishLC)
#' ap1 <- agePrecision(~otolithC+scaleC,data=WhitefishLC)
#' summary(ap1)
#' summary(ap1,what="precision")
#' summary(ap1,what="difference")
#' summary(ap1,what="difference",percent=FALSE)
#' summary(ap1,what="absolute")
#' summary(ap1,what="absolute",percent=FALSE)
#' summary(ap1,what="absolute",trunc.diff=4)
#' summary(ap1,what="absolute",percent=FALSE)
#' summary(ap1,what=c("precision","difference"))
#'
#' barplot(ap1$rawdiff,ylab="Frequency",xlab="Otolith - Scale Age")
#' summary(ap1,what="detail")
#'
#' ## Example with three age estimates
#' ap2 <- agePrecision(~otolithC+finrayC+scaleC,data=WhitefishLC)
#' summary(ap2)
#' summary(ap2,what="precision")
#' summary(ap2,what="difference")
#' summary(ap2,what="difference",percent=FALSE)
#' summary(ap2,what="absolute")
#' summary(ap2,what="absolute",percent=FALSE)
#' summary(ap2,what="absolute",trunc.diff=4)
#' summary(ap2,what="absolute",percent=FALSE,trunc.diff=4)
#' summary(ap2,what="detail")
#'
#' @rdname agePrecision
#' @export
agePrecision <- function(formula,data) {
  # change formula to have only a RHS
  tmp <- as.character(formula)[-1]
  formula <- stats::as.formula(paste("~",paste(tmp,collapse="+")))
  tmp <- iHndlFormula(formula,data)
  
  if (!tmp$Etype=="numeric") STOP("All variables must be numeric.")
  # get dataframe of just ages (for simplicity)
  d <- tmp$mf
  # number of structures, complete sample size, and valid n (all non-NA values)
  R <- ncol(d)
  n <- nrow(d)
  validn <- sum(stats::complete.cases(d))
  
  ## Precision alculations (APE and ACV) on each fish
  # Mean, SD of assigned ages
  age.avg <- apply(d,1,mean)
  age.sd <- apply(d,1,stats::sd)
  # Summed absolute deviation
  tmp.adevs <- abs(apply(d,2,'-',age.avg))
  age.ad <- apply(tmp.adevs,1,sum)
  # APE & ACV for each fish
  APE.j <- ((age.ad/age.avg)/R)*100
  ACV.j <- (age.sd/age.avg)*100
  # Replaced NAs with 0 in APE.j and ACV.j for when age.avg==0
  tmp <- which(age.avg==0)
  APE.j[tmp] <- 0
  ACV.j[tmp] <- 0
  # Put results into a data.frame to return
  detail.df <- data.frame(d,avg=age.avg,sd=age.sd,APE=APE.j,ACV=ACV.j)
  ## Summary precision calculations (mean APE, ACV, total agreement) for all fish
  APE <- mean(APE.j,na.rm=TRUE)
  ACV <- mean(ACV.j,na.rm=TRUE)
  # all ages agree if sd=0 (use sum and na.rm to remove NAs)
  all.agree <- sum(detail.df$sd==0,na.rm=TRUE)/validn*100
  
  ## Raw age agreement summaries
  # find all pairs of comparisons
  prs <- t(utils::combn(names(d),2))
  # maximum possible difference is max age - min age ... use this to set the levels
  #   for the agreement table.
  tmp <- max(d,na.rm=TRUE)-min(d,na.rm=TRUE)
  poss.lvls <- seq(-tmp,tmp,1)
  # create a matrix to contain the results of comparing each pair
  ragree <- matrix(NA,ncol=length(poss.lvls),nrow=nrow(prs))
  # cycle through each paired comparison putting results in agreement matrix
  for (i in seq_len(nrow(prs))) {
    tmp <- d[,prs[i,1]]-d[,prs[i,2]]
    ragree[i,] <- table(factor(tmp,levels=poss.lvls))
  }
  # relabel rows and columns of agreement table
  rownames(ragree) <- apply(prs,1,paste,collapse=" - ")
  colnames(ragree) <- poss.lvls
  # delete right- and left-most columns that contain all zeros
  tmp <- c(which(rcumsum(colSums(ragree))==0),which(cumsum(colSums(ragree))==0))
  if (length(tmp>0)) ragree <- ragree[,-tmp]
  
  ## Absolute age agreement summaries
  # maximum possible difference is max age - min age ... use this to set the levels
  #   for the agreement table.
  poss.lvls <- 0:(max(d,na.rm=TRUE)-min(d,na.rm=TRUE))
  # create a matrix to contain the results of comparing each pair
  aagree <- matrix(NA,ncol=length(poss.lvls),nrow=nrow(prs))
  # cycle through each paired comparison putting results in agreement matrix
  for (i in seq_len(nrow(prs))) {
    tmp <- abs(d[,prs[i,1]]-d[,prs[i,2]])
    aagree[i,] <- table(factor(tmp,levels=poss.lvls))
  }
  # relabel rows and columns of agreement table
  rownames(aagree) <- apply(prs,1,paste,collapse=" v. ")
  colnames(aagree) <- poss.lvls
  # delete right-most columns that contain all zeros
  tmp <- which(rcumsum(colSums(aagree))==0)
  if (length(tmp>0)) aagree <- aagree[,-tmp]
  
  ## Put together an output list
  d <- list(detail=detail.df,rawdiff=as.table(ragree),absdiff=as.table(aagree),
            APE=APE,ACV=ACV,PercAgree=all.agree,R=R,n=n,validn=validn)
  class(d) <- "agePrec"
  d 
}

#' @rdname agePrecision
#' @export
summary.agePrec <- function(object,what=c("precision","difference","absolute difference","details"),
                            percent=TRUE,trunc.diff=NULL,digits=4,...) {
  what <- match.arg(what,several.ok=TRUE)
  retres <- ifelse(length(what)==1,TRUE,FALSE)
  showmsg <- ifelse (length(what)>1,TRUE,FALSE)
  if ("precision" %in% what) {
    if (showmsg) cat("Precision summary statistics\n")
    tmp <- with(object,data.frame(n=n,validn=validn,R=R,ACV=ACV,APE=APE,PercAgree=PercAgree)) 
    print(tmp,row.names=FALSE,digits=digits)
    what <- iHndlMultWhat(what,"precision","cat")
  }
  if ("absolute difference" %in% what) {
    tmp <- object$absdiff
    ## potentially convert to truncated distribution
    if (!is.null(trunc.diff)) {
      if(trunc.diff<=0) STOP("'trunc.diff' must be positive.")
      if (length(dim(tmp))==1) {
        # find positions in vector to be truncated
        trpos <- as.numeric(names(tmp))>=trunc.diff
        # find sum of truncated ages if there are any
        tmp <- c(tmp[!trpos],sum(tmp[trpos]))
        names(tmp)[length(tmp)] <- paste0(trunc.diff,"+")
        tmp <- as.table(tmp)
      } else {
        # find positions in vector to be truncated
        trpos <- as.numeric(colnames(tmp))>=trunc.diff
        # find sum of truncated ages if there are any
        tmp <- cbind(tmp[,!trpos],rowSums(tmp[,trpos]))
        colnames(tmp)[ncol(tmp)] <- paste0(trunc.diff,"+")
        # check if non-truncated was only one column so that
        # the column names can be fixed
        colnames(tmp)[1] <- colnames(object$absdiff)[1]
        tmp <- as.table(tmp)
      }
    }
    msg <- "of fish by absolute differences in ages\n between pairs of estimates\n"
    if (percent) {
      msg <- paste("Percentage",msg)
      # need to check if 1-D, then handle as a vector
      if (length(dim(tmp))==1) tmp <- tmp/sum(tmp)*100 
      else tmp <- prop.table(tmp,margin=1)*100      
    } else msg <- paste("Frequency",msg)
    if (showmsg) cat(msg)
    print(tmp,digits=digits)
    what <- iHndlMultWhat(what,"absolute difference","cat")
  }  
  if ("difference" %in% what) {
    tmp <- object$rawdiff
    msg <- "of fish by differences in ages\n between pairs of estimates\n"
    if (percent) {
      msg <- paste("Percentage",msg)
      # need to check if 1-D, then handle as a vector
      if (length(dim(tmp))==1) tmp <- tmp/sum(tmp)*100
      else tmp <- prop.table(tmp,margin=1)*100      
    } else msg <- paste("Frequency",msg)
    if (showmsg) cat(msg)
    print(tmp,digits=digits)
    what <- iHndlMultWhat(what,"difference","cat")
  }
  if ("details" %in% what) {
    if (showmsg) cat("Intermediate calculations for each individual\n")
    tmp <- object$detail 
    print(tmp,digits=digits)
    what <- iHndlMultWhat(what,"detail","cat")
  }
  if (retres) invisible(tmp)
}
