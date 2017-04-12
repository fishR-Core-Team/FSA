#' @title Compute and view possible biases between paired sets of ages.
#'
#' @description Constructs age-agreement tables, statistical tests to detect bias, and plots to visualize potential bias in paired age assignments.  Ages may be from, for example, two readers of the same structure, one reader at two times, two structures (e.g., scales, spines, otoliths), or one structure and known ages.
#'
#' @param formula A formula of the form \code{nrefvar~refvar}, where \code{nrefvar} and \code{refvar} generically represent the variables that contain the paired \dQuote{nonreference} and \dQuote{reference} age assignments, respectively.  See details.
#' @param data A data.frame that minimally contains the paired age assignments given in \code{formula}.
#' @param ref.lab A string that contains a label for the reference age assignments.
#' @param nref.lab A string that contains a label for the nonreference age assignments.
#' @param method A string that indicates which method to use for adjusting p-values for multiple comparisons.  See \code{?p.adjust.methods}.
#' @param sig.level A numeric value used to determine whether a p-value indicates a significant result.  The confidence level used in \code{plot} is 100*(1-\code{sig.level}).
#' @param min.n.CI A numeric value (default is 3) that indicates the smallest sample size for which a confidence interval should be computed.
#' @param what A string that indicates what type of summary to print or plot to construct.  See details.
#' @param flip.table A logical that indicates whether the age-agreement table should be \sQuote{flipped} (i.e., rows are reversed so that younger ages are at the bottom of the table).  This makes the table more directly comparable to the age-bias plot.
#' @param zero.print A string that indicates what should be printed in place of the zeros on an age-agreement table.  The default is to print a single dash.
#' @param digits A numeric value that indicates the minimum number of digits to print when showing \code{what="bias"} or \code{what="diff.bias"} in \code{summary}.
#' @param cont.corr A string that indicates the continuity correction method to be used with (only) McNemar test.  If \code{"none"} (default) then no continuity correction is used, if \code{"Yates"} then 0.5 is used, and if \code{"Edwards"} then 1 is used. 
#' @param x,object An object of class \code{ageBias}, usually a result from \code{ageBias}.
#' @param difference A logical that indicates whether or not the difference between the two age assignments should be used.  See details.
#' @param xlab,ylab A string that contains a label for the x-axis (reference) or y-axis (non-reference) age assignments, respectively. 
#' @param show.n A logical that indicates whether the sample sizes for each level of the x-axis variable is shown (\code{=TRUE}, default) or not (\code{=FALSE}).
#' @param nYpos A numeric value that indicates the relative Y position of the sample size values when \code{show.n=TRUE}.  For example, if \code{nYpos=1.03} then the sample size values will be centered at 3 percent above the top end of the y-axis.
#' @param cex.n  A character expansion value for the size of the sample size values.
#' @param lwd A single numeric value that can be used to control the separate \sQuote{lwd} argument (e.g., \code{lwd.CI}, \code{lwd.range}).
#' @param show.pts A logical that indicates whether to show the raw data points.
#' @param pch.pts A numeric value that indicates the plotting character to be used when plotting the raw data points.
#' @param col.pts A string or numeric value that indicates the color to be used for plotting the raw data points.  The default is to use black with the transparency found in \code{transparency}.
#' @param transparency A numeric value (between 0 and 1) that indicates the level of transparency to use for plotting the raw data points.  If expressed as 1/x, then x points plotted on top of each other will represent the color in \code{col.pts}.
#' @param pch.mean A numeric value that indicates the plotting character to be used for the mean values (i.e., center of confidence interval bars).
#' @param pch.mean.sig A numeric value that indicates the plotting character to be used for the mean values (i.e., center of confidence interval bars) when the means are considered significant.
#' @param cex.mean A character expansion value for the size of the mean symbol in \code{pch.mean}.
#' @param col.CI A string or numeric value that indicates the color to be used for confidence interval bars that are considered non-significant.
#' @param col.CIsig A string or numeric value that indicates the color to be used for confidence interval bars that are considered significant.
#' @param lwd.CI A numeric value that indicates the line width for the confidence interval bars.
#' @param sfrac A numeric value that controls the size of the ends of the confidence interval bars.  See \code{sfrac} in \code{\link[plotrix]{plotCI}} of \pkg{plotrix}.
#' @param show.range A logical that indicates whether to show vertical bars that represent the range of the data points.
#' @param col.range A string or numeric value that indicates the color to be used for the interval representing the range of the data.
#' @param lwd.range A numeric value that indicates the line width for the interval representing the range of the data.
#' @param col.agree A string or numeric value that indicates the color for the 1:1 or zero (if difference) reference line.
#' @param lwd.agree A numeric value that indicates the line width for the 1:1 or zero (if difference) reference line.
#' @param lty.agree A numeric value that indicates the line type for the 1:1 or zero (if difference) reference line.
#' @param cex.numbers A character expansion value for the size of the numbers plotted when \code{what="numbers"} is used.
#' @param xlim,ylim A numeric vector of length 2 that contains the limits of the x-axis (reference ages) or y-axis (non-reference ages), respectively.
#' @param xaxt,yaxt A string which specifies the x- and y-axis types. Specifying \dQuote{n} suppresses plotting of the axis.  See \code{?par}.
#' @param \dots Additional arguments for methods.
#'
#' @details Generally, one of the two age estimates will be identified as the \dQuote{reference} set.  In some cases this may be the true ages, the ages from the more experience reader, the ages from the first reading, or the ages from the structure generally thought to provide the most accurate results.  In other cases, such as when comparing two novice readers, the choice may be arbitrary.  The reference ages will form the columns of the age-agreement table and will be the \dQuote{constant} age used in the t-tests and age-bias plots (i.e., the x-axis).  See further details below.
#' 
#' The age-agreement table is constructed with  \code{what="table"} in \code{summary}.  The agreement table can be \dQuote{flipped} (i.e., the rows in descending rather than ascending order) with \code{flip.table=TRUE}.  By default, the tables are shown with zeros replaced by dashes.  This behavior can be changed with \code{zero.print}.
#'
#' Three statistical tests of symmetry for the age-agreement table can be computed with \code{what=} in \code{summary}.  The \dQuote{unpooled} or Bowker test as described in Hoenig et al. (1995) is constructed with \code{what="Bowker"}, the \dQuote{semi-pooled} or Evans-Hoenig test as described in Evans and Hoenig (1998) is constructed with \code{what="EvansHoenig"}, and the \dQuote{pooled} or McNemar test as described in Evans and Hoenig (1998) is constructed with \code{what="McNemar"}.  All three tests are run simultaneously with \code{what="symmetry"}.
#'
#' An age-bias plot, as defined by Campana et al. (1995), is constructed with \code{what="bias"} (the default) in \code{plot}. The reference variable from the \code{ageBias} call is plotted on the x-axis. Plotted confidence intervals are computed for the mean of the non-reference ages at each of the reference ages. The level of confidence is controlled by \code{sig.level=} given in the original \code{ageBias} call (i.e., confidence level is 100*(1-\code{sig.level})). Confidence intervals are only shown if the sample size is greater than the value in \code{min.n.CI=}. Confidence intervals plotted in red with an open dot (by default; these can be changed with \code{col.CIsig} and \code{pch.mean.sig}, respectively) do not contain the reference age (see discussion of t-tests below). Vertical lines that connect the minimum to the maximum observed values of the y-axis variable at each age of the x-axis variable are plotted in grey if \code{show.range=TRUE}. Individual points are plotted if \code{show.pts=TRUE}. The 1:1 (45 degree) agreement line is shown for comparative purposes. The sample sizes at each age of the x-axis variable are shown if \code{show.n=TRUE} (the default). The position of the sample sizes is controlled with \code{nYpos=}.
#'
#' A modified age-bias plot, as defined by Muir et al. (2008), is constructed as defined above but by also including \code{difference=TRUE} in \code{plot} so that the y-axis is the difference in the paired reference and non-reference ages from the \code{ageBias} call (specifically, nonreference-reference).
#'
#' The frequency of observations at each unique (x,y) coordinate are shown by using \code{what="numbers"} in \code{plot}.
#'
#' A \dQuote{sunflower plot}, which contains a symbol for each unique (x,y) coordinate with as many \dQuote{petals} as observations at that point, is constructed with \code{what="sunflower"} in \code{plot}.  A sunflower plot with differences between the two structures can be constructed by also including \code{difference=TRUE}.
#'
#' Individual t-tests to determine if the mean age of the non-reference set at a particular age of the reference set is equal to the reference age (e.g., is the mean age of the non-reference set at age-3 of the reference set statistically equal to 3?) are shown with \code{what="bias"} in \code{summary}.  The results provide a column that indicates whether the difference is significant or not as determined by adjusted p-values from the t-tests and using the significance level provided in \code{sig.level} (defaults to 0.05).  Similar results for the difference in ages (e.g., is the mean row variable age minus column variable age at column variable age-3 equal to 0?) are constructed with \code{what="diff.bias"} in \code{summary}.
#'
#' The sample size present in the age-agreement table is found with \code{what="n"}.
#'
#' @return \code{ageBias} returns a list with the following items:
#' \itemize{
#'   \item data A data.frame with the original paired age assignments and the difference between those assignments.
#'   \item agree The age-agreement table.
#'   \item bias A data.frame that contains the bias statistics.
#'   \item bias.diff A data.frame that contains the bias statistics for the differences.
#'   \item ref.lab A string that contains an optional label for the age assignments in the columns (reference) of the age-agreement table.
#'   \item nref.lab A string that contains an optional label for the age assignments in the rows (non-reference) of the age-agreement table.
#'}
#'
#' The \code{summary} returns the result if \code{what=} contains one item, otherwise it returns nothing.  Nothing is returned by \code{plot}, but see details for a description of the plot that is produced.
#' 
#' @section Testing: Tested all symmetry test results against results in Evans and Hoenig (2008), the McNemar and Evans-Hoenig results against results from \code{\link[fishmethods]{compare2}} in \pkg{fishmethods}, and all results using the \code{\link[FSAdata]{AlewifeLH}} data set from \pkg{FSAdata} against results from the online resource at http://www.nefsc.noaa.gov/fbp/age-prec/.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @seealso See \code{\link{agePrecision}} for measures of precision between pairs of age assignments.  See \code{\link[fishmethods]{compare2}} in \pkg{fishmethods} for similar functionality.
#'
#' @section IFAR Chapter: 4-Age Comparisons.
#'
#' @references Ogle, D.H.  2016.  \href{http://derekogle.com/IFAR}{Introductory Fisheries Analyses with R}.  Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Campana, S.E., M.C. Annand, and J.I. McMillan. 1995.  Graphical and statistical methods for determining the consistency of age determinations. Transactions of the American Fisheries Society 124:131-138.  [Was (is?) available from http://www.bio.gc.ca/otoliths/documents/Campana\%20et\%20al\%201995\%20TAFS.pdf.]
#'
#' Evans, G.T. and J.M. Hoenig.  1998.  Testing and viewing symmetry in contingency tables, with application to readers of fish ages.  Biometrics 54:620-629.
#'
#' Hoenig, J.M., M.J. Morgan, and C.A. Brown. 1995.  Analysing differences between two age determination methods by tests of symmetry.  Canadian Journal of Fisheries and Aquatic Sciences 52:364-368.
#' 
#' McBride, R.S.  2015. Diagnosis of paired age agreement: A simulation approach of accuracy and precision effects. ICES Journal of Marine Science 72:2149-2167.
#'
#' Muir, A.M., M.P. Ebener, J.X. He, and J.E. Johnson.  2008.  A comparison of the scale and otolith methods of age estimation for lake whitefish in Lake Huron.  North American Journal of Fisheries Management 28:625-635.  [Was (is?) available from http://www.tandfonline.com/doi/abs/10.1577/M06-160.1]
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
#' # flip the table -- ease of comparison to age-bias plot
#' summary(ab1,what="table",flip.table=TRUE)
#'
#' ## default plot
#' plot(ab1)
#' ## demonstrates squaring up the axes
#' plot(ab1,ylim=c(-1,23),xlim=c(-1,23))
#' ## plot with the data points shown
#' plot(ab1,show.pts=TRUE)
#' ## plot with the range shown
#' plot(ab1,show.range=TRUE)
#' ## plot with no difference in significance bar colors
#' plot(ab1,col.CIsig="black")
#' ## plot with no difference in points for significance ages
#' plot(ab1,pch.mean.sig=19)
#' plot(ab1,pch.mean=21)
#' ## plot with no difference in points or colors for significance ages
#' plot(ab1,pch.mean.sig=19,col.CIsig="black")
#' plot(ab1,pch.mean=21,col.CIsig="black")
#' ## Remove sample sizes
#' plot(ab1,show.n=FALSE)
#' ## Move sample sizes (and change text size)
#' plot(ab1,nYpos=0.02,cex.n=0.5)
#' ## Suppress confidence intervals for n < a certain value
#' ##   must set this in the original ageBias() call
#' ab2 <- ageBias(scaleC~otolithC,data=WhitefishLC,min.n.CI=5,
#'                ref.lab="Otolith Age",nref.lab="Scale Age")
#' plot(ab2)
#' 
#' ## plot of differences
#' ##  (note could use all of the same modifications as demonstrated above)
#' plot(ab1,difference=TRUE)
#' 
#' ## "Numbers" plot
#' plot(ab1,what="number",col.agree="gray50")
#' 
#' ## sunflower plot
#' plot(ab1,what="sunflower")
#' plot(ab1,what="sunflower",difference=TRUE)
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
  ## get variable names separately (r=ref, nr=nonref)
  nref.name <- tmp$Rname
  ref.name <- tmp$Enames
  ## rename dataframe of just ages (for simplicity)
  d <- tmp$mf
  ## sample size
  n <- nrow(d)
  ## add differences data
  d$diff <- d[,nref.name]-d[,ref.name]
  
  ## Summarizations of nrdata by rdata (more 'true' structure)
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
            ref.lab=ref.lab,nref.lab=nref.lab)
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
# table for symmetry tests.  Specifically, it removes the main
# diagonal, finds the upper and lower triangles, and returns
# them all in a list.  It is called by the iBowker, iMcNemar,
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
  for (i in 1:nrow(AAT$at)) {
    for (j in 1:ncol(AAT$at)) {
      diffs[i,j] <- as.numeric(rownames(AAT$at)[j])-as.numeric(colnames(AAT$at)[i])
    }
  }
  # Find max diff
  max.diff <- max(diffs)
  # Find parts of Evans Hoenig calcualtions -- finds individual off-diagonals
  #   and then computes the ratio that forms the chi-square parts
  rat <- numeric(nrow(AAT$at)-1)
  for (i in 1:max.diff) {
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
plot.ageBias <- function(x,what=c("bias","sunflower","numbers"),difference=FALSE,
                         xlab=x$ref.lab,ylab=x$nref.lab,show.n=TRUE,nYpos=1.03,cex.n=0.75,
                         lwd=1,show.pts=FALSE,pch.pts=20,
                         col.pts="black",transparency=1/10,
                         pch.mean=19,pch.mean.sig=21,cex.mean=lwd,
                         col.CI="black",col.CIsig="red",lwd.CI=lwd,sfrac=0,
                         show.range=FALSE,col.range="gray",lwd.range=lwd,
                         col.agree="gray80",lwd.agree=lwd,lty.agree=2,
                         cex.numbers=0.9,
                         xlim=NULL,ylim=NULL,
                         yaxt=graphics::par("yaxt"),
                         xaxt=graphics::par("xaxt"),...) { # nocov start
  what <- match.arg(what)
  col.pts <- col2rgbt(col.pts,transparency)
  switch(what,
         bias={ iAgeBiasPlot(x,difference,
                             xlab,ifelse(!difference,ylab,paste(ylab,"-",xlab)),
                             show.n,nYpos,cex.n,show.pts,pch.pts,col.pts,
                             pch.mean,pch.mean.sig,cex.mean,
                             col.CI,col.CIsig,lwd.CI,sfrac,
                             show.range,col.range,lwd.range,
                             col.agree,lwd.agree,lty.agree,
                             xlim,ylim,yaxt,...) },
         sunflower={ iAgeBiasSunflowerPlot(x,difference,
                             xlab,ifelse(!difference,ylab,paste(ylab,"-",xlab)),
                             xlim,ylim,lwd.agree,lty.agree,col.agree,yaxt=yaxt,...) },
         numbers={ iAgeBiasNumPlot(x,xlab,ylab,xlim,ylim,lwd.agree,lty.agree,col.agree,
                                   cex.numbers,yaxt,xaxt,...) }
  ) # end switch
} # nocov end


#=============================================================
# This internal function is used to produce the age-bias plot.
# This is called by ageBias().
#=============================================================
iAgeBiasPlot <- function(obj,difference,xlab,ylab,show.n,nYpos,cex.n,
                         show.pts,pch.pts,col.pts,
                         pch.mean,pch.mean.sig,cex.mean,col.CI,col.CIsig,lwd.CI,
                         sfrac,show.range,col.range,lwd.range,
                         col.agree,lwd.agree,lty.agree,xlim,ylim,yaxt,...) { # nocov start
  # identify whether difference data should be used or not, put in a tmp data frame
  if (!difference) d <- obj$bias
    else d <- obj$bias.diff
  # Control the axis limits (especially if none are given)
  axlmts <- iabAxisLmts(d,xlim,ylim,difference,show.range,show.pts)  
  # Plot more tick marks    
  graphics::par(lab=c(length(d[,1]),length(d$mean),7))    
  # Set base plot with Mean of 2nd vs. 1st age range
  graphics::plot(d$mean~d[,1],xlim=axlmts$xlim,ylim=axlmts$ylim,xlab=xlab,ylab=ylab,
       col="white",yaxt="n",...)
  # Helps keep y-axis as integers (needed for difference plot)
  if (yaxt!="n") {graphics::axis(2,seq(axlmts$ylim[1],axlmts$ylim[2],1))}
  # agreement line -- horizontal for difference and 45 degree for bias plot
  if (difference) graphics::abline(h=0,lwd=lwd.agree,lty=lty.agree,col=col.agree)
  else graphics::abline(a=0,b=1,lwd=lwd.agree,lty=lty.agree,col=col.agree)
  # add individual points if asked for
  if (show.pts) {
    if (difference) graphics::points(obj$d[,2],obj$d[,3],col=col.pts,pch=pch.pts)
    else graphics::points(obj$d[,2],obj$d[,1],col=col.pts,pch=pch.pts)
  }
  # add range of individual points if asked for
  if (show.range) {
    plotrix::plotCI(x=d[,1],y=d$mean,li=d$min,ui=d$max,add=TRUE,
                    slty=1,scol=col.range,pch=pch.mean,
                    lwd=lwd.range,gap=0,sfrac=0.005)
  }
  # add on CIs for mean
  #  for ages that are signficantly different
  if (any(d$sig)) {
    plotrix::plotCI(x=d[,1][d$sig],y=d$mean[d$sig],li=d$LCI[d$sig],ui=d$UCI[d$sig],
                    add=TRUE,slty=1,scol=col.CIsig,pch=pch.mean.sig,lwd=lwd.CI,gap=0,sfrac=sfrac)
    graphics::points(x=d[,1][d$sig],y=d$mean[d$sig],pch=pch.mean.sig,cex=cex.mean,col=col.CIsig)
  }
  #  for ages that are not significantly different
  if (any(!d$sig)) {
    plotrix::plotCI(x=d[,1][!d$sig],y=d$mean[!d$sig],li=d$LCI[!d$sig],ui=d$UCI[!d$sig],
                    add=TRUE,slty=1,scol=col.CI,pch=pch.mean,lwd=lwd.CI,gap=0,sfrac=sfrac)
    graphics::points(x=d[,1][!d$sig],y=d$mean[!d$sig],pch=pch.mean,cex=cex.mean)
  }
  # show the sample sizes at the top
  if (show.n) graphics::text(d[,1],graphics::grconvertY(nYpos,"npc"),d$n,cex=cex.n,xpd=TRUE)
} # nocov end

#=============================================================
# Internal function to produce the age-bias numbers plot.
#=============================================================
iAgeBiasNumPlot <- function(obj,xlab,ylab,xlim,ylim,
                            lwd.agree,lty.agree,col.agree,
                            cex.numbers,yaxt,xaxt,...) { # nocov start
  # convert age-agreement table into a data frame with all zeros removed
  # y,x in d[,1] and d[,2], respectively
  # lables in d[,3]
  d <- as.data.frame(obj$agree)
  d[,1] <- fact2num(d[,1])
  d[,2] <- fact2num(d[,2])
  d <- d[d[,3]>0,]
  # Control the axis limits (especially if none are given) ...
  # sent obj$bias so that axes would match the other plots
  axlmts <- iabAxisLmts(obj$bias,xlim,ylim,difference=FALSE,show.range=FALSE,show.pts=TRUE,show.CIs=FALSE)  
  # make an empty plot
  graphics::plot(d[,2],d[,1],type="n",xlab=xlab,ylab=ylab,xlim=axlmts$xlim,ylim=axlmts$ylim,yaxt="n",xaxt="n",...)
  # Helps keep axes as integers
  if (yaxt!="n") {graphics::axis(2,seq(axlmts$ylim[1],axlmts$ylim[2],1))}
  if (xaxt!="n") {graphics::axis(1,seq(axlmts$xlim[1],axlmts$xlim[2],1))}
  # add the one-to-one line
  graphics::lines(axlmts$xlim,axlmts$xlim,lwd=lwd.agree,lty=lty.agree,col=col.agree)
  # add the numbers at each point
  graphics::text(d[,2],d[,1],labels=d[,3],cex=cex.numbers)
} # nocov end

#=============================================================
# Internal function used to produce the age-bias sunflower plot.
#=============================================================
iAgeBiasSunflowerPlot <- function(obj,difference,xlab,ylab,xlim,ylim,
                                  lwd.agree,lty.agree,col.agree,yaxt,...) { # nocov start
  x <- obj$d[,2]
  ifelse(difference,y <- obj$d[,3],y <- obj$d[,1])
  if (is.null(ylim)) ylim <- range(y)
  if (is.null(xlim)) xlim <- range(x)
  graphics::sunflowerplot(x,y,seg.col="blue",size=1/10,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,yaxt="n",...)
  # Helps keep y-axis as integers
  if (yaxt!="n") {graphics::axis(2,seq(ylim[1],ylim[2],1))}
  # agreement line -- horizontal for difference and 45 degree for bias plot
  if (difference) graphics::abline(h=0,lwd=lwd.agree,lty=lty.agree,col=col.agree)
  else graphics::abline(a=0,b=1,lwd=lwd.agree,lty=lty.agree,col=col.agree)
} # nocov end


#=============================================================
# This internal function is used to find appropriate axis
# limits for the age-bias plot. 
#=============================================================
iabAxisLmts <- function(d,xlim,ylim,difference,show.range,show.pts,show.CIs=TRUE) { # nocov start
  # If no xlim given then make xlim the range of x values
  # which are given in the first position of d (note that
  # d is the age-bias statistics)
  if (!is.null(xlim)) xlmt <- xlim
  else xlmt <- range(d[,1],na.rm=TRUE)
  # If no ylim is given then make ylim.  Making ylim depends
  # on whether differences are used and whether ranges are
  # shown or not.
  if (!is.null(ylim)) ylmt <- ylim
  else {
    # build up a vector that will ultimately find the range
    # to form the axes.  Begin by filling with the means.
    tmp.min <- tmp.max <- d$mean
    # if show.cis then add in LCI and UCI.  CIs will be shown
    # for all real age-bias plots.  This is primarily for use
    # with the numbers plot
    if (show.CIs) {
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





#' @title Compute measures of precision among sets of ages.
#'
#' @description Computes overall measures of precision for multiple age assignments made on the same individuals.  Ages may be from two or more readers of the same structure, one reader at two or more times, or two or more structures (e.g., scales, spines, otoliths).  Measures of precision include ACV (Average Coefficient of Variation), APE (Average Percent Error), and various percentage difference values.
#'
#' @param formula A formula of the form \code{~var1+var2+var3+...} or, alternatively, \code{var1~var2+var3+...}, where the \code{varX} generically represent the variables that contain the age assignments.  The alternative formula allows for similar code as used in \code{\link{ageBias}} and can have only one variable on the left-hand side.
#' @param data A data.frame that minimally contains the variables in \code{formula}.
#' @param object An object of class \code{agePrec}, usually from \code{agePrecision}.
#' @param what A string (or vector of strings) that indicates what type of summary to print.  See details.
#' @param percent A logical that indicates whether the difference table (see details) should be represented as percentages (\code{TRUE}; default) or frequency (\code{FALSE}) of fish.
#' @param trunc.diff A single integer that identifies the age for which all values that age and greater are combined into one category.  See the examples.
#' @param digits A single numeric that indicates the minimum number of digits to print when using \code{summary}.
#' @param \dots Additional arguments for methods.
#'
#' @details If \code{what="precision"} in \code{summary} then a summary table that contains the following items will be printed:
#' \itemize{
#'   \item n Number of fish in \code{data}.
#'   \item validn Number of fish in \code{data} that have non-\code{NA} data for all R age estimates.
#'   \item R Number of age estimates given in \code{formula}.
#'   \item ACV The mean coefficient of variation.  See the \href{http://derekogle.com/IFAR}{IFAR chapter} for calculation details.
#'   \item APE The mean average percent error.  See the \href{http://derekogle.com/IFAR}{IFAR chapter} for calculation details.
#'   \item PercAgree The percentage of fish for which all age assignments perfectly agree.
#' }
#'
#' If \code{what="difference"} is used in \code{summary}, then a table that describes either the percentage (if \code{percent=TRUE}, default) or frequency of fish by the difference in paired age assignments.  This table has one row for each possible pair of age assignments.
#'
#' If \code{what="absolute difference"} is used in \code{summary}, then a table that describes either the percentage (if \code{percent=TRUE}, default) or frequency of fish by the absolute value of the difference in paired age assignments.  This table has one row for each possible pair of age assignments.  The \dQuote{1} column, for example, represents age assignments that disagree by one year (in either direction).
#'
#' If \code{what="detail"} is used in \code{summary}, then a data frame of the original \code{data} along with the intermediate calculations of the average age, standard deviation of age, APE, and ACV for each individual will be printed.  These details are generally only used to check or to understand calculations.
#' 
#' All percentage calculations above use the \code{validn} value in the denominator.
#' 
#' @return The main function returns a list with the following items:
#' \itemize{
#'   \item detail A data.frame with all data given in \code{data} and intermediate calculations for each fish.  See details
#'   \item rawdiff A frequency table of fish by differences for each pair of ages.
#'   \item absdiff A frequency table of fish by absolute differences for each pair of ages.
#'   \item APE The mean average percent error.
#'   \item ACV The mean coefficient of variation.
#'   \item n Number of fish in \code{data}.
#'   \item validn Number of fish in \code{data} that have non-\code{NA} data for all R age estimates.
#'   \item R Number of age estimates for each fish given in \code{formula}.
#' }
#'
#' The \code{summary} returns the result if \code{what=} contains one item, otherwise it returns nothing.  See details for what is printed.
#' 
#' @section Testing: Tested all precision results against published results in Herbst and Marsden (2011) for the \code{\link{WhitefishLC}} data and the results for the \code{\link[FSAdata]{AlewifeLH}} data set from \pkg{FSAdata} against results from the online resource at http://www.nefsc.noaa.gov/fbp/age-prec/.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @section IFAR Chapter: 4-Age Comparisons.
#'
#' @seealso See \code{\link{ageBias}} for computation of the full age agreement table, along with tests and plots of age bias.
#' 
#' @references Ogle, D.H.  2016.  \href{http://derekogle.com/IFAR}{Introductory Fisheries Analyses with R}.  Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Beamish, R.J. and D.A. Fournier.  1981.  A method for comparing the precision of a set of age determinations.  Canadian Journal of Fisheries and Aquatic Sciences 38:982-983.  [Was (is?) available from http://www.pac.dfo-mpo.gc.ca/science/people-gens/beamish/PDF_files/compareagecjfas1981.pdf.]
#'
#'Campana, S.E.  1982.  Accuracy, precision and quality control in age determination, including a review of the use and abuse of age validation methods.  Journal of Fish Biology 59:197-242.  [Was (is?) available from http://www.denix.osd.mil/nr/crid/Coral_Reef_Iniative_Database/References_for_Reef_Assessment_files/Campana,\%202001.pdf.]
#'
#'Campana, S.E., M.C. Annand, and J.I. McMillan. 1995.  Graphical and statistical methods for determining the consistency of age determinations. Transactions of the American Fisheries Society 124:131-138.  [Was (is?) available from http://www.bio.gc.ca/otoliths/documents/Campana\%20et\%20al\%201995\%20TAFS.pdf.]
#'
#'Chang, W.Y.B. 1982.  A statistical method for evaluating the reproducibility of age determination.  Canadian Journal of Fisheries and Aquatic Sciences 39:1208-1210.  [Was (is?) available from http://www.nrcresearchpress.com/doi/abs/10.1139/f82-158.]
#' 
#' McBride, R.S.  2015. Diagnosis of paired age agreement: A simulation approach of accuracy and precision effects. ICES Journal of Marine Science, 72:2149-2167.
#'
#' @aliases agePrecision plot.agePrec summary.agePrec
#'
#' @keywords htest manip
#' 
#' @examples
#' ## Example with just two age assignments
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
#' ## Example with three age assignments
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
  for (i in 1:nrow(prs)) {
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
  for (i in 1:nrow(prs)) {
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
    msg <- "of fish by absolute differences in ages\n between pairs of assignments\n"
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
    msg <- "of fish by differences in ages\n between pairs of assignments\n"
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
