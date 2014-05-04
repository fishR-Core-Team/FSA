#' Compute and view possible biases between paired sets of ages.
#'
#' Constructs age-agreement tables, statistical tests to detect bias, and plots to visualize potential bias in paired age assignments.  The age assignments may be from two readers of the same structure, one reader at two times, or two stuctures (e.g., scales, spines, otoliths).
#'
#' The main function, \code{ageBias}, requires a formula of the form \code{col~row}, where \code{col} and \code{row} generically represent the variables that contain the age assignments that will form the columns and rows, respectively, of the age-agreement table.  If one age assignment is thought to be more accurate than the other, then it should form the columns and, thus, should be on the left-hand-side of the formula.  The variable that forms the columns in the age-agreement table will be the \dQuote{constant} age used in the t-tests and age-bias plots (i.e,. the x-axis).  See further details below.
#'
#' The age-agreement table is constructed with  \code{what="table"} in \code{summary}.  The agreement table can be \dQuote{flipped}, i.e., the rows are in descending rather than ascending order, with \code{flip.table=TRUE}.  By default the tables are shown with zeroes replaced by dashes.  This behavior can be changed with \code{zero.print}.
#'
#' Three statistical tests of the symmetry for the age-agreement table can be computed with \code{what} in \code{summary}.  The \dQuote{unpooled} or Bowker's test as described in Hoenig et al. (1995) is constructed with \code{what="Bowkers"}, the \dQuote{semi-pooled} or Evans-Hoengit test as described in Evans and Hoenig (1998) is constructed with \code{what="EvansHoenig"}, and the \dQuote{pooled} or McNemar's test as described in Evans and Hoenig (1998) is constructed with \code{what="McNemars"}.  All three tests are run simultaneously with \code{what="symmetry"}.
#'
#' An age-bias plot, as defined by Campana et al. (1995), is constructed with \code{what="bias"} (the default) in \code{plot}.  The variable that formed the columns in the \code{ageBias} call is plotted on the x-axis.  Confidence intervals plotted in red are computed for the mean of the y-axis variable at each age of the x-axis variable.  The level of confidence is controlled by \code{sig.level} given in the original \code{ageBias} call (i.e., confident level is 100*(1-\code{sig.level}).  Confidence intervals are only shown if the sample size is greater than the value in \code{min.n.CI}.  Vertical lines that connect the minimum to the maximum observed value of the y-axis variable at each age of the x-axis variable are plotted in grey if \code{show.range=TRUE}.  The 1:1 (45 degree) agreement line is shown for comparative purposes.  The sample sizes at each age of the x-axis variable are shown if \code{show.n=TRUE} (the default).  The position of the sample sizes is controlled with \code{nYpos}.
#'
#' An age-bias plot, as defined by Muir et al. (2008), is constructed as defined above but by also including \code{difference=TRUE} so that the y-axis is the difference in the paired ages in the rows and columns from the \code{ageBias} call (specifically, columns-rows).
#'
#' The frequency of observations at each unique (x,y) coordinate are shown is constructed by using \code{what="numbers"} in \code{plot}.
#'
#' A \dQuote{sunflower plot} which contains a symbol for each unique (x,y) coordinate with as many \dQuote{petals} as observations at that point is constructed with \code{what="sunflower"} in \code{plot}.  A sunflower plot with differences between the two structures can be constructed by also including \code{difference=TRUE}.
#'
#' Individual t-tests to determine if the mean age of the row variable at a particular age of the column variable is equal to the column variable age (e.g., is the mean age of the row variable at age-3 of the column variable statistically equal to 3?) are constructed with \code{what="bias"} in \code{summary}.  The results provide a column that indicates whether the difference is significant or not as determined by adjusted p-value from the t-test and using the signficance level provided in \code{sig.level} (defaults to 0.05).  Similar results for the difference in ages (e.g., is the mean row variable age minus column variable age at column variable age-3 equal to 0?) are constructed with \code{what="diff.bias"} in \code{summary}.
#'
#' The sample size present in the age-agreement table is found with \code{what="n"}.
#'
#' @aliases ageBias plot.ageBias summary.ageBias
#'
#' @param formula A formula of the form \code{col~row}, where \code{col} and \code{row} generically represent the variables that contain the ages that will form the columns and rows, respectively, of the age-agreement table.  See details.
#' @param data A data.frame that minimally contains the paired age assignments given \code{formula}.
#' @param col.lab A string that contains a label for the column age assignments.
#' @param row.lab A string that contains a label for the row age assignments.
#' @param method A string that indicates which method to use when adjusting p-values for multiple comparisons.  See \code{?p.adjust.methods}.
#' @param sig.level A value used to determine whether a p-value indicates a significant result.  The confidence level used in \code{plot} is 100*(1-\code{sig.level}).
#' @param min.n.CI A value (default is 5) that indicates the smallest sample size for which a confidence interval should be computed.
#' @param object An object saved from the \code{ageBias} call (i.e., of class \code{ageBias}).
#' @param x An object saved from the \code{ageBias} call (i.e., of class \code{ageBias}).
#' @param what A string that indicates what type of summary to print or plot to construct.  See details.
#' @param difference A logical that indicates whether or not the difference between the two age assignments should be used.  See details.
#' @param zero.print A string that indicates what should be printed in place of the zeroes on an age-agreement table.  The default is to print a single dash.
#' @param digits A value that indicates the minimum number of digits to print when showing \code{what="bias"} or \code{what="diff.bias"} in \code{summary}.
#' @param flip.table A logical that indicates whether the age-agreement table should be \sQuote{flipped} (i.e., rows are reversed so that the younger ages are at the bottom of the table).  This makes the table more directly comparable to the age-bias plot.
#' @param cont.corr A string that indicates the continuity correction method to be used with (only) McNemars test.  If \code{"none"} (default) then no continuity correction is used, if \code{"Yates"} then 0.5 is used, and if \code{"Edwards"} then 1 is used.
#' @param xlab A string that contains a label for the x-axis age assignments.
#' @param ylab A string that contains a label for the y-axis age assignments.
#' @param xlim A numeric vector of the limits of the x-axis.
#' @param ylim A numeric vector of the limits of the y-axis.
#' @param yaxt A string which specifies the x-axis type. Specifying \dQuote{n} suppresses plotting of the axis.  See \code{?par}. 
#' @param show.n A logical that indicates whether the sample sizes for each level of the x-axis variable is shown (\code{=TRUE}, default) or not (\code{=FALSE}).
#' @param nYpos A numeric that indicates the relative Y position of the sample size values when \code{show.n=TRUE}.  For example, if \code{nYpos=1.1} then the sample size values will be 10 percent above the top end of the y-axis.
#' @param show.pts A logical that indicates whether to show the raw data points on an age-bias plot.
#' @param pch.pts A value that indicates the plotting character to be used when plotting the raw data points on an age bias plot.
#' @param col.pts A string or value that indicates the color to be used for plotting the raw data points.  The default is to use black with a transparency found in \code{transparency} on an age bias plot.
#' @param transparency A value (between 0 and 1) that indicates the level of transparency to use for plotting the raw data points on an age bias plot.  If expressed as a fraction of 1/x then x points plotted on top of each other will represent the color in \code{col.pts}.
#' @param show.rng A logical that indicates whether to show vertical bars that represent the range of the data points on an age bias plot.
#' @param col.rng A string or value that indicates the color to be used for the interval representing the range of the data on an age bias plot.
#' @param lwd.rng A value that indicates the line width for the interval representing the range of the data on an age bias plot.
#' @param pch.mean A value that indicates the plotting character to be used for mean values (i.e., center of confidence interval bars) on an age bias plot.
#' @param col.err A string or value that indicates the color to be used for confidence interval bars that are considered non-significant on an age bias plot.
#' @param col.err.sig A string or value that indicates the color to be used for confidence interval bars that are considered significant on an age bias plot.
#' @param lwd.err A value that indicates the line width for the confidence interval bars on an age bias plot.
#' @param col.ref A value or string that indicates the color for the 1:1 or zero (if difference) reference line on an age bias plot.
#' @param lwd.ref A value that indicates the line width for the 1:1 or zero (if difference) reference line on an age bias plot.
#' @param lty.ref A value that indicates the line type for the 1:1 or zero (if difference) reference line on an age bias plot.
#' @param \dots Additional arguments for methods.
#'
#' @return \code{ageBias} returns a list with the following items:
#' \itemize{
#'   \item data A data frame with the original age assignments and the difference between those two age assignements.
#'   \item agree The age-agreement table.
#'   \item bias A data.frame that contains the bias statistics.
#'   \item bias.diff A data.frame that contains the bias statistics for the differences.
#'   \item col.lab A string that contains an optional label for the column structure or readings.
#'   \item row.lab A string that contains an optional label for the row structure or readings.
#'}
#'
#' A data frame that contains the symmetry test results if \code{summary} and \code{what="symmetry"}, \code{what="Bowkers"}, \code{what="McNemars"}, or \code{what="EvansHoenig"}; otherwise, nothing is returned by \code{summary}  Nothing is returned by \code{plot}, but see details for a description of the plot that is produced.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso See \code{\link{agePrecision}} for measures of precision between pairs of age assignments.  See \code{compare2} in \pkg{fishmethods} for similar functionality.
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/AgeComparisons.pdf}
#'
#' @references Campana, S.E., M.C. Annand, and J.I. McMillan. 1995.  Graphical and statistical methods for determining the consistency of age determinations.  Transactions of the American Fisheries Society, 124:131-138. \url{http://www.bio.gc.ca/otoliths/documents/Campana\%20et\%20al\%201995\%20TAFS.pdf}
#'
#' Evans, G.T. and J.M. Hoenig.  1998.  Testing and viewing symmetry in contingency tables, with apprlication to readers of fish ages.  Biometrics 54:620-629. \url{http://www.fisheries.vims.edu/hoenig/pdfs/Viewing.pdf}.
#'
#' Hoenig, J.M., M.J. Morgan, and C.A. Brown. 1995.  Analysing differences between two age determination methods by tests of symmetry. Canadian Journal of Fisheries And Aquatic Systems, 52:364-368.  \url{http://www.fisheries.vims.edu/hoenig/pdfs/Hoenig_Morgan_Brown_AgeDeterminationSymmetry.pdf}
#'
#' Muir, A.M., M.P. Ebener, J.X. He, and J.E. Johnson.  2008.  A comparison of the scale and otolith methods of age estimation for lake whitefish in Lake Huron.  North American Journal of Fisheries Management, 28:625-635. \url{http://www.tandfonline.com/doi/abs/10.1577/M06-160.1}
#'
#' @keywords htest manip
#'
#' @examples
#'data(WhitefishLC)
#'ab1 <- ageBias(otolithC~scaleC,data=WhitefishLC,col.lab="Otolith Age",row.lab="Scale Age")
#'summary(ab1)
#'summary(ab1,what="symmetry")
#'summary(ab1,what="Bowkers")
#'summary(ab1,what="EvansHoenig")
#'summary(ab1,what="McNemars")
#'summary(ab1,what="McNemars",cont.corr="Yates")
#'summary(ab1,what="bias")
#'summary(ab1,what="diff.bias")
#'summary(ab1,what="n")
#'summary(ab1,what=c("n","symmetry","table"))
#'# show the zeroes (rather than dashes)
#'summary(ab1,what="table",zero.print="0")
#'# flip the table -- ease of comparison to age-bias plot
#'summary(ab1,what="table",flip.table=TRUE)
#'
#'## default plot
#'plot(ab1)
#'## demonstrates controlling the y-axis limits
#'plot(ab1,ylim=c(0,10))
#'## plot with the data points shown
#'plot(ab1,show.pts=TRUE,transparency=1/3)
#'## plot with the range shown
#'plot(ab1,show.rng=TRUE)
#'## plot with no difference in significance bar colors
#'plot(ab1,col.err="black",col.err.sig="black")
#'## plot of differences (note could use same modifications as shown above)
#'plot(ab1,difference=TRUE)
#'## sunflower plot
#'plot(ab1,what="sunflower")
#'plot(ab1,what="sunflower",difference=TRUE)
#'## "Numbers" plot
#'plot(ab1,what="number",col.ref="gray50")
#'
#'## Unit tests for what="symmetry"
#'######## Create Evans & Hoenig (2008) X matrix as a check (table 1)
#'X.dat <- data.frame(ageR=c(2,2,2,2,2,2,2,2),
#'                    ageC=c(1,1,1,1,3,3,3,3))
#'X <- ageBias(ageC~ageR,data=X.dat)
#'summary(X,what="table",zero.print=0)
#'summary(X,what="symmetry")   # check
#'
#'######## Create Evans & Hoenig (2008) Y matrix as a check (table 1)
#'Y.dat <- data.frame(ageR=c(1,1,1,2,2,2),
#'                    ageC=c(2,2,3,3,3,3))
#'Y <- ageBias(ageC~ageR,data=Y.dat)
#'summary(Y,what="table",zero.print=0)
#'summary(Y,what="symmetry")   # check
#'
#'######## Create Evans & Hoenig (2008) Z matrix (McBride's matrix) as a check
#'Z.dat <- data.frame(ageR=c(1,1,1,2,2,2,3),
#'                    ageC=c(2,2,2,3,3,3,1))
#'Z <- ageBias(ageC~ageR,data=Z.dat)
#'summary(Z,what="table",zero.print=0)
#'summary(Z,what="symmetry")  #check
#'
#'@rdname ageBias
#'@export
ageBias <- function(formula,data,col.lab=tmp$Rname,row.lab=tmp$Enames[1],
                    method=p.adjust.methods,sig.level=0.05,min.n.CI=5) {
  ## internal function to create summarized data frame
  abdf <- function(tmp,cname,diff=FALSE,min.n.CI) {
    # Ages of cdata strux
    x <- fact2num(tmp[,1])
    # Is it appropriate to compute a CI
    canCI <- tmp$n>=min.n.CI & tmp$sd>0
    # Fill SEs, p-values calcs, and CIs with NA (will leave NA for those where CI was inappropriate)
    tmp$SE <- tmp$t <- tmp$p.value <- tmp$LCI <- tmp$UCI <- NA
    # SE of 2nd strux
    tmp$SE[canCI] <- tmp$sd[canCI]/sqrt(tmp$n[canCI])
    # t test statistic that mean of 2nd strux equals value of 1st strux
    if (!diff) tmp$t[canCI] <- (tmp$mean[canCI]-x[canCI])/tmp$SE[canCI]
    else tmp$t[canCI] <- tmp$mean[canCI]/tmp$SE[canCI]
    # two-tailed p-value (adjusted for multiple comparisons)
    tmp$p.value <- pt(abs(tmp$t),df=tmp$n-1,lower.tail=FALSE)*2
    tmp$adj.p <- round(p.adjust(tmp$p.value,method=p.adjust.methods),5)
    # Assign significant difference (fill with FALSE then change to TRUE if sig)
    tmp$sig <- rep(FALSE,nrow(tmp))
    tmp$sig[tmp$adj.p<sig.level] <- TRUE
    # CIs
    tmp$LCI[canCI] <- tmp$mean[canCI]+qt(sig.level/2,tmp$n[canCI]-1)*tmp$SE[canCI]
    tmp$UCI[canCI] <- tmp$mean[canCI]+qt(sig.level/2,tmp$n[canCI]-1,lower.tail=FALSE)*tmp$SE[canCI]
    # put together as a dataframe
    tmpdf <- data.frame(age=x,n=tmp$n,min=tmp$min,max=tmp$max,mean=tmp$mean,SE=tmp$SE,
                        t=tmp$t,adj.p=tmp$adj.p,sig=tmp$sig,
                        LCI=tmp$LCI,UCI=tmp$UCI,canCI=canCI)
    # label first column with col.data name
    names(tmpdf)[1] <- cname  
    tmpdf
  } ## end abdf internal function
  
  ## Main Function
  tmp <- hndlFormula(formula,data,expNumR=1,expNumE=1)
  if (!tmp$metExpNumR) stop("'ageBias' must have only one LHS variable.",call.=FALSE)
  if (!tmp$Rclass %in% c("numeric","integer")) stop("LHS variable must be numeric",call.=FALSE)
  if (!tmp$metExpNumE) stop("'ageBias' must have only one RHS variable.",call.=FALSE)
  if (!tmp$Eclass %in% c("numeric","integer")) stop("RHS variable must be numeric",call.=FALSE)
  # sample size
  n <- nrow(tmp$mf)
  # get variable names, separately and together
  cname <- tmp$Rname
  rname <- tmp$Enames
  bname <- c(cname,rname)
  # rename dataframe of just ages (for simplicity)
  d <- tmp$mf
  # add differences data
  d$diff <- d[,rname]-d[,cname]
  ## Summarizations of rdata by cdata (more 'true' structure)
  # turn off warnings for not using factor data (turn back on below)
  options(warn=-1)
  # Summary stats of cdata by ages of rdata
  bias.df <- abdf(Summarize(as.formula(paste(rname,"~",cname)),data=d),cname,min.n.CI=min.n.CI)
  # Summary stats of diff by cdata
  bias2.df <- abdf(Summarize(as.formula(paste("diff~",cname)),data=d),cname,diff=TRUE,min.n.CI=min.n.CI)
  options(warn=0)
  
  ## Agreement contingency table adjusted to be square  
  # finds overall range of ages
  ages <- min(d[,c(cname,rname)]):max(d[,c(cname,rname)])
  # converts row and column data to factor with ages levels
  rf <- factor(d[,rname],levels=ages)
  cf <- factor(d[,cname],levels=ages)
  # Agreement contingency table
  agree.table <- table(rf,cf,dnn=c(row.lab,col.lab))
  
  ## Put together an output list
  d <- list(data=d,agree=agree.table,bias=bias.df,bias.diff=bias2.df,
            col.lab=col.lab,row.lab=row.lab)
  class(d) <- "ageBias"
  d
}


#'@rdname ageBias
#'@method plot ageBias
#'@S3method plot ageBias
plot.ageBias <- function(x,what=c("bias","sunflower","numbers"),difference=FALSE,
                         xlab=x$col.lab,ylab=x$row.lab,show.n=TRUE,nYpos=1.1,
                         show.pts=FALSE,pch.pts=19,col.pts=rgb(0,0,0,transparency),transparency=1/10,
                         pch.mean=3,col.err="blue",col.err.sig="red",lwd.err=2,
                         show.rng=FALSE,col.rng="gray",lwd.rng=2,
                         col.ref="black",lwd.ref=1,lty.ref=2,
                         xlim=NULL,ylim=NULL,yaxt=par("yaxt"),...) {

  ## Internal function for finding appropriate axis limits for the age-bias plot
  abAxisLmts <- function(d,xlim,ylim,show.n,difference) {
    if (!is.null(xlim)) xlmt <- xlim
      else xlmt <- range(d[,1],na.rm=TRUE)
    if (!is.null(ylim)) ylmt <- ylim
      else {
        if (!difference) ylmt <- c(floor(min(c(d$min,d$LCI,xlmt),na.rm=TRUE)),
                                   ceiling(max(c(d$max,d$UCI,xlmt),na.rm=TRUE)))
        else ylmt <- c(floor(min(c(d$min,d$LCI),na.rm=TRUE)),
                       ceiling(max(c(d$max,d$UCI),na.rm=TRUE)))
      }
    # return values
    list(xlim=xlmt,ylim=ylmt)
  }
  
  ## Internal function for producing the age-bias plot
  biasplot <- function(obj,difference,xlab,ylab,show.n,nYpos=nYpos,show.pts,show.rng,
                       col.err,col.err.sig,col.pts,col.rng,xlim,ylim,yaxt,...) {
    # identify whether difference data should be used or not, put in a tmp data frame
    if (!difference) d <- obj$bias
      else d <- obj$bias.diff
    # Control the axis limits (especially if none are given)
    axlmts <- abAxisLmts(d,xlim,ylim,show.n,difference)  
    # Plot more tick marks    
    par(lab=c(length(d[,1]),length(d$mean),7))    
    # Mean of 2nd vs. 1st age range
    plot(d$mean~d[,1],xlim=axlmts$xlim,ylim=axlmts$ylim,xlab=xlab,ylab=ylab,
         pch=pch.mean,yaxt="n",...)
    # Helps keep y-axis as integers (needed for difference plot)
    if (yaxt!="n") {axis(2,seq(axlmts$ylim[1],axlmts$ylim[2],1))}
    # agreement line -- horizontal for difference and 45 degree for bias plot
    if (difference) abline(h=0,lwd=lwd.ref,lty=lty.ref,col=col.ref)
      else abline(a=0,b=1,lwd=lwd.ref,lty=lty.ref,col=col.ref)
    # add individual points if asked for
    if (show.pts) {
      if (difference) points(obj$d[,1],obj$d[,3],col=col.pts,pch=pch.pts)
        else points(obj$d[,1],obj$d[,2],col=col.pts,pch=pch.pts)
    }
    # add range of individual points if asked for
    if (show.rng) {
      plotrix::plotCI(x=d[,1],y=d$mean,li=d$min,ui=d$max,add=TRUE,slty=1,scol=col.rng,pch=pch.mean,lwd=lwd.rng,gap=0,sfrac=0.005)
    }
    # add on CIs for mean
    #  for ages that are signficantly different
    plotrix::plotCI(x=d[,1][d$sig],y=d$mean[d$sig],li=d$LCI[d$sig],ui=d$UCI[d$sig],add=TRUE,slty=1,scol=col.err.sig,pch=pch.mean,lwd=lwd.err,gap=0)
    #  for ages that are not significantly different
    plotrix::plotCI(x=d[,1][!d$sig],y=d$mean[!d$sig],li=d$LCI[!d$sig],ui=d$UCI[!d$sig],add=TRUE,slty=1,scol=col.err,pch=pch.mean,lwd=lwd.err,gap=0)
    # show the sample sizes at the top
    if (show.n) text(d[,1],grconvertY(nYpos,"npc"),d$n,cex=0.75,xpd=TRUE)
  } ## end internal age-bias plot function

  ## Internal function for producing the sunflower plot
  asunflowerplot <- function(obj,difference,xlab,ylab,xlim,ylim,lwd.ref,lty.ref,col.ref,...) {
    x <- obj$d[,1]
    if (difference) {
      y <- obj$d[,3]
      if (is.null(ylim)) ylim <- range(y)
      if (is.null(xlim)) xlim <- range(x)
    } else {
      y <- obj$d[,2]
      if (is.null(ylim)) ylim <- range(x,y)
      if (is.null(xlim)) xlim <- range(x,y)
    }
    sunflowerplot(x,y,seg.col="blue",size=1/10,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,...)
    # agreement line -- horizontal for difference and 45 degree for bias plot
    if (difference) abline(h=0,lwd=lwd.ref,lty=lty.ref,col=col.ref)
      else abline(a=0,b=1,lwd=lwd.ref,lty=lty.ref,col=col.ref)
  }  ## end internal sunflowerplot function
  
  ## Internal function for producing numbers plot
  anumplot <- function(obj,xlab,ylab,xlim,ylim,lwd.ref,lty.ref,col.ref,...) {
    # convert age-agreement table into a data frame with all zeroes removed
    d <- as.data.frame(obj$agree)
    d[,1] <- fact2num(d[,1])
    d[,2] <- fact2num(d[,2])
    d <- d[d[,3]>0,]
    # isolate the x-, y- coordinates and number of values at each x,y (in lbls)
    y <- d[,1]
    x <- d[,2]
    lbls <-d[,3]
    # check for axis limits
    if (is.null(ylim)) ylim <- range(x,y)
    if (is.null(xlim)) xlim <- range(x,y)
    # make an empty plot
    plot(x,y,type="n",xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,...)
    # add the one-to-one line
    lines(xlim,xlim,lwd=lwd.ref,lty=lty.ref,col=col.ref)
    # add the numbers at each point
    text(x,y,labels=lbls)
  }  ## end internal anumplot function
  

  ## Main function
  what <- match.arg(what)
  if (what=="bias") biasplot(x,difference,xlab,ifelse(!difference,ylab,paste(ylab,"-",xlab)),
                             show.n,nYpos=nYpos,show.pts,show.rng,col.err,col.err.sig,
                             col.pts,col.rng,xlim,ylim,yaxt,...)
  else if (what=="sunflower") asunflowerplot(x,difference,xlab,ifelse(!difference,ylab,paste(ylab,"-",xlab)),xlim,ylim,lwd.ref,lty.ref,col.ref,...)
  else anumplot(x,xlab,ylab,xlim,ylim,lwd.ref,lty.ref,col.ref,...)
}

#'@rdname ageBias
#'@method summary ageBias
#'@S3method summary ageBias
summary.ageBias <- function(object,what=c("table","symmetry","Bowkers","EvansHoenig","McNemars","bias","diff.bias","n"),
                            flip.table=FALSE,zero.print="-",digits=3,cont.corr=c("none","Yates","Edwards"),
                            ...) {
  ## Internal function to handle the age-agreement table for symmetry tests
  ##   Removes the main diagonal, finds the upper and lower triangles, and
  ##   returns them all in a list
  hndlAgreeTable <- function(obj) {
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
  
  ## Internal function to compute Bowker's Test
  Bowkers <- function(obj) {
    AAT <- hndlAgreeTable(obj)
    # Chi-sq parts (Evans and Hoenig's eq 1, Hoenig et al.'s eq 3)
    rat <- ((AAT$lo-t(AAT$up))^2)/(AAT$lo+t(AAT$up))
    # Add chi-square parts
    chi.sq <- sum(rat,na.rm=TRUE)
    # Count number of chi-sq parts
    df <- sum(as.numeric(!is.na(rat)))
    # Find the p-value 
    p <- pchisq(chi.sq,df,lower.tail=FALSE)
    # Return dataframe
    data.frame(symTest="Bowkers",df=df,chi.sq=chi.sq,p=p)
  } ## End internal Bowker's Test function
  
  ## Internal function to compute Bowker's Test
  McNemars <- function(obj,cont.cor) {
    # handle continuity correction
    if (cont.cor=="none") {
      title <- "McNemars"
      cc <- 0
    } else if (cont.cor=="Yates") {
      title <- "McNemars (Yates Correction)"
      cc <- 0.5
    } else if (cont.cor== "Edwards") {
      title <- "McNemars (Edwards Correction)"
      cc <- 1
    } else stop("Continuity correction is incorrect",call.=FALSE)
    AAT <- hndlAgreeTable(obj)
    # Chi-sq parts (Evans and Hoenig's eq 2, but include the correction factor)
    top <- (abs(sum((AAT$lo-t(AAT$up)),na.rm=TRUE))-cc)^2
    bot <- sum(AAT$lo+t(AAT$up),na.rm=TRUE)
    chi.sq <- top/bot
    df <- 1
    # Find the p-value 
    p <- pchisq(chi.sq,df,lower.tail=FALSE)
    # Return list
    data.frame(symTest=title,df=df,chi.sq=chi.sq,p=p)
  } ## End internal McNemar's Test function
  
  ## Internal function to compute Evans Hoenig's Test
  EvansHoenig <- function(obj) {
    AAT <- hndlAgreeTable(obj)
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
    p <- pchisq(chi.sq,df,lower.tail=FALSE)
    # Return data.frame
    data.frame(symTest="EvansHoenig",df=df,chi.sq=chi.sq,p=p)
  } ## End internal Evans Hoenig's Test function
  
  ## Main function
  what <- match.arg(what,several.ok=TRUE)
  if ("n" %in% what) {
    cat("Sample size in the age-agreement table is ",sum(object$agree),".\n",sep="")
    what <- hndlMultWhat(what,"n")
  }
  if ("bias" %in% what) {
    cat("Summary of",object$row.lab,"by",object$col.lab,"\n")
    print(object$bias[-ncol(object$bias)],row.names=FALSE,digits=digits)
    what <- hndlMultWhat(what,"bias")
  }
  if ("diff.bias" %in% what) {
    cat("Summary of",object$row.lab,"-",object$col.lab,"by",object$col.lab,"\n")
    print(object$bias.diff[-ncol(object$bias.diff)],row.names=FALSE,digits=digits)
    what <- hndlMultWhat(what,"diff.bias")
  }
  if ("table" %in% what) {
    # show the age-agreement table
    if (!flip.table) {
      cat("Raw agreement table (square)\n")
      print(object$agree,zero.print=zero.print)
    } else {
      cat("Raw agreement table (square & flipped)\n")
      tmp <- object$agree[nrow(object$agree):1,] # flip the rows
      class(tmp) <- "table"                      # for printing purposes
      print(tmp,zero.print=zero.print)
    }
    what <- hndlMultWhat(what,"table")
  }
  if (any(c("symmetry","Bowkers","EvansHoenig","McNemars") %in% what)) {
    symTest <- NULL # to avoide "global bindings" warning in rcmd check
    tmp <- McNemars(object,match.arg(cont.corr))
    tmp <- rbind(tmp,EvansHoenig(object))
    tmp <- rbind(tmp,Bowkers(object))
    # if what="symmetry" print all results, otherwise only print what is asked for
    cat("Agreement Table Symmetry Test Results\n")
    if ("symmetry" %in% what) tmp
      else Subset(tmp,grepl(what,symTest))
  }
}
