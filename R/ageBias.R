#'Viewing and computing bias between paired sets of ages.
#'
#'Computes overall measures of bias and creates plots for visualizing bias in
#'paired age assignments.  The paired age assignments can consist of age measurements
#'recorded for two structures (e.g., otoliths and scales), for two readers of the
#'same structure, or for one reader at two times.
#'
#'The main function, \code{ageBias}, requires a formula of the form \code{col~row},
#'where \code{col} and \code{row} generically represent the variables that
#'contain the ages that will form the columns and rows, respectively, of the age-
#'agreement table.  If one age assignment is thought to be more accurate than the
#'other, then it should form the columns and should be on the left-hand-side of
#'the formula.  The variable that forms the columns in the age-agreement table will
#'be the \dQuote{constant} age used in the t-tests (see \code{summary}) and age-bias
#'plots (i.e,. the x-axis in \code{plot}).
#'
#'The results can be shown as an age-bias plot, as defined by Campana et al. (1995),
#'by using \code{what="bias"} (the default) in \code{plot}.  The variable that formed
#'the columns in the original \code{ageBias} call is plotted on the x-axis.  Confidence
#'intervals plotted in red are computed for the mean of the y-axis data at each
#'\dQuote{level} (i.e., age) of the x-axis data.  The level of confidence interval
#'is controlled by \code{sig.level} given in the original \code{ageBias} call.
#'Vertical lines that connect the minimum to the maximum value of the y-axis data
#'at each \dQuote{level} of the x-axis data are plotted in grey if \code{show.range=TRUE}.
#'A 1:1 (45 degree) agreement line is shown for comparative purposes.  The sample
#'sizes at each \dQuote{level} of the x-axis data is shown if \code{show.n=TRUE}
#'(the default).  The age-bias plot can be modified, following Muir et al. (2008),
#'with \code{difference=TRUE} so that the y-axis data is the difference in the
#'paired ages in the rows and columns from the original \code{ageBias} call
#'(specifically, columns-rows).
#'
#'The data can also be viewed as a sunflower plot by using \code{what="sunflower"}
#'in \code{plot}.  A sunflower plot contains a point for each unique (x,y) coordinate
#'with a \dQuote{petal} added to each point for each observation that has the same
#'(x,y) coordinate.  Thus, points with more petals have more observations at that
#'point.  The differences between the two structures can be shown by including
#'\code{difference=TRUE}.
#'
#'The data can also be viewed as a \dQuote{numbers} plot by using \code{what="numbers"}
#'in \code{plot}.  A \dQuote{numbers} plot shows the number of observations that
#'occur at each unique (x,y) coordinate.
#'
#'The age-agreement table can be seen with  \code{what="table"} in \code{summary}.
#'The agreement table can be flipped by including \code{flip.table=TRUE}.  The
#'results from one of two tests of symmetry (along with the age-agreement table)
#'are given with either \code{what="Bowkers"} for Bowker's test (described in
#'Hoenig et al. (1995)) or \code{what="McNemars"} for McNemar's test (described in
#'Evans and Hoenig (1998)) in \code{summary}.  The null hypothesis for these tests
#'is that the agreement table is symmetric.
#'
#'Individual t-tests to determine if the mean row-variables ages at a particular
#'age of the column-variable (e.g., is the mean row-variable age at column-variable
#'age-3 statistically equal to 3?) are constructed with \code{what="bias"} in 
#'\code{summary}.  The results provide a column indicating whether the difference
#'is significant or not as determined by adjusted p-value from the t-test and using
#'the signficance level provided in \code{sig.level} (defaults to 0.05).  Similar
#'results for the difference in ages (e.g., is the mean row-variable age minus
#'column variable age at column-variable age-3 equal to 0?) can be constructed by
#'including \code{what="diff.bias"} in \code{summary}.
#'
#'See \code{\link{agePrecision}} for measures of precision between pairs of age assignments.
#'
#'@aliases ageBias plot.ageBias summary.ageBias
#'@param formula A formula of the form \code{col~row}, where \code{col} and 
#'\code{row} generically represent the variables that contain the ages that will
#'form the columns and rows, respectively, of the age-agreement table.  See details.
#'@param data A data.frame that minimally contains the paired age assignments.
#'See description for \code{formula} and details.
#'@param col.lab A string that contains a label for the column age assignments.
#'@param row.lab A string that contains a label for the row age assignments.
#'@param method A string indicating which method to use when adjusting p-values
#'for multiple comparisons.
#'@param sig.level A value to be used for determining whether a p-value suggests
#'a significant result or not.  The confidence level used in \code{plot} is 1-\code{sig.level}.
#'@param min.n.CI A value that indicates the smallest sample size for which a
#'confidence interval should be computed.  Default is 5.
#'@param object An object saved from the \code{ageBias} call (i.e., of class \code{ageBias}).
#'@param x An object saved from the \code{ageBias} call (i.e., of class \code{ageBias}).
#'@param what A string that indicates what type of summary to print or plot to
#'construct.  See details.
#'@param difference A logical indicating whether or not the difference between the
#'two ageing structures should be used or not.  See details.
#'@param zero.print A string indicating what should be printed in place of the
#'zeroes on an age-agreement table.  The default is to print a single dash.
#'@param digits A numeric indicating the minimum number of digits to print when
#'showing \code{what="bias"} or \code{what="diff.bias"} in \code{summary}.
#'@param flip.table A logical indicating whether the age-agreement table should
#'be \sQuote{flipped} (i.e., rows are reversed so that the younger ages are at
#'the bottom of the table).  This makes the table more directly comparable to the
#'age-bias plot.
#'@param cont.corr A continuity correction method to be used with (only) McNemars test.
#'If \code{"none"} (default) then no continuity correction is used, if \code{"Yates"}
#'then 0.5 is used, and if \code{"Edwards"} then 1 is used.
#'@param xlab A string that contains a label for the x-axis age assignments.
#'@param ylab A string that contains a label for the y-axis age assignments.
#'@param show.n A logical that indicates whether the sample sizes for each level
#'of the x-axis variable is shown (\code{=TRUE}, default) or not (\code{=FALSE}).
#'@param show.pts A logical that indicates whether to show the raw data points or
#'not.  See \code{col.pts} below.
#'@param show.rng A logical that indicates whether to show vertical bars that
#'represent the range of the data points or not.  See \code{col.rng} below.
#'@param pch.mean A value that indicates the plotting character to be used for mean
#'values (i.e., center of confidence interval bars).
#'@param col.err A string or value that indicates the color to be used for
#'confidence interval bars that are considered non-significant.
#'@param col.err.sig A string or value that indicates the color to be used for
#'confidence interval bars that are considered significant.
#'@param lwd.err A value that indicates the line width for the confidence interval bars.
#'@param pch.pts A value that indicates the plotting character to be used when
#'plotting the raw data points.
#'@param col.pts A string or value that indicates the color to be used for plotting
#'the raw data points.  The default is to use black with a transparency found
#'in \code{transparency}.
#'@param transparency A value (between 0 and 1) that indicates the level of
#'transparency to use for plotting the raw data points.  If expressed as a
#'fraction of 1/x then x points plotted on top of each other will represent the
#'color in \code{col.pts}.
#'@param col.rng A string or value that indicates the color to be used for the
#'interval representing the range of the data.
#'@param lwd.rng A value that indicates the line width for the interval
#'representing the range of the data.
#'@param col.ref A value or string that indicates the color for the 1:1 or zero (if
#'difference) reference line.
#'@param lwd.ref A value that indicates the line width for the 1:1 or zero (if
#'difference) reference line.
#'@param lty.ref A value that indicates the line type for the 1:1 or zero (if
#'difference) reference line.
#'@param xlim A numeric vector of the limits of the x-axis.
#'@param ylim A numeric vector of the limits of the y-axis.
#'@param yaxt A string which specifies the x-axis type. Specifying
#'\dQuote{n} suppresses plotting of the axis.  See \sQuote{?par}.
#'@param \dots Additional arguments for methods.
#'@return \code{ageBias} returns a list with the following items:
#'\itemize{
#'  \item data A data frame with the original two age assignments and the difference
#'between those two age assignements.
#'  \item agree The age-agreement table.
#'  \item bias A data.frame that contains the bias statistics.
#'  \item bias.diff A data.frame that contains the bias statistics for the differences.
#'  \item col.lab A character string containing an optional label for the
#'column structure or readings.
#'  \item row.lab A character string containing an optional label for the
#'row structure or readings.
#'}
#'
#'The \code{summary} and \code{plot} function do not return anything.
#'
#'@seealso \code{\link{agePrecision}} and \code{compare2} in \pkg{fishmethods}.
#'
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/AgeComparisons.pdf}
#'
#'@references Campana, S.E., M.C. Annand, and J.I. McMillan. 1995.  Graphical and
#'statistical methods for determining the consistency of age determinations.
#'Transactions of the American Fisheries Society, 124:131-138.  \url{http://www.bio.gc.ca/otoliths/publication-eng.php}
#'
#'Evans, G.T. and J.M. Hoenig.  1998.  Testing and viewing symmetry in contingency
#'tables, with apprlication to readers of fish ages.  Biometrics 54:620-629.
#'\url{http://www.fisheries.vims.edu/hoenig/pdfs/Viewing.pdf}.
#'
#'Hoenig, J.M., M.J. Morgan, and C.A. Brown. 1995.  Analysing differences
#'between two age determination methods by tests of symmetry. Canadian Journal
#'of Fisheries And Aquatic Systems, 52:364-368.  \url{http://www.fisheries.vims.edu/hoenig/pdfs/Hoenig_Morgan_Brown_AgeDeterminationSymmetry.pdf}
#'
#'Muir, A.M., M.P. Ebener, J.X. He, and J.E. Johnson.  2008.  A comparison of
#'the scale and otolith methods of age estimation for lake whitefish in Lake
#'Huron.  North American Journal of Fisheries Management, 28:625-635.
#'
#'@keywords htest manip
#'@examples
#'data(WhitefishLC)
#'ab1 <- ageBias(otolithC~scaleC,data=WhitefishLC,col.lab="Otolith Age",row.lab="Scale Age")
#'summary(ab1)
#'summary(ab1,what="Bowkers")
#'summary(ab1,what="EvansHoenig")
#'summary(ab1,what="McNemars")
#'summary(ab1,what="McNemars",cont.corr="Yates")
#'summary(ab1,what="bias")
#'summary(ab1,what="diff.bias")
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
                         xlab=x$col.lab,ylab=x$row.lab,show.n=TRUE,
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
  biasplot <- function(obj,difference,xlab,ylab,show.n,show.pts,show.rng,col.err,col.err.sig,col.pts,col.rng,xlim,ylim,yaxt,...) {
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
    if (show.n) text(d[,1],grconvertY(1.1,"npc"),d$n,cex=0.75,xpd=TRUE)
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
                             show.n,show.pts,show.rng,col.err,col.err.sig,col.pts,col.rng,xlim,ylim,yaxt,...)
  else if (what=="sunflower") asunflowerplot(x,difference,xlab,ifelse(!difference,ylab,paste(ylab,"-",xlab)),xlim,ylim,lwd.ref,lty.ref,col.ref,...)
  else anumplot(x,xlab,ylab,xlim,ylim,lwd.ref,lty.ref,col.ref,...)
}

#'@rdname ageBias
#'@method summary ageBias
#'@S3method summary ageBias
summary.ageBias <- function(object,what=c("table","Bowkers","EvansHoenig","McNemars","bias","diff.bias","symmetry"),
                            flip.table=FALSE,zero.print="-",digits=3,cont.corr=c("none","Yates","Edwards"),
                            ...) {
  ## Internal function to compute Bowker's Test
  Bowkers <- function(obj) {
    # rename agreement table
    at <- obj$agree
    # Remove the values on the diagonal
    diag(at) <- NA
    # Find the values on the lower- and upper- triangles
    lo <- up <- at
    lo[upper.tri(lo)] <- NA
    up[lower.tri(up)] <- NA
    # Chi-sq parts (Evans and Hoenig's eq 1, Hoenig et al.'s eq 3)
    rat <- ((lo-t(up))^2)/(lo+t(up))
    # Add chi-square parts
    chi.sq <- sum(rat,na.rm=TRUE)
    # Count number of chi-sq parts
    df <- sum(as.numeric(!is.na(rat)))
    # Find the p-value 
    p <- pchisq(chi.sq,df,lower.tail=FALSE)
    # Return list
    list(title="Bowker's Test of Symmetry",res=data.frame(df,chi.sq,p))  
  } ## End internal Bowker's Test function
  
  ## Internal function to compute Bowker's Test
  McNemars <- function(obj,cont.cor) {
    # handle continuity correction
    if (cont.cor=="none") {
      title <- "McNemar's Test of Symmetry"
      cc <- 0
    } else if (cont.cor=="Yates") {
      title <- "McNemar's Test of Symmetry (Yates Correction)"
      cc <- 0.5
    } else if (cont.cor== "Edwards") {
      title <- "McNemar's Test of Symmetry (Edwards Correction)"
      cc <- 1
    } else stop("Continuity correction is incorrect",call.=FALSE)
    # rename agreement table
    at <- obj$agree
    # Remove the values on the diagonal
    diag(at) <- NA
    # Find the values on the lower- and upper- triangles
    lo <- up <- at
    lo[upper.tri(lo)] <- NA
    up[lower.tri(up)] <- NA
    # Chi-sq parts (Evans and Hoenig's eq 2, but include the correction factor)
    top <- (abs(sum((lo-t(up)),na.rm=TRUE))-cc)^2
    bot <- sum(lo+t(up),na.rm=TRUE)
    chi.sq <- top/bot
    df <- 1
    # Find the p-value 
    p <- pchisq(chi.sq,df,lower.tail=FALSE)
    # Return list
    list(title=title,res=data.frame(df,chi.sq,p))  
  } ## End internal McNemar's Test function
  
  ## Internal function to compute Evans Hoenig's Test
  EvansHoenig <- function(obj) {
    # rename agreement table
    at <- obj$agree
    # Remove the values on the diagonal
    diag(at) <- NA
    # Create matrix of differences in potential ages
    diffs <- at
    for (i in 1:nrow(at)) {
      for (j in 1:ncol(at)) {
        diffs[i,j] <- as.numeric(rownames(at)[j])-as.numeric(colnames(at)[i])
      }
    }
    # Find max diff
    max.diff <- max(diffs)
    # Find parts of Evans Hoenig calcualtions -- finds individual off-diagonals
    #   and then computes the ratio that forms the chi-square parts
    rat <- numeric(nrow(at)-1)
    for (i in 1:max.diff) {
      above <- at[diffs==i]
      below <- at[diffs==-i]
      rat[i] <- (sum(above-below)^2)/sum(above+below)
    }
    # Remove those values that were na (i.e., these were diagonals w/ no obs)
    rat <- rat[!is.na(rat)]
    # Find degrees-of-freedom ... number of off-diagonals with observations
    df <- length(rat)
    # sum the chi-square parts to get a full chi-square value
    chi.sq <- sum(rat)
    p <- pchisq(chi.sq,df,lower.tail=FALSE)
    # Return list
    list(title="Evans-Hoenig's Test of Symmetry",res=data.frame(df,chi.sq,p))  
  } ## End internal Evans Hoenig's Test function
  
  ## Main function
  what <- match.arg(what)
  if (what=="bias") {
    cat("Summary of",object$row.lab,"by",object$col.lab,"\n")
    print(object$bias[-ncol(object$bias)],row.names=FALSE,digits=digits)
  }
  if (what=="diff.bias") {
    cat("Summary of",object$row.lab,"-",object$col.lab,"by",object$col.lab,"\n")
    print(object$bias.diff[-ncol(object$bias.diff)],row.names=FALSE,digits=digits)
  }
  if (what %in% c("table","Bowkers","EvansHoenig","McNemars","symmetry")) {
    if (what=="symmetry") message("Use of what='symmetry' is deprecated, use what='Bowkers' instead")
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
  }
  if (what %in% c("Bowkers","EvansHoenig","McNemars","symmetry")) {
    if (what=="Bowkers" | what=="symmetry") symres <- Bowkers(object)
    if (what=="McNemars") symres <- McNemars(object,match.arg(cont.corr))
    if (what=="EvansHoenig") symres <- EvansHoenig(object)
    cat("\n",symres$title,"\n")
    print(symres$res,row.names=FALSE)
    # invisibly return the table
    invisible(symres$res)    
  }
}
