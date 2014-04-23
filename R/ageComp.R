#'Results for comparing paired sets of ages.
#'
#'Computes overall measures of precision and bias for paired age assignments.
#'The paired age assignments can consist of age measurements recorded for two
#'structures (e.g., otoliths and scales), for two readers of the same
#'structure, or for one reader at two times.
#'
#'The main function, \code{ageComp}, requires a formula of the form \code{col~row},
#'where \code{col} and \code{row} generically represent the variables that
#'contain the ages that will form the columns and rows, respectively, of the age-
#'agreement table.  If one age assignment is thought to be more accurate than the
#'other, then it should form the columns and should be on the left-hand-side of
#'the formula.  It should also be noted that the variable that forms the columns
#'in the age-agreement table will also form the x-axis in the various plots
#'produced by \code{plot}.
#'
#'If \code{what="agreement"} in \code{summary} then only the age-agreement table
#'is printed.  If \code{what="differences"} then only a table of the percentage
#'of individuals by absolute difference in age is printed.  If \code{what="prec.stats"}
#'then summary statistics of overall percentage agreement, overall absolute percentage
#'error (APE), and overall coefficien to variation (CV) are printed.  All of the
#'previous items are printed if \code{what="precision"} (the default).
#'
#'If \code{what="detail"} in \code{summary} then a data frame of the original data
#'plus the intermediate caculations of the differences in assigned ages, average
#'age, standard deviation of age, APE, and CV for each individual fish will be
#'printed .  If \code{what="symmetry"} then Bowker's test of symmetry, as proposed
#'by Hoenig (1995), to determine if systematic differences exist between paired age
#'assignments is printed.
#'
#'If \code{what="bias"} (the default) in \code{plot} then an age-bias plot as
#'described by Campana et al. (1995) is produced.  The variable that formed the
#'columns in the original \code{ageComp} call is plotted on the x-axis.  Confidence
#'intervals plotted in red are computed for the mean of the y-axis data at each
#'\dQuote{level} (i.e., age) of the x-axis data.  The level of confidence interval
#'is controlled by \code{sig.level} given in the original \code{ageComp} call.
#'Vertical lines that connect the minimum to the maximum value of the y-axis data
#'at each \dQuote{level} of the x-axis data are plotted in grey.  A 1:1 (45 degree)
#'agreement line is shown for comparative purposes.  The sample sizes at each
#'\dQuote{level} of the x-axis data is shown if \code{show.n=TRUE} (the default).
#'
#'If \code{what="difference"} in \code{plot} then an age-difference plot as used
#'by Muir et al. (2008) is produced.  The plot is similar to the age-bias plot
#'described above except that the y-axis data is the difference in the paired
#'ages in the rows and columns from the original \code{ageComp} call (specifically,
#'columns-rows).
#'
#'If \code{what="sunflower"} in \code{plot} then a sunflower plot is constructed.
#'A sunflower plot contains a point for each unique (x,y) coordinate with a 
#'\dQuote{petal} added to each point for each observation that has the same (x,y)
#'coordinate.  Thus, points with more petals have more observations at that point.
#'
#'@aliases ageComp plot.ageComp summary.ageComp
#'@param formula A formula of the form \code{col~row}, where \code{col} and 
#'\code{row} generically represent the variables that contain the ages that will
#'form the columns and rows, respectively, of the age-agreement table.  See details.
#'@param data A data.frame that minimally contains the paired age assignments.
#'See description for \code{formula} and etails.
#'@param col.lab A string that contains a label for the column age assignments.
#'@param row.lab A string that contains a label for the row age assignments.
#'@param method A string that indicates which method to use when adjusting p-values
#'for multiple comparisons.
#'@param sig.level A value to be used for determining whether a p-value suggests
#'a significant result or not.  The confidence level used in \code{plot} is 1-\code{sig.level}.
#'@param object An object saved from the \code{ageComp} call (i.e., of class \code{ageComp}).
#'@param x An object saved from the \code{ageComp} call (i.e., of class \code{ageComp}).
#'@param what A string that indicates what type of summary to print or plot to
#'construct.  See details.
#'@param zero.print A string that indicates what should be printed in place of the
#'zeroes on an age-agreement table.  The default is to print a single dash.
#'@param flip.table A logical that indicates whether the age-agreement table should
#'be \sQuote{flipped} (i.e., rows are reversed so that the younger ages are at
#'the bottom of the table).  This makes the table more directly comparable to the
#'age-bias plot.
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
#'@return \code{ageComp} returns a list with the following items:
#'\itemize{
#'\item detail A data frame will all given and computed information about each fish.
#'\item absdiff A table of percentage by absolute differences in age.
#'\item agree The age-agreement table.
#'\item stats The summary statistics of percent agreement, AVE, and CV.
#'\item bias A data.frame that contains the bias statistics.
#'\item bias.diff A data.frame that contains the bias statistics for the differences.
#'\item col.lab A string that contains an optional label for the column structure
#' or readings.
#'\item row.lab A string that contains an optional label for the row structure
#' or readings.
#'}
#'
#'The \code{summary} and \code{plot} function do not return anything.
#'
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/AgeComparisons.pdf}
#'@references Beamish, R.J. and D.A. Fournier.  1981.  A method for comparing
#'the precision of a set of age determinations.  Canadian Journal of Fisheries
#'and Aquatic Sciences, 38:982-983.
#'
#'Campana, S.E.  1982.  Accuracy, precision and quality control in age
#'determination, including a review of the use and abuse of age validation
#'methods. Journal of Fish Biology, 59:197-242.
#'
#'Campana, S.E., M.C. Annand, and J.I. McMillan. 1995.  Graphical and
#'statistical methods for determining the consistency of age determinations.
#'Transactions of the American Fisheries Society, 124:131-138.
#'
#'Chang, W.Y.B. 1982.  A statistical method for evaluating the reproducibility
#'of age determination.  Canadian Journal of Fisheries and Aquatic Sciences,
#'39:1208-1210.
#'
#'Hoenig, J.M., M.J. Morgan, and C.A. Brown. 1995.  Analysing differences
#'between two age determination methods by tests of symmetry. Canadian Journal
#'of Fisheries And Aquatic Systems, 52:364-368.
#'
#'Muir, A.M., M.P. Ebener, J.X. He, and J.E. Johnson.  2008.  A comparison of
#'the scale and otolith methods of age estimation for lake whitefish in Lake
#'Huron.  North American Journal of Fisheries Management, 28:625-635.
#'@keywords htest manip
#'@examples
#'data(WhitefishLC)
#'ac1 <- ageComp(otolithC~scaleC,data=WhitefishLC,col.lab="Otolith Age",row.lab="Scale Age")
#'summary(ac1)
#'summary(ac1,what="agreement")
#'summary(ac1,what="prec.stats")
#'summary(ac1,what="detail")
#'summary(ac1,what="symmetry")
#'summary(ac1,what="bias")
#'summary(ac1,what="bias.diff")
#'# show the zeroes (rather than dashes)
#'summary(ac1,what="agreement",zero.print="0")
#'# flip the table -- ease of comparison to age-bias plot
#'summary(ac1,what="agreement",flip.table=TRUE)
#'
#'## default plot
#'plot(ac1)
#'## plot with the data points shown
#'plot(ac1,show.pts=TRUE)
#'## plot with the range shown
#'plot(ac1,show.rng=TRUE)
#'## plot with no difference in significance bar colors
#'plot(ac1,col.err="red",col.err.sig="red")
#'## plot of differences (not could use same modifications as shown above)
#'plot(ac1,what="difference")
#'## sunflower plot
#'plot(ac1,what="sunflower")
#'
#'@rdname ageComp
#'@export
ageComp <- function(formula,data,col.lab="First",row.lab="Second",method=p.adjust.methods,sig.level=0.05) {
  warning("ageComp has been deprecated.\n  It will not be maintained and will be deleted from future versions.\n  Please begin to use ageBias and agePrecision.",call.=FALSE)
  tmp <- getVarFromFormula(formula,data,expNumVars=2)
  col.name <- tmp[1]
  row.name <- tmp[2]
  col.data <- data[,col.name]
  row.data <- data[,row.name]
  n <- length(col.data)                                                                     # Sample size
 # Calculations on each fish
  d <- data.frame(col.data,row.data)                                                        # Construct the data.frame
  age.avg <- apply(d,1,mean)                                                                # Mean of assigned age
  age.sd <- apply(d,1,sd)                                                                   # SD of assigned age
  age.ad <- abs(col.data-age.avg)+abs(row.data-age.avg)                                     # Summed absolute deviation
  APE.j <- (100/2)*age.ad/age.avg                                                           # APE
  CV.j <- 100*age.sd/age.avg                                                                # CV
  age.diff <- col.data-row.data                                                             # Difference in assigned ages
  detail.df <- data.frame(col.data,row.data,diff=age.diff,avg=age.avg,sd=age.sd,APE=APE.j,CV=CV.j) # put together data frame of individual details
  colnames(detail.df)[1:2] <- c(col.name,row.name)                                          # get names of entry variables
 # Summary calculation for all fish
  APE <- mean(APE.j,na.rm=TRUE)                                                             # Overall APE
  CV <- mean(CV.j,na.rm=TRUE)                                                               # Overall CV
  agree <- 100*length(d[age.diff==0,1])/n                                                   # Overall percent agreement
  stats.df <- data.frame(n,agree,APE,CV)                                                    # put together dataframe of summary statistics
  abs.diff.table <- 100*table(abs(age.diff))/n                                              # Percent agreement for difference values
 # Summarizations of row.data by col.data
  y <- data.frame(Summarize(row.data~factor(col.data)))                                     # Summary stats of 2nd strux by ages of first strux
  x <- aggregate(col.data,by=list(col.data),mean)[,2]                                       # Age range of 1st for plotting
  canCI <- y$n>1 & y$sd>0                                                                   # Is it appropriate to compute a CI
  y$SE <- y$t <- y$p.value <- y$LCI <- y$UCI <- NA                                          # Fill CIs with NA (will leave NA for those where CI was inappropriate)
  y$SE[canCI] <- y$sd[canCI]/sqrt(y$n[canCI])                                               # SE of 2nd strux
  y$t[canCI] <- (y$mean[canCI]-x[canCI])/y$SE[canCI]                                        # t test statistic that mean of 2nd strux equals value of 1st strux
  y$p.value <- pt(abs(y$t),df=y$n-1,lower.tail=FALSE)*2                                     # two-tailed p-value
  y$adj.p <- round(p.adjust(y$p.value,method=p.adjust.methods),5)
  bias.df <- data.frame(age=x,n=y$n,min=y$min,max=y$max,mean=y$mean,SE=y$SE,
                        t=y$t,adj.p=y$adj.p,canCI=canCI)
  names(bias.df)[1] <- col.name                                                             # label first column with col.data name
# Summarizations of diff.data by col.data
  diff.data <- row.data-col.data                                                            # compute differences
  y <- data.frame(Summarize(diff.data~factor(col.data)))                                    # Summary stats of differences by ages of first strux
  x <- aggregate(col.data,by=list(col.data),mean)[,2]                                       # Age range of 1st for plotting
  canCI <- y$n>1 & y$sd>0                                                                   # Is it appropriate to compute a CI
  y$SE <- y$t <- y$p.value <- y$LCI <- y$UCI <- NA                                          # Fill CIs with NA (will leave NA for those where CI was inappropriate)
  y$SE[canCI] <- y$sd[canCI]/sqrt(y$n[canCI])                                               # SE of differences
  y$t[canCI] <- (y$mean[canCI]-0)/y$SE[canCI]                                               # t test statistic that mean of differences equals 0
  y$p.value <- pt(abs(y$t),df=y$n-1,lower.tail=FALSE)*2                                     # two-tailed p-value
  y$adj.p <- round(p.adjust(y$p.value,method=p.adjust.methods),5)
  bias2.df <- data.frame(age=x,n=y$n,min=y$min,max=y$max,mean=y$mean,SE=y$SE,
                         t=y$t,adj.p=y$adj.p,canCI=canCI)
  names(bias2.df)[1] <- col.name                                                            # label first column with col.data name
 # Agreement contingency table adjusted to be square  
  ages <- min(col.data,row.data):max(col.data,row.data)                                     # finds overall range of ages
  rf <- factor(row.data,levels=ages)                                                        # converts row data to factor with ages levels
  cf <- factor(col.data,levels=ages)                                                        # ditto for column data (this will force a square table)
  agree.table <- table(rf,cf,dnn=c(row.lab,col.lab))                                        # Agreement contingency table
 # Put together an output list
  d <- list(detail=detail.df,absdiff=abs.diff.table,agree=agree.table,
            stats=stats.df,bias=bias.df,bias.diff=bias2.df,col.lab=col.lab,
            row.lab=row.lab,sig.level=sig.level)
  class(d) <- "ageComp"
  d
}

#'@rdname ageComp
#'@method plot ageComp
#'@S3method plot ageComp
plot.ageComp <- function(x,what=c("bias","difference","sunflower"),xlab=x$col.lab,
                         ylab=x$row.lab,show.n=TRUE,show.pts=FALSE,show.rng=FALSE,
                         pch.mean=3,col.err="blue",col.err.sig="red",lwd.err=2,
                         pch.pts=19,col.pts=rgb(0,0,0,transparency),transparency=1/20,
                         col.rng="gray",lwd.rng=2,
                         col.ref="black",lwd.ref=1,lty.ref=2,
                         xlim=NULL,ylim=NULL,yaxt=par("yaxt"),...) {

  biasplot <- function(obj,difference,xlab,ylab,show.n,show.pts,show.rng,col.err,col.err.sig,col.pts,col.rng,xlim,ylim,yaxt,...) {
    conf.level <- (1-obj$sig.level)
    if (!difference) d <- obj$bias
      else d <- obj$bias.diff
    x.c <- d[,1]
    d$LCI <- d$UCI <- NA
    d$LCI[d$canCI] <- d$mean[d$canCI]+qt((1-conf.level)/2,d$n[d$canCI]-1)*d$SE[d$canCI]                             # Lower CIs
    d$UCI[d$canCI] <- d$mean[d$canCI]+qt((1-conf.level)/2,d$n[d$canCI]-1,lower.tail=FALSE)*d$SE[d$canCI]            # Upper CIs
    d$sig <- rep(FALSE,nrow(d))
    d$sig[d$adj.p<(1-conf.level)] <- TRUE
    if (is.null(ylim)) {ylmt <- c(floor(min(c(d$min,d$LCI),na.rm=TRUE)),ceiling(max(c(d$max,d$UCI),na.rm=TRUE)))}   # Find a good y-scale for difference plot
      else {ylmt=ylim}
    if (is.null(xlim)) { xlmt <- range(x.c,na.rm=TRUE) }                                                            # Natural scale for x for difference plot
      else {xlmt=xlim}
    if (!difference & is.null(xlim) & is.null(ylim)) { xlmt <- ylmt <- range(c(x.c,ylmt),na.rm=TRUE) }              # Make square x- and y-scales for bias plot unless xlim or ylim given
    if(show.n & is.null(ylim)) ylmt[1] <- ylmt[1]-round(0.10*(ylmt[2]-ylmt[1]),0)                                   # If to show n then make range bigger
    par(lab=c(length(x.c),length(d$mean),7))                                                                        # Plot more tick marks
    plot(d$mean~x.c,xlim=xlmt,ylim=ylmt,xlab=xlab,ylab=ylab,pch=pch.mean,yaxt="n",...)                                    # Mean of 2nd vs. 1st age range
    if (yaxt!="n") {axis(2,seq(ylmt[1],ylmt[2],1))}                                                                 # Helps keep y-axis as integers (needed for difference plot)
    if (difference) { abline(h=0,lwd=lwd.ref,lty=lty.ref,col=col.ref) }                                                                     # agreement line (for difference plot)
      else { abline(a=0,b=1,lwd=lwd.ref,lty=lty.ref,col=col.ref)  }                                                                         # 45 agreement line (for bias plot)
    if (show.pts) {
      if (!difference) points(obj$detail[,1],obj$detail[,2],col=col.pts,pch=pch.pts)
      else points(obj$detail[,1],obj$detail[,2]-obj$detail[,1],col=col.pts,pch=pch.pts)
    }
    if (show.rng) {
      plotrix::plotCI(x=x.c,y=d$mean,li=d$min,ui=d$max,add=TRUE,slty=1,scol=col.rng,pch=pch.mean,lwd=lwd.rng,gap=0,sfrac=0.005) # Put on ranges
    }
    plotrix::plotCI(x=x.c[d$sig],y=d$mean[d$sig],li=d$LCI[d$sig],ui=d$UCI[d$sig],add=TRUE,slty=1,scol=col.err.sig,pch=pch.mean,lwd=lwd.err,gap=0) # Put on CIs for significant points
    plotrix::plotCI(x=x.c[!d$sig],y=d$mean[!d$sig],li=d$LCI[!d$sig],ui=d$UCI[!d$sig],add=TRUE,slty=1,scol=col.err,pch=pch.mean,lwd=lwd.err,gap=0) # Put on CIs for non-significant points
    
    if (show.n) {                                                                                                   # Plot the ns if asked for
      text(x.c[odd(x.c)],grconvertY(0.025,"npc"),d$n[odd(x.c)],cex=0.75)                                            #   odd aged ns will be lower
      text(x.c[!odd(x.c)],grconvertY(0.055,"npc"),d$n[!odd(x.c)],cex=0.75)                                          #   even aged ns will be higher
    }
  }

  asunflowerplot <- function(obj,xlab,ylab,xlim,ylim,...) {
    x <- obj$detail[,1]
    y <- obj$detail[,2]
    if (is.null(xlim)) xlim <- range(x,y)
    if (is.null(ylim)) ylim <- range(x,y)
    sunflowerplot(x,y,seg.col="blue",size=1/10,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,...)                        # Make the plot
    abline(a=0,b=1,lwd=2,lty=2)                                                                                    # 45 agreement line
  }

  warning("ageComp has been deprecated.\n  It will not be maintained and will be deleted from future versions.\n  Please begin to use ageBias and agePrecision.",call.=FALSE)
  what <- match.arg(what)
  if (what=="bias") biasplot(x,difference=FALSE,xlab,ylab,show.n,show.pts,show.rng,col.err,col.err.sig,col.pts,col.rng,xlim,ylim,yaxt,...)
    else if (what=="difference") biasplot(x,difference=TRUE,xlab,ylab=paste(ylab,"-",xlab),show.n,show.pts,show.rng,col.err,col.err.sig,col.pts,col.rng,xlim,ylim,yaxt,...)
      else asunflowerplot(x,xlab,ylab,xlim,ylim,...)
}

#'@rdname ageComp
#'@method summary ageComp
#'@S3method summary ageComp
summary.ageComp <- function(object,what=c("precision","agreement","differences",
                                          "prec.stats","symmetry","bias","bias.diff",
                                          "detail"),
                            zero.print="-",flip.table=FALSE,
                            ...) {
  warning("ageComp has been deprecated.\n  It will not be maintained and will be deleted from future versions.\n  Please begin to use ageBias and agePrecision.",call.=FALSE)
  what <- match.arg(what)
  ret <- NULL
  if (what=="precision" | what=="agreement" | what=="symmetry") {
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
  if (what=="precision" | what=="prec.stats" | what=="differences") {
    cat("\nPercentage by absolute differences in age")
    print(object$absdiff)                                     
  }
  if (what=="precision" | what=="prec.stats") {
    cat("\nPrecision summary statistics\n")
    print(object$stats,row.names=FALSE)
  }
  if (what=="detail") {
    cat("Intermediate calculations for each individual.\n")
    print(object$detail)
  }
  if (what=="bias") {
    bias.df <- object$bias[-ncol(object$bias)]
    bias.df$sigDiff <- rep("",nrow(bias.df))
    bias.df$sigDiff[bias.df$adj.p<object$sig.level] <- "yes"
    cat("\nSummary of",object$row.lab,"by",object$col.lab,"\n")
    print(bias.df,row.names=FALSE)
  }
  if (what=="bias.diff") {
    bias.df <- object$bias.diff[-ncol(object$bias.diff)]
    bias.df$sigDiff <- rep("",nrow(bias.df))
    bias.df$sigDiff[bias.df$adj.p<object$sig.level] <- "yes"
    cat("\nSummary of",object$row.lab,"-",object$col.lab,"by",object$col.lab,"\n")
    print(bias.df,row.names=FALSE)
  }
  if (what=="symmetry") {
    at <- object$agree                                                                     # rename agreement table
    diag(at) <- NA                                                                         # Remove the values on the diagonal
    lo <- up <- at
    lo[upper.tri(lo)] <- NA                                                                # Find the values on the lower-
    up[lower.tri(up)] <- NA                                                                #   & upper-triangles
    rat <- ((lo-t(up))^2)/(lo+t(up))                                                       # Chi-sq parts (Hoenig's eq 3)
    chi.sq <- sum(rat,na.rm=TRUE)                                                          # Add chi-square parts
    df <- sum(as.numeric(!is.na(rat)))                                                     # Count number of chi-sq parts
    p <- pchisq(chi.sq,df,lower.tail=FALSE)                                                # Find the p-value 
    cat("\nBowker's (Hoenig's) Test of Symmetry\n")                                        # Print symmetry test
    Hoenig <- data.frame(df,chi.sq,p)
    print(Hoenig,row.names=FALSE)
    invisible(Hoenig)
  }
}
