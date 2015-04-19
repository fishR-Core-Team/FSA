#' @title Creates separate histograms by levels.
#'
#' @description Creates separate histograms of a quantitative variable by levels of a factor variable.
#'
#' @details The formula must be of the form \code{~quantitative}, \code{quantitative~1}, \code{quantitative~factor}, or \code{quantitative~factor*factor2} where \code{quantitative} is the quantitative variable to construct the histograms for and \code{factor} or \code{factor2} are factor variables that contain the levels for which separate histograms should be constructed.
#'
#' If the formula is of the form \code{~quantitative} or \code{quantitative~1} then only a single histogram of the quantitative variable will be produced.  This allows \code{hist.formula()} to be used similarly to \code{hist()} but with a \code{data=} argument.
#'
#' The function produces a single (but see below) graphic that consists of a grid on which the separate histograms are printed.  The rows and columns of this grid are determined to construct a plot that is as square as possible.  However, the rows and columns can be set by the user with the \code{nrow=} and \code{ncol=} arguments.  If the product of the number of rows and number of columns set by the user is less than the total number of histograms to be constructed then multiple pages of histograms will be produced (each requiring the user to click on the graph to go to the next graph).  The x-axis of each separate histogram will be labeled identically.  The default x-axis label is the name of the quantitative variable.  This can be changed by the user with the \code{xlab=} argument.
#'
# 'By default each histogram is labeled with a main title that consists of the levels formed by the right-hand-side of the formula.  If a string is given in \code{pre.main=} then that string will be appended with the names of the level formed by the right-hand-side of the formula.  If \code{pre.main=NULL} then NO main title will be printed.
#'
#' I changed the \code{right=} argument from that used in the base \code{hist()} so that right-open (left-closed) is the default.
#' 
#' I added the \code{iaxs=} argument and set the default to \code{TRUE} so that \code{xaxs="i"} and \code{yaxs="i"} are used when plotting both axes.  This removes the \dQuote{floating} x-axis that R typically plots for histograms.
#'
#' @note Students often need to look at the distribution of a quantitative variable separated for different levels of a categorical variable.  One method for examining these distributions is with \code{boxplot(quantitative~factor)}.  Other methods use functions in \pkg{Lattice} and \pkg{ggplots2} but these packages have some learning \sQuote{overhead} for newbie students.  The formula notation, however, is a common way in R to tell R to separate a quantitative variable by the levels of a factor.  Thus, this function adds code for formulas to the generic \code{hist} function.  This allows newbie students to use a common notation (i.e., formula) to easily create multiple histograms of a quantitative variable separated by the levels of a factor.
#'
#' @param formula A formula of class(formula).  See details.
#' @param data An optional data frame that contains the variables in the model.
#' @param main A character string to be used as the main title only when a single histogram is produced.
#' @param right A logical that indicates if the histogram bins are right-closed (left open) intervals (\code{=TRUE}) or not (\code{=FALSE}; default).
#' @param pre.main A character string to be used as a prefix for the main title.  See details.
#' @param xlab A character label for the x-axis.  Defaults to name of quantitative variable in the formula.
#' @param ylab A character label for the y-axis.  Defaults to \dQuote{Frequency}.
#' @param same.breaks A logical that indicates whether the same break positions should be used on each histogram.  Defaults to \code{TRUE}.
#' @param same.ylim A logicial that indicates whether the same limits for the y-axis should be used on each histogram.  Defaults to \code{TRUE}.  Ignored if \code{ylmts} is non-null.
#' @param ymax A single value that sets the maximum y-axis limit for each histogram or a vector of length equal to the number of groups that sets the maximum y-axis limit for each histogram separately.
#' @param col A string that indicates the color for the bars on the histogram.  Defaults to a light shade of gray (i.e., \code{"gray90"}).
#' @param nrow A numeric that contains the number of rows to use on the graphic.
#' @param ncol A numeric that contains the number of columns to use on the graphic.
#' @param byrow A logical that indicates if the histograms should fill rows first (\code{=TRUE} or columns first (\code{=FALSE}).
#' @param iaxs A logical that indicates whether both axes should be plotted using \code{xaxs="i"} and \code{yaxs="i"} (the Default) or \code{xaxs="r"} and \code{yaxs="r"} (what R typically does).
#' @param \dots Other arguments to pass through to the default \code{hist()}.
#'
#' @return Nothing is returned; however, a graphic is produced.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}, but this implementation is largely a modification of the code provided by Marc Schwartz on the R-help mailing list on 1Jun07.
#'
#' @seealso See base \code{\link{hist}} for related functionality and \code{\link[plotrix]{multhist}} in \pkg{plotrix} for similar functionality.
#'
#' @keywords hplot
#'
#' @examples
#' ## Using the defaults
#' hist(Sepal.Length~Species,data=iris)
#'
#' ## Add x-labels and use a pre-fix on the main labels
#' hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",pre.main="Species==")
#'
#' ## Use different breaks and different y-axis limits for each graph
#' hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",same.breaks=FALSE,same.ylim=FALSE)
#'
#' ## Use same but user-controlled breaks for each graph
#' hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",breaks=seq(4,8,1))
#'
#' ## Use same but user-controlled maximum value for y-axis
#' hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",ymax=30)
#'
#' ## Control the organization of the 'grid' of histograms
#' hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",nrow=1,ncol=3)
#'
#' ## Use right=FALSE & freq=FALSE arguments to demonstrate sending an argument used by base hist()
#' hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",right=FALSE,freq=FALSE)
#'
#' ## Add a junk variable to the iris data set to show two factors on RHS
#' iris$junk <- factor(sample(c("A","B"),nrow(iris),replace=TRUE))
#' hist(Sepal.Length~Species*junk,data=iris,xlab="Sepal Length (cm)")
#'
#' ## Single histogram without grouping using formula notation
#' hist(~Sepal.Length,data=iris,xlab="Sepal Length (cm)")
#' 
#' ## Single histogram with "axis correction" turned off (compare to previous)
#' hist(~Sepal.Length,data=iris,xlab="Sepal Length (cm)",iaxs=FALSE)
#' 
#' ## Single histogram with "axis correction", testing xlim and ylim
#' hist(~Sepal.Length,data=iris,xlab="Sepal Length (cm)",xlim=c(3.8,8.2),ylim=c(0,35))
#'  
#' @rdname hist.formula
#' @export
hist.formula <- function(formula,data=NULL,main="",right=FALSE,
                         pre.main="",xlab=names(DF)[1],ylab="Frequency",
                         same.breaks=TRUE,same.ylim=TRUE,ymax=NULL,col="gray90",
                         nrow=round(sqrt(num)),ncol=ceiling(sqrt(num)),byrow=TRUE,
                         iaxs=TRUE,...) {
  opar <- par("mfrow")
  DF <- model.frame(formula,data=data)
  if (dim(DF)[2]==1) {
    h <- hist(DF[,1],xlab=xlab,ylab=ylab,main=main,right=right,col=col,
              xaxs=ifelse(iaxs,"i","r"),yaxs=ifelse(iaxs,"i","r"),...)
    if (iaxs) abline(h=0,xpd=FALSE)  # will assure a line at y=0
    invisible(h)
  } else {
    if (attr(attr(DF, "terms"),"dataClasses")[1]!="numeric") stop("Single variable in formula must be a numeric vector.",call.=FALSE)
    if (dim(DF)[2]>3) stop("hist.formula only works with one quantitative variable on LHS\n and one or two factor variables on RHS of formula.",call.=FALSE)
    if (attr(attr(DF, "terms"),"dataClasses")[2]!="factor") {
      warning("Variable on RHS of formula must be a factor.  Will convert to factor\n and continue.  This may result in an error.",call.=FALSE)
      DF[,2] <- factor(DF[,2])
    }
    if (dim(DF)[2]==2) {
      DF.split <- split(DF[[1]],DF[[2]])
    } else {
      if (attr(attr(DF, "terms"),"dataClasses")[3]!="factor") {
        warning("Variable on RHS of formula must be a factor.  Will convert to factor\n and continue.  This may result in an error.",call.=FALSE)
        DF[,3] <- factor(DF[,3])
      }
      DF[,4] <- interaction(DF[,2],DF[,3])
      DF.split <- split(DF[[1]],DF[[4]])
    }
    num <- length(names(DF.split))
    if (same.breaks) { breaks <- hist(DF[[1]],right=right,plot=FALSE,warn.unused=FALSE,...)$breaks }
    if (is.null(ymax)) {
      for (i in 1:num) {  # used to find highest count on all histograms
        ymax[i] <- max(hist(DF.split[[i]],right=right,plot=FALSE,warn.unused=FALSE,...)$counts)
      }
      if (same.ylim) { ymax <- rep(max(ymax),length(ymax)) }
    } else {
      if (length(ymax)==1) ymax <- rep(ymax,num)
      else if (length(ymax)!= num) stop("'ymax' argument must be 'NULL', a vector of length 1,\n or a vector of length equal to the number of groups.",call.=FALSE)
    }
    if (num <= (nrow*ncol)) {
      if (byrow) par(mfrow=c(nrow,ncol))
        else par(mfcol=c(nrow,ncol))
      for (i in 1:num) {
        if (!is.null(pre.main)) main <- paste(pre.main,names(DF.split)[i],sep="")
        hist(DF.split[[i]],main=main,xlab=xlab,ylab=ylab,right=right,ylim=c(0,ymax[i]),col=col,
             xaxs=ifelse(iaxs,"i","r"),yaxs=ifelse(iaxs,"i","r"),...)
        if (iaxs) abline(h=0,xpd=FALSE)  # will assure a line at y=0
      }
    } else {
      max.per.page <- nrow*ncol
      if (byrow) par(mfrow=c(nrow,ncol))
        else par(mfcol=c(nrow,ncol))
      for (i in 1:ceiling(num/max.per.page)) {
        ifelse((num-(i-1)*max.per.page)>=max.per.page,todo <- max.per.page,todo <- num-(i-1)*max.per.page)
        for (j in 1:todo) {
          pos <- (i-1)*max.per.page+j
          if (!is.null(pre.main)) main <- paste(pre.main,names(DF.split)[pos],sep="")
          hist(DF.split[[pos]],main=main,xlab=xlab,ylab=ylab,right=right,ylim=c(0,ymax[pos]),col=col,
               xaxs=ifelse(iaxs,"i","r"),yaxs=ifelse(iaxs,"i","r"),...)
          if (iaxs) abline(h=0,xpd=FALSE)  # will assure a line at y=0
        }  
      }
    }
    par(mfrow=opar)
  }
}
