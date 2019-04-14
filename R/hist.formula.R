#' @title Creates separate histograms by levels.
#'
#' @description Creates separate histograms of a quantitative variable by levels of a factor variable.
#'
#' @details The formula must be of the form \code{~quantitative}, \code{quantitative~1}, \code{quantitative~factor}, or \code{quantitative~factor*factor2} where \code{quantitative} is the quantitative variable to construct the histograms for and \code{factor} or \code{factor2} are factor variables that contain the levels for which separate histograms should be constructed.
#'
#' If the formula is of the form \code{~quantitative} or \code{quantitative~1} then only a single histogram of the quantitative variable will be produced. This allows \code{hist.formula()} to be used similarly to \code{hist()} but with a \code{data=} argument.
#'
#' The function produces a single (but see below) graphic that consists of a grid on which the separate histograms are printed. The rows and columns of this grid are determined to construct a plot that is as square as possible. However, the rows and columns can be set by the user with the \code{nrow=} and \code{ncol=} arguments. If the product of the number of rows and number of columns set by the user is less than the total number of histograms to be constructed then multiple pages of histograms will be produced (each requiring the user to click on the graph to go to the next graph). The x-axis of each separate histogram will be labeled identically. The default x-axis label is the name of the quantitative variable. This can be changed by the user with the \code{xlab=} argument.
#'
# 'By default each histogram is labeled with a main title that consists of the levels formed by the right-hand-side of the formula. If a string is given in \code{pre.main=} then that string will be appended with the names of the level formed by the right-hand-side of the formula. If \code{pre.main=NULL} then NO main title will be printed.
#'
#' The default for \code{right=} is not the same as that used in \code{hist()} from \pkg{graphics}. Thus, right-open (left-closed) bins are the default.
#' 
#' The \code{iaxs=} argument defaults to \code{TRUE} so that \code{xaxs="i"} and \code{yaxs="i"} are used for both axes, which eliminates the \dQuote{floating} x-axis that R typically plots for histograms.
#'
#' @note Students often need to look at the distribution of a quantitative variable separated for different levels of a categorical variable. One method for examining these distributions is with \code{boxplot(quantitative~factor)}. Other methods use functions in \pkg{Lattice} and \pkg{ggplots2} but these packages have some learning \sQuote{overhead} for newbie students. The formula notation, however, is a common way in R to tell R to separate a quantitative variable by the levels of a factor. Thus, this function adds code for formulas to the generic \code{hist} function. This allows newbie students to use a common notation (i.e., formula) to easily create multiple histograms of a quantitative variable separated by the levels of a factor.
#'
#' @param formula A formula. See details.
#' @param data An optional data frame that contains the variables in the model.
#' @param main A character string used as the main title for when a SINGLE histogram is produced.
#' @param right A logical that indicates if the histogram bins are right-closed (left open) intervals (\code{=TRUE}) or not (\code{=FALSE}; default).
#' @param pre.main A character string to be used as a prefix for the main title when multiple histograms are produced. See details.
#' @param xlab A character label for the x-axis. Defaults to name of quantitative variable in \code{formula}.
#' @param ylab A character label for the y-axis. Defaults to \dQuote{Frequency}.
#' @param same.breaks A logical that indicates whether the same break values (i.e., bins) should be used on each histogram. Ignored if \code{breaks} or \code{w} is provided by the user. Defaults to \code{TRUE}.
#' @param breaks A single numeric that indicates the number of bins or breaks or a vector that contains the lower values of the breaks. Ignored if \code{w} is not \code{NULL}. See \code{\link[graphics]{hist}} for more details.
#' @param w A single numeric that indicates the width of the bins to use. The bins will start at \dQuote{rounded} values depending on the value of \code{w}. See \code{\link{lencat}} for more details.
#' @param same.ylim A logical that indicates whether the same limits for the y-axis should be used on each histogram. Defaults to \code{TRUE}.
#' @param ymax A single value that sets the maximum y-axis limit for each histogram or a vector of length equal to the number of groups that sets the maximum y-axis limit for each histogram separately. If \code{NULL} (default), then a value will be found.
#' @param col A string that indicates the color for the bars on the histogram. Defaults to a light shade of gray (i.e., \code{"gray90"}).
#' @param nrow A single numeric that contains the number of rows to use on the graphic.
#' @param ncol A single numeric that contains the number of columns to use on the graphic.
#' @param byrow A single logical that indicates if the histograms should fill rows first (\code{=TRUE} or columns first (\code{=FALSE}).
#' @param iaxs A single logical that indicates whether both axes should be plotted using \code{xaxs="i"} and \code{yaxs="i"} (the default) or \code{xaxs="r"} and \code{yaxs="r"} (what R typically does).
#' @param \dots Other arguments to pass through to the default \code{hist()}.
#'
#' @return A graphic is produced and nothing is returned unless \code{formula} results in only one histogram. In that case, an object of class \code{"histogram"} is returned, which is described in \code{\link[graphics]{hist}}.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}, but this implementation is largely a modification of the code provided by Marc Schwartz on the R-help mailing list on 1Jun07.
#'
#' @section IFAR Chapter: 3-Plotting Fundamentals.
#'
#' @references Ogle, D.H. 2016. \href{http://derekogle.com/IFAR}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#'
#' @seealso See base \code{\link[graphics]{hist}} for related functionality and \code{\link[plotrix]{multhist}} in \pkg{plotrix} for similar functionality.
#'
#' @keywords hplot
#'
#' @examples
#' ## Using the defaults
#' hist(Sepal.Length~Species,data=iris)
#'
#' ## Add x-labels and use a pre-fix on the main labels
#' hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",
#'      pre.main="Species==")
#'
#' ## Use different breaks and different y-axis limits for each graph
#' hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",
#'      same.breaks=FALSE,same.ylim=FALSE)
#'
#' ## Use same but user-controlled breaks for each graph
#' hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",
#'      breaks=seq(4,8,1))
#'
#' ## Use same but user-controlled maximum value for y-axis
#' hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",ymax=30)
#'
#' ## Control the organization of the 'grid' of histograms
#' hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",nrow=1,ncol=3)
#'
#' ## Use right=FALSE & freq=FALSE to demon sending an argument used by base hist()
#' hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",right=FALSE,
#'      freq=FALSE,ymax=2)
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
#' hist(~Sepal.Length,data=iris,xlab="Sepal Length (cm)",
#'      xlim=c(3.8,8.2),ylim=c(0,35))
#' hist(~Sepal.Length,data=iris,xlab="Sepal Length (cm)",
#'      xlim=c(3.8,8.2),ymax=35)
#'
#' ## Using the bin width argument
#' hist(~Sepal.Length,data=iris,xlab="Sepal Length (cm)",w=1)
#' hist(~Sepal.Length,data=iris,xlab="Sepal Length (cm)",w=0.25)
#' hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",w=1)
#' hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",w=0.25)
#' 
#' ## Using a vector (and not a data.frame)
#' vec <- 1:100
#' hist(~vec)
#' 
#' @rdname hist.formula
#' @export
hist.formula <- function(formula,data=NULL,main="",right=FALSE,
                         pre.main="",xlab=NULL,ylab="Frequency",
                         same.breaks=TRUE,breaks="Sturges",w=NULL,
                         same.ylim=TRUE,ymax=NULL,col="gray90",
                         nrow=round(sqrt(num)),ncol=ceiling(sqrt(num)),
                         byrow=TRUE,iaxs=TRUE,...) {
  ## Handle the formula
  tmp <- iHndlFormula(formula,data)
  if (tmp$vnum>3) 
    STOP("`hist.formula' only works with 1 response and",
         " 1 or 2 explanatory variables.")
  if (tmp$vnum==1){
    ## Histogram from a single variable
    if (!tmp$vclass %in% c("numeric","integer")) STOP("Variable must be numeric.")
    if (is.null(xlab)) xlab <- tmp$vname
    # Get the variable
    resp <- tmp$mf[,1]
    # Make breaks if w is not NULL (will over-ride breaks)
    if (!is.null(w)) {
      iCheckW(w)
      startcat <- floor(min(resp,na.rm=TRUE)/w)*w
      breaks <- seq(startcat,max(resp,na.rm=TRUE)+w,w)
    }
    # Handle y-axis limits ... if ylim= in dots then just use that.
    # However, if not and ymax is given then set ylim to be c(0,ymax).
    # Otherwise leave as NULL and let hist() figure it out
    if ("ylim" %in% names(list(...))) {
      withr::local_par(list(new=FALSE)) # hack to fix #46
      h <- graphics::hist(tmp$mf[,1],xlab=xlab,ylab=ylab,
                          main=main,right=right,col=col,
                          xaxs=ifelse(iaxs,"i","r"),yaxs=ifelse(iaxs,"i","r"),
                          breaks=breaks,...)
    } else {
      if(!is.null(ymax)) ylim <- c(0,ymax)
      else ylim <- NULL
      withr::local_par(list(new=FALSE)) # hack to fix #46
      h <- graphics::hist(tmp$mf[,1],xlab=xlab,ylab=ylab,
                          main=main,right=right,col=col,
                          xaxs=ifelse(iaxs,"i","r"),yaxs=ifelse(iaxs,"i","r"),
                          breaks=breaks,ylim=ylim,...)
    }
    # assure a line at y=0
    if (iaxs & iPlotExists()) graphics::abline(h=0,xpd=FALSE)
    invisible(h)
  } else {
    ## Multiple histograms
    # Checks and work with response variable
    if (tmp$Rnum>1) STOP("LHS may contain only one variable.")
    if (!tmp$Rclass %in% c("numeric","integer"))
      STOP("LHS variable must be numeric.")
    resp <- tmp$mf[,tmp$Rpos]
    if (is.null(xlab)) xlab <- tmp$Rname
    # Checks and work with explanatory variable(s)
    if (tmp$EFactNum!=tmp$Enum) STOP("RHS may contain only factor variables.")
    if (tmp$Enum==2) {
      expl <- interaction(tmp$mf[,tmp$EFactPos[1]],tmp$mf[,tmp$EFactPos[2]])
    } else {
      expl <- tmp$mf[,tmp$EFactPos] 
    }

    ## Split the data.frame for processing and prepare to make the histograms
    DF.split <- split(resp,expl)
    num <- length(names(DF.split))
    ## Prepare to make the histograms
    # Make breaks if w is not NULL (will over-ride breaks)
    if (!is.null(w)) {
      iCheckW(w)
      startcat <- floor(min(resp,na.rm=TRUE)/w)*w
      breaks <- seq(startcat,max(resp,na.rm=TRUE)+w,w)
    } else if (same.breaks) {
      breaks <- graphics::hist(resp,right=right,plot=FALSE,warn.unused=FALSE,
                               breaks=breaks,...)$breaks 
    }
    # Handle ylims
    if (is.null(ymax)) {
      for (i in seq_len(num)) {  # find highest count on all histograms
        ymax[i] <- max(graphics::hist(DF.split[[i]],right=right,plot=FALSE,
                                      warn.unused=FALSE,breaks=breaks,...)$counts)
      }
      if (same.ylim) { ymax <- rep(max(ymax),length(ymax)) }
    } else {
      if (length(ymax)==1) ymax <- rep(ymax,num)
      else if (length(ymax)!= num)
        STOP("'ymax' argument must be 'NULL', a vector of length 1,\n",
             "or a vector of length equal to the number of groups.")
    }
    ## Make the histograms
    # nocov start
    if (num <= (nrow*ncol)) {
      if (byrow) withr::local_par(list(mfrow=c(nrow,ncol)))
        else withr::local_par(list(mfcol=c(nrow,ncol)))
      for (i in seq_len(num)) {
        if (!is.null(pre.main)) main <- paste0(pre.main,names(DF.split)[i])
        graphics:: hist(DF.split[[i]],main=main,
                        xlab=xlab,ylab=ylab,right=right,
                        ylim=c(0,ymax[i]),col=col,xaxs=ifelse(iaxs,"i","r"),
                        yaxs=ifelse(iaxs,"i","r"),breaks=breaks,...)
        # assures a line at y=0
        if (iaxs & iPlotExists()) graphics::abline(h=0,xpd=FALSE)
      }
    } else {
      max.per.page <- nrow*ncol
      if (byrow) withr::local_par(list(mfrow=c(nrow,ncol)))
        else withr::local_par(list(mfcol=c(nrow,ncol)))
      for (i in seq_len(ceiling(num/max.per.page))) {
        ifelse((num-(i-1)*max.per.page)>=max.per.page,todo <- max.per.page,
               todo <- num-(i-1)*max.per.page)
        for (j in seq_len(todo)) {
          pos <- (i-1)*max.per.page+j
          if (!is.null(pre.main)) main <- paste0(pre.main,names(DF.split)[pos])
          graphics::hist(DF.split[[pos]],main=main,
                         xlab=xlab,ylab=ylab,right=right,
                         ylim=c(0,ymax[pos]),col=col,
                         xaxs=ifelse(iaxs,"i","r"),yaxs=ifelse(iaxs,"i","r"),
                         breaks=breaks,...)
          # assures a line at y=0
          if (iaxs & iPlotExists()) graphics::abline(h=0,xpd=FALSE)
        }  
      }
    }
  } # nocov end
}
