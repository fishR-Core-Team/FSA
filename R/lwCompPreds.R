#' @title Constructs plots of predicted weights at given lengths among different groups.
#'
#' @description Constructs plots of predicted weights at given lengths among different groups. These plots allow the user to explore differences in predicted weights at a variety of lengths when the weight-length relationship is not the same across a variety of groups.
#'
#' @param object An \code{lm} object (i.e., returned from fitting a model with \code{lm}). This model should have log(weight) as the response and log(length) as the explanatory covariate and an explanatory factor variable that describes the different groups.
#' @param lens A numeric vector that indicates the lengths at which the weights should be predicted.
#' @param qlens A numeric vector that indicates the quantiles of lengths at which weights should be predicted. This is ignored if \code{lens} is non-null.
#' @param qlens.dec A single numeric that identifies the decimal place that the lengths derived from \code{qlens} should be rounded to (Default is 1).
#' @param base A single positive numeric value that indicates the base of the logarithm used in the \code{lm} object in \code{object}. The default is \code{exp(1)}, or the value e.
#' @param interval A single string that indicates whether to plot confidence (\code{="confidence"}), prediction (\code{="prediction"}), or both (\code{="both"}) intervals.
#' @param center.value A single numeric value that indicates the log length used if the log length data was centered when constructing \code{object}.
#' @param lwd A single numeric that indicates the line width to be used for the confidence and prediction interval lines (if not \code{interval="both"}) and the prediction connections line. If \code{interval="both"} then the width of the prediction interval will be one less than this value so that the CI and PI appear different.
#' @param connect.preds A logical that indicates whether the predicted values should be connected with a line across groups or not.
#' @param show.preds A logical that indicates whether the predicted values should be plotted with a point for each group or not.
#' @param col.connect A color to use for the line that connects the predicted values (if \code{connect.preds=TRUE}).
#' @param ylim A numeric vector of length two that indicates the limits of the y-axis to be used for each plot. If null then limits will be chosen for each graph individually.
#' @param main.pre A character string to be used as a prefix for the main title. See details.
#' @param cex.main A numeric value for the character expansion of the main title. See details.
#' @param xlab A single string for labeling the x-axis.
#' @param ylab A single string for labeling the y-axis.
#' @param yaxs A single string that indicates how the y-axis is formed. See \code{par} for more details.
#' @param rows A single numeric that contains the number of rows to use on the graphic.
#' @param cols A single numeric that contains the number of columns to use on the graphic.
#' @param \dots Other arguments to pass through to the \code{plot} function.
#'
#' @return None. However, a plot is produced.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @section IFAR Chapter: 7-Weight-Length.
#'
#' @references Ogle, D.H. 2016. \href{http://derekogle.com/IFAR}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#'
#' @keywords manip
#'
#' @examples
#' # load weight-length data
#' data(ChinookArg)
#' # add log length and weight data
#' ChinookArg$logtl <- log(ChinookArg$tl)
#' ChinookArg$logwt <- log(ChinookArg$w)
#' # fit model to assess equality of slopes
#' lm1 <- lm(logwt~logtl*loc,data=ChinookArg)
#' anova(lm1)
#'
#' # set par so that the plots will look decent
#' par(mar=c(3.5,3.5,1,1),mgp=c(1.8,0.4,0),tcl=-0.2)
#' # show predicted weights (w/ CI) at the default quantile lengths for each year
#' lwCompPreds(lm1,xlab="Location")
#' # show predicted weights (w/ CI) at the quartile lengths for each year
#' lwCompPreds(lm1,xlab="Location",qlens=c(0.25,0.5,0.75))
#' # show predicted weights (w/ CI) at certain lengths for each year
#' lwCompPreds(lm1,xlab="Location",lens=c(60,90,120,150))
#' # show predicted weights (w/ just PI) at certain lengths for each year
#' lwCompPreds(lm1,xlab="Location",lens=c(60,90,120,150),interval="prediction")
#' # show predicted weights (w/ CI and PI) at certain lengths for each year
#' lwCompPreds(lm1,xlab="Location",lens=c(60,90,120,150),interval="both")
#' # show predicted weights (w/ CI and points at the prediction) at certain lengths for each year
#' lwCompPreds(lm1,xlab="Location",lens=c(60,90,120,150),show.preds=TRUE)
#' # show predicted weights (w/ CI but don't connect means) at certain lengths for each year
#' lwCompPreds(lm1,xlab="Location",lens=c(60,90,120,150),connect.preds=FALSE,show.preds=TRUE)
#'
#' # fit model with centered data
#' mn.logtl <- mean(ChinookArg$logtl,na.rm=TRUE)
#' ChinookArg$clogtl <- ChinookArg$logtl-mn.logtl
#' lm2 <- lm(logwt~clogtl*loc,data=ChinookArg)
#' lwCompPreds(lm2,xlab="Location",center.value=mn.logtl)
#' lwCompPreds(lm2,xlab="Location",lens=c(60,90,120,150),center.value=mn.logtl)
#'
#' # fit model with a different base (plot should be the same as the first example)
#' ChinookArg$logtl <- log10(ChinookArg$tl)
#' ChinookArg$logwt <- log10(ChinookArg$w)
#' lm1 <- lm(logwt~logtl*loc,data=ChinookArg)
#' lwCompPreds(lm1,base=10,xlab="Location")
#' 
#' if (interactive()) {
#'   # should give error, does not work for only a simple linear regression
#'   lm2 <- lm(logwt~logtl,data=ChinookArg)
#'   lwCompPreds(lm2)
#'   # or a one-way ANOVA
#'   lm3 <- lm(logwt~loc,data=ChinookArg)
#'   lwCompPreds(lm3)   
#' }
#' 
#' @export lwCompPreds
lwCompPreds <- function(object,lens=NULL,qlens=c(0.05,0.25,0.5,0.75,0.95),qlens.dec=1,base=exp(1),
                        interval=c("confidence","prediction","both"),center.value=0,
                        lwd=1,connect.preds=TRUE,show.preds=FALSE,col.connect="gray50",
                        ylim=NULL,main.pre="Length==",cex.main=0.8,
                        xlab="Groups",ylab="Predicted Weight",yaxs="r",
                        rows=round(sqrt(num)),cols=ceiling(sqrt(num))) {
  # check and get inerval type
  interval <- match.arg(interval)
  # check base type
  if (!is.numeric(base)) STOP("'base' must be a numeric.")
  if (length(base)!=1) STOP("'base' must be a single numeric value.")
  if (base<=0) STOP("'base' must be a positive number.")
  ## check and extract information from the formula
  formula <- 
  tmp <- iHndlFormula(stats::formula(object),stats::model.frame(object),
                      expNumR=1,expNumE=2,expNumENums=1,expNumEFacts=1)
  if (!tmp$metExpNumR) STOP("'object' formula must have only one variable on LHS.")
  if (!tmp$metExpNumE) STOP("'object' formula must have two and only two variables on RHS.")
  if (!tmp$metExpNumENums) STOP("'object' formula must have one and only one numeric variable on RHS.")
  if (!tmp$metExpNumEFacts) STOP("'object' formula must have one and only one factor variable on RHS.")

  # get the model.frame -- may not be needed
  # nocov start
  mf <- tmp$mf                                                        
  # get names of groups in last term of model (should be factor var)
  grps <- levels(mf[,tmp$EFactPos])                                                       
  # get the explanatory variable names
  vn <- tmp$Enames

  if (is.null(lens)) {
    # if no lens are provided then use provided quantile probabilities
    lens <- round(stats::quantile(base^(mf[,tmp$ENumPos]+center.value),qlens),qlens.dec)
    # must unname the lens when quartiles are used to remove later warning
    lens <- unname(lens)
  }
  # number of plots to construct
  num <- length(lens)
  # reset par so as to make nice plots
  opar <- graphics::par(mfrow=c(rows,cols))
  # cycle through the lengths
  for (i in seq_along(lens)) {
    # find results for each length
    res <- iMakeLWPred(object,lens[i],grps,vn,interval,center.value,base)
    # make plot for each length
    iPlotLWPred(res,grps,ylim,xlab,ylab,paste0(main.pre,lens[i]),
                cex.main,lwd,connect.preds,col.connect,interval,show.preds,yaxs)
  }
  graphics::par(opar)
} # nocov end


iMakeLWPred <- function(object,lens,grps,vn,interval,center.value,base) {
  #  Make a new data.frame with lengths and groups in it.
  nd <- data.frame(log(lens,base=base)-center.value,grps)
  #    label columns in the new data.frame to match model term names
  colnames(nd) <- vn
  if (interval=="both" | interval=="prediction") { #  If PI is asked for ...
    #  Predict (with PI) wt and put in a data frame
    resp <- data.frame(base^(stats::predict(object,nd,interval="prediction")))
    colnames(resp) <- c("pred","LPI","UPI")    
  }
  if (interval=="both" | interval=="confidence") { #  If CI is asked for ...
    #  Predict (with CI) wt and put in a data frame
    resc <- data.frame(base^(stats::predict(object,nd,interval="confidence")))
    colnames(resc) <- c("pred","LCI","UCI")    
  }
  #  Combine prediction, PI, and CI for wt into a results data.frame
  if (interval=="both") res <- data.frame(resp,resc[,c("LCI","UCI")])
  else if (interval=="prediction") res <- resp
  else res <- resc
  # rename row labels
  rownames(res) <- grps
  invisible(res)
} # End internal iMakeLWPred()


iPlotLWPred <- function(res,grps,ylim,xlab,ylab,main,cex.main,lwd,connect.preds,col.connect,interval,show.preds,yaxs) {  # nocov start
  #   find number of groups to plot along x-axis
  x.num <- length(grps)
  #   find y-axis range if none was provided
  if (is.null(ylim)) ylim <- range(res)
  # create a base plot
  graphics::plot(0,xlab=xlab,ylab=ylab,col="white",xlim=c(0.5,x.num+0.5),
                 ylim=ylim,xaxt="n",yaxs=yaxs)
  graphics::mtext(main,cex=cex.main)
  #   label the axis with the group labels
  graphics::axis(1,at=seq_len(x.num),labels=grps)
  if (interval=="confidence") {# if just confidence, plot in lwd black line
    with(res,plotrix::plotCI(seq_len(x.num),pred,ui=UCI,li=LCI,add=TRUE,
                             pch=ifelse(show.preds,16,"."),lwd=lwd))
  } else if (interval=="prediction") {# if just prediction, plot in lwd black line
    with(res,plotrix::plotCI(seq_len(x.num),pred,ui=UPI,li=LPI,add=TRUE,
                             pch=ifelse(show.preds,16,"."),lwd=lwd))
  } else if (interval=="both") { # if both make PI thinner and blue
    with(res,plotrix::plotCI(seq_len(x.num),pred,ui=UCI,li=LCI,add=TRUE,
                             pch=ifelse(show.preds,16,"."),lwd=lwd))
    with(res,plotrix::plotCI(seq_len(x.num),pred,ui=UPI,li=LPI,add=TRUE,
                             pch=ifelse(show.preds,16,"."),
                             lwd=ifelse(lwd>1,lwd-1,1),scol="blue"))
  }
  # connect the means if desired
  if (connect.preds) graphics::lines(seq_len(x.num),res$pred,lty=1,
                                     lwd=lwd,col=col.connect)
}  # nocov end

