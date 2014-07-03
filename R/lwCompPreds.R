#' @title Constructs plots of predicted weights at given lengths among different groups.
#'
#' @description Constructs plots of predicted weights at given lengths among different groups.  These plots allow the user to explore differences in predicted weights at a variety of lengths when the weight-length relationship is not the same across a variety of groups.
#'
#' @param object An \code{lm} object (i.e., returned from fitting a model with \code{lm}).  This model should have log(weight) as the response and log(length) as the explanatory covariate and an explanatory factor variable that describes the different groups.
#' @param lens A numeric vector that indicates the lengths at which the weights should be predicted.
#' @param quant.lens A numeric vector that indicates the quantiles of lengths at which weights should be predicted.  This is ignored if \code{lens} is non-null.
#' @param interval A single string that indicates whether to plot confidence (\code{="confidence"}), prediction (\code{="prediction"}), or both (\code{="both"}) intervals.
#' @param center.value A single numeric value that indicates the log length used if the log length data was centered when constructing \code{object}.
#' @param lwd A single numeric that indicates the line width to be used for the confidence and prediction interval lines (if not \code{interval="both"}) and the prediction connections line.  If \code{interval="both"} then the width of the prediction interval will be one less than this value so that the CI and PI appear different.
#' @param connect.preds A logical that indicates whether the predicted values should be connected with a line across groups or not.
#' @param show.preds A logical that indicates whether the predicted values should be plotted with a point for each group or not.
#' @param col.connect A color to use for the line that connects the predicted values (if \code{connect.preds=TRUE}).
#' @param ylim A numeric vector of length two that indicates the limits of the y-axis to be used for each plot.  If null then limits will be chosen for each graph individually.
#' @param main.pre A character string to be used as a prefix for the main title.  See details.
#' @param cex.main A numeric value for the character expansion of the main title.  See details.
#' @param xlab A single string for labelling the x-axis.
#' @param ylab A single string for labelling the y-axis.
#' @param rows A single numeric that contains the number of rows to use on the graphic.
#' @param cols A single numeric that contains the number of columns to use on the graphic.
#' @param \dots Other arguments to pass through to the \code{plot} function.
#'
#' @return None.  However, a plot is produced.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/LengthWeight.pdf}
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
#' lwCompPreds(lm1,xlab="Location",quant.lens=c(0.25,0.5,0.75))
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
#' if (interactive()) {
#'   # should give error, does not work for only a simple linear regression
#'   lm2 <- lm(logwt~logtl,data=ChinookArg)
#'   lwCompPreds(lm2)
#'   # or a one-way ANOVA
#'   lm3 <- lm(logwt~loc,data=ChinookArg)
#'   lwCompPreds(lm3)
#'   
#' }
#' @export lwCompPreds
lwCompPreds <- function(object,lens=NULL,quant.lens=c(0,0.25,0.5,0.75,1),
                        interval=c("confidence","prediction","both"),center.value=0,
                        lwd=1,connect.preds=TRUE,show.preds=FALSE,col.connect="gray50",
                        ylim=NULL,main.pre="Length==",cex.main=0.8,
                        xlab="Groups",ylab="Predicted Weight",
                        rows=round(sqrt(num)),cols=ceiling(sqrt(num))) {
  # check and get inerval type
  interval <- match.arg(interval)
  ## check and extract information from the formula
  formula <- 
  tmp <- iHndlFormula(formula(object),model.frame(object),expNumR=1,expNumE=2,expNumENums=1,expNumEFacts=1)
  if (!tmp$metExpNumR) stop("'object' formula must have only one variable on LHS.",call.=FALSE)
  if (!tmp$metExpNumE) stop("'object' formula must have two and only two variables on RHS.",call.=FALSE)
  if (!tmp$metExpNumENums) stop("'object' formula must have one and only one numeric variable on RHS.",call.=FALSE)
  if (!tmp$metExpNumEFacts) stop("'object' formula must have one and only one factor variable on RHS.",call.=FALSE)

  # get the model.frame -- may not be needed
  mf <- tmp$mf                                                        
  # get names of groups in last term of model (should be factor var)
  grps <- levels(mf[,tmp$EFactPos])                                                       
  # get the explanatory variable names
  vn <- tmp$Enames

  if (is.null(lens)) {
    # if no lens are provided then use provided quantile probabilities
    lens <- quantile(exp(mf[,tmp$ENumPos]+center.value),quant.lens)
    # must unname the lens when quartiles are used to remove later warning
    lens <- unname(lens)
  }
  # number of plots to construct
  num <- length(lens)
  # reset par so as to make nice plots
  old.par <- par(mfrow=c(rows,cols)); on.exit(par(old.par))
  # cycle through the lengths
  for (i in 1:length(lens)) {
    # find results for each length
    res <- iMakeLWPred(object,lens[i],grps,vn,interval,center.value)
    # make plot for each length
    iPlotLWPred(res,grps,ylim,xlab,ylab,paste(main.pre,lens[i],sep=""),
                cex.main,lwd,connect.preds,col.connect,interval,show.preds)
  }
}


iMakeLWPred <- function(object,lens,grps,vn,interval,center.value) {
  #  Make a new data.frame with lengths and groups in it.
  nd <- data.frame(log(lens)-center.value,grps)
  #    label columns in the new data.frame to match model term names
  colnames(nd) <- vn
  if (interval=="both" | interval=="prediction") { #  If PI is asked for ...
    #  Predict (with PI) wt and put in a data frame
    resp <- data.frame(exp(predict(object,nd,interval="prediction")))
    colnames(resp) <- c("pred","LPI","UPI")    
  }
  if (interval=="both" | interval=="confidence") { #  If CI is asked for ...
    #  Predict (with CI) wt and put in a data frame
    resc <- data.frame(exp(predict(object,nd,interval="confidence")))
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


iPlotLWPred <- function(res,grps,ylim,xlab,ylab,main,cex.main,lwd,connect.preds,col.connect,interval,show.preds) {
  #   find number of groups to plot along x-axis
  x.num <- length(grps)
  #   find y-axis range if none was provided
  if (is.null(ylim)) ylim=range(res)
  # create a base plot
  plot(0,xlab=xlab,ylab=ylab,col="white",xlim=c(0.5,x.num+0.5),ylim=ylim,xaxt="n")
  mtext(main,cex=cex.main)
  #   label the axis with the group labels
  axis(1,at=1:x.num,labels=grps)
  if (interval=="confidence") {# if just confidence, plot in lwd black line
    with(res,plotCI(1:x.num,pred,ui=UCI,li=LCI,add=TRUE,pch=ifelse(show.preds,16,"."),lwd=lwd))
  } else if (interval=="prediction") {# if just prediction, plot in lwd black line
    with(res,plotCI(1:x.num,pred,ui=UPI,li=LPI,add=TRUE,pch=ifelse(show.preds,16,"."),lwd=lwd))
  } else if (interval=="both") { # if both make PI thinner and blue
    with(res,plotCI(1:x.num,pred,ui=UCI,li=LCI,add=TRUE,pch=ifelse(show.preds,16,"."),lwd=lwd))
    with(res,plotCI(1:x.num,pred,ui=UPI,li=LPI,add=TRUE,pch=ifelse(show.preds,16,"."),lwd=ifelse(lwd>1,lwd-1,1),scol="blue"))
  }
  # connect the means if desired
  if (connect.preds) lines(1:x.num,res$pred,lty=1,lwd=lwd,col=col.connect)
} # End of internal iPlotLWPred

