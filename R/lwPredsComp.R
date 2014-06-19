#' @title Constructs plots of predicted weights at given lengths among different groups.
#'
#' @description Constructs plots of predicted weights at given lengths among different groups.  These plots allow the user to explore differences in predicted weights at a variety of lengths when the length-weight relationship is not the same across a variety of groups.
#'
#' @param mdl An \code{lm} object (i.e., returned from fitting a model with \code{lm}).  This model should have log(weight) as the response and log(length) as the explanatory covariate and an explanatory factor variable that describes the different groups.
#' @param lens A numeric that indicates the lengths at which the weights should be predicted.
#' @param quant.lens A numeric that indicates the quantiles of length to be used for predicing weight.  This is ignored if \code{lens} is non-null.
#' @param xlab A string for labelling the x-axis.
#' @param ylab A string for labelling the y-axis.
#' @param interval A string that indicates whether to plot confidence (\code{="confidence"}), prediction (\code{="prediction"}), or both (\code{="both"}) intervals.
#' @param center.value A numeric that indicates the value of log length used when centering the log length data.
#' @param ylim A numeric of length two that indicates the limits of the y-axis to be used for each plot.  If null then limits will be chosen for each graph individually.
#' @param main.pre A character string to be used as a prefix for the main title.  See details.
#' @param lwd A numeric used to indicate the line width to be used.  Note that this will be the width of the confidence interval lines and the prediction interval and mean connection lines will be one less than this value.
#' @param connect.mean A logical that indicates whether the means should be connected with a line or not.
#' @param rows A numeric that contains the number of rows to use on the graphic.
#' @param cols A numeric that contains the number of columns to use on the graphic.
#' @param \dots Other arguments to pass through to the \code{hist} function.
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
#' # load ruffe length-weight data
#' data(ChinookArg)
#' # add log length and weight data
#' ChinookArg$logtl <- log(ChinookArg$tl)
#' ChinookArg$logwt <- log(ChinookArg$w)
#' # fit model to assess equality of slopes
#' lm1 <- lm(logwt~logtl*loc,data=ChinookArg)
#' anova(lm1)
#'
#' # show predicted weights (w/ CI and PI) at the default quantile lengths for each year
#' lwPredsComp(lm1,xlab="Location")
#' # show predicted weights (w/ CI and PI) at the quartile lengths for each year
#' lwPredsComp(lm1,xlab="Location",quant.lens=c(0.25,0.5,0.75))
#' # show predicted weights (w/ CI and PI) at certain lengths for each year
#' lwPredsComp(lm1,xlab="Location",lens=c(60,90,120,150))
#' # show predicted weights (just PI) at certain lengths for each year
#' lwPredsComp(lm1,xlab="Locatoin",lens=c(60,90,120,150),interval="p")
#' # show predicted weights (just CI) at certain lengths for each year
#' lwPredsComp(lm1,xlab="Location",lens=c(60,90,120,150),interval="c")
#'
#' # fit model with centered data
#' mn.logtl <- mean(ChinookArg$logtl,na.rm=TRUE)
#' ChinookArg$clogtl <- ChinookArg$logtl-mn.logtl
#' lm2 <- lm(logwt~clogtl*loc,data=ChinookArg)
#' lwPredsComp(lm2,xlab="Location",center.value=mn.logtl)
#' lwPredsComp(lm2,xlab="Location",lens=c(60,90,120,150),center.value=mn.logtl)
#'
#' @export
lwPredsComp <- function(mdl,lens=NULL,quant.lens=c(0,0.25,0.5,0.75,1),
                         interval=c("both","confidence","prediction"),
                         center.value=0,
                         xlab="Groups",ylab="Predicted Weight",
                         ylim=NULL,main.pre="Length==",lwd=2,connect.mean=TRUE,
                         rows=round(sqrt(num)),cols=ceiling(sqrt(num))) {
  # INTERNAL -- Make predictions
  make.pred <- function(mdl,lens,grps,vn,interval,center.value) {
    #  Make a new data.frame with lengths and groups in it.
    nd <- data.frame(log(lens)-center.value,grps)
    #    label columns in the new data.frame to match model term names
    colnames(nd) <- vn
    if (interval=="both" | interval=="prediction") { #  If PI is asked for ...
      #  Predict (with PI) wt and put in a data frame
      resp <- data.frame(exp(predict(mdl,nd,interval="p")))
      colnames(resp) <- c("pred","LPI","UPI")    
    }
    if (interval=="both" | interval=="confidence") { #  If CI is asked for ...
      #  Predict (with CI) wt and put in a data frame
      resc <- data.frame(exp(predict(mdl,nd,interval="c")))
      colnames(resc) <- c("pred","LCI","UCI")    
    }
    #  Combine prediction, PI, and CI for wt into a results data.frame
    if (interval=="both") res <- data.frame(resp,resc[,c("LCI","UCI")])
      else if (interval=="prediction") res <- resp
        else res <- resc
    # rename row labels
    rownames(res) <- grps
    invisible(res)
  } # End internal make.pred()
  
  # INTERNAL -- Make sub-plot
  plot.pred <- function(res,grps,ylim,xlab,ylab,main,lwd,interval) {
    #   find number of groups to plot along x-axis
    x.num <- length(grps)
    #   find y-axis range if none was provided
    if (is.null(ylim)) ylim=range(res)
    # create a base plot
    plot(0,xlab=xlab,ylab=ylab,col="white",xlim=c(0.5,x.num+0.5),ylim=ylim,xaxt="n",main=main)
    #   label the axis with the group labels
    axis(1,at=1:x.num,labels=grps)
    #   plot the prediction intervals in black, thinner lines
    if (interval=="both" | interval=="prediction") with(res,plotCI(1:x.num,pred,ui=UPI,li=LPI,add=TRUE,pch=19,lwd=lwd-1))
    # plot the confidence intervals in blue, thicker lines
    if (interval=="both" | interval=="confidence") with(res,plotCI(1:x.num,pred,ui=UCI,li=LCI,add=TRUE,pch=19,lwd=lwd,scol="blue"))
    #   connect the means if desired
    if (connect.mean) lines(1:x.num,res$pred,lty=3,lwd=lwd-1)
  } # End of internal plot.pred

  interval <- match.arg(interval)
  # extract formula from model
  formula <- formula(mdl)
  # get the model.frame -- may not be needed
  mf <- model.frame(mdl)                                                        
  # get names of groups in last term of model (should be factor var)
  grps <- levels(mf[,3])                                                       
  # get the explanatory variable names
  vn <- attr(delete.response(terms(formula)),"term.labels")[1:2]

  if (is.null(lens)) {
    # if no lens are provided then use provided quantile probabilities
    lens <- quantile(exp(mf[,2]+center.value),quant.lens)
    # must unname the lens when quartiles are used to remove later warning
    lens <- unname(lens)
  }
  # number of plots to construct
  num <- length(lens)
  # reset par so as to make nice plots
  old.par <- par(mfrow=c(rows,cols),mar=c(4,4,1.5,1.5),mgp=c(2,0.75,0)); on.exit(par(old.par))
  # cycle through the lengths
  for (i in 1:length(lens)) {
    # find results for each length
    res <- make.pred(mdl,lens[i],grps,vn,interval,center.value)
    # make plot for each length
    plot.pred(res,grps,ylim,xlab,ylab,paste(main.pre,lens[i],sep=""),lwd,interval)
  }
}
