#'Constructs plots of predicted weights at given lengths among different groups.
#'
#'Constructs plots of predicted weights at given lengths among different groups.
#'These plots allow the user to explore differences in predicted weights at a variety
#'of lengths when the length-weight relationship is not the same across a
#'variety of groups.
#'
#'@param mdl An \code{lm} object (i.e., returned from fitting a model with
#'\code{lm}).  This model should have log(weight) as the response and
#'log(length) as the explanatory covariate and an explanatory factor variable
#'that describes the different groups.
#'@param lens A numeric that indicates the lengths at which the weights should be predicted.
#'@param quant.lens A numeric that indicates the quantiles of length to be used for
#'predicing weight.  This is ignored if \code{lens} is non-null.
#'@param xlab A string for labelling the x-axis.
#'@param ylab A string for labelling the y-axis.
#'@param interval A string that indicates whether to plot confidence
#'(\code{="confidence"}), prediction (\code{="prediction"}), or both (\code{="both"}) intervals.
#'@param center.value A numeric that indicates the value of log length used when
#'centering the log length data.
#'@param ylim A numeric of length two that indicates the limits of the y-axis
#'to be used for each plot.  If null then limits will be chosen for each graph individually.
#'@param main.pre A character string to be used as a prefix for the main title.
#'See details.
#'@param lwd A numeric used to indicate the line width to be used.  Note that
#'this will be the width of the confidence interval lines and the prediction
#'interval and mean connection lines will be one less than this value.
#'@param connect.mean A logical that indicates whether the means should be
#'connected with a line or not.
#'@param rows A numeric that contains the number of rows to use on the graphic.
#'@param cols A numeric that contains the number of columns to use on the graphic.
#'@param \dots Other arguments to pass through to the \code{hist} function.
#'@return None.  However, a plot is produced.
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/LengthWeight.pdf}
#'@export
#'@keywords manip
#'@examples
#'# load ruffe length-weight data
#'data(RuffeWs)
#'# extract just data from the St. Louis River Harbor
#'slrh <- subset(RuffeWs,grepl("St. Louis",RuffeWs$locShort))
#'# add log length and weight data
#'slrh$logtl <- log(slrh$tl)
#'slrh$logwt <- log(slrh$wt)
#'# create a year factor variable
#'slrh$fyear <- factor(slrh$year)
#'# fit model to assess equality of slopes
#'lm1 <- lm(logwt~logtl*fyear,data=slrh)
#'anova(lm1)
#'
#'# show predicted weights (w/ CI and PI) at the default quantile lengths for each year
#'lwPredsComp(lm1,xlab="Year")
#'# show predicted weights (w/ CI and PI) at the quartile lengths for each year
#'lwPredsComp(lm1,xlab="Year",quant.lens=c(0.25,0.5,0.75))
#'# show predicted weights (w/ CI and PI) at certain lengths for each year
#'lwPredsComp(lm1,xlab="Year",lens=c(60,90,120,150))
#'# show predicted weights (just PI) at certain lengths for each year
#'lwPredsComp(lm1,xlab="Year",lens=c(60,90,120,150),interval="p")
#'# show predicted weights (just CI) at certain lengths for each year
#'lwPredsComp(lm1,xlab="Year",lens=c(60,90,120,150),interval="c")
#'
#'# fit model with centered data
#'mn.logtl <- mean(slrh$logtl,na.rm=TRUE)
#'slrh$clogtl <- slrh$logtl-mn.logtl
#'lm2 <- lm(logwt~clogtl*fyear,data=slrh)
#'lwPredsComp(lm2,xlab="Year",center.value=mn.logtl)
#'lwPredsComp(lm2,xlab="Year",lens=c(60,90,120,150),center.value=mn.logtl)
#'
lwPredsComp <- function(mdl,lens=NULL,quant.lens=c(0,0.25,0.5,0.75,1),
                         interval=c("both","confidence","prediction"),
                         center.value=0,
                         xlab="Groups",ylab="Predicted Weight",
                         ylim=NULL,main.pre="Length==",lwd=2,connect.mean=TRUE,
                         rows=round(sqrt(num)),cols=ceiling(sqrt(num))) {
  make.pred <- function(mdl,lens,grps,vn,interval,center.value) {               # Make predictions sub-function
    nd <- data.frame(log(lens)-center.value,grps)                               #  Make a new data.frame with lengths and groups in it.
    colnames(nd) <- vn                                                          #    label columns in the new data.frame to match model term names
    if (interval=="both" | interval=="prediction") {                            #  If PI is asked for ...
      plogwt <- predict(mdl,nd,interval="p")                                    #    Predict (with PI) logwt
      pwt <- exp(plogwt)                                                        #    Predict (with PI) wt
      resp <- data.frame(pwt)                                                   #    Put results into a data.frame
      colnames(resp) <- c("pred","LPI","UPI")                                   #    rename column labels    
    }
    if (interval=="both" | interval=="confidence") {                            #  If CI is asked for ...
      clogwt <- predict(mdl,nd,interval="c")                                    #    Predict (with CI) logwt
      cwt <- exp(clogwt)                                                        #    Predict (with CI) wt
      resc <- data.frame(cwt)                                                   #    Put results into a data.frame
      colnames(resc) <- c("pred","LCI","UCI")                                   #    Rename column labels    
    }
    if (interval=="both") res <- data.frame(resp,resc[,c("LCI","UCI")])         #  Combine prediction, PI, and CI for wt into a results data.frame
      else if (interval=="prediction") res <- resp
        else res <- resc
    rownames(res) <- grps                                                       #    rename row labels
    invisible(res)                                                              #  Return results data.frame
  }                                                                             # End of make.pred()
  
  plot.pred <- function(res,grps,ylim,xlab,ylab,main,lwd,interval) {            # Make sub-plot sub-function
    x.num <- length(grps)                                                       #   find number of groups to plot along x-axis
    if (is.null(ylim)) ylim=range(res)                                          #   find y-axis range if none was provided
    plot(0,xlab=xlab,ylab=ylab,col="white",xlim=c(0.5,x.num+0.5),ylim=ylim,xaxt="n",main=main)  # create a base plot
    axis(1,at=1:x.num,labels=grps)                                              #   label the axis with the group labels
    if (interval=="both" | interval=="prediction")
      {plotCI(1:x.num,res$pred,ui=res$UPI,li=res$LPI,add=TRUE,pch=19,lwd=lwd-1)}#   plot the prediction intervals in black, thinner lines
    if (interval=="both" | interval=="confidence")
      {plotCI(1:x.num,res$pred,ui=res$UCI,li=res$LCI,add=TRUE,pch=19,lwd=lwd,scol="blue")}  # plot the confidence intervals in blue, thicker lines
    if (connect.mean) lines(1:x.num,res$pred,lty=3,lwd=lwd-1)                   #   connect the means if desired
  }                                                                             # End of plot.pred

  interval <- match.arg(interval)
  formula <- formula(mdl)                                                       # extract formula from model
  mf <- model.frame(mdl)                                                        # get the model.frame -- may not be needed
  grps <- levels(mf[,3])                                                        # get names of groups in last term of model (should be factor var)
  vn <- attr(delete.response(terms(formula)),"term.labels")[1:2]                # get the explanatory variable names

  if (is.null(lens)) {                                                          # if no lens are provided then ...
    lens <- quantile(exp(mf[,2]+center.value),quant.lens)                                    #   use provided quantile probabilities
    lens <- unname(lens)                                                        # must unname the lens when quartiles are used to remove later warning
  }
  num <- length(lens)                                                           # number of plots to construct
  old.par <- par(mfrow=c(rows,cols),mar=c(4,4,1.5,1.5),mgp=c(2,0.75,0)); on.exit(par(old.par))   # reset par so as to make nice plots
  for (i in 1:length(lens)) {                                                   # cycle through the lengths
    res <- make.pred(mdl,lens[i],grps,vn,interval,center.value)                 #   find results for each length
    plot.pred(res,grps,ylim,xlab,ylab,paste(main.pre,lens[i],sep=""),lwd,interval) #   make plot for each length
  }
}
