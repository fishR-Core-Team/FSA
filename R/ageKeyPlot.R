#' Plots to visualize age-length keys.
#' 
#' Various plots to visualize the proportion of fish of certain ages within
#' length categories in an age-length key.
#' 
#' This is a first draft of this function.  Please send suggestions to the author.
#' 
#' @param key A numeric matrix containing the age-length key.
#' @param type A character indicating the type of plot to construct.  See examples.
#' @param xlab A character label for the x-axis.
#' @param ylab A character label for the y-axis in all plots but when \code{type="bubble"}
#' (see \code{bubble.ylab=)}.
#' @param showLegend A logical indicating whether a legend should be displayed for
#' when \code{type="barplot"} and \code{type="area"}.  See examples.
#' @param lbl.cex A numeric character expansion value for labels inside the bars
#' when \code{type="barplot"} and \code{showLegend=FALSE} or on the lines when
#' \code{type="lines"} or \code{type="splines"}.
#' @param leg.cex A numeric character expansion value for labels on the legend when
#' \code{type="barplot"} or \code{type="area"} and \code{showLegend=TRUE}.
#' @param lwd A numeric that indicates the line width for when \code{type="lines"} or
#' \code{type="splines"}.
#' @param span A numeric that indicates the span value to use in \code{loess} when
#' \code{type="splines"}.
#' @param grid A logical that indicates whether a grid should be placed under
#' the bubbles when \code{type="bubble"} or a character or appropriate vector
#' that identifies a color for the grid.  See examples.
#' @param col A string indicating the color of the bubbles when \code{type="bubble"}.
#' @param buf A single numeric that indicates the relative width of the bubbles
#' when \code{type="bubble"}. A value of 0.5 would mean that two full-width bubbles
#' would touch either in the x- or y-direction (i.e., this would represent half
#' of the minimum of the physical distance between values one-unit apart on the
#' x- and y-axes).  Set this to a value less than 0.5 so that the bubble do not
#' touch (the default is 0.45).
#' @param bubble.ylab A string for labeling the y-axis when \code{type="bubble"}.
#' @param add A logical indicating whether the data should be adding to an already
#' existing plot.  May be useful for visually comparing age-length keys.  Only
#' implemented when \code{type="bubble"}.
#' @param xlim A numeric of length 2 that provide the limits for the x-axis.
#' @param ylim A numeric of length 2 that provide the limits for the y-axis.
#' @param \dots Additional arguments to pass to \code{plot}, \code{barplot}, or
#' \code{stackpoly}.
#' @return None, but a plot is constructed.
#' @seealso \code{\link{ageKey}} and \code{\link{ageKeyPrep}}.
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/AgeLengthKey.pdf}
#' @export
#' @keywords plot
#' @examples
#'## Get data with length measurements and some assigned ages -- same as in ageKey()
#'data(WR79)
#'# get the age sample from the data frame
#'WR.age <- Subset(WR79, !is.na(age))
#'# add length categories
#'WR.age.mod <- lencat(~len,data=WR.age,startcat=35,w=5,drop.levels=TRUE)
#'# create age-length key
#'raw <- table(WR.age.mod$LCat, WR.age.mod$age)
#'( WR.key <- prop.table(raw, margin=1) )
#'ageKeyPlot(WR.key,"barplot")
#'ageKeyPlot(WR.key,"barplot",showLegend=TRUE)
#'ageKeyPlot(WR.key,"area",showLegend=TRUE)
#'ageKeyPlot(WR.key,"splines")
#'ageKeyPlot(WR.key,"lines")
#'ageKeyPlot(WR.key,"bubble")
#'ageKeyPlot(WR.key,"bubble",grid=FALSE)
#'ageKeyPlot(WR.key,"bubble",grid="blue")
#'ageKeyPlot(WR.key,"bubble",grid=rgb(0,0,0,0.2),col=rgb(0,0,0,0.5))
#'
ageKeyPlot <- function(key,type=c("barplot","area","lines","splines","bubble"),
                      xlab="Length",ylab="Proportion",showLegend=FALSE,
                      lbl.cex=1.25,leg.cex=1,lwd=2,span=0.25,
                      grid=TRUE,col="gray80",buf=0.45,bubble.ylab="Age",add=FALSE,
                      xlim=NULL,ylim=NULL,...) {
  ## INTERNAL -- Identify the ages and lengths in the key and the number of each
  agesANDlens <- function(key) {
    ages <- as.numeric(colnames(key))
    num.ages <- length(ages)  
    lens <- as.numeric(rownames(key))
    num.lens <- length(lens)
    list(num.ages=num.ages,ages=ages,num.lens=num.lens,lens=lens)
  } ## end internal agesANDlens function
  ## INTERNAL -- Add age labels inside of bars on barplot
  addLabelsToBars <- function(key,alsum,lbl.cex) {
    # Make colors for the age labels inside the bars (dark on light, light on dark)
    age.clr <- rep("black",alsum$num.ages)
    age.clr[which(colMeans(col2rgb(area.clr))<120)] <- "white"
    # Add the age labels inside the bars    
    for (i in 1:alsum$num.lens) {
      if (!all(is.na(key[i,]))) { # don't put labels if length is all NA
        j <- 1
        if(key[i,j]>0) text(i-0.5,key[i,j]/2,alsum$ages[j],col=age.clr[j],cex=lbl.cex)
        prv <- key[i,j]
        while(prv<1 & j<alsum$num.ages) {
          j <- j+1
          if(key[i,j]>0) text(i-0.5,prv+key[i,j]/2,alsum$ages[j],col=age.clr[j],cex=lbl.cex)
          prv <- prv+key[i,j]
        }
      }
    }
  } ## end internal addLabelsToBars function
  ## INTERNAL -- Add age labels to lines in line and spline plots
  addLabelsToLines <- function(maxvals,lbl.cex) {
    text(maxvals[,1],maxvals[,2],maxvals[,3],cex=lbl.cex)
  } ## end internal addLabelsToLines function
  ## INTERNAL -- Setup a figure layout if a legend is added to bar or area plot
  prepLayoutForLegend <- function(){
    layout(matrix(c(2,1),nrow=2),heights=c(1,14))
  } ## end internal prepLayoutForLegend
  ## INTERNAL -- Add a legend to bar or area plot
  addLegend <- function(alsum,cex=leg.cex){
    par(mar=c(0.2,11.0,0.2,8.0))
    barplot(matrix(1,nrow=alsum$num.ages,ncol=1),horiz=TRUE,xaxt="n")
    text(c(1,alsum$num.ages)-0.5,c(0.75,0.75),range(alsum$ages),col=c("white","black"),cex=leg.cex) 
  } ## end internal addLegend function
  ## INTERNAL -- convert the key to a data.frame for the bubble plot
  unmatKey <- function(key,alsum) {
    tmpK <- data.frame(len=rep(alsum$lens,times=alsum$num.ages),
                       age=rep(alsum$ages,each=alsum$num.lens),
                       prop=as.vector(key))
    tmpK[tmpK$prop>0,]
  } ## end internal unmatKey function
  ## INTERNAL -- find inches argument (scale of radius for bubbles) for bubble plot
  bubbleIn <- function(alsum,buf) {
    # find "inches" between concurrent values on the X,Y user scales
    tmpX <- grconvertX(alsum$lens[1:2],"user","inches")
    tmpY <- grconvertY(alsum$ages[1:2],"user","inches")
    # find minimum diff in X,Y inches per 1 concurrent set of values of user scale * the buffer
    min(diff(tmpX),diff(tmpY))*buf
  } ## end internal bubbleIn function
  ## INTERNAL -- add bubles to an existing plot
  addBubbles <- function(key,alsum,buf,col) {
    tmp <- unmatKey(key,alsum)
    with(tmp,symbols(len,age,circles=sqrt(tmp$prop),inches=bubbleIn(alsum,buf),
                     bg=col,fg=rgb(0,0,0,0.5),add=TRUE))
  } ## end internal addBubbles function
  
  ## Start Main Function
  op <- par(mar=c(3.25,3.25,0.7,0.7),mgp=c(1.7,0.5,0),tcl=-0.2)
  # check key
  key.row.sum <- apply(key,1,sum)
  if (any(key.row.sum>0.01 & key.row.sum<0.99)) warning("Key contains a row that does not sum to 0 or 1.",call.=FALSE)
  # match arguments
  type <- match.arg(type)
  # Get ages and lengths summaries
  alsum <- agesANDlens(key)
  # Create colors for the areas in the bar and poly plots
  area.clr <- chooseColors("gray",alsum$num.ages)
  # Create colors for the lines in the line plots
  line.clr <- chooseColors("rich",alsum$num.ages)
  if (type=="barplot") {
    if (showLegend) prepLayoutForLegend()
    barplot(t(key),space=0,col=area.clr,xlab=xlab,ylab=ylab,...)
    if (showLegend) {
      addLegend(alsum,leg.cex)
    } else {
      addLabelsToBars(key,alsum,lbl.cex)
    }
  } else if (type=="area") {
    if (any(is.na(rowSums(key)))) stop("A stacked area plot cannot be constructed with a 'key' that has lengths with no ages.",call.=FALSE)
    else {
      prepLayoutForLegend()
      plotrix::stackpoly(key,stack=TRUE,col=area.clr,axis4=FALSE,xlab=xlab,ylab=ylab,xaxt="n",xat=0,...)
      axis(1,1:alsum$num.lens,alsum$lens)
      addLegend(alsum,leg.cex)
    }
  } else if (type=="lines") {
    if (any(is.na(rowSums(key)))) warning("A stacked area plot cannot be constructed with a 'key' that has lengths with no ages.",call.=FALSE)
    if (is.null(xlim)) xlim <- range(alsum$lens)
    if (is.null(ylim)) ylim <- c(0,1)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,...)
    maxvals <- matrix(NA,nrow=alsum$num.ages,ncol=3)
    for(i in 1:alsum$num.ages) {
      lines(alsum$lens,key[,i],col=line.clr[i],lwd=lwd)
      tmp <- min(which(key[,i]==max(key[,i],na.rm=TRUE)))
      maxvals[i,] <- c(alsum$lens[tmp],key[tmp,i],alsum$ages[i])
    }
    addLabelsToLines(maxvals,lbl.cex)
  } else if(type=="splines") { # splines
    if (is.null(xlim)) xlim <- range(alsum$lens)
    if (is.null(ylim)) ylim <- c(0,1)
    plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,...)
    plens <- seq(min(alsum$lens),max(alsum$lens),0.1)
    maxvals <- matrix(NA,nrow=alsum$num.ages,ncol=3)
    lens <- alsum$lens  # needed for making predictions below    
    for(i in 1:alsum$num.ages) {
      tmp <- key[,i]
      options(warn=-1)
      tmp <- loess(tmp~lens,span=span)
      options(warn=0)
      pprob <- predict(tmp,data.frame(lens=plens))
      lines(plens,pprob,col=line.clr[i],lwd=lwd)
      tmp <- min(which(pprob==max(pprob)))
      maxvals[i,] <- c(plens[tmp],pprob[tmp],alsum$ages[i])
    }
    addLabelsToLines(maxvals,lbl.cex)
  } else {# Bubble plot
    # if grid is a logical and is TRUE then give default color, if FALSE then set to NULL
    if (is.logical(grid)) {
      if (grid) grid <- "gray80"
      else grid <- NULL
    } 
    if (!add) {
      # not adding to an existing bubble plot, so make the base plot
      alsum <- agesANDlens(key)
      if (is.null(xlim)) xlim <- range(alsum$lens)+c(-1,1)*buf
      if (is.null(ylim)) ylim <- range(alsum$ages)+c(-1,1)*buf
      plot(NA,xlim=xlim,ylim=ylim,xlab=xlab,ylab=bubble.ylab,...)
      if (!is.null(grid)) {
        abline(h=alsum$ages,col=grid,lty=2)
        abline(v=alsum$lens,col=grid,lty=2)
      }
    }
    addBubbles(key,alsum,buf,col)
  }
  layout(1)
  par(op)
}
