#'Constructs skeleton plots of radial or proportional radial measurements made
#'at annuli on fish calcified structures.
#'
#'Constructs skeleton plots of radial or proportional radial measurements made
#'at annuli on fish calcified structures.  These plots can be used to identify
#'clearly incorrect radial measurements.
#'
#'This function constructs a plot of horizonal lines for each individual with
#'the annular increments marked with vertical lines.  This is simply an
#'exploratory graph to allow visual comparison of patterns among individuals.
#'
#'This function requires one-fish-per-line radial measurement data.  See
#'\code{\link{gReshape}} and \code{\link{gConvert}} if your data is
#'one-increment-per-line or increment measurements.  This function also assumes
#'that the data frame has the total radius-at-capture and the fish's age.  See
#'\code{\link{addRadCap}} to add the total radius at capture variable (if your
#'data was increments).
#'
#'@param df A data frame that contains the radial growth measurement data in
#'one-fish-per-line format.
#'@param indivs A string that contains the variable name or a vector of strings
#'that contains the variable names that identify individual fish in the data frame.
#'@param in.pre A string that indicates the prefix for all variable names
#'that contains the radial growth measurement data in the input data frame.
#'@param in.var A vector of column numbers or variable names that contain the
#'radial growth measurement data in the input data frame.
#'@param radcap A string that contains the variable name that holds the total
#'structure radius at the time of capture.
#'@param agevar A string that contains the variable name that holds the assigned ages.
#'@param proportions A logical that indicates whether raw radial measurements
#'(\code{=FALSE}, default) or proportional (of the total radius) measurements
#'are plotted.
#'@param numperpg A numeric that indicates the number of individuals to plot per page.
#'@param xlab A string for labeling the x-axis.
#'@param lwd A numeric that indicates the width of the lines plotted.
#'@param col A string that indicates the color to use for the plotted lines.
#'@param pch A numeric that indicates the plotting character used to mark the
#'annuli.  The defaul (\code{=124}) is a vertical line.
#'@param ymar A numeric that indicates how wide the y-axis margin should be for
#'labelling the individuals.
#'@param \dots Optional arguments sent to the \code{lines()} or \code{points()}.
#'@return None.  However, a plot is produced.
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@seealso \code{\link{gReshape}}, \code{\link{gConvert}}
#'@export
#'@keywords manip
#'@examples
#'data(SMBassWB)
#'head(SMBassWB)     # to see column names & some data
#'## the following is not run because it requires interaction by the user
#'\dontrun{
#'growthRadPlot(SMBassWB,c("yearcap","gear","fish"),in.pre="anu",radcap="radcap",
#'  agevar="agecap",ymar=6)
#'}
#'
growthRadPlot <- function(df,indivs,in.pre="rad",in.var,radcap="radcap",agevar="age",
                          proportions=FALSE,numperpg=20,xlab=ifelse(proportions,
                          "Radial Measurements","Proportions of Total Radius"),
                          lwd=2,col="black",pch=124,ymar=5,...) {

 # find the variables in df that contain the increments
  if (missing(in.pre) & missing(in.var)) stop("You must use one of the in.pre or in.var arguments.",call.=FALSE)
  if (!missing(in.pre) & !missing(in.var)) warning("Both in.var and cols arguments were used.  Only the in.var argument will be used.",call.=FALSE)
  if (!missing(in.pre) & missing(in.var)) { in.var <- grep(in.pre,names(df)) }
  
  if (length(indivs)>1) ylabs <- apply(df[,indivs],1,paste,collapse=".")                          # combine labels if >1 item is sent to indivs= argument
    else ylabs <- df[,indivs]

 # make the plot    
  op <- par(mar=c(3,ymar,1,3),mgp=c(1.75,0.75,0),ask=TRUE); on.exit(par(op))
  yvals <- -(1:numperpg)                                                                          # set values for y-axis of the plots
  ifelse(proportions,max.x <- 1,max.x <- max(df[,radcap]))                                        # set maximum for x-axis
      # x-axis label depends on measure type
  for (i in 1:ceiling(dim(df)[1]/numperpg) ) {                                                    # loop through pages of plots
    plot(1,xlab=xlab,ylab="",ylim=range(yvals),xlim=c(0,max.x),col="white",yaxt="n")              #   set up plot schematic
    for (j in (numperpg*(i-1)+1):(numperpg*i)) {                                                  #   loop through fish to be put on current page
       xvals <- df[j,in.var]                                                                      #     get increment values for this fish
       xvals <- c(0,xvals[!is.na(xvals)])                                                         #     make fist value a zero (to start) and remove NAs
       if (proportions) xvals <- xvals/df[j,radcap]                                               #     convert to proportions if measure==proportion
       if (length(xvals)>1) {                                                                     #     only put line on if there is a line to plot
         if (proportions) lines(c(0,1),rep(yvals[j-(numperpg*(i-1))],2),lwd=lwd,col=col,...)      #     put the line down
           else lines(c(0,df[j,radcap]),rep(yvals[j-(numperpg*(i-1))],2),lwd=lwd,col=col,...)
         points(xvals,rep(yvals[j-(numperpg*(i-1))],length(xvals+1)),lwd=lwd,col=col,pch=pch,...) #     mark increments with a vertical line
       }                                                                                          
    }                                                                                             #   end fish on current page loop (j)
    axis(2,at=yvals,labels=ylabs[(numperpg*(i-1)+1):(numperpg*i)],las=1)                          #   put y-axis label of fish id
    mtext(paste(indivs,collapse="."),at=grconvertX(-0.075,"npc"),xpd=TRUE)
    axis(4,at=yvals,labels=df[(numperpg*(i-1)+1):(numperpg*i),agevar],las=1)
    mtext("age",at=grconvertX(1.05,"npc"),xpd=TRUE)                                               
  }                                                                                               # end pages loop (i)

}
                                        
