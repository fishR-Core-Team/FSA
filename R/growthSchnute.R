#' @name growthSchnute
#' 
#' @title The four-parameter growth function from Schnute (1981).
#'
#' @description The four-parameter growth function from Schnute (1981).  Use \code{schnuteModels()} to see the equations for each model.
#'
#' @param t A numeric vector of ages over which to model growth.
#' @param case A string that indicates the case of the Schnute growth function to use.
#' @param t1 The (young) age that corresponds to \code{L1}.  Set to minimum value in \code{t} by default.
#' @param t3 The (old) age that corresponds to \code{L3}.  Set to maximum value in \code{t} by default.
#' @param L1 The mean size/length at \code{t1}.
#' @param L3 The mean size/length at \code{t3}.
#' @param a  A dimensionless parameter that is related to the time/age at the inflection point.
#' @param b A dimensionless parameter that is related to size/length at the inflection point.
#' @param \dots Not implemented.
#' 
#' @return \code{schnute} returns a predicted size/length given the case of the function and the provided parameter values.
#' 
#' \code{schnuteModels} returns a graphic that uses \code{\link{plotmath}} to show the model formulae in a pretty format.
#' 
#' @author Derek H. Ogle.
#'
#' @section IFAR Chapter: None specifically, but \href{https://fishr.wordpress.com/books/ifar/}{9-Individual Growth} is related.
#'
#' @seealso See \code{\link{vbFuns}}, \code{\link{gompFuns}}, and \code{\link{logisticFuns}} for similar functionality for other models.
#'
#' @references Schnute, J.  1981.  A versatile growth model with statistical stable parameters.  Canadian Journal of Fisheris and Aquatic Sciences 38:1128-1140.
#' 
#' @keywords manip
#'
#' @examples
#' ## See the formulae
#' \dontrun{windows(5,5)}
#' schnuteModels()
#' 
#' ## Simple examples
#' ages <- 1:15
#' s1 <- schnute(ages,case=1,t1=1,t3=15,L1=30,L3=400,a=0.3,b=1)
#' s2 <- schnute(ages,case=2,t1=1,t3=15,L1=30,L3=400,a=0.3,b=1)
#' s3 <- schnute(ages,case=3,t1=1,t3=15,L1=30,L3=400,a=0.3,b=1)
#' s4 <- schnute(ages,case=4,t1=1,t3=15,L1=30,L3=400,a=0.3,b=1)
#'
#' plot(s1~ages,type="l",lwd=2)
#' lines(s2~ages,lwd=2,col="red")
#' lines(s3~ages,lwd=2,col="blue")
#' lines(s4~ages,lwd=2,col="green")
#' 
NULL

#' @rdname growthSchnute
#' @export
schnute <- function(t,case=1,t1=NULL,t3=NULL,L1=NULL,L3=NULL,a=NULL,b=NULL) {
  ## check case
  case <- as.character(case)
  if (!case %in% c("1","2","3","4")) stop("'case' must be 1, 2, 3, or 4.",call.=FALSE)
  ## needed to get around global binding issue
  b <- b
  ## check t1 and t3
  if (length(t)==1) {
    if (is.null(t1)) stop("Must provide a 't1' if 't' is only one value.",call.=FALSE)
    if (is.null(t3)) stop("Must provide a 't3' if 't' is only one value.",call.=FALSE)
  } else {
    if (is.null(t1)) t1 <- min(t,na.rm=TRUE)
    if (is.null(t3)) t3 <- max(t,na.rm=TRUE)
  }
  if (t1==t3) stop("'t1' cannot equal 't3'.",call.=FALSE)
  if (t1>t3) {
    warning("'t1' was greater than 't3'; values reversed.",call.=FALSE)
    tmp <- t3
    t3 <- t1
    t1 <- tmp
  }
  ## check L1 and L3
  if (L1>L3) stop ("'L1' cannot be greater than 'L3'",call.=FALSE)
  ## Compute values based on case
  switch(case,
         "1"={ val <- ((L1^b)+((L3^b)-(L1^b))*((1-exp(-a*(t-t1)))/(1-exp(-a*(t3-t1)))))^(1/b) },
         "2"={ val <- L1*exp(log(L3/L1)*((1-exp(-a*(t-t1)))/(1-exp(-a*(t3-t1))))) },
         "3"={ val <- ((L1^b)+((L3^b)-(L1^b))*((t-t1)/(t3-t1)))^(1/b) },
         "4"={ val <- L1*exp(log(L3/L1)*((t-t1)/(t3-t1))) }
  )
  val
}

#' @rdname growthSchnute
#' @export
schnuteModels <- function(...) {
  op <- par(mar=c(0,0,3,0),cex=1.25)
  plot(1,type="n",ylim=c(0,4),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",main="FSA Schnute Growth Model Cases",...)
  iGrowthModels("Schnute1", 0.1,3.5)
  iGrowthModels("Schnute2", 0.1,2.5)
  iGrowthModels("Schnute3", 0.1,1.5)
  iGrowthModels("Schnute4", 0.1,0.5)
  par(op)
}

## iGrowthModels internal function for plotting the different models is found in vbModels().
