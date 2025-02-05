#' @title The four-parameter growth function from Schnute (1981).
#'
#' @description The four-parameter growth function from Schnute (1981).
#'
#' @param t A numeric vector of ages over which to model growth.
#' @param case A string that indicates the case of the Schnute growth function to use.
#' @param L1 The mean size/length at \code{t1}.
#' @param L3 The mean size/length at \code{t3}.
#' @param a A dimensionless parameter that is related to the time/age at the inflection point.
#' @param b A dimensionless parameter that is related to size/length at the inflection point.
#' @param t1 The (young) age that corresponds to \code{L1}. Set to minimum value in \code{t} by default.
#' @param t3 The (old) age that corresponds to \code{L3}. Set to maximum value in \code{t} by default.
#' 
#' @return \code{Schnute} returns a predicted size given the case of the function and the provided parameter values.
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#'
#' @section IFAR Chapter: None specifically, but 12-Individual Growth is related.
#'
#' @seealso See \code{\link{vbFuns}}, \code{\link{GompertzFuns}}, \code{\link{RichardsFuns}}, \code{\link{logisticFuns}}, and \code{\link{SchnuteRichards}} for similar functionality for other models.
#'
#' @references Schnute, J. 1981. A versatile growth model with statistical stable parameters. Canadian Journal of Fisheries and Aquatic Sciences 38:1128-1140.
#' 
#' @keywords manip
#'
#' @examples
#' ## See the formulae
#' showGrowthFun("Schnute",case=1,plot=TRUE)
#' 
#' ## Simple examples
#' ages <- 1:15
#' s1 <- Schnute(ages,case=1,L1=30,L3=400,a=0.3,b=1)
#' s2 <- Schnute(ages,case=2,L1=30,L3=400,a=0.3,b=1)
#' s3 <- Schnute(ages,case=3,L1=30,L3=400,a=0.3,b=1)
#' s4 <- Schnute(ages,case=4,L1=30,L3=400,a=0.3,b=1)
#'
#' plot(s1~ages,type="l",lwd=2)
#' lines(s2~ages,lwd=2,col="red")
#' lines(s3~ages,lwd=2,col="blue")
#' lines(s4~ages,lwd=2,col="green")
#' 
#' @rdname Schnute
#' @export
Schnute <- function(t,case=1,L1=NULL,L3=NULL,a=NULL,b=NULL,t1=NULL,t3=NULL) {
  ## check case
  case <- as.character(case)
  if (!case %in% c("1","2","3","4")) STOP("'case' must be 1, 2, 3, or 4.")
  ## needed to get around global binding issue
  b <- b
  ## check t1 and t3
  if (length(t)==1) {
    if (is.null(t1)) STOP("Must provide a 't1' if 't' is only one value.")
    if (is.null(t3)) STOP("Must provide a 't3' if 't' is only one value.")
  } else {
    if (is.null(t1)) t1 <- min(t,na.rm=TRUE)
    if (is.null(t3)) t3 <- max(t,na.rm=TRUE)
  }
  if (t1==t3) STOP("'t1' cannot equal 't3'.")
  if (t1>t3) {
    WARN("'t1' was greater than 't3'; values reversed.")
    tmp <- t3
    t3 <- t1
    t1 <- tmp
  }
  ## check L1 and L3
  if (L1>L3) stop ("'L1' cannot be greater than 'L3'")
  ## Compute values based on case
  switch(case,
         "1"={ val <- ((L1^b)+((L3^b)-(L1^b))*((1-exp(-a*(t-t1)))/(1-exp(-a*(t3-t1)))))^(1/b) },
         "2"={ val <- L1*exp(log(L3/L1)*((1-exp(-a*(t-t1)))/(1-exp(-a*(t3-t1))))) },
         "3"={ val <- ((L1^b)+((L3^b)-(L1^b))*((t-t1)/(t3-t1)))^(1/b) },
         "4"={ val <- L1*exp(log(L3/L1)*((t-t1)/(t3-t1))) }
  )
  val
}
