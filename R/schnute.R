#' @title The four-parameter growth function from Schnute (1981).
#'
#' @description The four-parameter growth function from Schnute (1981).
#'
#' @param t A numeric vector of ages over which to model growth.
#' @param case A string that indicates the case of the Schnute growth function to use.
#' @param t1 The (young) age that corresponds to \code{L1}.  Set to minimum value in \code{t} by default.
#' @param t3 The (old) age that corresponds to \code{L3}.  Set to maximum value in \code{t} by default.
#' @param L1 The mean size/length at \code{t1}.
#' @param L3 The mean size/length at \code{t3}.
#' @param a  A dimensionless parameter that is related to the time/age at the inflection point.
#' @param gamma A dimensionless parameter that is related to size/length at the inflection point.
#' @return A predicted size/length given the case of the function and the provided parameter values..
#' 
#' @author Derek H. Ogle.
#'
#' @section IFAR Chapter: None specifically, but \href{https://fishr.wordpress.com/books/ifar/}{9-Individual Growth} is related.
#'
#' @seealso See \code{\link{schnuteModels}} for equations for each case.
#'
#' @references Schnute, J.  1981.  A versatile growth model with statistical stable parameters.  Canadian Journal of Fisheris and Aquatic Sciences 38:1128-1140.
#' 
#' @keywords manip
#'
#' @examples
#' ages <- 1:15
#' s1 <- schnute(ages,case=1,L1=30,L3=400,a=0.3,gamma=1)
#' s2 <- schnute(ages,case=2,L1=30,L3=400,a=0.3,gamma=1)
#' s3 <- schnute(ages,case=3,L1=30,L3=400,a=0.3,gamma=1)
#' s4 <- schnute(ages,case=4,L1=30,L3=400,a=0.3,gamma=1)
#'
#' plot(s1~ages,type="l",lwd=2)
#' lines(s2~ages,lwd=2,col="red")
#' lines(s3~ages,lwd=2,col="blue")
#' lines(s4~ages,lwd=2,col="green")
#' 
#' @export
schnute <- function(t,case=1,t1=min(t),t3=max(t),L1=NULL,L3=NULL,a=NULL,gamma=NULL) {
  case <- as.character(case)
  if (!case %in% c("1","2","3","4")) stop("'case' must be 1, 2, 3, or 4.",call.=FALSE)
  g <- gamma
  switch(case,
         "1"={ val <- ((L1^g)+((L3^g)-(L1^g))*((1-exp(-a*(t-t1)))/(1-exp(-a*(t3-t1)))))^(1/g) },
         "2"={ val <- L1*exp(log(L3/L1)*((1-exp(-a*(t-t1)))/(1-exp(-a*(t3-t1))))) },
         "3"={ val <- ((L1^g)+((L3^g)-(L1^g))*((t-t1)/(t3-t1)))^(1/g) },
         "4"={ val <- L1*exp(log(L3/L1)*((t-t1)/(t3-t1))) }
  )
  val
}