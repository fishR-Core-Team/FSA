#'Creates a function for a specific stock-recruitment model parameterization.
#'
#'Creates a function for a specific stock-recruitment model parameterization.
#'
#' @param type A string that indicates the type of the stock-recruitment model.
#' @param param A numeric that indicates the parameterization of the stock-recruitment model type.
#' @param msg A logical that indicates whether a message about the model and parameter definitions should be output.
#'
#' @return A function that can be used to predict recruitment given a vector of stock sizes and values for the model parameters.  The result should be saved to an object that can then be used as a function name.  When the resulting function is used the parameters are ordered as shown when the definitions of the parameters are printed after the function is called (assuming that \code{msg=TRUE}).  The values for both parameters can be included as a vector of length two in the first parameter argument.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}.  Thanks to Greg Snow for a hint about returning the functions.
#'
#' @seealso \code{\link{srStarts}}, \code{\link{srModels}}, and \code{\link{srSim}}
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/StockRecruit.pdf}
#'
#' @references Beverton, R.J.H. and S.J. Holt.  1957.  On the dynamics of exploited fish populations, Fisheries Investigations (Series 2), volume 19. United Kingdom Ministry of Agriculture and Fisheries, 533 pp.
#'
#'Quinn II, T.J. and R.B. Deriso. 1999. Quantitative Fish Dynamics. Oxford University Press.
#'
#'Ricker, W.E. 1954. Stock and recruitment. Journal of the Fisheries Research Board of Canada 11:559-623.
#'
#'Ricker, W.E. 1975. \href{http://www.dfo-mpo.gc.ca/Library/1485.pdf}{Computation and interpretation of biological statistics of fish populations}. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada.
#' 
#' @keywords manip
#' 
#' @examples
#'## Simple Examples
#'( bh1 <- srFuns() )               # Beverton-Holt #1 parameterization
#'stock <- sample(1:1000,50)
#'stock <- stock[order(stock)]
#'plot(bh1(stock,a=0.5,b=0.01)~stock,type="b",pch=19)
#'
#'( r1 <- srFuns("Ricker") )       # Ricker #1 parameterization
#'points(r1(stock,a=0.5,b=0.005)~stock,type="b",pch=19,col="red")
#'
#'## Examples of fitting stock-recruitment models
#'data(CodNorwegian)
#'
#'# Fitting the Beverton-Holt #1 parameterization with multiplicative errors
#'bh1s <- srStarts(recruits~stock,data=CodNorwegian)
#'fit1 <- nls(log(recruits)~log(bh1(stock,a,b)),data=CodNorwegian,start=bh1s)
#'summary(fit1,correlation=TRUE)
#'plot(recruits~stock,data=CodNorwegian,pch=19,xlim=c(0,200))
#'curve(bh1(x,a=coef(fit1)[1],b=coef(fit1)[2]),from=0,to=200,col="red",lwd=3,add=TRUE)
#'
#'# Fitting the Ricker #3 parameterization with multiplicative errors
#'r3 <- srFuns("Ricker",param=3)
#'r3s <- srStarts(recruits~stock,data=CodNorwegian,type="Ricker",param=3)
#'fit2 <- nls(log(recruits)~log(r3(stock,a,Rp)),data=CodNorwegian,start=r3s)
#'summary(fit2,correlation=TRUE)
#'curve(r3(x,a=coef(fit2)[1],Rp=coef(fit2)[2]),from=0,to=200,col="blue",lwd=3,add=TRUE)
#'
#' @export
srFuns <- function(type=c("BevertonHolt","Ricker"),param=1,msg=FALSE) {
  type <- match.arg(type)
  switch(type,
    BevertonHolt={
      if (is.na(match(param,1:4))) stop("'param' argument must be in 1:4 when type='BevertonHolt'.",call.=FALSE)
      if (param==1) {
        if (msg) {
          cat("You have chosen the first parameterization of the 'Beverton-Holt' stock-recruitment model.\n\n")
          cat("  E[R|S] = a*S/(1+b*S)\n\n")
          cat("where a = the density-independent slope near S=0.\n")
           cat("      b = density-dependent parameter.\n\n")
         }
         function(S,a,b=NULL) {
           if (length(a)>1) { b <- a[2]; a <- a[1] }
           a*S/(1+b*S)
         }
      } else if (param==2) {
        if (msg) {
          cat("You have chosen the second parameterization of the 'Beverton-Holt' stock-recruitment model.\n\n")
          cat("  E[R|S] = a*S/(1+a*(S/Rp))\n\n")
          cat("where a = the density-independent slope near S=0.\n")
          cat("      Rp = peak recruitment at the asymptote.\n\n")
        }
        function(S,a,Rp=NULL) {
          if (length(a)>1) { Rp <- a[2]; a <- a[1] }
          a*S/(1+a*S/Rp)
        }
      } else if (param==3) {
        if (msg) {
          cat("You have chosen the third parameterization of the 'Beverton-Holt' stock-recruitment model.\n\n")
          cat("  E[R|S] = S/(a+b*S)\n\n")
        }
        function(S,a,b=NULL) {
          if (length(a)>1) { b <- a[2]; a <- a[1] }
          S/(a+b*S)
        }
      } else {
        if (msg) {
          cat("You have chosen the fourth parameterization of the 'Beverton-Holt' stock-recruitment model.\n\n")
          cat("  E[R|S] = S/(a+S/Rp)\n\n")
          cat("where Rp = peak recruitment at the asymptote.\n\n")
        }
        function(S,a,Rp=NULL) {
          if (length(a)>1) { Rp <- a[2]; a <- a[1] }
          S/(a+S/Rp)
        }
      }
    },  # end B-H
    Ricker={
      if (is.na(match(param,1:3))) stop("'param' argument must be in 1:3 when type='Ricker'.",call.=FALSE)
      if (param==1) {
        if (msg) {
          cat("You have chosen the first parameterization of the 'Ricker' stock-recruitment model.\n\n")
          cat("  E[R|S] = a*S*exp(-b*S)\n\n")
          cat("where a = the density-independent slope near S=0.\n")
          cat("      b = density-dependent parameter.\n\n")
        }
        function(S,a,b=NULL) {
          if (length(a)>1) { b <- a[2]; a <- a[1] }
          a*S*exp(-b*S)
        }
      } else if (param==2) {
        if (msg) {
          cat("You have chosen the second parameterization of the 'Ricker' stock-recruitment model.\n\n")
          cat("  E[R|S] = S*exp(a-b*S)\n\n")
          cat("where b = density-dependent parameter.\n\n")
        }
        function(S,a,b=NULL) {
          if (length(a)>1) { b <- a[2]; a <- a[1] }
          S*exp(a-b*S)
        }
      } else {
        if (msg) {
          cat("You have chosen the third parameterization of the 'Ricker' stock-recruitment model.\n\n")
          cat("  E[R|S] = a*S*exp(-a*S/(e*Rp))\n\n")
          cat("where a = the density-independent slope near S=0.\n")
          cat("      Rp = peak level of recruitment.\n\n")
        }
        function(S,a,Rp=NULL) {
          if (length(a)>1) { Rp <- a[2]; a <- a[1] }
          a*S*exp(-a/exp(1)*S/Rp)
        }
      }
    }  # end Ricker
  )  # end type switch
}
