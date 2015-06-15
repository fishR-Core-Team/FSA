#' @name growthRichards
#' 
#' @title Creates a function for a specific parameterization of the Richards growth function.
#'
#' @description Creates a function for a specific parameterization of the Richards growth function.  Use \code{RichardsModels()} to see the equations for each model.
#'
#' @param param A single numeric that indicates the parameterization of the Richards growth function.
#' @param simple A logical that indicates whether the user should be allowed to send all parameter values in the first parameter argument (\code{=FALSE}; default) or whether all individual parameters must be specified (\code{=TRUE}).
#' @param msg A logical that indicates whether a message about the model and parameter definitions should be output (\code{=TRUE}) or not (\code{=FALSE}; default).
#' @param cex A single numeric expansion value for use with \code{RichardsModels}.
#' @param \dots Not implemented.
#' 
#' @return \code{RichardsFuns} returns a function that can be used to predict fish length or weight given a vector of ages and values for the function parameters.  The result should be saved to an object that can then be used as a function name.  When the resulting function is used, the parameters are ordered as shown when the definitions of the parameters are printed after the function is called (if \code{msg=TRUE}).  If \code{simple=FALSE}, then the values for all parameters may be included as a vector in the first parameter argument.  If \code{simple=TRUE}, then all parameters must be declared individually.  The resulting function is somewhat easier to read when \code{simple=TRUE}.
#' 
#' \code{RichardsModels} returns a graphic that uses \code{\link{plotmath}} to show the model formulae in a pretty format.
#'
#' @note Within FSA, Linf is the mean asymptotic length, ti is the age at the inflection point, k is related to growth (slope at the inflection point), b is related to the vertical position of the inflection point, and L0 is the mean length at age-0.
#' 
#' The parameterizations (1-6) corresponse to models/equations 1, 4, 5, 6, 7, and 8, respectively, in Tjorve and Tjorve (2010).  Note that their A, S, k, d, and B are Linf, a, k, b, and L0 here (in FSA).
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}, thanks to Gabor Grothendieck for a hint about using \code{get()}.
#'
#' @section IFAR Chapter: None specifically, but \href{https://fishr.wordpress.com/books/ifar/}{9-Individual Growth} is related.
#'
#' @seealso See \code{\link{vbFuns}}, \code{\link{GompertzFuns}}, \code{\link{logisticFuns}}, and \code{\link{Schnute}} for similar functionality for other models.
#'
#' @references 
#' Richards, F. J.  1959.  A flexible growth function for empirical use.  Journal of Experimental Biology 10:290-300.
#' 
#' Tjorve, E. and K. M. C. Tjorve.  2010.  \href{https://www.researchgate.net/profile/Even_Tjorve/publication/46218377_A_unified_approach_to_the_Richards-model_family_for_use_in_growth_analyses_why_we_need_only_two_model_forms/links/54ba83b80cf29e0cb04bd24e.pdf}{A unified approach to the Richards-model family for use in growth analyses: Why we need only two model forms.}  Journal of Theoretical Biology 267:417-425.
#' 
#' @keywords manip
#'
#' @examples
#' ## See the formulae
#' \dontrun{windows(5,5)}
#' RichardsModels()
#' 
#' ## Simple Examples
#' ( rich1 <- RichardsFuns() )
#' ages <- 0:15
#' plot(rich1(ages,Linf=800,k=0.5,a=1,b=6)~ages,type="b",pch=19)
#'
#' ( rich2 <- RichardsFuns(2) )
#' plot(rich2(ages,Linf=800,k=0.5,ti=3,b=6)~ages,type="b",pch=19)
#'
#' ( rich3 <- RichardsFuns(3) )
#' plot(rich3(ages,Linf=800,k=0.5,ti=3,b=0.15)~ages,type="b",pch=19)
#'
#' ( rich4 <- RichardsFuns(4) )
#' plot(rich4(ages,Linf=800,k=0.5,ti=3,b=0.95)~ages,type="b",pch=19)
#' lines(rich4(ages,Linf=800,k=0.5,ti=3,b=1.5)~ages,type="b",pch=19,col="blue")
#' 
#' ( rich5 <- RichardsFuns(5) )
#' plot(rich5(ages,Linf=800,k=0.5,L0=50,b=1.5)~ages,type="b",pch=19)
#' 
#' ( rich6 <- RichardsFuns(6) )
#' plot(rich6(ages,Linf=800,k=0.5,ti=3,Lninf=50,b=1.5)~ages,type="b",pch=19)
#' 
#' ( rich2c <- RichardsFuns(2,simple=TRUE) ) # compare to rich2
#'
#' #######################################################################################
#' ## Examples of fitting Richards models
#' ##   After the last example a plot is constructed with four lines on top of each
#' ##   other illustrating that the parameterizations all produce the same fitted values.
#' # Make some fake data using the original parameterization
#' 
#' gompO <- GompertzFuns("original")
#' # setup ages, sample sizes (general reduction in numbers with
#' # increasing age), and additive SD to model
#' t <- 1:15
#' n <- c(10,40,35,25,12,10,10,8,6,5,3,3,3,2,2)
#' sd <- 15
#' # expand ages
#' ages <- rep(t,n)
#' # get lengths from gompertz and a random error for individuals
#' set.seed(333454)
#' lens <- gompO(ages,Linf=450,a=1,gi=0.3)+rnorm(length(ages),0,sd)
#' # put together as a data.frame
#' df <- data.frame(age=ages,len=round(lens,0))
#' 
#' # Fit first Richards parameterization
#' fit1 <- nls(len~rich1(age,Linf,k,a,b),data=df,start=list(Linf=450,k=0.25,a=0.65,b=3))
#' summary(fit1,correlation=TRUE)
#' plot(len~age,data=df,pch=19,col=rgb(0,0,0,1/5))
#' curve(rich1(x,Linf=coef(fit1)),from=0,to=15,col="red",lwd=10,add=TRUE)
#'
#' # Fit second Richards parameterization
#' fit2 <- nls(len~rich2(age,Linf,k,ti,b),data=df,start=list(Linf=450,k=0.25,ti=3,b=3))
#' summary(fit2,correlation=TRUE)
#' curve(rich2(x,Linf=coef(fit2)),from=0,to=15,col="blue",lwd=7,add=TRUE)
#' 
#' # Fit third Richards parameterization
#' fit3 <- nls(len~rich3(age,Linf,k,ti,b),data=df,start=list(Linf=450,k=0.25,ti=3,b=-0.3))
#' summary(fit3,correlation=TRUE)
#' curve(rich3(x,Linf=coef(fit3)),from=0,to=15,col="green",lwd=4,add=TRUE)
#' 
#' # Fit fourth Richards parameterization
#' fit4 <- nls(len~rich4(age,Linf,k,ti,b),data=df,start=list(Linf=450,k=0.25,ti=3,b=0.7))
#' summary(fit4,correlation=TRUE)
#' curve(rich4(x,Linf=coef(fit4)),from=0,to=15,col="black",lwd=1,add=TRUE)
#' 
NULL

#' @rdname growthRichards
#' @export
RichardsFuns <- function(param=1,simple=FALSE,msg=FALSE) {
  Richards1 <- function(t,Linf,k=NULL,a=NULL,b=NULL) {
    if (length(Linf)==4) { k <- Linf[[2]]
                           a <- Linf[[3]]
                           b <- Linf[[4]]
                           Linf <- Linf[[1]] }
    Linf*(1-a*exp(-k*t))^b
  }
  SRichards1 <- function(t,Linf,k,a,b) {
    Linf*(1-a*exp(-k*t))^b
  }
  Richards2 <- function(t,Linf,k=NULL,ti=NULL,b=NULL) {
    if (length(Linf)==4) { k <- Linf[[2]]
                           ti <- Linf[[3]]
                           b <- Linf[[4]]
                           Linf <- Linf[[1]] }
    Linf*(1-(1/b)*exp(-k*(t-ti)))^b
  }
  SRichards2 <- function(t,Linf,k,ti,b) {
    Linf*(1-(1/b)*exp(-k*(t-ti)))^b
  }
  Richards3 <- function(t,Linf,k=NULL,ti=NULL,b=NULL) {
    if (length(Linf)==4) { k <- Linf[[2]]
                           ti <- Linf[[3]]
                           b <- Linf[[4]]
                           Linf <- Linf[[1]] }
    Linf/((1+b*exp(-k*(t-ti)))^(1/b))
  }
  SRichards3 <- function(t,Linf,k,ti,b) {
    Linf/((1+b*exp(-k*(t-ti)))^(1/b))
  }
  Richards4 <- function(t,Linf,k=NULL,ti=NULL,b=NULL) {
    if (length(Linf)==4) { k <- Linf[[2]]
                           ti <- Linf[[3]]
                           b <- Linf[[4]]
                           Linf <- Linf[[1]] }
    Linf*(1+(b-1)*exp(-k*(t-ti)))^(1/(1-b))
  }
  SRichards4 <- function(t,Linf,k,ti,b) {
    Linf*(1+(b-1)*exp(-k*(t-ti)))^(1/(1-b))
  }
  Richards5 <- function(t,Linf,k=NULL,L0=NULL,b=NULL) {
    if (length(Linf)==4) { k <- Linf[[2]]
                           L0 <- Linf[[3]]
                           b <- Linf[[4]]
                          Linf <- Linf[[1]] }
    Linf*(1+(((L0/Linf)^(1-b))-1)*exp(-k*t))^(1/(1-b))
  }
  SRichards5 <- function(t,Linf,k,L0,b) {
    Linf*(1+(((L0/Linf)^(1-b))-1)*exp(-k*t))^(1/(1-b))
  }
  Richards6 <- function(t,Linf,k=NULL,ti=NULL,Lninf=NULL,b=NULL) {
    if (length(Linf)==5) { k <- Linf[[2]]
                           ti <- Linf[[3]]
                           Lninf <- Linf[[3]]
                           b <- Linf[[4]]
                           Linf <- Linf[[1]] }
    Lninf+(Linf-Lninf)*(1+(b-1)*exp(-k*(t-ti)))^(1/(1-b))
  }
  SRichards6 <- function(t,Linf,k,ti,Lninf,b) {
    Lninf+(Linf-Lninf)*(1+(b-1)*exp(-k*(t-ti)))^(1/(1-b))
  }
  ## Main function
  if (!param %in% 1:6) stop("'param' must be in 1:6.")
  type <- paste0("Richards",param)
  if (msg) {
    switch(type,
      Richards1= {
        message("You have chosen the '",type,"' parameterization.",
                "  E[L|t] = Linf*(1-a*exp(-k*t))^b\n\n",
                "  where Linf = asymptotic mean length\n",
                "           k = a constant that controls the slope at the inflection point\n",
                "           a = a dimensionless shape parameter\n",
                "           b = a constant that controls the y- value of the inflection point\n\n")
      },
      Richards2= {
        message("You have chosen the '",type,"' parameterization.",
                "  Linf*(1-(1/b)*exp(-k*(t-ti)))^b\n\n",
                "  where Linf = asymptotic mean length\n",
                "           k = a constant that controls the slope at the inflection point\n",
                "          ti = time/age at the inflection point\n",
                "           b = a constant that controls the y- value of the inflection point\n\n")
      },
      Richards3= {
        message("You have chosen the '",type,"' parameterization.",
                "  Linf/((1+b*exp(-k*(t-ti)))^(1/b))\n\n",
                "  where Linf = asymptotic mean length\n",
                "           k = a constant that controls the slope at the inflection point\n",
                "          ti = time/age at the inflection point\n",
                "           b = a constant that controls the y- value of the inflection point\n\n")
      },
      Richards4= {
        message("You have chosen the '",type,"' parameterization.",
                "  Linf*(1+(b-1)*exp(-k*(t-ti)))^(1/(1-b))\n\n",
                "  where Linf = asymptotic mean length\n",
                "           k = a constant that controls the slope at the inflection point\n",
                "          ti = time/age at the inflection point\n",
                "           b = a constant that controls the y- value of the inflection point\n\n")
      },
      Richards5= {
        message("You have chosen the '",type,"' parameterization.",
                "  Linf*(1+(((L0/Linf)^(1-b))-1)*exp(-k*t))^(1/(1-b))\n\n",
                "  where Linf = asymptotic mean length\n",
                "           k = a constant that controls the slope at the inflection point\n",
                "          L0 = mean length at t=0\n",
                "           b = a constant that controls the y- value of the inflection point\n\n")
      },
      Richards6= {
        message("You have chosen the '",type,"' parameterization.",
                "  Lninf+(Linf-Lninf)*(1+(b-1)*exp(-k*(t-ti)))^(1/(1-b))\n\n",
                "  where Linf = upper asymptotic mean length\n",
                "           k = a constant that controls the slope at the inflection point\n",
                "       Lninf = lower asymptotic mean length\n",
                "          ti = time/age at the inflection point\n",
                "           b = a constant that controls the y- value of the inflection point\n\n")
      }
    )
  }
  if (simple) type <- paste("S",type,sep="")
  get(type)
}

#' @rdname growthRichards
#' @export
RichardsModels <- function(cex=1,...) {
  op <- par(mar=c(0,0,3,0),cex=cex)
  plot(1,type="n",ylim=c(0,9),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",main="FSA Richards Growth Model Cases",...)
  iGrowthModels("Richards1", 0.05,8.5)
  iGrowthModels("Richards2", 0.05,7.1)
  iGrowthModels("Richards3", 0.05,5.2)
  iGrowthModels("Richards4", 0.05,3.75)
  iGrowthModels("Richards5", 0.05,2)
  iGrowthModels("Richards6", 0.05,0.25)
  par(op)
}

## iGrowthModels internal function for plotting the different models is found in vbModels().
