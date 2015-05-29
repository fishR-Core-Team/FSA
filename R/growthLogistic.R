#' @name growthLogistic
#' 
#' @title Creates a function for a specific parameterization of the logistic growth function.
#'
#' @description Creates a function for a specific parameterization of the logistic growth function.  Use \code{logisticModels()} to see the equations for each model.
#'
#' @param type A string that indicates the parameterization of the logistic growth function.
#' @param simple A logical that indicates whether the user should be allowed to send all parameter values in the first parameter argument (\code{=FALSE}; default) or whether all individual parameters must be specified (\code{=TRUE}).
#' @param msg A logical that indicates whether a message about the model and parameter definitions should be output (\code{=TRUE}) or not (\code{=FALSE}; default).
#' @param \dots Not implemented.
#' 
#' @return \code{logisticFuns} returns a function that can be used to predict fish length or weight given a vector of ages and values for the function parameters.  The result should be saved to an object that can then be used as a function name.  When the resulting function is used, the parameters are ordered as shown when the definitions of the parameters are printed after the function is called (if \code{msg=TRUE}).  #'If \code{simple=FALSE}, then the values for all parameters may be included as a vector in the first parameter argument.  If \code{simple=TRUE}, then all parameters must be declared individually.  The resulting function is somewhat easier to read when \code{simple=TRUE}.
#' 
#' \code{logisticModels} returns a graphic that uses \code{\link{plotmath}} to show the model formulae in a pretty format.
#'
#' @note Within FSA, L0 is the mean length at age 0, Linf is the mean asymptotic length, ti is the age at the inflection point, gninf is the instantaneous growth rate at negative infinity.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}, thanks to Gabor Grothendieck for a hint about using \code{get()}.
#'
#' @section IFAR Chapter: None specifically, but \href{https://fishr.wordpress.com/books/ifar/}{9-Individual Growth} is related.
#'
#' @seealso See \code{\link{vbFuns}}, \code{\link{gompFuns}}, and \code{\link{schnute}} for similar functionality for other models.
#'
#' @references 
#' Campana, S.E. and C.M. Jones.  1992.  Analysis of otolith microstructure data.  Pages 73-100 In D.K. Stevenson and S.E. Campana, editors.  Otolith microstructure examination and analysis.  Canadian Special Publication of Fisheries and Aquatic Sciences 117.
#' 
#' Winsor, C.P.  1932.  \href{http://www.ncbi.nlm.nih.gov/pmc/articles/PMC1076153/pdf/pnas01729-0009.pdf}{The Gompertz curve as a growth curve}.  Proceedings of the National Academy of Sciences.  18:1-8.
#' 
#' @keywords manip
#'
#' @examples
#' ## See the formulae
#' \dontrun{windows(5,5)}
#' logisticModels()
#' 
#' ## Simple Examples
#' ( log1 <- logisticFuns() )               # First Campana-Jones parameterization
#' ages <- 0:15
#' plot(log1(ages,Linf=800,gninf=0.5,ti=5)~ages,type="b",pch=19)
#'
#' ( log2 <- logisticFuns("CJ2") )          # Second Campana-Jones parameterization
#' plot(log2(ages,Linf=800,gninf=0.5,a=10)~ages,type="b",pch=19)
#'
#' ( log2c <- logisticFuns("CJ2",simple=TRUE) ) # compare to log2
#'
#' #######################################################################################
#' ## Examples of fitting Logistic models
#' ##   After the last example a plot is constructed with three lines on top of each
#' ##   other illustrating that the parameterizations all produce the same fitted values.
#' # Make some fake data using the original parameterization
#' 
#' gompO <- gompFuns("original")
#' # setup ages, sample sizes (general reduction in numbers with
#' # increasing age), and additive SD to model
#' t <- 1:15
#' n <- c(10,40,35,25,12,10,10,8,6,5,3,3,3,2,2)
#' sd <- 15
#' # expand ages
#' ages <- rep(t,n)
#' # get lengths from gompertz and a random error for individuals
#' lens <- gompO(ages,Linf=450,a=1,gi=0.3)+rnorm(length(ages),0,sd)
#' # put together as a data.frame
#' df <- data.frame(age=ages,len=round(lens,0))
#' 
#' # Fit first Campana-Jones parameterization
#' fit1 <- nls(len~log1(age,Linf,gninf,ti),data=df,start=list(Linf=450,gninf=0.45,ti=4))
#' summary(fit1,correlation=TRUE)
#' plot(len~age,data=df,pch=19,col=rgb(0,0,0,1/5))
#' curve(log1(x,Linf=coef(fit1)),from=0,to=15,col="red",lwd=10,add=TRUE)
#'
#' # Fit second Campana-Jones parameterization
#' fit2 <- nls(len~log2(age,Linf,gninf,a),data=df,start=list(Linf=450,gninf=0.45,a=7))
#' summary(fit2,correlation=TRUE)
#' curve(log2(x,Linf=coef(fit2)),from=0,to=15,col="blue",lwd=5,add=TRUE)
#'
#' # Fit Richards-like parameterization (using simple=TRUE model)
#' log3 <- logisticFuns("Richards",simple=TRUE)
#' fit3 <- nls(len~log3(age,Linf,L0,gninf),data=df,start=list(Linf=450,L0=30,gninf=0.45))
#' summary(fit3,correlation=TRUE)
#' curve(log3(x,Linf=coef(fit3)[1],L0=coef(fit3)[2],gninf=coef(fit3)[3]),
#'       from=0,to=15,col="green",lwd=2,add=TRUE)
#' 
NULL

#' @rdname growthLogistic
#' @export
logisticFuns <- function(type=c("CJ1","CJ2","Richards","LRichards"),simple=FALSE,msg=FALSE) {
  CJ1 <- function(t,Linf,gninf=NULL,ti=NULL) {
    if (length(Linf)==3) { gninf <- Linf[[2]]
                           ti <- Linf[[3]]
                           Linf <- Linf[[1]] }
    Linf/(1+exp(-gninf*(t-ti)))
  }
  SCJ1 <- function(t,Linf,gninf,ti) {
    Linf/(1+exp(-gninf*(t-ti)))
  }
  CJ2<- function(t,Linf,gninf=NULL,a=NULL) {
    if (length(Linf)==3) { gninf <- Linf[[2]]
                           a <- Linf[[3]]
                           Linf <- Linf[[1]] }
    Linf/(1+a*exp(-gninf*t))
  }
  SCJ2<- function(t,Linf,gninf,a) {
    Linf/(1+a*exp(-gninf*t))
  }
  Richards <- LRichards <- function(t,Linf,L0=NULL,gninf=NULL) {
    if (length(Linf)==3) { L0 <- Linf[[2]]
                           gninf <- Linf[[3]]
                           Linf <- Linf[[1]] }
    L0*Linf/(L0+(Linf-L0)*exp(-gninf*t))
  }
  SRichards <- SLRichards <- function(t,Linf,L0,gninf) {
    L0*Linf/(L0+(Linf-L0)*exp(-gninf*t))
  }
  ## Main function
  type <- match.arg(type)
  comcat <- "parameterization of the logistic growth function.\n\n"
  if (msg) {
    switch(type,
      CJ1= {
        cat("You have chosen the 'CJ1'",comcat)
        cat("  E[L|t] = Linf/(1+exp(-gninf*(t-ti)))\n\n")
        cat("where Linf = asymptotic mean length\n")
        cat("      gninif = instantaneous growth rate at t=-infinity\n")
        cat("      ti = time at the inflection point\n\n")
      },
      CJ2= {
        cat("You have chosen the 'CJ2'",comcat)
        cat("  E[L|t] = Linf/(1+a*exp(-gninf*t))\n\n")
        cat("where Linf = asymptotic mean length\n")
        cat("      gi = instantaneous growth rate at the inflection point\n")
        cat("      a = a dimensionless parameter related to growth\n\n")
      },
      LRichards= {
        cat("You have chosen the 'Richards'",comcat)
        cat("  E[L|t] = L0*Linf/(L0+(Linf-L0)*exp(-gninf*t))\n\n")
        cat("where Linf = asymptotic mean length\n")
        cat("      L0 = mean length at time/age 0\n")
        cat("      gi = instantaneous growth rate at the inflection point\n\n")
      }
    )
  }
  if (simple) type <- paste("S",type,sep="")
  get(type)
}

#' @rdname growthLogistic
#' @export
logisticModels <- function(...) {
  op <- par(mar=c(0,0,3,0),cex=1.25)
  plot(1,type="n",ylim=c(0,3),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",main="FSA Logistic Growth Parameterizations",...)
  iGrowthModels("CJ1", 0.1,2.5)
  iGrowthModels("CJ2", 0.1,1.5)
  iGrowthModels("Richards", 0.1,0.5)
  par(op)
}

## iGrowthModels internal function for plotting the different models is found in vbModels().
