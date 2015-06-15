#' @name growthLogistic
#' 
#' @title Creates a function for a specific parameterization of the logistic growth function.
#'
#' @description Creates a function for a specific parameterization of the logistic growth function.  Use \code{logisticModels()} to see the equations for each model.
#'
#' @param type A string that indicates the parameterization of the logistic growth function.
#' @param simple A logical that indicates whether the user should be allowed to send all parameter values in the first parameter argument (\code{=FALSE}; default) or whether all individual parameters must be specified (\code{=TRUE}).
#' @param msg A logical that indicates whether a message about the model and parameter definitions should be output (\code{=TRUE}) or not (\code{=FALSE}; default).
#' @param cex A single numeric expansion value for use with \code{logisticModels}.
#' @param \dots Not implemented.
#' 
#' @return \code{logisticFuns} returns a function that can be used to predict fish length or weight given a vector of ages and values for the function parameters.  The result should be saved to an object that can then be used as a function name.  When the resulting function is used, the parameters are ordered as shown when the definitions of the parameters are printed after the function is called (if \code{msg=TRUE}).  If \code{simple=FALSE}, then the values for all parameters may be included as a vector in the first parameter argument.  If \code{simple=TRUE}, then all parameters must be declared individually.  The resulting function is somewhat easier to read when \code{simple=TRUE}.
#' 
#' \code{logisticModels} returns a graphic that uses \code{\link{plotmath}} to show the model formulae in a pretty format.
#'
#' @note Within FSA, L0 is the mean length at age 0, Linf is the mean asymptotic length, ti is the age at the inflection point, gninf is the instantaneous growth rate at negative infinity.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}, thanks to Gabor Grothendieck for a hint about using \code{get()}.
#'
#' @section IFAR Chapter: None specifically, but \href{https://fishr.wordpress.com/books/ifar/}{9-Individual Growth} is related.
#'
#' @seealso See \code{\link{vbFuns}}, \code{\link{GompertzFuns}}, \code{\link{logisticFuns}}, and \code{\link{Schnute}} for similar functionality for other models.
#'
#' @references 
#' Campana, S.E. and C.M. Jones.  1992.  \href{http://www.dfo-mpo.gc.ca/Library/141734.pdf}{Analysis of otolith microstructure data}.  Pages 73-100 In D.K. Stevenson and S.E. Campana, editors.  Otolith microstructure examination and analysis.  Canadian Special Publication of Fisheries and Aquatic Sciences 117.
#' 
#' Haddon, M., C. Mundy, and D. Tarbath. 2008.  \href{http://aquaticcommons.org/8857/1/haddon_Fish_Bull_2008.pdf}{Using an inverse-logistic model to describe growth increments of blacklip abalone (\emph{Haliotis rubra}) in Tasmania}.  Fishery Bulletin 106:58-71. 
#' 
#' Karkach, A. S.  2006.  \href{http://www.demographic-research.org/volumes/vol15/12/15-12.pdf}{Trajectories and models of individual growth}.  Demographic Research 15:347-400.
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
#' ( log3 <- logisticFuns("Karkach") )       # Karkach parametrization
#' plot(log3(ages,L0=10,Linf=800,gninf=0.5)~ages,type="b",pch=19)
#' 
#' ( log4 <- logisticFuns("HaddonI") )       # Haddon's inverse logistic
#'
#' #######################################################################################
#' ## Examples of fitting Logistic models
#' ##   After the last example a plot is constructed with three lines on top of each
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
#' # Fit Karkach parameterization (using simple=TRUE model)
#' log3 <- logisticFuns("Karkach",simple=TRUE)
#' fit3 <- nls(len~log3(age,Linf,L0,gninf),data=df,start=list(Linf=450,L0=30,gninf=0.45))
#' summary(fit3,correlation=TRUE)
#' curve(log3(x,Linf=coef(fit3)[1],L0=coef(fit3)[2],gninf=coef(fit3)[3]),
#'       from=0,to=15,col="green",lwd=2,add=TRUE)
#' 
NULL

#' @rdname growthLogistic
#' @export
logisticFuns <- function(type=c("CJ1","CJ2","Karkach","HaddonI"),simple=FALSE,msg=FALSE) {
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
  Karkach <- function(t,Linf,L0=NULL,gninf=NULL) {
    if (length(Linf)==3) { L0 <- Linf[[2]]
                           gninf <- Linf[[3]]
                           Linf <- Linf[[1]] }
    L0*Linf/(L0+(Linf-L0)*exp(-gninf*t))
  }
  SKarkach <- function(t,Linf,L0,gninf) {
    L0*Linf/(L0+(Linf-L0)*exp(-gninf*t))
  }
  HaddonI <- function(Lm,dLmax,L50=NULL,L95=NULL) {
    if (length(dLmax)==3) { L50=dLmax[2]
                            L95=dLmax[3]
                            dLmax=dLmax[1] }
    dLmax/(1+exp(log(19)*((Lm-L50)/(L95-L50))))
  }
  SHaddonI <- function(Lm,dLmax,L50,L95) {
    dLmax/(1+exp(log(19)*((Lm-L50)/(L95-L50))))
  }
  ## Main function
  type <- match.arg(type)
  comcat <- "parameterization of the logistic growth function.\n\n"
  if (msg) {
    switch(type,
      CJ1= {
        message("You have chosen the 'CJ1'",comcat,
                "  E[L|t] = Linf/(1+exp(-gninf*(t-ti)))\n\n",
                "  where Linf = asymptotic mean length\n",
                "      gninif = instantaneous growth rate at t=-infinity\n",
                "          ti = time at the inflection point\n\n")
      },
      CJ2= {
        message("You have chosen the 'CJ2'",comcat,
                "  E[L|t] = Linf/(1+a*exp(-gninf*t))\n\n",
                "  where Linf = asymptotic mean length\n",
                "          gi = instantaneous growth rate at the inflection point\n",
                "           a = a dimensionless parameter related to growth\n\n")
      },
      Karkach= {
        message("You have chosen the 'Karkach'",comcat,
                "  E[L|t] = L0*Linf/(L0+(Linf-L0)*exp(-gninf*t))\n\n",
                "  where Linf = asymptotic mean length\n",
                "          L0 = mean length at time/age 0\n",
                "          gi = instantaneous growth rate at the inflection point\n\n")
      },
      HaddonI= {
        message("You have chosen the 'Haddon Inverse'",comcat,
                "  E[Lr-Lm|dt] = dLmax/(1+exp(log(19)*((Lm-L50)/(L95-L50))))\n\n",
                "  where dLmax = maximum growth increment during the study\n",
                "          L50 = length at marking to produces a growth increment of 0.5*dLmax",
                "          L95 = length at marking to produces a growth increment of 0.95*dLmax\n\n",
                "  and the data are Lr = length at time of recapture\n",
                "                   Lm = length at time of marking\n")
      }
    )
  }
  if (simple) type <- paste("S",type,sep="")
  get(type)
}

#' @rdname growthLogistic
#' @export
logisticModels <- function(type=c("size","tagging"),cex=1.25,...) {
  ## Set some plotting parameters
  op <- par(mar=c(0,0,3,0),cex=cex)
  ## Check the type argument
  type <- match.arg(type)
  ## Show the models
  if (type=="size") {
    plot(1,type="n",ylim=c(0,3),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
         main="FSA Logistic Growth Parameterizations",...)
    iGrowthModels("CJ1", 0.1,2.5)
    iGrowthModels("CJ2", 0.1,1.5)
    iGrowthModels("Karkach", 0.1,0.5)
  } else {
    plot(1,type="n",ylim=c(0,3),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
         main="FSA Logistic Growth Tagging Parameterizations",cex=cex,...)
    iGrowthModels("HaddonI", 0.1,2.5)
  }
  par(op)
}

## iGrowthModels internal function for plotting the different models is found in vbModels().
