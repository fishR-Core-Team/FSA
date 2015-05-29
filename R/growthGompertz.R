#' @name growthGompertz
#' 
#' @title Creates a function for a specific parameterization of the Gompertz growth function
#'
#' @description Creates a function for a specific parameterization of the Gompertz growth function.  Use \code{gompModels()} to see the equations for each model.
#'
#' @param type A string that indicates the parameterization of the Gompertz function.
#' @param simple A logical that indicates whether the user should be allowed to send all parameter values in the first parameter argument (\code{=FALSE}; default) or whether all individual parameters must be specified (\code{=TRUE}).
#' @param msg A logical that indicates whether a message about the model and parameter definitions should be output (\code{=TRUE}) or not (\code{=FALSE}; default).
#' @param \dots Not implemented.
#' 
#' @return \code{gompFuns} returns a function that can be used to predict fish length or weight given a vector of ages and values for the function parameters.  The result should be saved to an object that can then be used as a function name.  When the resulting function is used, the parameters are ordered as shown when the definitions of the parameters are printed after the function is called (if \code{msg=TRUE}).  If \code{simple=FALSE}, then the values for all parameters may be included as a vector in the first parameter argument.  If \code{simple=TRUE}, then all parameters must be declared individually.  The resulting function is somewhat easier to read when \code{simple=TRUE}.
#' 
#' \code{gompModels} returns a graphic that uses \code{\link{plotmath}} to show the model formulae in a pretty format.
#'
#' @note The parameterizations and parameters for the Gompertz function are varied and confusing in the literature.  I have attempted to use a uniform set of paraemters in these functions but that makes a direct comparison to the literature difficult.  Common sources for Gompertz models are listed in the references below.  I make some comments here to aid the comparison.  It is likely worth your while to look at \code{\link{gompModels}} while you make these comparisons.
#' 
#' Within FSA, L0 is the mean length at age 0, Linf is the mean asymptotic length, ti is the age at the inflection point, gi is the instantaneous growth rate at the inflection point, t* is a dimensionless parameter related to time/age, and a is a dimensionless parameter related to growth.
#'
#' In the Quinn and Deriso (1999) models (the \sQuote{QD} models), the a parameter here is equal to lambda/K there and the gi parameter here is equal to the K parameter there.  Also note that their Y is L here.
#' 
#' In the Ricker (1979)[p. 705] models (the \sQuote{Ricker} models), the a parameter here is equal to k there and the gi paramter here is equal to the g parameter there.  Also note that their w is L here.  In the Ricker (1979) models as presented in Campana and Jones (1992), the a parameter here is equal to k there and the gi paramter here is equal to the G parameter there.  Also note that their X is L here.
#' 
#' The model in Ricker (1975)[p. 232] is the same as \sQuote{Ricker2} where the a parameter here is qual to G there and the gi parameter here is equal to the g parameter there.  Also note that their w is L here.
#' 
#' The model in Quist et al. (2012)[p. 714] is the same as \sQuote{Ricker1} where the gi parameter here is equal to G there and the ti parameter here is equal to the t0 parameter there.  This parameterization can also be called with \code{type="AFS"}.
#' 
#' The model in Katsanevakis and Maravelias (2008) is the same as \sQuote{Ricker1} where the gi parameter here is equal to k2 there and the ti parameter here is equal to t2 there.  This parameterization can also be called with \code{type="MK"}.
#'   
#' The \sQuote{Ricker2} and \sQuote{QD1}; \sQuote{Ricker3} and \sQuote{QD2}; and \sQuote{Ricker1}, \sQuote{AFS}, and \sQuote{MK} parameterizations are synonymous in their usage here.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}, thanks to Gabor Grothendieck for a hint about using \code{get()}.
#'
#' @section IFAR Chapter: None specifically, but \href{https://fishr.wordpress.com/books/ifar/}{9-Individual Growth} is related.
#'
#' @seealso See \code{\link{vbFuns}}, \code{\link{logisticFuns}}, and \code{\link{schnute}} for similar functionality for other models.
#'
#' @references 
#' Campana, S.E. and C.M. Jones.  1992.  Analysis of otolith microstructure data.  Pages 73-100 In D.K. Stevenson and S.E. Campana, editors.  Otolith microstructure examination and analysis.  Canadian Special Publication of Fisheries and Aquatic Sciences 117.
#' 
#' Gompertz, B.  1825.  On the nature of the function expressive of the law of human mortality, and on a new method of determining the value of life contingencies.  Philosophical Transactions of the Royal Society of London.  115:513-583. 
#' 
#' Katsanevakis, S. and C.D. Maravelias.  2008.  Modelling fish growth: multi-model inference as a better alternative to a priori using von Bertalanffy equation.  Fish and Fisheries 9:178-187.
#' 
#' Quinn, T. J. and R. B. Deriso. 1999. Quantitative Fish Dynamics. Oxford University Press, New York, New York. 542 pages.
#' 
#' Quist, M.C., M.A. Pegg, and D.R. DeVries.  2012.  Age and Growth.  Chapter 15 in A.V. Zale, D.L Parrish, and T.M. Sutton, Editors  Fisheries Techniques, Third Edition.  American Fisheries Society, Bethesda, MD.
#' 
#' Ricker, W.E. 1975. \href{http://www.dfo-mpo.gc.ca/Library/1485.pdf}{Computation and interpretation of biological statistics of fish populations}. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada.
#' 
#' Ricker, W.E. 1979.  \href{https://books.google.com/books?id=CB1qu2VbKwQC&pg=PA705&lpg=PA705&dq=Gompertz+fish&source=bl&ots=y34lhFP4IU&sig=EM_DGEQMPGIn_DlgTcGIi_wbItE&hl=en&sa=X&ei=QmM4VZK6EpDAgwTt24CABw&ved=0CE8Q6AEwBw#v=onepage&q=Gompertz fish&f=false}{Growth rates and models}.  Pages 677-743 In W.S. Hoar, D.J. Randall, and J.R. Brett, editors.  Fish Physiology, Vol. 8: Bioenergetics and Growth.  Academic Press, NY, NY.
#' 
#' Winsor, C.P.  1932.  \href{http://www.ncbi.nlm.nih.gov/pmc/articles/PMC1076153/pdf/pnas01729-0009.pdf}{The Gompertz curve as a growth curve}.  Proceedings of the National Academy of Sciences.  18:1-8.
#' 
#' @keywords manip hplot
#'
#' @examples
#' ## See the formulae
#' \dontrun{windows(5,5)}
#' gompModels()
#' 
#' ## Simple Examples
#' ( gomp1 <- gompFuns() )              # First Ricker parameterization
#' ages <- 0:15
#' plot(gomp1(ages,Linf=800,gi=0.5,ti=5)~ages,type="b",pch=19)
#'
#' ( gomp2 <- gompFuns("Ricker2") )     # Second Ricker parameterization
#' plot(gomp2(ages,L0=2,a=6,gi=0.5)~ages,type="b",pch=19)
#'
#' ( gomp2c <- gompFuns("Ricker2",simple=TRUE) )   # compare to gomp2
#'
#' #######################################################################################
#' ## Examples of fitting Gompertz models
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
#' # Fit first Ricker parameterization
#' fit1 <- nls(len~gomp1(age,Linf,gi,ti),data=df,start=list(Linf=500,gi=0.3,ti=3))
#' summary(fit1,correlation=TRUE)
#' plot(len~age,data=df,pch=19,col=rgb(0,0,0,1/5))
#' curve(gomp1(x,Linf=coef(fit1)),from=0,to=15,col="red",lwd=10,add=TRUE)
#'
#' # Fit third Ricker parameterization
#' fit2 <- nls(len~gomp2(age,L0,a,gi),data=df,start=list(L0=30,a=3,gi=0.3))
#' summary(fit2,correlation=TRUE)
#' curve(gomp2(x,L0=coef(fit2)),from=0,to=15,col="blue",lwd=5,add=TRUE)
#'
#' # Fit third Quinn and Deriso parameterization (using simple=TRUE model)
#' gomp3 <- gompFuns("QD3",simple=TRUE)
#' fit3 <- nls(len~gomp3(age,Linf,gi,t0),data=df,start=list(Linf=500,gi=0.3,t0=0))
#' summary(fit3,correlation=TRUE)
#' curve(gomp3(x,Linf=coef(fit3)[1],gi=coef(fit3)[2],t0=coef(fit3)[3]),
#'       from=0,to=15,col="green",lwd=2,add=TRUE)
#' 
NULL

#' @rdname growthGompertz
#' @export
gompFuns <- function(type=c("Ricker1","Ricker2","Ricker3",
                          "QD1","QD2","QD3","KM","AFS","original"),
                   simple=FALSE,msg=FALSE) {
  original <- function(t,Linf,a=NULL,gi=NULL) {
  if (length(Linf)==3) { a <- Linf[[2]]
                         gi <- Linf[[3]]
                         Linf <- Linf[[1]] }
  Linf*exp(-exp(a-gi*t))
  }
  Soriginal <-function(t,Linf,a,gi) {
    Linf*exp(-exp(a-gi*t))
  }
  Ricker1 <- KM <- AFS <- function(t,Linf,gi=NULL,ti=NULL) {
    if (length(Linf)==3) { gi <- Linf[[2]]
    ti <- Linf[[3]]
    Linf <- Linf[[1]] }
    Linf*exp(-exp(-gi*(t-ti)))
  }
  SRicker1 <- SKM <- SAFS <- function(t,Linf,gi,ti) {
    Linf*exp(-exp(-gi*(t-ti)))
  }
  QD1 <- Ricker2 <- function(t,L0,a=NULL,gi=NULL) {
    if (length(L0)==3) { a <- L0[[2]]
                         gi <- L0[[3]]
                         L0 <- L0[[1]] }
    L0*exp(a*(1-exp(-gi*t)))
  }
  SQD1 <- SRicker2 <- function(t,L0,a,gi) {
    L0*exp(a*(1-exp(-gi*t)))
  }
  QD2 <- Ricker3 <-  function(t,Linf,a=NULL,gi=NULL) {
    if (length(Linf)==3) { a <- Linf[[2]]
                           gi <- Linf[[3]]
                           Linf <- Linf[[1]] }
    Linf*exp(-a*exp(-gi*t))
  }
  SQD2 <- SRicker3 <- function(t,Linf,a,gi) {
    Linf*exp(-a*exp(-gi*t))
  }
  QD3 <- function(t,Linf,gi=NULL,t0=NULL) {
    if (length(Linf)==3) { gi <- Linf[[2]]
                           t0 <- Linf[[3]]
                           Linf <- Linf[[1]] }
    Linf*exp(-(1/gi)*exp(-gi*(t-t0)))
  }
  SQD3 <- function(t,Linf,gi,t0) {
    Linf*exp(-(1/gi)*exp(-gi*(t-t0)))
  }
  ## Main function
  type <- match.arg(type)
  comcat <- "parameterization of the Gompertz function.\n\n"
  if (msg) {
    switch(type,
      original= {
        cat("You have chosen the 'original'",comcat)
        cat("  E[L|t] = Linf*exp(-exp(a-gi*t))\n\n")
        cat("where Linf = asymptotic mean length\n")
        cat("      gi = decrease in growth rate at the inflection point\n")
        cat("      a = an undefined parameter\n\n")
      },
      Ricker1,KM,AFS= {
        cat("You have chosen the 'Ricker1'/'KM'/'AFS'",comcat)
        cat("  E[L|t] = Linf*exp(-exp(-gi*(t-ti)))\n\n")
        cat("where Linf = asymptotic mean length\n")
        cat("      gi = instantaneous growth rate at the inflection point\n")
        cat("      ti = time at the inflection point\n\n")
      },
      Ricker2,QD1= {
        cat("You have chosen the 'Ricker1'/'QD1'",comcat)
        cat("  E[L|t] = L0*exp(a*(1-exp(-gi*t)))\n\n")
        cat("where Linf = asymptotic mean length\n")
        cat("      gi = instantaneous growth rate at the inflection point\n")
        cat("      a = dimenstionless parameter related to growth\n\n")
      },
      Ricker3,QD2= {
        cat("You have chosen the 'Ricker3' or 'QD2'",comcat)
        cat("  E[L|t] = Linf*exp(-(a/gi)*exp(-gi*t))\n\n")
        cat("where Linf = asymptotic mean length\n")
        cat("      gi = instantaneous growth rate at the inflection point\n")
        cat("      a = dimenstionless parameter related to growth\n\n")
      },
      QD3= {
        cat("You have chosen the 'QD3",comcat)
        cat("  E[L|t] = Linf*exp(-(1/gi)*exp(-gi*(t-t0)))\n\n")
        cat("where Linf = asymptotic mean length\n")
        cat("      gi = instantaneous growth rate at the inflection point\n")
        cat("      t0 = a dimensionless parameter related to time/age\n\n")
      }
    )
  }
  if (simple) type <- paste("S",type,sep="")
  get(type)
}


#' @rdname growthGompertz
#' @export
gompModels <- function(...) {
  op <- par(mar=c(0,0,3,0),cex=1.25)
  plot(1,type="n",ylim=c(0,5),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",main="FSA Gompertz Parameterizations",...)
  iGrowthModels("gOriginal", 0.1,4.5)
  iGrowthModels("gRicker1", 0.1,3.5)
  iGrowthModels("gRicker2", 0.1,2.5)
  iGrowthModels("gRicker3",     0.1,1.5)
  iGrowthModels("gQD3",0.1,0.5)
  par(op)
}

## iGrowthModels internal function for plotting the different models is found in vbModels().
