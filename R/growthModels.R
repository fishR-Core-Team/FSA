#' @name growthModels
#' 
#' @title Creates a function for a specific parameterization of the von Bertalanffy, Gompertz, Richards, and logistic growth functions.
#'
#' @description Creates a function for a specific parameterizations of the von Bertalanffy, Gompertz, Richards, and logistic growth functions.  Use \code{vbModels()}, \code{GompertzModels()}, \code{RichardsModels()}, and \code{logisticModels()} to see the equations for each growth function.
#'
#' @param type A string (for von Bertalanffy, Gompertz, and logistic) or numeric (for Richards) that indicates the specific parameterization of the growth function.
#' @param simple A logical that indicates whether the function will accept all parameter values in the first parameter argument (\code{=FALSE}; DEFAULT) or whether all individual parameters must be specified in separate arguments (\code{=TRUE}).
#' @param msg A logical that indicates whether a message about the growth function and parameter definitions should be output (\code{=TRUE}) or not (\code{=FALSE}; DEFAULT).
#' @param family A string that indicates the family of growth functions to display.  Choices are \code{"size"} (DEFAULT), \code{"tagging"}, and \code{"seasonal"} (only for the von Bertalanffy models).
#' @param cex A single numeric character expansion value.
#' @param \dots Not implemented.
#' 
#' @return The functions ending in \code{Funs} return a function that can be used to predict fish size given a vector of ages and values for the growth function parameters and, in some parameterizations, values for some constants.  The result should be saved to an object that is then the function name.  When the resulting function is used, the parameters are ordered as shown when the definitions of the parameters are printed after the function is called (if \code{msg=TRUE}).  If \code{simple=FALSE}, then the values for all parameters may be included as a vector in the first parameter argument.  Similarly, the values for all constants may be included as a vector in the first constant argument (i.e., \code{t1}).  If \code{simple=TRUE}, then all parameters and constants must be declared individually.  The resulting function is somewhat easier to read when \code{simple=TRUE}, but is less general for some applications.
#' 
#' The functions ending in \code{Models} return a graphic that uses \code{\link{plotmath}} to show the growth function equations in a pretty format.
#'
#' @note Take note of the folling for each type of growth function:
#' \itemize{
#'   \item von Bertalanffy
#'   \itemize{
#'     \item The \sQuote{original} and \sQuote{vonBertalanffy} and the \sQuote{typical} and \sQuote{BevertonHolt} parameterizations are synonymous.
#'   }
#'   \item Gompertz
#'   \itemize{
#'     \item The \sQuote{Ricker2} and \sQuote{QD1}; \sQuote{Ricker3} and \sQuote{QD2}; and \sQuote{Ricker1}, \sQuote{AFS}, and \sQuote{KM} parameterizations are synonymous in their usage here.
#'     \item The parameterizations and parameters for the Gompertz function are varied and confusing in the literature.  I have attempted to use a uniform set of paraemters in these functions but that makes a direct comparison to the literature difficult.  Common sources for Gompertz models are listed in the references below.  I make some comments here to aid the comparison.  It is likely worth your while to look at \code{GompertzModels} while you make these comparisons.
#'     \item Within FSA, L0 is the mean length at age 0, Linf is the mean asymptotic length, ti is the age at the inflection point, gi is the instantaneous growth rate at the inflection point, t* is a dimensionless parameter related to time/age, and a is a dimensionless parameter related to growth.
#'     \item In the Quinn and Deriso (1999) models (the \sQuote{QD} models), the a parameter here is equal to lambda/K there and the gi parameter here is equal to the K parameter there.  Also note that their Y is L here.
#'     \item In the Ricker (1979)[p. 705] models (the \sQuote{Ricker} models), the a parameter here is equal to k there and the gi paramter here is equal to the g parameter there.  Also note that their w is L here.  In the Ricker (1979) models as presented in Campana and Jones (1992), the a parameter here is equal to k there and the gi paramter here is equal to the G parameter there.  Also note that their X is L here.
#'     \item The model in Ricker (1975)[p. 232] is the same as \sQuote{Ricker2} where the a parameter here is qual to G there and the gi parameter here is equal to the g parameter there.  Also note that their w is L here.
#'     \item The model in Quist et al. (2012)[p. 714] is the same as \sQuote{Ricker1} where the gi parameter here is equal to G there and the ti parameter here is equal to the t0 parameter there.  This parameterization can also be called with \code{type="AFS"}.
#'     \item The model in Katsanevakis and Maravelias (2008) is the same as \sQuote{Ricker1} where the gi parameter here is equal to k2 there and the ti parameter here is equal to t2 there.  This parameterization can also be called with \code{type="KM"}.
#'   }
#'   \item Richards
#'   \itemize{
#'     \item Within FSA, Linf is the mean asymptotic length, ti is the age at the inflection point, k is related to growth (slope at the inflection point), b is related to the vertical position of the inflection point, and L0 is the mean length at age-0.
#'     \item The parameterizations (1-6) correspond to models/equations 1, 4, 5, 6, 7, and 8, respectively, in Tjorve and Tjorve (2010).  Note that their A, S, k, d, and B are Linf, a, k, b, and L0 here (in FSA).
#'   }
#'   \item logistic
#'   \itemize{
#'     \item Within FSA, L0 is the mean length at age 0, Linf is the mean asymptotic length, ti is the age at the inflection point, and gninf is the instantaneous growth rate at negative infinity.
#'   }
#' }
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}, thanks to Gabor Grothendieck for a hint about using \code{get()}.
#'
#' @section IFAR Chapter: \href{https://fishr.wordpress.com/books/ifar/}{12-Individual Growth}.
#'
#' @seealso See \code{\link{Schnute}} for an implementation of the Schnute (1981) model.
#'
#' @references Ogle, D.H.  2015.  Introductory Fisheries Analyses with R.  Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Campana, S.E. and C.M. Jones.  1992.  \href{http://www.dfo-mpo.gc.ca/Library/141734.pdf}{Analysis of otolith microstructure data}.  Pages 73-100 In D.K. Stevenson and S.E. Campana, editors.  Otolith microstructure examination and analysis.  Canadian Special Publication of Fisheries and Aquatic Sciences 117.
#' 
#' Fabens, A. 1965. Properties and fitting of the von Bertalanffy growth curve. Growth 29:265-289.
#'
#' Francis, R.I.C.C.  1988.  Are growth parameters estimated from tagging and age-length data comparable?  Canadian Journal of Fisheries and Aquatic Sciences, 45:936-942.
#'
#' Gallucci, V.F. and T.J. Quinn II. 1979.  Reparameterizing, fitting, and testing a simple growth model.  Transactions of the American Fisheries Society, 108:14-25.
#'
#' Garcia-Berthou, E., G. Carmona-Catot, R. Merciai, and D.H. Ogle.  \href{https://www.researchgate.net/publication/257658359_A_technical_note_on_seasonal_growth_models}{A technical note on seasonal growth models.}  Reviews in Fish Biology and Fisheries 22:635-640.
#' 
#' Gompertz, B.  1825.  On the nature of the function expressive of the law of human mortality, and on a new method of determining the value of life contingencies.  Philosophical Transactions of the Royal Society of London.  115:513-583.
#' 
#' Haddon, M., C. Mundy, and D. Tarbath. 2008.  \href{http://aquaticcommons.org/8857/1/haddon_Fish_Bull_2008.pdf}{Using an inverse-logistic model to describe growth increments of blacklip abalone (\emph{Haliotis rubra}) in Tasmania}.  Fishery Bulletin 106:58-71. 
#'
#' Karkach, A. S.  2006.  \href{http://www.demographic-research.org/volumes/vol15/12/15-12.pdf}{Trajectories and models of individual growth}.  Demographic Research 15:347-400.
#' 
#' Katsanevakis, S. and C.D. Maravelias.  2008.  Modelling fish growth: multi-model inference as a better alternative to a priori using von Bertalanffy equation.  Fish and Fisheries 9:178-187.
#'
#' Mooij, W.M., J.M. Van Rooij, and S. Wijnhoven.  1999.  Analysis and comparison of fish growth from small samples of length-at-age data: Detection of sexual dimorphism in Eurasian perch as an example.  Transactions of the American Fisheries Society 128:483-490.
#'
#' Polacheck, T., J.P. Eveson, and G.M. Laslett.  2004.  Increase in growth rates of southern bluefin tuna (\emph{Thunnus maccoyii}) over four decades: 1960 to 2000.  Canadian Journal of Fisheries and Aquatic Sciences, 61:307-322.
#' 
#' Quinn, T. J. and R. B. Deriso. 1999. Quantitative Fish Dynamics. Oxford University Press, New York, New York. 542 pages.
#' 
#' Quist, M.C., M.A. Pegg, and D.R. DeVries.  2012.  Age and Growth.  Chapter 15 in A.V. Zale, D.L Parrish, and T.M. Sutton, Editors  Fisheries Techniques, Third Edition.  American Fisheries Society, Bethesda, MD.
#' 
#' Richards, F. J.  1959.  A flexible growth function for empirical use.  Journal of Experimental Biology 10:290-300.
#' 
#' Ricker, W.E. 1975. \href{http://www.dfo-mpo.gc.ca/Library/1485.pdf}{Computation and interpretation of biological statistics of fish populations}. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada.
#' 
#' Ricker, W.E. 1979.  \href{https://books.google.com/books?id=CB1qu2VbKwQC&pg=PA705&lpg=PA705&dq=Gompertz+fish&source=bl&ots=y34lhFP4IU&sig=EM_DGEQMPGIn_DlgTcGIi_wbItE&hl=en&sa=X&ei=QmM4VZK6EpDAgwTt24CABw&ved=0CE8Q6AEwBw#v=onepage&q=Gompertz\%20fish&f=false}{Growth rates and models}.  Pages 677-743 In W.S. Hoar, D.J. Randall, and J.R. Brett, editors.  Fish Physiology, Vol. 8: Bioenergetics and Growth.  Academic Press, NY, NY.
#'
#' Schnute, J.  1981.  A versatile growth model with statistically stable parameters. Canadian Journal of Fisheries and Aquatic Sciences, 38:1128-1140.
#'
#' Somers, I. F. 1988. \href{http://www.worldfishcenter.org/Naga/na_2914.pdf}{On a seasonally oscillating growth function.} Fishbyte 6(1):8-11.
#' 
#' Tjorve, E. and K. M. C. Tjorve.  2010.  \href{https://www.researchgate.net/profile/Even_Tjorve/publication/46218377_A_unified_approach_to_the_Richards-model_family_for_use_in_growth_analyses_why_we_need_only_two_model_forms/links/54ba83b80cf29e0cb04bd24e.pdf}{A unified approach to the Richards-model family for use in growth analyses: Why we need only two model forms.}  Journal of Theoretical Biology 267:417-425.
#' 
#' Troynikov, V. S., R. W. Day, and A. M. Leorke.  \href{https://www.researchgate.net/profile/Robert_Day2/publication/249340562_Estimation_of_seasonal_growth_parameters_using_a_stochastic_gompertz_model_for_tagging_data/links/54200fa30cf203f155c2a08a.pdf}{Estimation of seasonal growth parameters using a stochastic Gompertz model for tagging data}.  Journal of Shellfish Research 17:833-838.
#' 
#' Vaughan, D. S. and T. E. Helser.  1990.  \href{http://docs.lib.noaa.gov/noaa_documents/NMFS/SEFSC/TM_NMFS_SEFSC/NMFS_SEFSC_TM_263.pdf}{Status of the red drum stock of the Atlantic coast: Stock assessment report for 1989}.  NOAA Technical Memorandum NMFS-SEFC-263, 117 p.
#'
#' Wang, Y.-G.  1998.  An improved Fabens method for estimation of growth parameters in the von Bertalanffy model with individual asymptotes.  Canadian Journal of Fisheries and Aquatic Sciences 55:397-400.
#'
#' Weisberg, S., G.R. Spangler, and L. S. Richmond. 2010. Mixed effects models for fish growth. Canadian Journal of Fisheries And Aquatic Sciences 67:269-277.
#' 
#' Winsor, C.P.  1932.  \href{http://www.ncbi.nlm.nih.gov/pmc/articles/PMC1076153/pdf/pnas01729-0009.pdf}{The Gompertz curve as a growth curve}.  Proceedings of the National Academy of Sciences.  18:1-8.
#'
#' @keywords manip hplot
#'
#' @examples 
#' ###########################################################
#' ## See the equations
#' \dontrun{windows(8,5)}
#' vbModels()
#' \dontrun{windows(6,5)}
#' vbModels("seasonal")
#' vbModels("tagging")
#' \dontrun{windows(5,5)}
#' GompertzModels()
#' GompertzModels("tagging")
#' RichardsModels()
#' logisticModels()
#' 
#' ###########################################################
#' ## Simple Examples -- Von B
#' ( vb1 <- vbFuns() )
#' ages <- 0:20
#' plot(vb1(ages,Linf=20,K=0.3,t0=-0.2)~ages,type="b",pch=19)
#' ( vb2 <- vbFuns("Francis") )
#' plot(vb2(ages,L1=10,L2=19,L3=20,t1=2,t3=18)~ages,type="b",pch=19)
#' ( vb2c <- vbFuns("Francis",simple=TRUE) )   # compare to vb2
#'
#' ## Simple Examples -- Gompertz
#' ( gomp1 <- GompertzFuns() )
#' plot(gomp1(ages,Linf=800,gi=0.5,ti=5)~ages,type="b",pch=19)
#' ( gomp2 <- GompertzFuns("Ricker2") )
#' plot(gomp2(ages,L0=2,a=6,gi=0.5)~ages,type="b",pch=19)
#' ( gomp2c <- GompertzFuns("Ricker2",simple=TRUE) )   # compare to gomp2
#' ( gompT <- GompertzFuns("Troynikov1"))
#'
#' ## Simple Examples -- Richards
#' ( rich1 <- RichardsFuns() )
#' plot(rich1(ages,Linf=800,k=0.5,a=1,b=6)~ages,type="b",pch=19)
#' ( rich2 <- RichardsFuns(2) )
#' plot(rich2(ages,Linf=800,k=0.5,ti=3,b=6)~ages,type="b",pch=19)
#' ( rich3 <- RichardsFuns(3) )
#' plot(rich3(ages,Linf=800,k=0.5,ti=3,b=0.15)~ages,type="b",pch=19)
#' ( rich4 <- RichardsFuns(4) )
#' plot(rich4(ages,Linf=800,k=0.5,ti=3,b=0.95)~ages,type="b",pch=19)
#' lines(rich4(ages,Linf=800,k=0.5,ti=3,b=1.5)~ages,type="b",pch=19,col="blue")
#' ( rich5 <- RichardsFuns(5) )
#' plot(rich5(ages,Linf=800,k=0.5,L0=50,b=1.5)~ages,type="b",pch=19)
#' ( rich6 <- RichardsFuns(6) )
#' plot(rich6(ages,Linf=800,k=0.5,ti=3,Lninf=50,b=1.5)~ages,type="b",pch=19)
#' ( rich2c <- RichardsFuns(2,simple=TRUE) ) # compare to rich2
#' 
#' ## Simple Examples -- Logistic
#' ( log1 <- logisticFuns() )
#' plot(log1(ages,Linf=800,gninf=0.5,ti=5)~ages,type="b",pch=19)
#' ( log2 <- logisticFuns("CJ2") )
#' plot(log2(ages,Linf=800,gninf=0.5,a=10)~ages,type="b",pch=19)
#' ( log2c <- logisticFuns("CJ2",simple=TRUE) ) # compare to log2
#' ( log3 <- logisticFuns("Karkach") )
#' plot(log3(ages,L0=10,Linf=800,gninf=0.5)~ages,type="b",pch=19)
#' ( log4 <- logisticFuns("HaddonI") )
#'
#' 
#' ###########################################################
#' ## Examples of fitting
#' ##   After the last example a plot is constructed with three
#' ##   or four lines on top of each other illustrating that the
#' ##   parameterizations all produce the same fitted values.
#' ##   However, observe the correlations in the summary() results.
#' 
#' ## Von B
#' data(SpotVA1) 
#' # Fitting the typical paramaterization of the von B function
#' fit1 <- nls(tl~vb1(age,Linf,K,t0),data=SpotVA1,start=vbStarts(tl~age,data=SpotVA1))
#' summary(fit1,correlation=TRUE)
#' plot(tl~age,data=SpotVA1,pch=19)
#' curve(vb1(x,Linf=coef(fit1)),from=0,to=5,col="red",lwd=10,add=TRUE)
#'
#' # Fitting the Francis paramaterization of the von B function
#' fit2 <- nls(tl~vb2c(age,L1,L2,L3,t1=0,t3=5),data=SpotVA1,
#'             start=vbStarts(tl~age,data=SpotVA1,type="Francis",ages2use=c(0,5)))
#' summary(fit2,correlation=TRUE)
#' curve(vb2c(x,L1=coef(fit2)[1],L2=coef(fit2)[2],L3=coef(fit2)[3],t1=0,t3=5),
#'       from=0,to=5,col="blue",lwd=5,add=TRUE)
#'
#' # Fitting the Schnute parameterization of the von B function
#' vb3 <- vbFuns("Schnute")
#' fit3 <- nls(tl~vb3(age,L1,L3,K,t1=0,t3=4),data=SpotVA1,
#'             start=vbStarts(tl~age,data=SpotVA1,type="Schnute",ages2use=c(0,4)))
#' summary(fit3,correlation=TRUE)
#' curve(vb3(x,L1=coef(fit3),t1=c(0,4)),from=0,to=5,col="green",lwd=2,add=TRUE)
#'
#' ## Gompertz
#' # Make some fake data using the original parameterization
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
#' gomp3 <- GompertzFuns("QD3",simple=TRUE)
#' fit3 <- nls(len~gomp3(age,Linf,gi,t0),data=df,start=list(Linf=500,gi=0.3,t0=0))
#' summary(fit3,correlation=TRUE)
#' curve(gomp3(x,Linf=coef(fit3)[1],gi=coef(fit3)[2],t0=coef(fit3)[3]),
#'       from=0,to=15,col="green",lwd=2,add=TRUE)
#' 
#' ## Richards
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
#' ## Logistic
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

#' @rdname growthModels
#' @export
vbFuns <- function(type=c("Typical","typical","BevertonHolt",
                          "Original","original","vonBertalanffy",
                          "GQ","GallucciQuinn","Mooij","Weisberg",
                          "Schnute","Francis","Laslett","Polacheck",
                          "Somers","Somers2",
                          "Fabens","Fabens2","Wang","Wang2","Wang3"),
                   simple=FALSE,msg=FALSE) {
  Typical <- typical <- BevertonHolt <- function(t,Linf,K=NULL,t0=NULL) {
  if (length(Linf)==3) { K <- Linf[[2]]
                         t0 <- Linf[[3]]
                         Linf <- Linf[[1]] }
  Linf*(1-exp(-K*(t-t0)))
}
  STypical <- Stypical <- SBevertonHolt <- function(t,Linf,K,t0) {
    Linf*(1-exp(-K*(t-t0)))
  }
  Original <- original <- vonBertalanffy <- function(t,Linf,L0=NULL,K=NULL) {
  if (length(Linf)==3) { L0 <- Linf[[2]]
                         K <- Linf[[3]]
                         Linf <- Linf[[1]] }
  Linf-(Linf-L0)*exp(-K*t)
}
  SOriginal <- Soriginal <- SvonBertalanffy <- function(t,Linf,L0,K) {
    Linf-(Linf-L0)*exp(-K*t)
  }
  GQ <- GallucciQuinn <- function(t,omega,K=NULL,t0=NULL) {
  if (length(omega)==3) { K <- omega[[2]]
                          t0 <- omega[[3]]
                          omega <- omega[[1]] }
  (omega/K)*(1-exp(-K*(t-t0)))
}
  SGQ <- SGallucciQuinn <- function(t,omega,K,t0) {
    (omega/K)*(1-exp(-K*(t-t0)))
  }
  Mooij <- function(t,Linf,L0=NULL,omega=NULL) {
  if (length(Linf)==3) { L0 <- Linf[[2]]
                         omega <- Linf[[3]]
                         Linf <- Linf[[1]] }
  Linf-(Linf-L0)*exp(-(omega/Linf)*t)
}
  SMooij <- function(t,Linf,L0,omega) {
    Linf-(Linf-L0)*exp(-(omega/Linf)*t)
  }
  Weisberg <- function(t,Linf,t50=NULL,t0=NULL) {
  if (length(Linf)==3) { t50 <- Linf[[2]]
                         t0 <- Linf[[3]]
                         Linf <- Linf[[1]] }
  Linf*(1-exp(-(log(2)/(t50-t0))*(t-t0)))
}
  SWeisberg <- function(t,Linf,t50,t0) {
    Linf*(1-exp(-(log(2)/(t50-t0))*(t-t0)))
  } 
  Schnute <- function(t,L1,L3=NULL,K=NULL,t1,t3=NULL) {
  if (length(L1)==3) { L3 <- L1[[2]]; K <- L1[[3]]; L1 <- L1[[1]] }
  if (length(t1)==2) { t3 <- t1[[2]]; t1 <- t1[[1]] }
  L1+(L3-L1)*((1-exp(-K*(t-t1)))/(1-exp(-K*(t3-t1))))
}
  SSchnute <- function(t,L1,L3,K,t1,t3) {
    L1+(L3-L1)*((1-exp(-K*(t-t1)))/(1-exp(-K*(t3-t1))))
  }
  Francis <- function(t,L1,L2=NULL,L3=NULL,t1,t3=NULL) {
  if (length(L1)==3) { L2 <- L1[[2]]; L3 <- L1[[3]]; L1 <- L1[[1]] }
  if (length(t1)==2) { t3 <- t1[[2]]; t1 <- t1[[1]] }
  r <- (L3-L2)/(L2-L1)
  L1+(L3-L1)*((1-r^(2*((t-t1)/(t3-t1))))/(1-r^2))
}
  SFrancis <- function(t,L1,L2,L3,t1,t3) {
    r <- (L3-L2)/(L2-L1)
    L1+(L3-L1)*((1-r^(2*((t-t1)/(t3-t1))))/(1-r^2))
  }
  Laslett <- Polacheck <- function(t,Linf,K1,K2,t0,a,b) {
    if (length(Linf)==6) { K1 <- Linf[[2]]; K2 <- Linf[[3]]
    t0 <- Linf[[4]]; a <- Linf[[5]]
    b <- Linf[[6]]; Linf <- Linf[[1]] }
    Linf*(1-exp(-K2*(t-t0))*((1+exp(-b*(t-t0-a)))/(1+exp(a*b)))^(-(K2-K1)/b))
  }
  SLaslett <- SPolacheck <- function(t,Linf,K1,K2,t0,a,b) {
    Linf*(1-exp(-K2*(t-t0))*((1+exp(-b*(t-t0-a)))/(1+exp(a*b)))^(-(K2-K1)/b))
  }
  Somers <- function(t,Linf,K,t0,C,ts) {
  if (length(Linf)==5) { K <- Linf[[2]]; t0 <- Linf[[3]]
                         C <- Linf[[4]]; ts <- Linf[[5]]
                         Linf <- Linf[[1]] }
  St <- (C*K)/(2*pi)*sin(2*pi*(t-ts))
  Sto <- (C*K)/(2*pi)*sin(2*pi*(t0-ts))
  Linf*(1-exp(-K*(t-t0)-St+Sto))
}
  SSomers <- function(t,Linf,K,t0,C,ts) {
    Linf*(1-exp(-K*(t-t0)-(C*K)/(2*pi)*sin(2*pi*(t-ts))+(C*K)/(2*pi)*sin(2*pi*(t0-ts))))
  }
  Somers2 <- function(t,Linf,K,t0,C,WP) {
  if (length(Linf)==5) { K <- Linf[[2]]; t0 <- Linf[[3]]
                         C <- Linf[[4]]; WP <- Linf[[5]]
                         Linf <- Linf[[1]] }
  Rt <- (C*K)/(2*pi)*sin(2*pi*(t-WP+0.5))
  Rto <- (C*K)/(2*pi)*sin(2*pi*(t0-WP+0.5))
  Linf*(1-exp(-K*(t-t0)-Rt+Rto))
}
  SSomers2 <- function(t,Linf,K,t0,C,WP) {
    Linf*(1-exp(-K*(t-t0)-(C*K)/(2*pi)*sin(2*pi*(t-WP+0.5))+(C*K)/(2*pi)*sin(2*pi*(t0-WP+0.5))))
  }
  Fabens <- function(Lm,dt,Linf,K) {
  if (length(Linf)==2) { K <- Linf[[2]]; Linf <- Linf[[1]] }
  Lm+(Linf-Lm)*(1-exp(-K*dt))
}
  SFabens <- function(Lm,dt,Linf,K) {
    Lm+(Linf-Lm)*(1-exp(-K*dt))
  }
  Fabens2 <- function(Lm,dt,Linf,K) {
  if (length(Linf)==2) { K <- Linf[[2]]; Linf <- Linf[[1]] }
  (Linf-Lm)*(1-exp(-K*dt))
}
  SFabens2 <- function(Lm,dt,Linf,K) {
    (Linf-Lm)*(1-exp(-K*dt))
  }
  Wang <- function(Lm,dt,Linf,K,b) {
  if (length(Linf)==3) { b <- Linf[[3]]; K <- Linf[[2]]
                         Linf <- Linf[[1]] }
  (Linf+b*(Lm-mean(Lm))-Lm)*(1-exp(-K*dt))
}
  SWang <- function(Lm,dt,Linf,K,b) {
    (Linf+b*(Lm-mean(Lm))-Lm)*(1-exp(-K*dt))
  }
  Wang2 <- function(Lm,dt,K,a,b) {
    if (length(K)==3) { b <- K[[3]]; a <- K[[2]]; K <- K[[1]] }
    (a+b*Lm)*(1-exp(-K*dt))
  }
  SWang2 <- function(Lm,dt,K,a,b) {
    (a+b*Lm)*(1-exp(-K*dt))
  }
  Wang3 <- function(Lm,dt,K,a,b) {
    if (length(K)==3) { b <- K[[3]]; a <- K[[2]]; K <- K[[1]] }
    Lm+(a+b*Lm)*(1-exp(-K*dt))
  }
  SWang3 <- function(Lm,dt,K,a,b) {
    Lm+(a+b*Lm)*(1-exp(-K*dt))
  }
  type <- match.arg(type)
  if (msg) {
    switch(type,
      Typical=,typical=,BevertonHolt= {
        message("You have chosen the 'Typical'/'typical' or 'BevertonHolt' parameterization.\n\n",
                "  E[L|t] = Linf*(1-exp(-K*(t-t0)))\n\n",
                "  where Linf = asymptotic mean length\n",
                "           K = exponential rate of approach to Linf\n",
                "          t0 = the theoretical age when length = 0 (a modeling artifact)\n\n")
      },
      Original=,original=,vonBertalanffy={
        message("You have chosen the 'Original'/'original' or 'vonBertalanffy` parameterization.\n\n",
                "  E[L|t] = Linf-(Linf-L0)*exp(-K*t)\n\n",
                "  where Linf = asymptotic mean length\n",
                "          L0 = the mean length at age-0 (i.e., hatching or birth)\n",
                "           K = exponential rate of approach to Linf\n\n")
      },
      Francis={
        message("You have chosen the 'Francis' parameterization.\n\n",
                "  E[L|t] = L1+(L3-L1)*[(1-r^(2*[(t-t1)/(t3-t1)]))/(1-r^2)]\n\n",
                "  where r = [(L3-L2)/(L2-L1)] and\n\n",
                "       L1 = the mean length at the first (small) reference age\n",
                "       L2 = the mean length at the intermediate reference age\n",
                "       L3 = the mean length at the third (large) reference age\n\n",
                "You must also supply the constant values (i.e., they are NOT model parameters) for\n",
                "       t1 = the first (usually a younger) reference age\n",
                "       t3 = the third (usually an older) reference age\n\n")
      },
      GQ=,GallucciQuinn={
        message("You have chosen the 'GQ' or 'GallucciQuinn' parameterization.\n\n",
                "  E[L|t] = [omega/K]*(1-exp(-K*(t-t0)))\n\n",
                "  where omega = growth rate near t0\n",
                "            K = exponential rate of approach to Linf\n",
                "           t0 = the theoretical age when length = 0 (a modeling artifact)\n\n")
      },
      Mooij={
        message("You have chosen the 'Mooij' parameterization.\n\n",
                "  E[L|t] = Linf-(Linf-L0)*exp(-(omega/Linf)*t)\n\n",
                "  where Linf = asymptotic mean length\n",
                "          L0 = the mean length at age-0 (i.e., hatching or birth)\n",
                "       omega = growth rate near L0\n\n")
      },
      Weisberg= {
        message("You have chosen the 'Weisberg' parameterization.\n\n",
                "  E[L|t] = Linf*(1-exp(-(log(2)/(t50-t0))*(t-t0)))\n\n",
                "  where Linf = asymptotic mean length\n",
                "         t50 = age when half of Linf is reached\n",
                "          t0 = the theoretical age when length = 0 (a modeling artifact)\n\n")
      },
      Schnute={
        message("You have chosen the 'Schnute' parameterization.\n\n",
                "  E[L|t] = L1+(L2-L1)*[(1-exp(-K*(t-t1)))/(1-exp(-K*(t2-t1)))]\n\n",
                "  where L1 = the mean length at the youngest age in the sample\n",
                "        L2 = the mean length at the oldest age in the sample\n",
                "         K = exponential rate of approach to Linf\n\n",
                "  You must also supply the constant values (i.e., they are NOT model parameters) for\n",
                "        t1 = the youngest age in the sample\n",
                "        t2 = the oldest age in the sample\n\n")
      },
      Laslett=,Polacheck={
        message("You have chosen the 'Laslett/Polacheck' 'double' parameterization.\n\n",
                "  E[L|t] = Linf*[1-exp(-K2*(t-to))((1+exp(-b(t-t0-a)))/(1+exp(ab)))^(-(K2-K1)/b)]\n\n",
                "  where Linf = asymptotic mean length\n",
                "          t0 = the theoretical age when length = 0 (a modeling artifact)\n",
                "          K1 = the first (younger ages) exponential rate of approach to Linf\n",
                "          K2 = the second (older ages) exponential rate of approach to Linf\n",
                "           b = governs the rate of transition from K1 to K2\n",
                "           a = the central age of the transition from K1 to K2\n\n")
      },
      Somers={
        message("You have chosen the 'Somers Seasonal' parameterization.\n\n",
                "  E[L|t] = Linf*(1-exp(-K*(t-to)-St+St0))\n\n",
                "  where St = (CK/2*pi)*sin(2*pi*(t-ts)) and\n",
                "       St0 = (CK/2*pi)*sin(2*pi*(t0-ts)) and\n\n",
                "  and Linf = asymptotic mean length\n",
                "         K = exponential rate of approach to Linf\n",
                "        t0 = the theoretical age when length = 0 (a modeling artifact)\n",
                "         C = proportional growth depression at 'winter peak'\n",
                "        ts = time from t=0 until the first growth oscillation begins.\n\n")
      },
      Somers2={
        message("You have chosen the modified 'Somers2 Seasonal' parameterization.\n\n",
                "  E[L|t] = Linf*(1-exp(-K*(t-to)-Rt+Rt0))\n\n",
                "  where Rt = (CK/2*pi)*sin(2*pi*(t-WP+0.5)) and\n",
                "       Rt0 = (CK/2*pi)*sin(2*pi*(t0-WP+0.5)) and\n\n",
                "  and Linf = asymptotic mean length\n",
                "         K = exponential rate of approach to Linf\n",
                "        t0 = the theoretical age when length = 0 (a modeling artifact)\n",
                "         C = proportional growth depression at 'winter peak'\n",
                "        WP = the 'winter peak' (point of slowest growth).\n\n")
      },
      Fabens={
        message("You have chosen the 'Fabens' parameterization for tag-return data.\n\n",
                "  E[Lr|Lm,dt] = Lm + (Linf-Lm)*(1-exp(-K*dt))\n\n",
                "  where Linf = asymptotic mean length\n",
                "           K = exponential rate of approach to Linf\n\n",
                "  and the data are Lr = length at time of recapture\n",
                "                   Lm = length at time of marking\n",
                "                   dt = time between marking and recapture.\n\n")
      },
      Fabens2={
        message("You have chosen the 'Fabens2' parameterization for tag-return data.\n\n",
                "  E[Lr|Lm,dt] = (Linf-Lm)*(1-exp(-K*dt))\n\n",
                "  where Linf = asymptotic mean length\n",
                "           K = exponential rate of approach to Linf\n\n",
                "  and the data are Lr = length at time of recapture\n",
                "                   Lm = length at time of marking\n",
                "                   dt = time between marking and recapture.\n\n")
      },
      Wang={
        message("You have chosen the 'Wang' parameterization for tag-return data.\n\n",
                "  E[Lr-Lm|Lm,dt] = (Linf+b(Lm-E(Lm))-Lm)*(1-exp(-K*dt))\n\n",
                "  where Linf = asymptotic mean length\n",
                "           K = exponential rate of approach to Linf\n",
                "           b = parameter\n\n",
                "  and the data are Lr = length at time of recapture\n",
                "                   Lm = length at time of marking\n",
                "                   dt = time between marking and recapture.\n\n",
                "  and with E(Lm) = expectation (i.e., mean) of Lm.\n\n")
      },
      Wang2={
        message("You have chosen the 'Wang2' parameterization for tag-return data.\n\n",
                "  E[Lr-Lm|Lm,dt] = (a+bLm)*(1-exp(-K*dt))\n\n",
                "  where K = exponential rate of approach to Linf\n",
                "     a, b = parameters\n\n",
                "  and the data are Lr = length at time of recapture\n",
                "                   Lm = length at time of marking\n",
                "                   dt = time between marking and recapture.\n\n")
      },
      Wang3={
        message("You have chosen the 'Wang3' parameterization for tag-return data.\n\n",
                "  E[Lr|Lm,dt] = Lm+(a+bLm)*(1-exp(-K*dt))\n\n",
                "  where K = exponential rate of approach to Linf\n",
                "     a, b = parameters\n\n",
                "  and the data are Lr = length at time of recapture\n",
                "                   Lm = length at time of marking\n",
                "                   dt = time between marking and recapture.\n\n")
      }
    )
  }
  if (simple) type <- paste("S",type,sep="")
  get(type)
}



#' @rdname growthModels
#' @export
GompertzFuns <- function(type=c("Ricker1","Ricker2","Ricker3",
                                "QD1","QD2","QD3","KM","AFS","original",
                                "Troynikov1","Troynikov2"),
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
  Troynikov1 <- function(Lm,dt,Linf,gi=NULL) {
    if (length(Linf)==2) { gi=Linf[2]
    Linf=Linf[1] }
    Linf*((Lm/Linf)^exp(-gi*dt))-Lm
  }
  STroynikov1 <- function(Lm,dt,Linf,gi) {
    Linf*((Lm/Linf)^exp(-gi*dt))-Lm
  }
  Troynikov2 <- function(Lm,dt,Linf,gi=NULL) {
    if (length(Linf)==2) { gi=Linf[2]
    Linf=Linf[1] }
    Linf*((Lm/Linf)^exp(-gi*dt))
  }
  STroynikov2 <- function(Lm,dt,Linf,gi) {
    Linf*((Lm/Linf)^exp(-gi*dt))
  }
  ## Main function
  type <- match.arg(type)
  comcat <- "parameterization of the Gompertz function.\n\n"
  if (msg) {
    switch(type,
           original= {
             message("You have chosen the 'original'",comcat,
                     "  E[L|t] = Linf*exp(-exp(a-gi*t))\n\n",
                     "where Linf = asymptotic mean length\n",
                     "      gi = decrease in growth rate at the inflection point\n",
                     "      a = an undefined parameter\n\n")
           },
           Ricker1=,KM=,AFS= {
             message("You have chosen the 'Ricker1'/'KM'/'AFS'",comcat,
                     "  E[L|t] = Linf*exp(-exp(-gi*(t-ti)))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "          gi = instantaneous growth rate at the inflection point\n",
                     "          ti = time at the inflection point\n\n")
           },
           Ricker2=,QD1= {
             message("You have chosen the 'Ricker2'/'QD1'",comcat,
                     "  E[L|t] = L0*exp(a*(1-exp(-gi*t)))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "          gi = instantaneous growth rate at the inflection point\n",
                     "           a = dimenstionless parameter related to growth\n\n")
           },
           Ricker3=,QD2= {
             message("You have chosen the 'Ricker3'/'QD2'",comcat,
                     "  E[L|t] = Linf*exp(-(a/gi)*exp(-gi*t))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "          gi = instantaneous growth rate at the inflection point\n",
                     "           a = dimenstionless parameter related to growth\n\n")
           },
           QD3= {
             message("You have chosen the 'QD3'",comcat,
                     "  E[L|t] = Linf*exp(-(1/gi)*exp(-gi*(t-t0)))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "          gi = instantaneous growth rate at the inflection point\n",
                     "          t0 = a dimensionless parameter related to time/age\n\n")
           },
           Troynikov1= {
             message("You have chosen the 'Troynikov1'",comcat,
                     "  E[Lr-Lm|dt] = Linf*((Lm/Linf)^exp(-gi*dt))-Lm\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "          gi = instantaneous growth rate at the inflection point\n\n",
                     "  and the data are Lr = length at time of recapture\n",
                     "                   Lm = length at time of marking\n",
                     "                   dt = time between marking and recapture.\n")
           },
           Troynikov2= {
             message("You have chosen the 'Troynikov2'",comcat,
                     "  E[Lr|dt] = Linf*((Lm/Linf)^exp(-gi*dt))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "          gi = instantaneous growth rate at the inflection point\n\n",
                     "  and the data are Lr = length at time of recapture\n",
                     "                   Lm = length at time of marking\n",
                     "                   dt = time between marking and recapture.\n")
           }
    )
  }
  if (simple) type <- paste("S",type,sep="")
  get(type)
}




#' @rdname growthModels
#' @export
RichardsFuns <- function(type=1,simple=FALSE,msg=FALSE) {
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
  if (!type %in% 1:6) stop("'type' must be in 1:6.")
  type <- paste0("Richards",type)
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



#' @rdname growthModels
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



#' @rdname growthModels
#' @export
vbModels <- function(family=c("size","seasonal","tagging"),cex=1,...) {
  ## Set some plotting parameters
  op <- par(mar=c(0,0,3,0),cex=cex)
  ## Check the type argument
  family <- match.arg(family)
  ## Show the models
  if (family=="size") {
    plot(1,type="n",ylim=c(0,7),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
         main="FSA von Bertalanffy Parameterizations",...)
    iGrowthModels("vbTypical", 0,6.0)
    iGrowthModels("vbOriginal",0,4.0)
    iGrowthModels("vbGQ",      0,2.0)
    iGrowthModels("vbMooij",   0,0.5)
    abline(v=0.5)
    iGrowthModels("vbWeisberg",0.50,6.0)
    iGrowthModels("vbSchnute", 0.50,4.0)
    iGrowthModels("vbFrancis", 0.50,2.0)
    iGrowthModels("vbFrancis2",0.65,0.5)
  } else if (family=="seasonal") {
    plot(1,type="n",ylim=c(0,6),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
         main="FSA von Bertalanffy Seasonal Parameterizations",...)
    iGrowthModels("vbSomers1", 0,5.5)
    iGrowthModels("vbSomers1a", 0.15,4.5)
    iGrowthModels("vbSomers2",  0,2.5)
    iGrowthModels("vbSomers2a", 0.15,1.5)
  } else {
    plot(1,type="n",ylim=c(0,7),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
         main="FSA von Bertalanffy Tag-Recapture Parameterizations",...)
    iGrowthModels("vbFabens1", 0,6.5)
    iGrowthModels("vbFabens2", 0,5)
    iGrowthModels("vbWang1",   0,3.5)
    iGrowthModels("vbWang2",   0,2.0)
    iGrowthModels("vbWang3",   0,0.5)
  }
  ## Return to the default plotting parameters
  par(op)
}


#' @rdname growthModels
#' @export
GompertzModels <- function(family=c("size","tagging"),cex=1.25,...) {
  ## Set some plotting parameters
  op <- par(mar=c(0,0,3,0),cex=cex)
  ## Check the family argument
  family <- match.arg(family)
  ## Show the models
  if (family=="size") {
    plot(1,type="n",ylim=c(0,5),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
         main="FSA Gompertz Parameterizations",...)
    iGrowthModels("gOriginal", 0.1,4.5)
    iGrowthModels("gRicker1",  0.1,3.5)
    iGrowthModels("gRicker2",  0.1,2.5)
    iGrowthModels("gRicker3",  0.1,1.5)
    iGrowthModels("gQD3",      0.1,0.5)
  } else {
    plot(1,type="n",ylim=c(0,3),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",
         main="FSA Gompertz Tagging Parameterizations",...)
    iGrowthModels("gTroynikov1", 0.1,2.5)
    iGrowthModels("gTroynikov2", 0.1,1.5)
  }
  par(op)
}


#' @rdname growthModels
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



#' @rdname growthModels
#' @export
logisticModels <- function(family=c("size","tagging"),cex=1.25,...) {
  ## Set some plotting parameters
  op <- par(mar=c(0,0,3,0),cex=cex)
  ## Check the type argument
  family <- match.arg(family)
  ## Show the models
  if (family=="size") {
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


##############################################################
## Internal function for plotting the different models.
## Send positions in xpos and ypos.
##############################################################
iGrowthModels <- function(which,xpos,ypos) {
  switch(which,
         vbOriginal= {text(xpos,ypos,expression(plain("Original: ")~~~E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-Kt}),pos=4)},
         vbTypical=  {text(xpos,ypos,expression(plain("Typical: ")~~~E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])},")")),pos=4)},
         vbGQ=       {text(xpos,ypos,expression(plain("GQ: ")~~~E(L[t])==frac(omega,K)*~bgroup("(",1-e^{-K*(t~-~t[0])},")")),pos=4)},
         vbMooij=    {text(xpos,ypos,expression(plain("Mooij: ")~~~E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-frac(omega,L[infinity])*~t}),pos=4)},
         vbWeisberg= {text(xpos,ypos,expression(plain("Weisberg: ")~~~E(L[t])==L[infinity]*bgroup("(",1-e^{-frac(log(2),(t[50]~-~t[0]))*(t~-~t[0])},")")),pos=4)},
         vbSchnute=  {text(xpos,ypos,expression(plain("Schnute: ")~~~E(L[t])==L[1]+(L[3]-L[1])*~frac(1-e^{-K*(~t~-~t[1])},1-e^{-K*(~t[3]~-~t[1])})),pos=4)},
         vbFrancis=  {text(xpos,ypos,expression(plain("Francis: ")~~~E(L[t])==L[1]+(L[3]-L[1])*~frac(1-r^{2*frac(t-t[1],t[3]-t[1])},1-r^{2})),pos=4)},
         vbFrancis2= {text(xpos,ypos,expression(plain("where" )~r==frac(L[3]-L[2],L[2]-L[1])),pos=4)},
         
         vbSomers1=  {text(xpos,ypos,expression(plain("Somers1: ")~~~E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])-S(t)+S(t[0])},")")),pos=4)},
         vbSomers1a= {text(xpos,ypos,expression(plain("where" )~S(t)==bgroup("(",frac(C*K,2)*~pi,")")*~sin(2*pi*(t-t[s]))),pos=4)},
         vbSomers2=  {text(xpos,ypos,expression(plain("Somers2: ")~~~E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])-R(t)+R(t[0])},")")),pos=4)},
         vbSomers2a= {text(xpos,ypos,expression(plain("where" )~R(t)==bgroup("(",frac(C*K,2)*~pi,")")*~sin(2*pi*(t-WP+0.5))),pos=4)},
         
         vbFabens1=  {text(xpos,ypos,expression(plain("Fabens1: ")~~~E(L[r]-L[m])==(L[infinity]-L[m])*bgroup("(",1-e^{-K*Delta*t},")")),pos=4)},
         vbFabens2=  {text(xpos,ypos,expression(plain("Fabens2: ")~~~E(L[r])==L[m]+(L[infinity]-L[m])*bgroup("(",1-e^{-K*Delta*t},")")),pos=4)},
         vbWang1=  {text(xpos,ypos,expression(plain("Wang1: ")~~~E(L[r]-L[m])==(L[infinity]+beta*(L[t]-L[t])-L[m])*bgroup("(",1-e^{-K*Delta*t},")")),pos=4)},
         vbWang2=  {text(xpos,ypos,expression(plain("Wang2: ")~~~E(L[r]-L[m])==(alpha+beta*L[t])*bgroup("(",1-e^{-K*Delta*t},")")),pos=4)},
         vbWang3=  {text(xpos,ypos,expression(plain("Wang3: ")~~~E(L[r])==L[m]+(alpha+beta*L[t])*bgroup("(",1-e^{-K*Delta*t},")")),pos=4)},
         
         gOriginal=  {text(xpos,ypos,expression(plain("Original:         ")~~~E(L[t])==L[infinity]*~e^{-e^{a-g[i]*t}}),pos=4)},
         gRicker1=   {text(xpos,ypos,expression(plain("Ricker1:          ")~~~E(L[t])==L[infinity]*~e^{-e^{-g[i]*(t-t[i])}}),pos=4)},
         gRicker2=   {text(xpos,ypos,expression(plain("Ricker2, QD1: ")~~~E(L[t])==L[0]*~e^{a*bgroup("(",1-e^{-g[i]*t},")")}),pos=4)},
         gRicker3=   {text(xpos,ypos,expression(plain("Ricker3, QD2: ")~~~E(L[t])==L[infinity]*~e^{-a*~e^{-g[i]*t}}),pos=4)},
         gQD3=       {text(xpos,ypos,expression(plain("QD3:              ")~~~E(L[t])==L[infinity]*~e^{-~frac(1,g[i])*~e^{-g[i]*~(~t~-~t^{plain("*")})}}),pos=4)},
         gTroynikov1={text(xpos,ypos,expression(plain("Troynikov1: ")~~~E(L[r]-L[m])==L[infinity]*~bgroup("(",frac(L[m],L[infinity]),")")^{e^{-g[i]*Delta*t}}-L[m]),pos=4)},
         gTroynikov2={text(xpos,ypos,expression(plain("Troynikov2: ")~~~E(L[r])==L[infinity]*~bgroup("(",frac(L[m],L[infinity]),")")^{e^{-g[i]*Delta*t}}),pos=4)},

         CJ1=  {text(xpos,ypos,expression(plain("CJ1: ")~~~E(L[t])==frac(L[infinity],1+g[-infinity]*(t-t[i]))),pos=4)},
         CJ2=  {text(xpos,ypos,expression(plain("CJ2: ")~~~E(L[t])==frac(L[infinity],1+~ae^{-g[-infinity]*t})),pos=4)},
         Karkach= {text(xpos,ypos,expression(plain("Karkach: ")~~~E(L[t])==frac(L[0]*L[infinity],L[0]+(L[infinity]-L[0])*e^{-g[-infinity]*t})),pos=4)},
         HaddonI={text(xpos,ypos,expression(plain("HaddonI: ")~~~E(L[r]-L[m])==frac(Delta*L[max],1+e^{log(19)*frac(L[m]~-~L[50],L[95]~-~L[50])})),pos=4)},

         Richards1=  {text(xpos,ypos,expression(plain("Richards1: ")~~~E(L[t])==L[infinity]*~bgroup("(",1-a*e^{-kt},")")^{b}),pos=4)},
         Richards2=  {text(xpos,ypos,expression(plain("Richards2: ")~~~E(L[t])==L[infinity]*~bgroup("(",1-frac(1,b)*~e^{-k*(t-t[i])},")")^{~b}),pos=4)},
         Richards3=  {text(xpos,ypos,expression(plain("Richards3: ")~~~E(L[t])==frac(L[infinity],bgroup("(",1+b*e^{-k*(t-t[i])},")")^{~frac(1,b)})),pos=4)},
         Richards4=  {text(xpos,ypos,expression(plain("Richards4: ")~~~E(L[t])==L[infinity]*~bgroup("(",1+(b-1)*~e^{-k*(t-t[i])},")")^{~frac(1,1-b)}),pos=4)},
         Richards5=  {text(xpos,ypos,expression(plain("Richards5: ")~~~E(L[t])==L[infinity]*~bgroup("[",bgroup("(",1+bgroup("(",frac(L[0],L[infinity]),")")^{1-b}-1,")")*~e^{-k*t},"]")^{~frac(1,1-b)}),pos=4)},
         Richards6=  {text(xpos,ypos,expression(plain("Richards6: ")~~~E(L[t])==L[-infinity]+(L[infinity]-L[-infinity])*~bgroup("(",1+(b-1)*~e^{-k*(t-t[i])},")")^{~frac(1,1-b)}),pos=4)},
         
         Schnute1=  {text(xpos,ypos,expression(plain("Case 1: ")~~~E(L[t])==bgroup("[",L[1]^{b}+(L[3]^{b}-L[1]^{b})*~frac(1-e^{-a*(~t~-~t[1])},1-e^{-a*(~t[3]~-~t[1])}),"]")^{~frac(1,b)}),pos=4)},
         Schnute2=  {text(xpos,ypos,expression(plain("Case 2: ")~~~E(L[t])==L[1]*e^{log~bgroup("(",frac(L[3],L[1]),")")*~frac(1-e^{-a*(~t~-~t[1])},1-e^{-a*(~t[3]~-~t[1])})}),pos=4)},
         Schnute3=  {text(xpos,ypos,expression(plain("Case 3: ")~~~E(L[t])==bgroup("[",L[1]^{b}+(L[3]^{b}-L[1]^{b})*~frac(~t~-~t[1],~t[3]~-~t[1]),"]")^{~frac(1,b)}),pos=4)},
         Schnute4=  {text(xpos,ypos,expression(plain("Case 4: ")~~~E(L[t])==L[1]*e^{log~bgroup("(",frac(L[3],L[1]),")")*~frac(~t~-~t[1],~t[3]~-~t[1])}),pos=4)}
  ) # end swich
} ## end iGrowthModels internal function




#' @title The four-parameter growth function from Schnute (1981).
#'
#' @description The four-parameter growth function from Schnute (1981).  Use \code{SchnuteModels()} to see the equations for each growth function.
#'
#' @param t A numeric vector of ages over which to model growth.
#' @param case A string that indicates the case of the Schnute growth function to use.
#' @param t1 The (young) age that corresponds to \code{L1}.  Set to minimum value in \code{t} by default.
#' @param t3 The (old) age that corresponds to \code{L3}.  Set to maximum value in \code{t} by default.
#' @param L1 The mean size/length at \code{t1}.
#' @param L3 The mean size/length at \code{t3}.
#' @param a  A dimensionless parameter that is related to the time/age at the inflection point.
#' @param b A dimensionless parameter that is related to size/length at the inflection point.
#' @param cex A single numeric expansion value for use with \code{SchnuteModels}.
#' @param \dots Not implemented.
#' 
#' @return \code{Schnute} returns a predicted size given the case of the function and the provided parameter values.
#' 
#' \code{SchnuteModels} returns a graphic that uses \code{\link{plotmath}} to show the growth function equation in a pretty format.
#' 
#' @author Derek H. Ogle.
#'
#' @section IFAR Chapter: None specifically, but \href{https://fishr.wordpress.com/books/ifar/}{12-Individual Growth} is related.
#'
#' @seealso See \code{\link{vbFuns}}, \code{\link{GompertzFuns}}, \code{\link{RichardsFuns}}, and \code{\link{logisticFuns}} for similar functionality for other models.
#'
#' @references Schnute, J.  1981.  A versatile growth model with statistical stable parameters.  Canadian Journal of Fisheris and Aquatic Sciences 38:1128-1140.
#' 
#' @keywords manip
#'
#' @examples
#' ## See the formulae
#' \dontrun{windows(5,5)}
#' SchnuteModels()
#' 
#' ## Simple examples
#' ages <- 1:15
#' s1 <- Schnute(ages,case=1,t1=1,t3=15,L1=30,L3=400,a=0.3,b=1)
#' s2 <- Schnute(ages,case=2,t1=1,t3=15,L1=30,L3=400,a=0.3,b=1)
#' s3 <- Schnute(ages,case=3,t1=1,t3=15,L1=30,L3=400,a=0.3,b=1)
#' s4 <- Schnute(ages,case=4,t1=1,t3=15,L1=30,L3=400,a=0.3,b=1)
#'
#' plot(s1~ages,type="l",lwd=2)
#' lines(s2~ages,lwd=2,col="red")
#' lines(s3~ages,lwd=2,col="blue")
#' lines(s4~ages,lwd=2,col="green")
#' 
#' @rdname Schnute
#' @export
Schnute <- function(t,case=1,t1=NULL,t3=NULL,L1=NULL,L3=NULL,a=NULL,b=NULL) {
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


#' @rdname Schnute
#' @export
SchnuteModels <- function(cex=1.25,...) {
  op <- par(mar=c(0,0,3,0),cex=cex)
  plot(1,type="n",ylim=c(0,4),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",main="FSA Schnute Growth Model Cases",...)
  iGrowthModels("Schnute1", 0.1,3.5)
  iGrowthModels("Schnute2", 0.1,2.5)
  iGrowthModels("Schnute3", 0.1,1.5)
  iGrowthModels("Schnute4", 0.1,0.5)
  par(op)
}
