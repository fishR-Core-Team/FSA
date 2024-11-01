#' @name growthModels
#' 
#' @title Creates a function for a specific parameterization of the von Bertalanffy, Gompertz, Richards, and logistic growth functions.
#'
#' @description Creates a function for a specific parameterizations of the von Bertalanffy, Gompertz, Richards, and logistic growth functions. Use \code{growthFunShow()} to see the equations for each growth function.
#'
#' @param type A string (in \code{growthFunShow}) that indicates the type of growth function to show.
#' @param param A string (for von Bertalanffy, Gompertz, and logistic) or numeric (for Richards) that indicates the specific parameterization of the growth function. See details.
#' @param case A numeric that indicates the specific case of the Schnute function to use. See details.
#' @param simple A logical that indicates whether the function will accept all parameter values in the first parameter argument (\code{=FALSE}; DEFAULT) or whether all individual parameters must be specified in separate arguments (\code{=TRUE}).
#' @param msg A logical that indicates whether a message about the growth function and parameter definitions should be output (\code{=TRUE}) or not (\code{=FALSE}; DEFAULT).
#' @param plot A logical that indicates whether the growth function expression should be shown as an equation in a simple plot.
#' @param \dots Not implemented.
#' 
#' @return The functions ending in \code{xxxFuns} return a function that can be used to predict fish size given a vector of ages and values for the growth function parameters and, in some parameterizations, values for constants. The result should be saved to an object that is then the function name. When the resulting function is used, the parameters are ordered as shown when the definitions of the parameters are printed after the function is called (if \code{msg=TRUE}). If \code{simple=FALSE} (DEFAULT), then the values for all parameters may be included as a vector in the first parameter argument (but in the same order). Similarly, the values for all constants may be included as a vector in the first constant argument (i.e., \code{t1}). If \code{simple=TRUE}, then all parameters and constants must be declared individually. The resulting function is somewhat easier to read when \code{simple=TRUE}, but is less general for some applications.
#' 
#' An expression of the equation for each growth function may be created with \code{growthFunShow}. In this function \code{type=} is used to select the major function type (e.g., von Bertalanffy, Gompertz, Richards, Logistic, Schnute) and \code{param=} is used to select a specific parameterization of that growth function. If \code{plot=TRUE}, then a simple graphic will be created with the equation using \code{\link{plotmath}} for a pretty format.
#'
#' @note Take note of the following for parameterizations (i.e., \code{param}) of each growth function:
#' \itemize{
#'   \item von Bertalanffy
#'   \itemize{
#'     \item The \sQuote{Original} and \sQuote{vonBertalanffy} are synonymous as are \sQuote{Typical}, \sQuote{Traditional}, and \sQuote{BevertonHolt}. Further note that the \sQuote{Ogle} parameterization has the \sQuote{Original}/\sQuote{vonBertalanffy} and \sQuote{Typical}/\sQuote{Traditional}/\sQuote{BevertonHolt} parameterizations as special cases.
#'   }
#'   \item Gompertz
#'   \itemize{
#'     \item The \sQuote{Ricker2} and \sQuote{QuinnDeriso1} are synonymous, as are \sQuote{Ricker3} and \sQuote{QuinnDeriso2}.
#'     \item The parameterizations and parameters for the Gompertz function are varied and confusing in the literature. I have attempted to use a uniform set of parameters in these functions, but that makes a direct comparison to the literature difficult. Common sources for Gompertz models are listed in the references below. I make some comments here to aid comparisons to the literature.
#'     \item Within FSA, L0 is the mean length at age 0, Linf is the mean asymptotic length, ti is the age at the inflection point, gi is the instantaneous growth rate at the inflection point, t0 is a dimensionless parameter related to time/age, and a is a dimensionless parameter related to growth.
#'     \item In the Quinn and Deriso (1999) functions (the \sQuote{QuinnDerisoX} functions), the a parameter here is equal to lambda/K there and the gi parameter here is equal to the K parameter there. Also note that their Y is L here.
#'     \item In the Ricker (1979)[p. 705] functions (the \sQuote{RickerX} functions), the a parameter here is equal to k there and the gi parameter here is equal to the g parameter there. Also note that their w is L here. In the Ricker (1979) functions as presented in Campana and Jones (1992), the a parameter here is equal to k parameter there and the gi parameter here is equal to the G parameter there. Also note that their X is L here.
#'     \item The function in Ricker (1975)[p. 232] is the same as \sQuote{Ricker2} where the a parameter here is qual to G there and the gi parameter here is equal to the g parameter there. Also note that their w is L here.
#'     \item The function in Quist \emph{et al.} (2012)[p. 714] is the same as \sQuote{Ricker1} where the gi parameter here is equal to the G parameter there and the ti parameter here is equal to the t0 parameter there.
#'     \item The function in Katsanevakis and Maravelias (2008) is the same as \sQuote{Ricker1} where the gi parameter here is equal to k2 parameter there and the ti parameter here is equal to the t2 parameter there.
#'   }
#'   \item Richards
#'   \itemize{
#'     \item Within FSA, Linf is the mean asymptotic length, ti is the age at the inflection point, k controls the slope at the inflection point (maximum relative growth rate), b is dimensionless but related to the vertical position (i.e., size) of the inflection point, a is dimensionless but related to the horizontal position (i.e., age) of the inflection point, and L0 is the mean length at age-0.
#'     \item The parameterizations (1-6) correspond to functions/equations 1, 4, 5, 6, 7, and 8, respectively, in Tjorve and Tjorve (2010). Note that their A, S, k, d, and B are Linf, a, k, b, and L0, respectively, here (in FSA). Their (Tjorve and Tjorve 2010) K does not appear here.
#'   }
#'   \item logistic
#'   \itemize{
#'     \item Within FSA, L0 is the mean length at age 0, Linf is the mean asymptotic length, ti is the age at the inflection point, and gninf is the instantaneous growth rate at negative infinity.
#'   }
#' }
#'
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}, thanks to Gabor Grothendieck for a hint about using \code{get()}.
#'
#' @section IFAR Chapter: 12-Individual Growth.
#'
#' @seealso See \code{\link{Schnute}} for an implementation of the Schnute (1981) model.
#'
#' @references Ogle, D.H. 2016. \href{https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Campana, S.E. and C.M. Jones. 1992. Analysis of otolith microstructure data. Pages 73-100 In D.K. Stevenson and S.E. Campana, editors. Otolith microstructure examination and analysis. Canadian Special Publication of Fisheries and Aquatic Sciences 117. [Was (is?) from https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/141734.pdf.]
#' 
#' Fabens, A. 1965. Properties and fitting of the von Bertalanffy growth curve. Growth 29:265-289.
#'
#' Francis, R.I.C.C. 1988. Are growth parameters estimated from tagging and age-length data comparable?  Canadian Journal of Fisheries and Aquatic Sciences, 45:936-942.
#'
#' Gallucci, V.F. and T.J. Quinn II. 1979. Reparameterizing, fitting, and testing a simple growth model. Transactions of the American Fisheries Society, 108:14-25.
#'
#' Garcia-Berthou, E., G. Carmona-Catot, R. Merciai, and D.H. Ogle. A technical note on seasonal growth models. Reviews in Fish Biology and Fisheries 22:635-640.
#' 
#' Gompertz, B. 1825. On the nature of the function expressive of the law of human mortality, and on a new mode of determining the value of life contingencies. Philosophical Transactions of the Royal Society of London. 115:513-583.
#' 
#' Haddon, M., C. Mundy, and D. Tarbath. 2008. Using an inverse-logistic model to describe growth increments of blacklip abalone (\emph{Haliotis rubra}) in Tasmania. Fishery Bulletin 106:58-71. [Was (is?) from https://spo.nmfs.noaa.gov/sites/default/files/pdf-content/2008/1061/haddon.pdf.]
#'
#' Karkach, A. S. 2006. Trajectories and models of individual growth. Demographic Research 15:347-400. [Was (is?) from https://www.demographic-research.org/volumes/vol15/12/15-12.pdf.]
#' 
#' Katsanevakis, S. and C.D. Maravelias. 2008. Modeling fish growth: multi-model inference as a better alternative to a priori using von Bertalanffy equation. Fish and Fisheries 9:178-187.
#'
#' Mooij, W.M., J.M. Van Rooij, and S. Wijnhoven. 1999. Analysis and comparison of fish growth from small samples of length-at-age data: Detection of sexual dimorphism in Eurasian perch as an example. Transactions of the American Fisheries Society 128:483-490.
#'
#' Polacheck, T., J.P. Eveson, and G.M. Laslett. 2004. Increase in growth rates of southern bluefin tuna (\emph{Thunnus maccoyii}) over four decades: 1960 to 2000. Canadian Journal of Fisheries and Aquatic Sciences, 61:307-322.
#' 
#' Quinn, T. J. and R. B. Deriso. 1999. Quantitative Fish Dynamics. Oxford University Press, New York, New York. 542 pages.
#' 
#' Quist, M.C., M.A. Pegg, and D.R. DeVries. 2012. Age and growth. Chapter 15 in A.V. Zale, D.L Parrish, and T.M. Sutton, editors. Fisheries Techniques, Third Edition. American Fisheries Society, Bethesda, MD.
#' 
#' Richards, F. J. 1959. A flexible growth function for empirical use. Journal of Experimental Biology 10:290-300.
#' 
#' Ricker, W.E. 1975. Computation and interpretation of biological statistics of fish populations. Technical Report Bulletin 191, Bulletin of the Fisheries Research Board of Canada. [Was (is?) from https://publications.gc.ca/collections/collection_2015/mpo-dfo/Fs94-191-eng.pdf.]
#' 
#' Ricker, W.E. 1979. Growth rates and models. Pages 677-743 In W.S. Hoar, D.J. Randall, and J.R. Brett, editors. Fish Physiology, Vol. 8: Bioenergetics and Growth. Academic Press, New York, NY. [Was (is?) from https://books.google.com/books?id=CB1qu2VbKwQC&pg=PA705&lpg=PA705&dq=Gompertz+fish&source=bl&ots=y34lhFP4IU&sig=EM_DGEQMPGIn_DlgTcGIi_wbItE&hl=en&sa=X&ei=QmM4VZK6EpDAgwTt24CABw&ved=0CE8Q6AEwBw#v=onepage&q=Gompertz\%20fish&f=false.]
#'
#' Schnute, J. 1981. A versatile growth model with statistically stable parameters. Canadian Journal of Fisheries and Aquatic Sciences, 38:1128-1140.
#'
#' Somers, I. F. 1988. On a seasonally oscillating growth function. Fishbyte 6(1):8-11. [Was (is?) from https://www.fishbase.us/manual/English/fishbaseSeasonal_Growth.htm.]
#' 
#' Tjorve, E. and K. M. C. Tjorve. 2010. A unified approach to the Richards-model family for use in growth analyses: Why we need only two model forms. Journal of Theoretical Biology 267:417-425. [Was (is?) from https://www.researchgate.net/profile/Even_Tjorve/publication/46218377_A_unified_approach_to_the_Richards-model_family_for_use_in_growth_analyses_why_we_need_only_two_model_forms/links/54ba83b80cf29e0cb04bd24e.pdf.]
#' 
#' Troynikov, V. S., R. W. Day, and A. M. Leorke. Estimation of seasonal growth parameters using a stochastic Gompertz model for tagging data. Journal of Shellfish Research 17:833-838. [Was (is?) from https://www.researchgate.net/profile/Robert_Day2/publication/249340562_Estimation_of_seasonal_growth_parameters_using_a_stochastic_gompertz_model_for_tagging_data/links/54200fa30cf203f155c2a08a.pdf.]
#' 
#' Vaughan, D. S. and T. E. Helser. 1990. Status of the Red Drum stock of the Atlantic coast: Stock assessment report for 1989. NOAA Technical Memorandum NMFS-SEFC-263, 117 p. [Was (is?) from https://repository.library.noaa.gov/view/noaa/5927/noaa_5927_DS1.pdf.]
#'
#' Wang, Y.-G. 1998. An improved Fabens method for estimation of growth parameters in the von Bertalanffy model with individual asymptotes. Canadian Journal of Fisheries and Aquatic Sciences 55:397-400.
#'
#' Weisberg, S., G.R. Spangler, and L. S. Richmond. 2010. Mixed effects models for fish growth. Canadian Journal of Fisheries And Aquatic Sciences 67:269-277.
#' 
#' Winsor, C.P. 1932. The Gompertz curve as a growth curve. Proceedings of the National Academy of Sciences. 18:1-8. [Was (is?) from https://pmc.ncbi.nlm.nih.gov/articles/PMC1076153/pdf/pnas01729-0009.pdf.]
#'
#' @keywords manip hplot
#'
#' @examples 
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
#' ( log4 <- logisticFuns("Haddon") )
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
#' plot(tl~age,data=SpotVA1,pch=19)
#' 
#' # Fitting the typical parameterization of the von B function
#' fit1 <- nls(tl~vb1(age,Linf,K,t0),data=SpotVA1,
#'             start=vbStarts(tl~age,data=SpotVA1))
#' summary(fit1,correlation=TRUE)
#' curve(vb1(x,Linf=coef(fit1)),from=0,to=5,col="red",lwd=10,add=TRUE)
#'
#' # Fitting the Francis parameterization of the von B function
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
#' plot(len~age,data=df,pch=19,col=rgb(0,0,0,1/5))
#'   
#' # Fit first Ricker parameterization
#' fit1 <- nls(len~gomp1(age,Linf,gi,ti),data=df,start=list(Linf=500,gi=0.3,ti=3))
#' summary(fit1,correlation=TRUE)
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
#' 
#' \dontrun{
#' # Fit first Richards parameterization ... DOES NOT CONVERGE
#' fit1 <- nls(len~rich1(age,Linf,k,a,b),data=df,
#'             start=list(Linf=450,k=0.3,a=0.2,b=3))
#' summary(fit1,correlation=TRUE)
#' curve(rich1(x,Linf=coef(fit1)),from=0,to=15,col="red",lwd=10,add=TRUE)
#'
#' # Fit second Richards parameterization ... DOES NOT CONVERGE
#' fit2 <- nls(len~rich2(age,Linf,k,ti,b),data=df,
#'             start=list(Linf=450,k=0.25,ti=3,b=3))
#' summary(fit2,correlation=TRUE)
#' curve(rich2(x,Linf=coef(fit2)),from=0,to=15,col="blue",lwd=7,add=TRUE)
#' }
#'
#' plot(len~age,data=df,pch=19,col=rgb(0,0,0,1/5))
#' 
#' # Fit third Richards parameterization
#' fit3 <- nls(len~rich3(age,Linf,k,ti,b),data=df,
#'             start=list(Linf=450,k=0.25,ti=3,b=-0.1))
#' summary(fit3,correlation=TRUE)
#' curve(rich3(x,Linf=coef(fit3)),from=0,to=15,col="green",lwd=4,add=TRUE)
#' 
#' # Fit fourth Richards parameterization
#' fit4 <- nls(len~rich4(age,Linf,k,ti,b),data=df,
#'             start=list(Linf=450,k=0.25,ti=3,b=0.7))
#' summary(fit4,correlation=TRUE)
#' curve(rich4(x,Linf=coef(fit4)),from=0,to=15,col="black",lwd=1,add=TRUE)
#' 
#' ## Logistic
#' plot(len~age,data=df,pch=19,col=rgb(0,0,0,1/5))
#' 
#' # Fit first Campana-Jones parameterization
#' fit1 <- nls(len~log1(age,Linf,gninf,ti),data=df,
#'             start=list(Linf=450,gninf=0.45,ti=4))
#' summary(fit1,correlation=TRUE)
#' curve(log1(x,Linf=coef(fit1)),from=0,to=15,col="red",lwd=10,add=TRUE)
#'
#' # Fit second Campana-Jones parameterization
#' fit2 <- nls(len~log2(age,Linf,gninf,a),data=df,
#'             start=list(Linf=450,gninf=0.45,a=7))
#' summary(fit2,correlation=TRUE)
#' curve(log2(x,Linf=coef(fit2)),from=0,to=15,col="blue",lwd=5,add=TRUE)
#'
#' # Fit Karkach parameterization (using simple=TRUE model)
#' log3 <- logisticFuns("Karkach",simple=TRUE)
#' fit3 <- nls(len~log3(age,Linf,L0,gninf),data=df,
#'             start=list(Linf=450,L0=30,gninf=0.45))
#' summary(fit3,correlation=TRUE)
#' curve(log3(x,Linf=coef(fit3)[1],L0=coef(fit3)[2],gninf=coef(fit3)[3]),
#'       from=0,to=15,col="green",lwd=2,add=TRUE)
#'
#'
#' #############################################################################
#' ## Create expressions of the models
#' #############################################################################
#' # Typical von Bertalanffy ... Show as a stand-alone plot
#' growthFunShow("vonBertalanffy","Typical",plot=TRUE)
#' # Get and save the expression
#' ( tmp <- growthFunShow("vonBertalanffy","Typical") )
#' # Use expression as title on a plot
#' lens <- vb1(ages,Linf=20,K=0.3,t0=-0.2)
#' plot(lens~ages,type="b",pch=19,main=tmp)
#' # Put expression in the main plot
#' text(10,5,tmp)
#' # Put multiple expressions on a plot
#' op <- par(mar=c(0.1,0.1,0.1,0.1))
#' plot(0,type="n",xlab="",ylab="",xlim=c(0,1),ylim=c(0,3),xaxt="n",yaxt="n")
#' text(0,2.5,"Original:",pos=4)
#' text(0.5,2.5,growthFunShow("vonBertalanffy","Original"))
#' text(0,1.5,"Typical:",pos=4)
#' text(0.5,1.5,growthFunShow("vonBertalanffy","Typical"))
#' text(0,0.5,"Francis:",pos=4)
#' text(0.5,0.5,growthFunShow("vonBertalanffy","Francis"))
#' par(op)
NULL

#' @rdname growthModels
#' @export
vbFuns <- function(param=c("Typical","typical","Traditional","traditional","BevertonHolt",
                           "Original","original","vonBertalanffy",
                           "GQ","GallucciQuinn","Mooij","Weisberg","Ogle",
                           "Schnute","Francis","Laslett","Polacheck",
                           "Somers","Somers2","Pauly",
                           "Fabens","Fabens2","Wang","Wang2","Wang3",
                           "Francis2","Francis3"),
                   simple=FALSE,msg=FALSE) {
  Ogle <- function(t,Linf,K=NULL,tr=NULL,Lr=NULL) {
    if (length(Linf)==4) {
      Lr <- Linf[[4]]
      tr <- Linf[[3]]
      K <- Linf[[2]]
      Linf <- Linf[[1]] }
    Lr+(Linf-Lr)*(1-exp(-K*(t-tr)))
  }
  SOgle <- function(t,Linf,K,tr,Lr) {
    Lr+(Linf-Lr)*(1-exp(-K*(t-tr)))
  }
  Typical <- typical <- Traditional <- traditional <- BevertonHolt <- function(t,Linf,K=NULL,t0=NULL) {
    if (length(Linf)==3) { K <- Linf[[2]]
    t0 <- Linf[[3]]
    Linf <- Linf[[1]] }
    Linf*(1-exp(-K*(t-t0)))
  }
  STypical <- Stypical <- STraditional <- Straditional <- SBevertonHolt <- function(t,Linf,K,t0) {
    Linf*(1-exp(-K*(t-t0)))
  }
  Original <- original <- vonBertalanffy <- function(t,Linf,K=NULL,L0=NULL) {
    if (length(Linf)==3) { K <- Linf[[2]]
    L0 <- Linf[[3]]
    Linf <- Linf[[1]] }
    Linf-(Linf-L0)*exp(-K*t)
  }
  SOriginal <- Soriginal <- SvonBertalanffy <- function(t,Linf,K,L0) {
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
  Pauly <- function(t,Linf,Kpr=NULL,t0=NULL,ts=NULL,NGT=NULL) {
    if (length(Linf)==5) { Kpr <- Linf[[2]]; t0 <- Linf[[3]]
    ts <- Linf[[4]]; NGT <- Linf[[5]]
    Linf <- Linf[[1]] }
    tpr <- iCalc_tpr(t,ts,NGT)
    q <- Kpr*(tpr-t0) +
      (Kpr*(1-NGT)/(2*pi))*sin((2*pi)/(1-NGT)*(tpr-ts)) -
      (Kpr*(1-NGT)/(2*pi))*sin((2*pi)/(1-NGT)*(t0-ts))
    Linf*(1-exp(-q))
  }
  SPauly <- function(t,Linf,Kpr,t0,ts,NGT) {
    tpr <- iCalc_tpr(t,ts,NGT)
    q <- Kpr*(tpr-t0) +
      (Kpr*(1-NGT)/(2*pi))*sin((2*pi)/(1-NGT)*(tpr-ts)) -
      (Kpr*(1-NGT)/(2*pi))*sin((2*pi)/(1-NGT)*(t0-ts))
    Linf*(1-exp(-q))
  }
  Fabens <- function(Lm,dt,Linf,K=NULL) {
    if (length(Linf)==2) { K <- Linf[[2]]; Linf <- Linf[[1]] }
    (Linf-Lm)*(1-exp(-K*dt))
  }
  SFabens <- function(Lm,dt,Linf,K) {
    (Linf-Lm)*(1-exp(-K*dt))
  }
  Fabens2 <- function(Lm,dt,Linf,K=NULL) {
    if (length(Linf)==2) { K <- Linf[[2]]; Linf <- Linf[[1]] }
    Lm+(Linf-Lm)*(1-exp(-K*dt))
  }
  SFabens2 <- function(Lm,dt,Linf,K) {
    Lm+(Linf-Lm)*(1-exp(-K*dt))
  }
  Wang <- function(Lm,dt,Linf,K=NULL,b=NULL) {
    if (length(Linf)==3) { b <- Linf[[3]]; K <- Linf[[2]]
    Linf <- Linf[[1]] }
    (Linf+b*(Lm-mean(Lm))-Lm)*(1-exp(-K*dt))
  }
  SWang <- function(Lm,dt,Linf,K,b) {
    (Linf+b*(Lm-mean(Lm))-Lm)*(1-exp(-K*dt))
  }
  Wang2 <- function(Lm,dt,K,a=NULL,b=NULL) {
    if (length(K)==3) { b <- K[[3]]; a <- K[[2]]; K <- K[[1]] }
    (a+b*Lm)*(1-exp(-K*dt))
  }
  SWang2 <- function(Lm,dt,K,a,b) {
    (a+b*Lm)*(1-exp(-K*dt))
  }
  Wang3 <- function(Lm,dt,K,a=NULL,b=NULL) {
    if (length(K)==3) { b <- K[[3]]; a <- K[[2]]; K <- K[[1]] }
    Lm+(a+b*Lm)*(1-exp(-K*dt))
  }
  SWang3 <- function(Lm,dt,K,a,b) {
    Lm+(a+b*Lm)*(1-exp(-K*dt))
  }
  Francis2 <- function(Lm,dt,g1,g2=NULL,L1,L2=NULL) {
    if (length(g1)==2) { g2 <- g1[[2]]; g1 <- g1[[1]] }
    if (length(L1)==2) { L2 <- L1[[2]]; L1 <- L1[[1]] }
    ((L2*g1-L1*g2)/(g1-g2)-Lm)*(1-(1+(g1-g2)/(L1-L2))^dt)
  }
  sFrancis2 <- function(Lm,dt,g1,g2,L1,L2) {
    ((L2*g1-L1*g2)/(g1-g2)-Lm)*(1-(1+(g1-g2)/(L1-L2))^dt)
  }
  Francis3 <- function(Lm,t1,t2,g1,g2=NULL,w=NULL,u=NULL,L1,L2=NULL) {
    if (length(g1)==2) { g2 <- g1[[2]]; g1 <- g1[[1]] }
    if (length(L1)==2) { L2 <- L1[[2]]; L1 <- L1[[1]] }
    S1 <- u*sin(2*pi*(t1-w))/(2*pi)
    S2 <- u*sin(2*pi*(t2-w))/(2*pi)
    ((L2*g1-L1*g2)/(g1-g2)-Lm)*(1-(1+(g1-g2)/(L1-L2))^((t2-t1)+S2-S1))
  }
  sFrancis3 <- function(Lm,t1,t2,g1,g2,w,u,L1,L2) {
    S1 <- u*sin(2*pi*(t1-w))/(2*pi)
    S2 <- u*sin(2*pi*(t2-w))/(2*pi)
    ((L2*g1-L1*g2)/(g1-g2)-Lm)*(1-(1+(g1-g2)/(L1-L2))^((t2-t1)+S2-S1))
  }
  
  param <- match.arg(param)
  if (msg) {
    switch(param,
           Ogle= {
             message("You have chosen the 'Ogle-Isermann' parameterization.\n\n",
                     "  E[L|t] = (Linf-Lr)*(1-exp(-K*(t-tr)))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "           K = exponential rate of approach to Linf\n",
                     "          tr = mean age at Lr\n",
                     "          Lr = mean length at tr\n\n",
                     "NOTE: either tr or Lr must be set by the user.\n\n")
           },
           Typical=,typical=,Traditional=,traditional=,BevertonHolt= {
             message("You have chosen the 'Typical', 'Traditional', or 'BevertonHolt' parameterization.\n\n",
                     "  E[L|t] = Linf*(1-exp(-K*(t-t0)))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "           K = exponential rate of approach to Linf\n",
                     "          t0 = the theoretical age when length = 0 (a modeling artifact)\n\n")
           },
           Original=,original=,vonBertalanffy={
             message("You have chosen the 'Original' or 'vonBertalanffy` parameterization.\n\n",
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
                     "You must also give values (i.e., they are NOT model parameters) for\n",
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
                     "  You must also give values (i.e., they are NOT model parameters) for\n",
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
           Pauly={
             message("You have chosen the 'Pauly Seasonal Cessation' parameterization.\n\n",
                     "  E[L|t] = Linf*(1-exp(-K'*(t'-to)-Vt'+Vt0))\n\n",
                     "  where Vt' = (K'(1-NGT)/2*pi)*sin(2*pi*(t'-ts)/(1-NGT)) and\n",
                     "        Vt0 = (K'(1-NGT)/2*pi)*sin(2*pi*(t0-ts)/(1-NGT)) and\n\n",
                     "  and Linf = asymptotic mean length\n",
                     "        K' = exponential rate of approach to Linf during growth period\n",
                     "        t0 = the theoretical age when length = 0 (a modeling artifact)\n",
                     "        ts = time from t=0 until the first growth oscillation begins\n",
                     "       NGT = length of no-growth period.\n\n")
           },
           Fabens={
             message("You have chosen the 'Fabens' parameterization for tag-return data.\n\n",
                     "  E[dL|Lm,dt] = (Linf-Lm)*(1-exp(-K*dt))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "           K = exponential rate of approach to Linf\n\n",
                     "  and the data are dL = change in length (from mark to recapture)\n",
                     "                   Lm = length at time of marking\n",
                     "                   dt = time between marking and recapture.\n\n")
           },
           Fabens2={
             message("You have chosen the 'Fabens2' parameterization for tag-return data.\n\n",
                     "  E[Lr|Lm,dt] = Lm + (Linf-Lm)*(1-exp(-K*dt))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "           K = exponential rate of approach to Linf\n\n",
                     "  and the data are Lr = length at time of recapture\n",
                     "                   Lm = length at time of marking\n",
                     "                   dt = time between marking and recapture.\n\n")
           },
           Wang={
             message("You have chosen the 'Wang' parameterization for tag-return data.\n\n",
                     "  E[dL|Lm,dt] = (Linf+b(Lm-E(Lm))-Lm)*(1-exp(-K*dt))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "           K = exponential rate of approach to Linf\n",
                     "           b = parameter\n\n",
                     "  and the data are dL = change in length (from mark to recapture)\n",
                     "                   Lm = length at time of marking\n",
                     "                   dt = time between marking and recapture.\n\n",
                     "  and with E(Lm) = expectation (i.e., mean) of Lm.\n\n")
           },
           Wang2={
             message("You have chosen the 'Wang2' parameterization for tag-return data.\n\n",
                     "  E[dL|Lm,dt] = (a+bLm)*(1-exp(-K*dt))\n\n",
                     "  where K = exponential rate of approach to Linf\n",
                     "     a, b = parameters\n\n",
                     "  and the data are dL = change in length (from mark to recapture)\n",
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
           },
           Francis2={
             message("You have chosen the 'Francis2' parameterization for tag-return data.\n\n",
                     "  E[dL|Lm,dt] = ((L2g1-L1g2)/(g1-g2)-Lm)*(1-(1+(g1-g2)/(L1-L2))^dt)\n\n",
                     "  where g1 = mean growth rate at the first (small) reference length L1\n",
                     "        g2 = mean growth rate at the second (large) reference length L2\n\n",
                     "You must also give values (i.e., they are NOT model parameters) for\n",
                     "       L1 = the first (usually a shorter) reference length\n",
                     "       L2 = the second (usually a longer) reference length\n",
                     "The data are dL = change in length (from mark to recapture)\n",
                     "             Lm = length at time of marking\n",
                     "             dt = time between marking and recapture.\n\n")
           },
           Francis3={
             message("You have chosen the 'Francis3' parameterization for tag-return data\n",
                     "  with a seasonal component.\n\n",
                     "  E[dL|Lm,t1,t2] = ((L2g1-L1g2)/(g1-g2)-Lm)*(1-(1+(g1-g2)/(L1-L2))^((t2-t1)+S2-S1))\n\n",
                     "  where S1 = u*sin(2*pi*(t1-w))/(2*pi) and\n",
                     "        S2 = u*sin(2*pi*(t2-w))/(2*pi) and\n\n",
                     "  where g1 = mean growth rate at the first (small) reference length L1\n",
                     "        g2 = mean growth rate at the second (large) reference length L2\n",
                     "        w  = time of year when the growth rate is maximum\n",
                     "        u  = describes the extent of seasonality.\n\n",
                     "You must also give values (i.e., they are NOT model parameters) for\n",
                     "       L1 = the first (usually a shorter) reference length\n",
                     "       L2 = the second (usually a longer) reference length\n",
                     "The data are dL = change in length (from mark to recapture)\n",
                     "                   Lm = length at time of marking\n",
                     "                   t1 = time at marking\n",
                     "                   t2 = time at recapture.\n\n")
           }
           
    )
  }
  if (simple) param <- paste0("S",param)
  get(param)
}



#' @rdname growthModels
#' @export
GompertzFuns <- function(param=c("Ricker1","Ricker2","Ricker3",
                                 "QuinnDeriso1","QuinnDeriso2","QuinnDeriso3",
                                 "QD1","QD2","QD3",
                                 "Original","original",
                                 "Troynikov1","Troynikov2"),
                         simple=FALSE,msg=FALSE) {
  Original <- original <- function(t,Linf,a=NULL,gi=NULL) {
    if (length(Linf)==3) { a <- Linf[[2]]
    gi <- Linf[[3]]
    Linf <- Linf[[1]] }
    Linf*exp(-exp(a-gi*t))
  }
  SOriginal <- Soriginal <-function(t,Linf,a,gi) {
    Linf*exp(-exp(a-gi*t))
  }
  Ricker1 <- function(t,Linf,gi=NULL,ti=NULL) {
    if (length(Linf)==3) { gi <- Linf[[2]]
    ti <- Linf[[3]]
    Linf <- Linf[[1]] }
    Linf*exp(-exp(-gi*(t-ti)))
  }
  SRicker1 <- function(t,Linf,gi,ti) {
    Linf*exp(-exp(-gi*(t-ti)))
  }
  QD1 <- QuinnDeriso1 <- Ricker2 <- function(t,L0,a=NULL,gi=NULL) {
    if (length(L0)==3) { a <- L0[[2]]
    gi <- L0[[3]]
    L0 <- L0[[1]] }
    L0*exp(a*(1-exp(-gi*t)))
  }
  SQD1 <- SQuinnDeriso1 <- SRicker2 <- function(t,L0,a,gi) {
    L0*exp(a*(1-exp(-gi*t)))
  }
  QD2 <- QuinnDeriso2 <- Ricker3 <-  function(t,Linf,a=NULL,gi=NULL) {
    if (length(Linf)==3) { a <- Linf[[2]]
    gi <- Linf[[3]]
    Linf <- Linf[[1]] }
    Linf*exp(-a*exp(-gi*t))
  }
  SQD2 <- SQuinnDeriso2 <- SRicker3 <- function(t,Linf,a,gi) {
    Linf*exp(-a*exp(-gi*t))
  }
  QD3 <- QuinnDeriso3 <- function(t,Linf,gi=NULL,t0=NULL) {
    if (length(Linf)==3) { gi <- Linf[[2]]
    t0 <- Linf[[3]]
    Linf <- Linf[[1]] }
    Linf*exp(-(1/gi)*exp(-gi*(t-t0)))
  }
  SQD3 <- SQuinnDeriso3 <- function(t,Linf,gi,t0) {
    Linf*exp(-(1/gi)*exp(-gi*(t-t0)))
  }
  Troynikov1 <- function(Lm,dt,Linf,gi=NULL) {
    if (length(Linf)==2) { gi <- Linf[2]
    Linf <- Linf[1] }
    Linf*((Lm/Linf)^exp(-gi*dt))-Lm
  }
  STroynikov1 <- function(Lm,dt,Linf,gi) {
    Linf*((Lm/Linf)^exp(-gi*dt))-Lm
  }
  Troynikov2 <- function(Lm,dt,Linf,gi=NULL) {
    if (length(Linf)==2) { gi <- Linf[2]
    Linf <- Linf[1] }
    Linf*((Lm/Linf)^exp(-gi*dt))
  }
  STroynikov2 <- function(Lm,dt,Linf,gi) {
    Linf*((Lm/Linf)^exp(-gi*dt))
  }
  ## Main function
  param <- match.arg(param)
  comcat <- "parameterization of the Gompertz function.\n\n"
  if (msg) {
    switch(param,
           Original=,original= {
             message("You have chosen the 'Original' ",comcat,
                     "  E[L|t] = Linf*exp(-exp(a-gi*t))\n\n",
                     "where Linf = asymptotic mean length\n",
                     "      gi = decrease in growth rate at the inflection point\n",
                     "      a = an undefined parameter\n\n")
           },
           Ricker1= {
             message("You have chosen the 'Ricker1' ",comcat,
                     "  E[L|t] = Linf*exp(-exp(-gi*(t-ti)))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "          gi = instantaneous growth rate at the inflection point\n",
                     "          ti = time at the inflection point\n\n")
           },
           Ricker2=,QD1=,QuinnDeriso1= {
             message("You have chosen the 'Ricker2'/'QuinnDeriso1' ",comcat,
                     "  E[L|t] = L0*exp(a*(1-exp(-gi*t)))\n\n",
                     "  where L0 = the mean length at age-0 (i.e., hatching or birth)\n",
                     "        gi = instantaneous growth rate at the inflection point\n",
                     "         a = dimenstionless parameter related to growth\n\n")
           },
           Ricker3=,QD2=,QuinnDeriso2= {
             message("You have chosen the 'Ricker3'/'QuinnDeriso2' ",comcat,
                     "  E[L|t] = Linf*exp(-(a/gi)*exp(-gi*t))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "          gi = instantaneous growth rate at the inflection point\n",
                     "           a = dimenstionless parameter related to growth\n\n")
           },
           QD3=,QuinnDeriso3= {
             message("You have chosen the 'QuinnDeriso3' ",comcat,
                     "  E[L|t] = Linf*exp(-(1/gi)*exp(-gi*(t-t0)))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "          gi = instantaneous growth rate at the inflection point\n",
                     "          t0 = a dimensionless parameter related to time/age\n\n")
           },
           Troynikov1= {
             message("You have chosen the 'Troynikov1' ",comcat,
                     "  E[Lr-Lm|dt] = Linf*((Lm/Linf)^exp(-gi*dt))-Lm\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "          gi = instantaneous growth rate at the inflection point\n\n",
                     "  and the data are Lr = length at time of recapture\n",
                     "                   Lm = length at time of marking\n",
                     "                   dt = time between marking and recapture.\n")
           },
           Troynikov2= {
             message("You have chosen the 'Troynikov2' ",comcat,
                     "  E[Lr|dt] = Linf*((Lm/Linf)^exp(-gi*dt))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "          gi = instantaneous growth rate at the inflection point\n\n",
                     "  and the data are Lr = length at time of recapture\n",
                     "                   Lm = length at time of marking\n",
                     "                   dt = time between marking and recapture.\n")
           }
    )
  }
  if (simple) param <- paste0("S",param)
  get(param)
}




#' @rdname growthModels
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
  if (!param %in% 1:6) STOP("'param' must be in 1:6.")
  param <- paste0("Richards",param)
  if (msg) {
    switch(param,
           Richards1= {
             message("You have chosen the '",param,"' parameterization.",
                     "  E[L|t] = Linf*(1-a*exp(-k*t))^b\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "           k = controls the slope at the inflection point\n",
                     "           a = a dimensionless parameter that controls the x- value of the inflection point\n",
                     "           b = a dimensionless parameter that controls the y- value of the inflection point\n\n")
           },
           Richards2= {
             message("You have chosen the '",param,"' parameterization.",
                     "  Linf*(1-(1/b)*exp(-k*(t-ti)))^b\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "           k = controls the slope at the inflection point\n",
                     "          ti = time/age at the inflection point\n",
                     "           b = a dimensionless parameter that controls the y- value of the inflection point\n\n")
           },
           Richards3= {
             message("You have chosen the '",param,"' parameterization.",
                     "  Linf/((1+b*exp(-k*(t-ti)))^(1/b))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "           k = controls the slope at the inflection point\n",
                     "          ti = time/age at the inflection point\n",
                     "           b = a dimensionless parameter that controls the y- value of the inflection point\n\n")
           },
           Richards4= {
             message("You have chosen the '",param,"' parameterization.",
                     "  Linf*(1+(b-1)*exp(-k*(t-ti)))^(1/(1-b))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "           k = controls the slope at the inflection point\n",
                     "          ti = time/age at the inflection point\n",
                     "           b = a dimensionless parameter that controls the y- value of the inflection point\n\n")
           },
           Richards5= {
             message("You have chosen the '",param,"' parameterization.",
                     "  Linf*(1+(((L0/Linf)^(1-b))-1)*exp(-k*t))^(1/(1-b))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "           k = controls the slope at the inflection point\n",
                     "          L0 = mean length at t=0\n",
                     "           b = a dimensionless parameter that controls the y- value of the inflection point\n\n")
           },
           Richards6= {
             message("You have chosen the '",param,"' parameterization.",
                     "  Lninf+(Linf-Lninf)*(1+(b-1)*exp(-k*(t-ti)))^(1/(1-b))\n\n",
                     "  where Linf = upper asymptotic mean length\n",
                     "           k = controls the slope at the inflection point\n",
                     "       Lninf = lower asymptotic mean length\n",
                     "          ti = time/age at the inflection point\n",
                     "           b = a dimensionless parameter that controls the y- value of the inflection point\n\n")
           }
    )
  }
  if (simple) param <- paste0("S",param)
  get(param)
}



#' @rdname growthModels
#' @export
logisticFuns <- function(param=c("CJ1","CJ2","Karkach","Haddon","CampanaJones1","CampanaJones2"),
                         simple=FALSE,msg=FALSE) {
  CJ1 <- CampanaJones1 <- function(t,Linf,gninf=NULL,ti=NULL) {
    if (length(Linf)==3) { gninf <- Linf[[2]]
    ti <- Linf[[3]]
    Linf <- Linf[[1]] }
    Linf/(1+exp(-gninf*(t-ti)))
  }
  SCJ1 <- SCampanaJones1 <- function(t,Linf,gninf,ti) {
    Linf/(1+exp(-gninf*(t-ti)))
  }
  CJ2 <- CampanaJones2 <- function(t,Linf,gninf=NULL,a=NULL) {
    if (length(Linf)==3) { gninf <- Linf[[2]]
    a <- Linf[[3]]
    Linf <- Linf[[1]] }
    Linf/(1+a*exp(-gninf*t))
  }
  SCJ2 <- SCampanaJones2 <- function(t,Linf,gninf,a) {
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
  Haddon <- function(Lm,dLmax,L50=NULL,L95=NULL) {
    if (length(dLmax)==3) { L50 <- dLmax[2]
    L95 <- dLmax[3]
    dLmax <- dLmax[1] }
    dLmax/(1+exp(log(19)*((Lm-L50)/(L95-L50))))
  }
  SHaddon <- function(Lm,dLmax,L50,L95) {
    dLmax/(1+exp(log(19)*((Lm-L50)/(L95-L50))))
  }
  ## Main function
  param <- match.arg(param)
  comcat <- "parameterization of the logistic growth function.\n\n"
  if (msg) {
    switch(param,
           CJ1=,CampanaJones1= {
             message("You have chosen the 'CampanaJones1' ",comcat,
                     "  E[L|t] = Linf/(1+exp(-gninf*(t-ti)))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "      gninif = instantaneous growth rate at t=-infinity\n",
                     "          ti = time at the inflection point\n\n")
           },
           CJ2=,CampanaJones2= {
             message("You have chosen the 'CampanaJones2' ",comcat,
                     "  E[L|t] = Linf/(1+a*exp(-gninf*t))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "          gi = instantaneous growth rate at the inflection point\n",
                     "           a = a dimensionless parameter related to growth\n\n")
           },
           Karkach= {
             message("You have chosen the 'Karkach' ",comcat,
                     "  E[L|t] = L0*Linf/(L0+(Linf-L0)*exp(-gninf*t))\n\n",
                     "  where Linf = asymptotic mean length\n",
                     "          L0 = mean length at time/age 0\n",
                     "          gi = instantaneous growth rate at the inflection point\n\n")
           },
           Haddon= {
             message("You have chosen the 'Haddon Inverse' ",comcat,
                     "  E[Lr-Lm|dt] = dLmax/(1+exp(log(19)*((Lm-L50)/(L95-L50))))\n\n",
                     "  where dLmax = maximum growth increment during the study\n",
                     "          L50 = length at marking to produces a growth increment of 0.5*dLmax",
                     "          L95 = length at marking to produces a growth increment of 0.95*dLmax\n\n",
                     "  and the data are Lr = length at time of recapture\n",
                     "                   Lm = length at time of marking\n")
           }
    )
  }
  if (simple) param <- paste0("S",param)
  get(param)
}


#' @title The four-parameter growth function from Schnute (1981).
#'
#' @description The four-parameter growth function from Schnute (1981).
#'
#' @param t A numeric vector of ages over which to model growth.
#' @param case A string that indicates the case of the Schnute growth function to use.
#' @param t1 The (young) age that corresponds to \code{L1}. Set to minimum value in \code{t} by default.
#' @param t3 The (old) age that corresponds to \code{L3}. Set to maximum value in \code{t} by default.
#' @param L1 The mean size/length at \code{t1}.
#' @param L3 The mean size/length at \code{t3}.
#' @param a  A dimensionless parameter that is related to the time/age at the inflection point.
#' @param b A dimensionless parameter that is related to size/length at the inflection point.
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
#' growthFunShow("Schnute",1,plot=TRUE)
#' growthFunShow("Schnute",2,plot=TRUE)
#' growthFunShow("Schnute",3,plot=TRUE)
#' growthFunShow("Schnute",4,plot=TRUE)
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


#' @title The five-parameter growth function from Schnute and Richards (1990).
#'
#' @description The five-parameter growth function from Schnute and Richards (1990). Note that this function is slightly modified (a \sQuote{+} was changed to a \sQuote{-} so that the \sQuote{a} parameter will be positive) from the original in Schnute and Richards (1990)
#'
#' @param t A numeric vector of ages over which to model growth.
#' @param Linf Mean asymptotic length.
#' @param k The "growth coefficient" with units of (year^(-c)).
#' @param a A dimensionless parameter
#' @param b A dimensionless parameter.
#' @param c A dimensionless parameter.
#' 
#' @return \code{SchnuteRichards} returns a predicted size given the provided parameter values.
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#'
#' @section IFAR Chapter: None specifically, but 12-Individual Growth is related.
#'
#' @seealso See \code{\link{vbFuns}}, \code{\link{GompertzFuns}}, \code{\link{RichardsFuns}}, \code{\link{logisticFuns}}, and \code{\link{Schnute}} for similar functionality for other models.
#'
#' @references Schnute, J.T. and L.J. Richards. 1990. A unified approach to the analysis of fish growth, maturity, and survivorship data. Canadian Journal of Fisheries and Aquatic Sciences. 47:24-40.
#' 
#' @keywords manip
#'
#' @examples
#' ## See the formulae
#' growthFunShow("SchnuteRichards",plot=TRUE)
#' 
#' ## Simple examples
#' ages <- 1:15
#' s1 <- SchnuteRichards(ages,Linf=100,k=0.03,a=0.01,b=0.005,c=2)
#' plot(s1~ages,type="l",lwd=2)
#' 
#' @rdname SchnuteRichards
#' @export
SchnuteRichards <- function(t,Linf=NULL,k=NULL,a=NULL,b=NULL,c=NULL) {
  if (length(Linf)==5) {
    k <- Linf[[2]]
    a <- Linf[[3]]
    b <- Linf[[4]]
    c <- Linf[[5]]
    Linf <- Linf[[1]]
  }
  Linf*(1-a*exp(-k*t^c))^(1/b)
}


#' @rdname growthModels
#' @export
growthFunShow <- function(type=c("vonBertalanffy","Gompertz","Richards",
                                 "Logistic","Schnute","SchnuteRichards"),
                          param=NULL,case=param,plot=FALSE,...) {
  type <- match.arg(type)
  switch(type,
         vonBertalanffy = { expr <- iSGF_VB(param) },
         Gompertz = { expr <- iSGF_GOMP(param) },
         Richards = { expr <- iSGF_RICHARDS(param) },
         Logistic = { expr <- iSGF_LOGISTIC(param) },
         Schnute = { expr <- iSGF_SCHNUTE(case) },
         SchnuteRichards = { expr <- iSGF_SCHNUTERICHARDS()})
  if (plot) {
    withr::local_par(list(mar=c(0.1,0.1,0.1,0.1)))
    graphics::plot(0,type="n",ylim=c(0,1),xlim=c(0,1),xaxt="n",yaxt="n",
                   xlab="",ylab="",bty="n",...)
    graphics::text(0.5,0.5,expr,...)
  }
  expr
}

################################################################################
## Internal functions for growth model expressions
################################################################################
iSGF_VB <- function(param=c("Original","original","vonBertalanffy",
                            "Typical","typical","Traditional","traditional","BevertonHolt",
                            "GallucciQuinn","GQ","Mooij","Weisberg","Ogle",
                            "Schnute","Francis","Laslett","Polacheck",
                            "Somers","Somers2","Pauly",
                            "Fabens","Fabens2","Wang","Wang2","Wang3")) {
  if(!is.character(param)) STOP("'param' must be a character string.")
  param <- match.arg(param)
  switch(param,
         Ogle= {
           expr <- expression(E(L[t])==L[infinity]~-~(L[infinity]-L[r])*~e^{-K(t~-~t[r])})
         },
         Typical=,typical=,Traditional=,traditional=,BevertonHolt= {
           expr <- expression(E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])},")"))
         },
         Original=,original=,vonBertalanffy= {
           expr <- expression(E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-Kt})
         },
         GallucciQuinn=,GQ= {
           expr <- expression(E(L[t])==frac(omega,K)*~bgroup("(",1-e^{-K*(t~-~t[0])},")"))
         },
         Mooij= {
           expr <- expression(E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-frac(omega,L[infinity])*~t})
         },
         Weisberg= {
           expr <- expression(E(L[t])==L[infinity]*bgroup("(",1-e^{-frac(log(2),(t[50]~-~t[0]))*(t~-~t[0])},")"))
         },
         Schnute= {
           expr <- expression(E(L[t])==L[1]+(L[3]-L[1])*~frac(1-e^{-K*(~t~-~t[1])},1-e^{-K*(~t[3]~-~t[1])}))
         },
         Francis= {
           expr <- expression(atop(E(L[t])==L[1]+(L[3]-L[1])*~frac(1-r^{2*frac(t-t[1],t[3]-t[1])},1-r^{2}),
                                   plain("where" )~r==frac(L[3]-L[2],L[2]-L[1])))
         },
         Laslett= {
           expr <- expression(plain("Not Yet Implemented"))
         },
         Polacheck= {
           expr <- expression(plain("Not Yet Implemented"))
         },
         Somers= {
           expr <- expression(atop(E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])-S(t)+S(t[0])},")"),
                                   plain("where" )~S(t)==bgroup("(",frac(C*K,2*~pi),")")*~sin(2*pi*(t-t[s]))))
         },
         Somers2= {
           expr <- expression(atop(E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])-R(t)+R(t[0])},")"),
                                   plain("where" )~R(t)==bgroup("(",frac(C*K,2*~pi),")")*~sin(2*pi*(t-WP+0.5))))
         },
         Pauly= {
           expr <- expression(atop(E(L[t])==L[infinity]*bgroup("(",1-e^{-Kpr*(tpr~-~t[0])-V(tpr)+V(t[0])},")"),
                                   plain("where" )~V(t)==bgroup("(",frac(Kpr(1-NGT),2*~pi),")")*~sin(frac(2*pi,1-NGT)*(t-t[s]))))
         },
         Fabens= {
           expr <- expression(E(L[r]-L[m])==(L[infinity]-L[m])*bgroup("(",1-e^{-K*Delta*t},")"))
         },
         Fabens2= {
           expr <- expression(E(L[r])==L[m]+(L[infinity]-L[m])*bgroup("(",1-e^{-K*Delta*t},")"))
         },
         Wang= {
           expr <- expression(E(L[r]-L[m])==(L[infinity]+beta*(L[t]-L[t])-L[m])*bgroup("(",1-e^{-K*Delta*t},")"))
         },
         Wang2= {
           expr <- expression(E(L[r]-L[m])==(alpha+beta*L[t])*bgroup("(",1-e^{-K*Delta*t},")"))
         },
         Wang3= {
           expr <- expression(E(L[r])==L[m]+(alpha+beta*L[t])*bgroup("(",1-e^{-K*Delta*t},")"))
         })
  expr
}

iSGF_GOMP <- function(param=c("Original","original","Ricker1","Ricker2","Ricker3",
                              "QuinnDeriso1","QuinnDeriso2","QuinnDeriso3","QD1","QD2","QD3",
                              "Troynikov1","Troynikov2")) {
  if(!is.character(param)) STOP("'param' must be a character string.")
  param <- match.arg(param)
  switch(param,
         Original=,original= {
           expr <- expression(E(L[t])==L[infinity]*~e^{-e^{a-g[i]*t}})
         },
         Ricker1= {
           expr <- expression(E(L[t])==L[infinity]*~e^{-e^{-g[i]*(t-t[i])}})
         },
         Ricker2=,QuinnDeriso1=,QD1= {
           expr <- expression(E(L[t])==L[0]*~e^{a*bgroup("(",1-e^{-g[i]*t},")")})
         },
         Ricker3=,QuinnDeriso2=,QD2= {
           expr <- expression(E(L[t])==L[infinity]*~e^{-a*~e^{-g[i]*t}})
         },
         QuinnDeriso3=,QD3= {
           expr <- expression(E(L[t])==L[infinity]*~e^{-~frac(1,g[i])*~e^{-g[i]*~(~t~-~t^{plain("*")})}})
         },
         Troynikov1= {
           expr <- expression(E(L[r]-L[m])==L[infinity]*~bgroup("(",frac(L[m],L[infinity]),")")^{e^{-g[i]*Delta*t}}-L[m])
         },
         Troynikov2= {
           expr <- expression(E(L[r])==L[infinity]*~bgroup("(",frac(L[m],L[infinity]),")")^{e^{-g[i]*Delta*t}})
         })
  expr
}

iSGF_RICHARDS <- function(param=1:6) {
  if (!is.numeric(param)) STOP("'param' must be numeric when type='Richards'.")
  if (!param %in% 1:6) STOP("'param' must be from 1-6 when type='Richards'.")
  if(param==1){
    expr <- expression(E(L[t])==L[infinity]*~bgroup("(",1-a*e^{-kt},")")^{b})
  } else if (param==2) {
    expr <- expression(E(L[t])==L[infinity]*~bgroup("(",1-frac(1,b)*~e^{-k*(t-t[i])},")")^{~b})
  } else if (param==3) {
    expr <- expression(E(L[t])==frac(L[infinity],bgroup("(",1+b*e^{-k*(t-t[i])},")")^{~frac(1,b)}))
  } else if (param==4) {
    expr <- expression(E(L[t])==L[infinity]*~bgroup("(",1+(b-1)*~e^{-k*(t-t[i])},")")^{~frac(1,1-b)})
  } else if (param==5) {
    expr <- expression(E(L[t])==L[infinity]*~bgroup("[",bgroup("(",1+bgroup("(",frac(L[0],L[infinity]),")")^{1-b}-1,")")*~e^{-k*t},"]")^{~frac(1,1-b)})
  } else {
    expr <- expression(E(L[t])==L[-infinity]+(L[infinity]-L[-infinity])*~bgroup("(",1+(b-1)*~e^{-k*(t-t[i])},")")^{~frac(1,1-b)})
  }
  expr
}

iSGF_LOGISTIC <- function(param=c("CJ1","CJ2","Karkach","Haddon","CampanaJones1","CampanaJones2")) {
  if(!is.character(param)) STOP("'param' must be a character string.")
  param <- match.arg(param)
  switch(param,
         CJ1=,CampanaJones1= {
           expr <- expression(E(L[t])==frac(L[infinity],1+e^{-g[-infinity]*(t-t[i])}))
         },
         CJ2=,CampanaJones2= {
           expr <- expression(E(L[t])==frac(L[infinity],1+~ae^{-g[-infinity]*t}))
         },
         Karkach= {
           expr <- expression(E(L[t])==frac(L[0]*L[infinity],L[0]+(L[infinity]-L[0])*e^{-g[-infinity]*t}))
         },
         Haddon= {
           expr <- expression(E(L[r]-L[m])==frac(Delta*L[max],1+e^{log(19)*frac(L[m]~-~L[50],L[95]~-~L[50])}))
         })
  expr
}

iSGF_SCHNUTE <- function(case=1:4) {
  if (!is.numeric(case)) STOP("'case' must be numeric when type='Schnute'.")
  if (!case %in% 1:4) STOP("'case' must be from 1-4 when type='Schnute'.")
  if(case==1){
    expr <- expression(E(L[t])==bgroup("[",L[1]^{b}+(L[3]^{b}-L[1]^{b})*~frac(1-e^{-a*(~t~-~t[1])},1-e^{-a*(~t[3]~-~t[1])}),"]")^{~frac(1,b)})
  } else if (case==2) {
    expr <- expression(E(L[t])==L[1]*e^{log~bgroup("(",frac(L[3],L[1]),")")*~frac(1-e^{-a*(~t~-~t[1])},1-e^{-a*(~t[3]~-~t[1])})})
  } else if (case==3) {
    expr <- expression(E(L[t])==bgroup("[",L[1]^{b}+(L[3]^{b}-L[1]^{b})*~frac(~t~-~t[1],~t[3]~-~t[1]),"]")^{~frac(1,b)})
  } else  {
    expr <- expression(E(L[t])==L[1]*e^{log~bgroup("(",frac(L[3],L[1]),")")*~frac(~t~-~t[1],~t[3]~-~t[1])})
  }
  expr
}

iSGF_SCHNUTERICHARDS <- function() {
  expression(E(L[t])==L[infinity]*~bgroup("(",1-a*e^{-kt^{c}},")")^{frac(1,b)})
}

################################################################################
## internal function to compute t-prime
################################################################################
iCalc_tpr <- function(t,ts,NGT) {
  ## Step 1
  SNG <- ts+(1-NGT)/2
  tmp.t <- t-SNG
  ## Step 2 (in parentheses) and Step 3
  tmp.t2 <- (tmp.t-floor(tmp.t))-NGT
  ## Step 4
  tmp.t2[tmp.t2<0] <- 0
  ## Step 5 (in parentheses) and Step 6 (also returns value)
  (floor(tmp.t)*(1-NGT)+tmp.t2) + SNG
}

