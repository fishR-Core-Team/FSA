#' @name makeGrowthFun
#' 
#' @title Creates a function for a specific parameterization of the von Bertalanffy, Gompertz, logistic, Richards, Schnute, and Schnute-Richards growth functions.
#'
#' @description Creates a function for a specific parameterizations of the von Bertalanffy, Gompertz, logistic, Richards, and Schnute-Richards growth functions. Use \code{showGrowthFun} to see the equations for each growth function.
#'
#' @param type A string (\dQuote{von Bertalanffy}, \dQuote{Gompertz}, \dQuote{logistic}, \dQuote{Richards}, \dQuote{Schnute}, \dQuote{Schnute-Richards}) that indicates the type of growth function to show.
#' @param param A single numeric that indicates the specific parameterization of the growth function. See details.
#' @param pname A single character that indicates the specific parameterization of the growth function. If \code{NULL} then \code{param} will be used. See details.
#' @param simple A logical that indicates whether the function will accept all parameter values in the first parameter argument (\code{=FALSE}; DEFAULT) or whether all individual parameters must be specified in separate arguments (\code{=TRUE}).
#' @param msg A logical that indicates whether a message about the growth function and parameter definitions should be output (\code{=TRUE}; DEFAULT) or not (\code{=FALSE}).
#' 
#' @return Returns a function that can be used to predict fish size given a vector of ages and values for the growth function parameters and, in some parameterizations, values for constants. The result should be saved to an object that is then the function name. When the resulting function is used, the parameters are ordered as shown when the definitions of the parameters are printed after the function is called (if \code{msg=TRUE}). If \code{simple=FALSE} (DEFAULT), then the values for all parameters may be included as a vector in the first parameter argument (but in the same order). Similarly, the values for all constants may be included as a vector in the first constant argument (i.e., \code{t1}). If \code{simple=TRUE}, then all parameters and constants must be declared individually. The resulting function is somewhat easier to read when \code{simple=TRUE}, but is less general for some applications.
#' 
#' An expression of the equation for each growth function may be created with \code{showGrowthFun} using the same \code{type=} and \code{param} arguments.
#'
#' @details
#' The von Bertalanffy parameterizations for simple length and annual age data are
#' 
#' \tabular{ccl}{
#' \strong{param} \tab \strong{Equation} \tab \strong{Note} \cr
#' 1  \tab \eqn{E(L_t)=L_\infty\left(1-e^{-K(t-t_0)}\right)} \tab Beverton and Holt \cr
#' 2  \tab \eqn{E(L_t)=L_\infty - (L_\infty-L_0)~e^{-Kt}} \tab von Bertalanffy \cr
#' 3  \tab \eqn{E(L_t)=\frac{\omega}{K}\left(1-e^{-K(t-t_0)}\right)} \tab Gallucci and Quinn \cr
#' 4  \tab \eqn{E(L_t)=L_\infty - (L_\infty-L_0)~e^{-\frac{\omega}{L_\infty}t}} \tab Mooij \cr
#' 5  \tab \eqn{E(L_t)=L_\infty\left(1-e^{-log(2)\frac{t-t_0}{t_{50}-t_0}}\right)} \tab Weisberg \cr
#' 6  \tab \eqn{E(L_t)=L_r + (L_\infty-L_r)~e^{-e^{-K(t-t_r)}}} \tab Ogle and Isermann \cr
#' 7  \tab \eqn{E(L_t)=L_1+(L_3-L_1)\frac{1-e^{-K(t-t_1)}}{1-e^{-K(t_3-t_1)}}} \tab Schnute \cr
#' 8  \tab \eqn{E(L_t)=L_1+(L_3-L_1)\frac{1-r^{2\frac{t-t_1}{t_3-t_1}}}{1-r^2}} \tab Francis \cr
#'    \tab with \eqn{r=\frac{L_3-L_2}{L_2-L_1}} \tab \cr
#' 9  \tab \eqn{E(L_t)=L_\infty\frac{\left(1-e^{-K_2(t-t_0)}\right)\left(1+e^{-b(t-t_0-a)}\right)}{\left(1+e^{ab}\right)^{-\frac{K_2K_1}{b}}}} \tab Laslett and Polacheck \cr
#' }
#' 
#' The von Bertalanffy parameterizations for simple length and seasonal age data are
#' 
#' \tabular{ccl}{
#' \strong{param} \tab \strong{Equation} \tab \strong{Note} \cr
#' 10 \tab \eqn{L_\infty\left(1-e^{-K(t-t_0)-\frac{CK}{2\pi \text{sin}(2\pi(t-t_s))}+\frac{CK}{2\pi \text{sin}(2\pi(t_0-t_s))}}\right)} \tab Somers \cr
#' 11 \tab \eqn{L_\infty\left(1-e^{-K(t-t_0)-\frac{CK}{2\pi \text{sin}(2\pi(t-WP+0.5))}+\frac{CK}{2\pi \text{sin}(2\pi(t_0-WP+0.5))}}\right)} \tab Somers (alt) \cr
#' 12 \tab \eqn{L_\infty(1-e^{-q}} \tab Pauly \cr
#'    \tab with \eqn{q=K'(t'-t_0) + \frac{K'(1-NGT)}{2\pi}\text{sin}(\frac{2\pi}{(1-NGT)(t'-t_s)}) - \frac{K'(1-NGT)}{2\pi}\text{sin}(\frac{2\pi}{(1-NGT)(t_0-t_s)})} \tab \cr
#' }
#' 
#' The von Bertalanffy parameterizations for tag-recapture data are
#' 
#' \tabular{ccl}{
#' \strong{param} \tab \strong{Equation} \tab \strong{Note} \cr
#' 13 \tab \eqn{E(L_r-L_m)=(L_\infty-L_m)\left(1-e^{-K\delta t}\right)} \tab Fabens \cr
#' 14 \tab \eqn{E(L_r)=L_m + (L_\infty-L_m)\left(1-e^{-K\delta t}\right)} \tab Fabens (alt)\cr
#' 15 \tab \eqn{E(L_r-L_m)=(L_\infty+\beta(\bar{L}_m-L_m)-L_m)\left(1-e^{-K\delta t}\right)} \tab Wang \cr
#' 16 \tab \eqn{E(L_r-L_m)=(\alpha+\beta L_m\left(1-e^{-K\delta t}\right)} \tab Wang 2 \cr
#' 17 \tab \eqn{E(L_r)=L_m+(\alpha+\beta L_m\left(1-e^{-K\delta t}\right)} \tab Wang 2 (alt)\cr
#' 18 \tab \eqn{E(L_r-L_m)=\left[\frac{L_2g_1-L_1g_2}{g_1-g_2}-L_m\right]\left[1-\left(1+\frac{g_1-g_2}{L_1+L_2}\right)^{dt}\right]} \tab Francis \cr
#' }
#' 
#' The von Bertalanffy parameterizations for seasonal tag-recapture data are
#' 
#' \tabular{ccl}{
#' \strong{param} \tab \strong{Equation} \tab \strong{Note} \cr
#' 19 \tab \eqn{E(L_r-L_m)=\left[\frac{L_2g_1-L_1g_2}{g_1-g_2}-L_m\right]\left[1-\left(1+\frac{g_1-g_2}{L_1+L_2}\right)^{t_2-t_1+S_2-S_1}\right]} \tab Francis \cr
#'    \tab with \eqn{S_1=u\text{sin}\left(\frac{2\pi(t_1-w)}{2\pi}\right)} \tab \cr
#'    \tab and \eqn{S_2=u\text{sin}\left(\frac{2\pi(t_2-w)}{2\pi}\right)} \tab \cr
#' }
#' 
#' The Gompertz parameterizations for simple length and annual age data are
#' 
#' \tabular{ccl}{
#' \strong{param} \tab \strong{Equation} \tab \strong{Note} \cr
#' 1  \tab \eqn{E(L_t)=L_\infty e^{-e^{a_1-g_it}}} \tab Gompertz \cr
#' 2  \tab \eqn{E(L_t)=L_\infty e^{-e^{-g_i(t-t_i)}}} \tab  \cr
#' 3  \tab \eqn{E(L_t)=L_0 e^{a_2(1-e^{-g_it})}} \tab  \cr
#' 4  \tab \eqn{E(L_t)=L_\infty e^{-a_2e^{-g_it}}} \tab  \cr
#' 5  \tab \eqn{E(L_t)=L_\infty e^{-\frac{1}{g_i}e^{-g_i(t-t_0)}}} \tab  \cr
#' }
#' 
#' The parameterizations and parameters for the Gompertz function are varied and confusing in the literature. I have attempted to use a uniform set of parameters in these functions, but that makes a direct comparison to the literature difficult. Common sources for Gompertz models are listed in the references below. I make some comments here to aid comparisons to the literature.
#' 
#' \itemize{
#'  \item Within FSA, \eqn{L_0} is the mean length at age 0, \eqn{L_\infty} is the mean asymptotic length, \eqn{t_i} is the age at the inflection point, \eqn{g_i} is the instantaneous growth rate at the inflection point, \eqn{t_0} is a the hypothetical age at a mean length of 0, and \eqn{a_1} and \eqn{a_2} are nuisance parameters with no real-world interpretations.
#' The function in Ricker (1975)[p. 232] is the same as the third parameterization where \eqn{a_2} here is \eqn{G} there and \eqn{g_i} here is \eqn{g}. Also their \eqn{w} is \eqn{L} here.
#'  \item In the Ricker (1979)[p. 705] functions (parameterizations 2-4), \eqn{a} here is \eqn{k} there and \eqn{g_i} here is \eqn{g} there. Also note that their \eqn{w} is \eqn{L} here. In the Ricker (1979) functions as presented in Campana and Jones (1992), \eqn{a} here is \eqn{k} there and \eqn{g_i} here is \eqn{G} there. Also note that their \eqn{X} is \eqn{L} here.
#'  \item In the Quinn and Deriso (1999) functions (parameterizations 3-5), \eqn{a} here is \eqn{\frac{\lambda}{K}} there and the \eqn{g_i} here is \eqn{K} there. Also note that their \eqn{Y} is \eqn{L} here.
#'  \item The function in Quist \emph{et al.} (2012)[p. 714] is the same as parameterization 2 where \eqn{g_i} here is \eqn{G} there and \eqn{t_i} here is \eqn{t_0} there.
#' \item The function in Katsanevakis and Maravelias (2008) is the same as parameterization 2 where \eqn{g_i} here is \eqn{k_2} there and \eqn{t_i} here is \eqn{t_2} there.
#' }
#' 
#' The Gompertz parameterizations for tag-recapture data are
#' 
#' \tabular{ccl}{
#' \strong{param} \tab \strong{Equation} \tab \strong{Note} \cr
#' 6  \tab \eqn{E(L_r-L_m)=L_{\infty}\left[\frac{L_m}{L_{\infty}}\right]^{e^{-g_i\Delta t}}-L_m} \tab Troynikov \cr
#' 7  \tab \eqn{E(L_r)=L_{\infty}\left[\frac{L_m}{L_{\infty}}\right]^{e^{-g_i\Delta t}}} \tab Troynikov (alt) \cr
#' }
#' 
#' The logistic parameterizations for simple length and annual age data are
#' 
#' \tabular{ccl}{
#' \strong{param} \tab \strong{Equation} \tab \strong{Note} \cr
#' 1  \tab \eqn{E(L_t)=\frac{L_\infty}{1+e^{-g_{-\infty}(t-t_i)}}} \tab Campana and Jones \cr
#' 2  \tab \eqn{E(L_t)=\frac{L_\infty}{1+ae^{-g_{-\infty}t}}} \tab Campana and Jones \cr
#' 3  \tab \eqn{E(L_t)=\frac{L_0L_\infty}{L_0+(L_\infty - L_0)e^{-g_{-\infty}t}}} \tab Karkach \cr
#' }
#' 
#' The logistic parameterizations for tag-recapture data are
#' 
#' \tabular{ccl}{
#' \strong{param} \tab \strong{Equation} \tab \strong{Note} \cr
#' 4  \tab \eqn{E(L_r-L_m)=\frac{\Delta L_{max}}{1+e^{log(19)/frac{L_m-L_{50}}{L_{95}-L_{50}}}}} \tab Haddon \cr
#' }
#' 
#' The Richards parameterizations for simple length and annual age data are
#' 
#' \tabular{ccl}{
#' \strong{param} \tab \strong{Equation} \tab \strong{Note} \cr
#' 1  \tab \eqn{E(L_t)=\frac{L_\infty}{\left[1+b_1e^{-k(t-t_i)}\right]^\frac{1}{b_1}}} \tab Tjorve and Tjorve (2010) Eqn 5 \cr
#' 2  \tab \eqn{E(L_t)=\frac{L_\infty}{\left(1+e^{-k(t-t_0)}\right)^{-b_2}}} \tab Tjorve and Tjorve (2010) Eqn 3(alt) \cr
#' 3  \tab \eqn{E(L_t)=L_\infty\left[1+\left(\left(\frac{L_0}{L_\infty}\right)^{1-b_3}-1\right)e^{-kt}\right]^\frac{1}{1-b_3}} \tab Tjorve and Tjorve (2010) Eqn 7 \cr
#' 4  \tab \eqn{E(L_t)=L_\infty\left[1-\frac{1}{b_2}e^{-k(t-t_i)}\right]^{b_2}} \tab Tjorve and Tjorve (2010) Eqn 4 \cr
#' 5  \tab \eqn{E(L_t)=L_\infty\left[1+(b_3-1)e^{-k(t-t_i)}\right]^\frac{1}{1-b_3}} \tab Tjorve and Tjorve (2010) Eqn 6 \cr
#' }
#' 
#' Only 4-parameter parameterizations from Tjorve and Tjorve (2010) that seemed useful for modeling fish growth are provided here. In Tjorve and Tjorve (2010) their \eqn{A}, \eqn{k}, \eqn{W_0}, and \eqn{T_i} are \eqn{L_\infty}, \eqn{k}, \eqn{L_0}, and \eqn{t_i}, and their \eqn{d} is \eqn{b_1}, \eqn{b_2}, and \eqn{b_3}, respectively, here (in FSA).
#'
#' The four cases for the Schnute model for simple length and annual age data are
#' 
#' \tabular{cl}{
#' \strong{Case} \tab \strong{Equation} \cr
#' 1 \tab \eqn{E(L_t)=\left[L^b_1+(L^b_3-L^b_1)\frac{1-e^{-a(t-t_1)}}{1-e^{-a(t_3-t_1)}}\right]^{\frac{1}{b}}} \cr
#' 2 \tab \eqn{E(L_t)=L_1e^{log\left(\frac{L_3}{L_1}\right)\frac{1-e^{-a(t-t_1)}}{1-e^{-a(t_3-t_1)}}}} \cr
#' 3 \tab \eqn{E(L_t)=\left[L^b_1+(L^b_3-L^b_1)\frac{t-t_1}{t_3-t_1}\right]^{\frac{1}{b}}} \cr
#' 4 \tab \eqn{E(L_t)=L_1e^{log\left(\frac{L_3}{L_1}\right)\frac{t-t_1}{t_3-t_1}}} \cr
#' } 
#' 
#' The Schnute-Richards model for simple length and annual age data is \eqn{E(L_t)=L_\infty\left(1-ae^{-kt^c}\right)^{1/b}}. Note that this function is slightly modified (a \eqn{+} was changed to a \eqn{-} so that \eqn{a} is positive) from the original in Schnute and Richards (1990).
#'
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}, thanks to Gabor Grothendieck for a hint about using \code{get()}.
#'
#' @section IFAR Chapter: 12-Individual Growth.
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
#' Schnute, J.T. and L.J. Richards. 1990. A unified approach to the analysis of fish growth, maturity, and survivorship data. Canadian Journal of Fisheries and Aquatic Sciences 47:24-40.
#'
#' Somers, I. F. 1988. On a seasonally oscillating growth function. Fishbyte 6(1):8-11. [Was (is?) from https://www.fishbase.us/manual/English/fishbaseSeasonal_Growth.htm.]
#' 
#' Tjorve, E. and K. M. C. Tjorve. 2010. A unified approach to the Richards-model family for use in growth analyses: Why we need only two model forms. Journal of Theoretical Biology 267:417-425. [Was (is?) from https://www.researchgate.net/profile/Even_Tjorve/publication/46218377_A_unified_approach_to_the_Richards-model_family_for_use_in_growth_analyses_why_we_need_only_two_model_forms/links/54ba83b80cf29e0cb04bd24e.pdf.]
#' 
#' Tjorve, K. M. C. and E. Tjorve. 2017. The use of Gompertz models in growth analyses, and new Gompertz-model approach: An addition to the Unified-Richards family. PLOS One. [Was (is?) from https://doi.org/10.1371/journal.pone.0178691.]
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
#' #===== Create typical von B function, calc length at single then multiple ages
#' vb <- makeGrowthFun()
#' vb(t=1,Linf=450,K=0.3,t0=-0.5)
#' vb(t=1:5,Linf=450,K=0.3,t0=-0.5)
#' 
#' #===== All parameters can be given to first parameter (default), unless simple=TRUE
#' vb(t=1,Linf=c(450,0.3,-0.5))
#' vbS <- makeGrowthFun(simple=TRUE)
#' \dontrun{vbS(t=1,Linf=c(450,0.3,-0.5))   # will error}
#' vbS(t=1,Linf=450,K=0.3,t0=-0.5)
#' 
#' #===== Create original von B, first using param, then using pname
#' vbO <- makeGrowthFun(param=2)
#' vbO2 <- makeGrowthFun(pname="Original")
#' vbO(t=1:5,Linf=450,K=0.3,L0=25)
#' vbO2(t=1:5,Linf=450,K=0.3,L0=25)
#' 
#' #===== Create the third parameterization of the logistic growth function
#' #         and show some details, and demo calculations
#' logi <- makeGrowthFun(type="logistic",param=3,msg=TRUE)
#' logi(t=1:10,Linf=450,gninf=0.3,L0=25)
#' 
#' #===== Simple example of comparing several models
#' vb <- makeGrowthFun(type="von Bertalanffy")
#' gomp <- makeGrowthFun(type="Gompertz",param=2)
#' logi <- makeGrowthFun(type="logistic")
#' 
#' ages <- 0:15
#' vb1 <- vb(ages,Linf=450,K=0.3,t0=-0.5)
#' gomp1 <- gomp(ages,Linf=450,gi=0.3,ti=3)
#' logi1 <- logi(ages,Linf=450,gninf=0.3,ti=3)
#' 
#' plot(vb1~ages,type="l",lwd=2,ylim=c(0,450),ylab="Length",xlab="Age")
#' lines(gomp1~ages,lwd=2,col="red")
#' lines(logi1~ages,lwd=2,col="blue")
#' 
#' #===== Simple example of four cases of Schnute model (note a,b choices)
#' ages <- 0:15
#' Schnute <- makeGrowthFun(type="Schnute")
#' s1 <- Schnute(ages,L1=30,L3=400,a=0.3,b=1,t1=1,t3=15)
#' s2 <- Schnute(ages,L1=30,L3=400,a=0.3,b=0,t1=1,t3=15)
#' s3 <- Schnute(ages,L1=30,L3=400,a=0,  b=1,t1=1,t3=15)
#' s4 <- Schnute(ages,L1=30,L3=400,a=0,  b=0,t1=1,t3=15)
#' 
#' plot(s1~ages,type="l",lwd=2,ylim=c(0,450),ylab="Length",xlab="Age")
#' lines(s2~ages,lwd=2,col="red")
#' lines(s3~ages,lwd=2,col="blue")
#' lines(s4~ages,lwd=2,col="green")
#' 
#' #===== Fitting the 8th parameterization of the von B growth model to data
#' # make von B function
#' vb8 <- makeGrowthFun(type="von Bertalanffy",param=8,msg=TRUE)
#' # get starting values
#' sv8 <- findGrowthStarts(tl~age,data=SpotVA1,type="von Bertalanffy",param=8,
#'                         constvals=c(t1=1,t3=5))
#' # fit function
#' nls8 <- nls(tl~vb8(age,L1,L2,L3,t1=c(t1=1,t3=5)),data=SpotVA1,start=sv8)
#' cbind(Est=coef(nls8),confint(nls8))
#' plot(tl~age,data=SpotVA1,pch=19,col=col2rgbt("black",0.1))
#' curve(vb8(x,L1=coef(nls8),t1=c(t1=1,t3=5)),col="blue",lwd=3,add=TRUE)
#' 
#' @rdname makeGrowthFun
#' @export

makeGrowthFun <- function(type=c("von Bertalanffy","Gompertz","logistic",
                                 "Richards","Schnute","Schnute-Richards"),
                        param=1,pname=NULL,simple=FALSE,msg=FALSE) {
  #===== Checks
  type <- match.arg(type)
  param <- iHndlGrowthModelParams(type,param,pname)

  #===== Make message (if asked to)
  # make a combined parameter name ... remove spaces and hyphens from type
  pnm <- paste0(gsub(" ","",type),param)
  pnm <- gsub("-","",pnm)

  if (msg) message(msgsGrow[[pnm]])
  
  #===== Return the function
  # make a name to find the proper function ... remove space from type
  if (simple) pnm <- paste0("S",pnm)
  # go get that function to return it
  get(pnm)
}


#===============================================================================
#== Internal Functions -- Make a function for each parameterization
#===============================================================================
#-------------------------------------------------------------------------------
#-- von Bertalanffy parameterizations
#-------------------------------------------------------------------------------
# was Typical, Traditional, BevertonHolt
vonBertalanffy1 <- function(t,Linf,K=NULL,t0=NULL) {
  if (length(Linf)==3) { 
    t0 <- Linf[[3]]
    K <- Linf[[2]]
    Linf <- Linf[[1]] }
  Linf*(1-exp(-K*(t-t0)))
}
SvonBertalanffy1 <- function(t,Linf,K,t0) { Linf*(1-exp(-K*(t-t0))) }
msg_vonBertalanffy1 <- paste0("You have chosen paramaterization 1 (Beverton-Holt) of ",
                              "the von Bertalanffy growth function.\n\n",
                              "  E[L|t] = Linf*(1-exp(-K*(t-t0)))\n\n",
                              "  where Linf = asymptotic mean length\n",
                              "           K = exponential rate of approach to Linf\n",
                              "          t0 = hypothetical time/age when mean length is 0\n\n")

# was Original, vonBertalanffy
vonBertalanffy2 <- function(t,Linf,K=NULL,L0=NULL) {
  if (length(Linf)==3) {
    L0 <- Linf[[3]]
    K <- Linf[[2]]
    Linf <- Linf[[1]] }
  Linf-(Linf-L0)*exp(-K*t)
}
SvonBertalanffy2 <- function(t,Linf,K,L0) { Linf-(Linf-L0)*exp(-K*t) }
msg_vonBertalanffy2 <- paste0("You have chosen paramaterization 2 (original) of ",
                              "the von Bertalanffy growth function.\n\n",
                              "  E[L|t] = Linf-(Linf-L0)*exp(-K*t)\n\n",
                              "  where Linf = asymptotic mean length\n",
                              "           K = exponential rate of approach to Linf\n\n",
                              "          L0 = mean length at age-0 (i.e., hatching or birth)\n\n")

# was GallucciQuinn
vonBertalanffy3 <- function(t,omega,K=NULL,t0=NULL) {
  if (length(omega)==3) {
    t0 <- omega[[3]]
    K <- omega[[2]]
    omega <- omega[[1]] }
  (omega/K)*(1-exp(-K*(t-t0)))
}
SvonBertalanffy3 <- function(t,omega,K,t0) { (omega/K)*(1-exp(-K*(t-t0))) }
msg_vonBertalanffy3 <- paste0("You have chosen paramaterization 3 (Gallucci-Quinn) of ",
                              "the von Bertalanffy growth function.\n\n",
                              "  E[L|t] = [omega/K]*(1-exp(-K*(t-t0)))\n\n",
                              "  where omega = growth rate near t0\n",
                              "            K = exponential rate of approach to Linf\n",
                              "           t0 = hypothetical time/age when mean length is 0\n\n")

# was Mooij 
vonBertalanffy4 <- function(t,Linf,L0=NULL,omega=NULL) {
  if (length(Linf)==3) {
    omega <- Linf[[3]]
    L0 <- Linf[[2]]
    Linf <- Linf[[1]] }
  Linf-(Linf-L0)*exp(-(omega/Linf)*t)
}
SvonBertalanffy4 <- function(t,Linf,L0,omega) { Linf-(Linf-L0)*exp(-(omega/Linf)*t) }
msg_vonBertalanffy4 <- paste0("You have chosen paramaterization 4 (Mooij) of ",
                              "the von Bertalanffy growth function.\n\n",
                              "  E[L|t] = Linf-(Linf-L0)*exp(-(omega/Linf)*t)\n\n",
                              "  where Linf = asymptotic mean length\n",
                              "          L0 = the mean length at age-0 (i.e., hatching or birth)\n",
                              "       omega = growth rate near L0\n\n")

# was Weisberg
vonBertalanffy5 <- function(t,Linf,t0=NULL,t50=NULL) {
  if (length(Linf)==3) {
    t50 <- Linf[[3]]
    t0 <- Linf[[2]]
    Linf <- Linf[[1]] }
  Linf*(1-exp(-(log(2)/(t50-t0))*(t-t0)))
}
SvonBertalanffy5 <- function(t,Linf,t0,t50) { Linf*(1-exp(-(log(2)/(t50-t0))*(t-t0))) }
msg_vonBertalanffy5 <- paste0("You have chosen paramaterization 5 (Weisberg) of ",
                              "the von Bertalanffy growth function.\n\n",
                              "  E[L|t] = Linf*(1-exp(-(log(2)/(t50-t0))*(t-t0)))\n\n",
                              "  where Linf = asymptotic mean length\n",
                              "          t0 = hypothetical time/age when mean length is 0\n",
                              "         t50 = age when half of Linf is reached\n\n")

# was Ogle (Ogle-Isermann)
vonBertalanffy6 <- function(t,Linf,K=NULL,tr=NULL,Lr=NULL) {
  if (length(Linf)==4) {
    Lr <- Linf[[4]]
    tr <- Linf[[3]]
    K <- Linf[[2]]
    Linf <- Linf[[1]] }
  Lr+(Linf-Lr)*(1-exp(-K*(t-tr)))
}
SvonBertalanffy6 <- function(t,Linf,K,tr,Lr) { Lr+(Linf-Lr)*(1-exp(-K*(t-tr))) }
msg_vonBertalanffy6 <- paste0("You have chosen paramaterization 6 (Ogle-Isermann) of ",
                              "the von Bertalanffy growth function.\n\n",
                              "  E[L|t] = (Linf-Lr)*(1-exp(-K*(t-tr)))\n\n",
                              "  where Linf = asymptotic mean length\n",
                              "           K = exponential rate of approach to Linf\n",
                              "          tr = mean age at Lr\n",
                              "          Lr = mean length at tr\n\n",
                              "NOTE: either tr or Lr must be set by the user.\n\n")

# was Schnute
vonBertalanffy7 <- function(t,L1,L3=NULL,K=NULL,t1,t3=NULL) {
  if (length(L1)==3) { K <- L1[[3]]; L3 <- L1[[2]]; L1 <- L1[[1]] }
  if (length(t1)==2) { t3 <- t1[[2]]; t1 <- t1[[1]] }
  L1+(L3-L1)*((1-exp(-K*(t-t1)))/(1-exp(-K*(t3-t1))))
}
SvonBertalanffy7 <- function(t,L1,L3,K,t1,t3) { L1+(L3-L1)*((1-exp(-K*(t-t1)))/(1-exp(-K*(t3-t1)))) }
msg_vonBertalanffy7 <- paste0("You have chosen paramaterization 7 (Schnute) of ",
                              "the von Bertalanffy growth function.\n\n",
                              "  E[L|t] = L1+(L2-L1)*[(1-exp(-K*(t-t1)))/(1-exp(-K*(t3-t1)))]\n\n",
                              "  where L1 = mean length at youngest age in sample\n",
                              "        L2 = mean length at oldest age in sample\n",
                              "         K = exponential rate of approach to Linf\n\n",
                              "  You must also give values (i.e., they are NOT model parameters) for\n",
                              "        t1 = youngest age in sample\n",
                              "        t3 = oldest age in sample\n\n")

# was Francis 
vonBertalanffy8 <- function(t,L1,L2=NULL,L3=NULL,t1,t3=NULL) {
  if (length(L1)==3) { L3 <- L1[[3]]; L2 <- L1[[2]]; L1 <- L1[[1]] }
  if (length(t1)==2) { t3 <- t1[[2]]; t1 <- t1[[1]] }
  r <- (L3-L2)/(L2-L1)
  L1+(L3-L1)*((1-r^(2*((t-t1)/(t3-t1))))/(1-r^2))
}
SvonBertalanffy8 <- function(t,L1,L2,L3,t1,t3) {
  r <- (L3-L2)/(L2-L1)
  L1+(L3-L1)*((1-r^(2*((t-t1)/(t3-t1))))/(1-r^2))
}
msg_vonBertalanffy8 <- paste0("You have chosen paramaterization 8 (Francis) of ",
                              "the von Bertalanffy growth function.\n\n",
                              "  E[L|t] = L1+(L3-L1)*[(1-r^(2*[(t-t1)/(t3-t1)]))/(1-r^2)]\n\n",
                              "  where r = [(L3-L2)/(L2-L1)] and\n\n",
                              "       L1 = mean length at first (small) reference age\n",
                              "       L2 = mean length at intermediate reference age\n",
                              "       L3 = mean length at third (large) reference age\n\n",
                              "You must also give values (i.e., they are NOT model parameters) for\n",
                              "       t1 = first (usually a younger) reference age\n",
                              "       t3 = third (usually an older) reference age\n\n")

# was Laslett <- Polacheck
vonBertalanffy9 <- function(t,Linf,K1=NULL,K2=NULL,t0=NULL,a=NULL,b=NULL) {
  if (length(Linf)==6) {
    b <- Linf[[6]]; a <- Linf[[5]]; t0 <- Linf[[4]]
    K1 <- Linf[[2]]; K2 <- Linf[[3]]
    Linf <- Linf[[1]] }
  Linf*(1-exp(-K2*(t-t0))*((1+exp(-b*(t-t0-a)))/(1+exp(a*b)))^(-(K2-K1)/b))
}
SvonBertalanffy9 <- function(t,Linf,K1,K2,t0,a,b) {
  Linf*(1-exp(-K2*(t-t0))*((1+exp(-b*(t-t0-a)))/(1+exp(a*b)))^(-(K2-K1)/b))
}
msg_vonBertalanffy9 <- paste0("You have chosen paramaterization 9 (Double von B) of ",
                              "the von Bertalanffy growth function.\n\n",
                              "  E[L|t] = Linf*[1-exp(-K2*(t-to))((1+exp(-b(t-t0-a)))/(1+exp(ab)))^(-(K2-K1)/b)]\n\n",
                              "  where Linf = asymptotic mean length\n",
                              "          t0 = hypothetical time/age when mean length is 0\n",
                              "          K1 = first exponential rate of approach to Linf\n",
                              "          K2 = second exponential rate of approach to Linf\n",
                              "           b = controls rate of transition from K1 to K2\n",
                              "           a = central age of transition from K1 to K2\n\n")

# was Somers
vonBertalanffy10 <- function(t,Linf,K=NULL,t0=NULL,C=NULL,ts=NULL) {
  if (length(Linf)==5) { 
    ts <- Linf[[5]]; C <- Linf[[4]]
    t0 <- Linf[[3]]; K <- Linf[[2]]
    Linf <- Linf[[1]] }
  St <- (C*K)/(2*pi)*sin(2*pi*(t-ts))
  Sto <- (C*K)/(2*pi)*sin(2*pi*(t0-ts))
  Linf*(1-exp(-K*(t-t0)-St+Sto))
}
SvonBertalanffy10 <- function(t,Linf,K,t0,C,ts) {
  Linf*(1-exp(-K*(t-t0)-(C*K)/(2*pi)*sin(2*pi*(t-ts))+(C*K)/(2*pi)*sin(2*pi*(t0-ts))))
}
msg_vonBertalanffy10 <- paste0("You have chosen paramaterization 10 (Somer's Seasonal) of ",
                               "the von Bertalanffy growth function.\n\n",
                               "  E[L|t] = Linf*(1-exp(-K*(t-to)-St+St0))\n\n",
                               "  where St = (CK/2*pi)*sin(2*pi*(t-ts)) and\n",
                               "       St0 = (CK/2*pi)*sin(2*pi*(t0-ts)) and\n\n",
                               "  and Linf = asymptotic mean length\n",
                               "         K = exponential rate of approach to Linf\n",
                               "        t0 = hypothetical time/age when mean length is 0\n",
                               "         C = proportional growth depression at 'winter peak'\n",
                               "        ts = time from t=0 until first growth oscillation begins.\n\n")

# was Somers2
vonBertalanffy11 <- function(t,Linf,K=NULL,t0=NULL,C=NULL,WP=NULL) {
  if (length(Linf)==5) {
    WP <- Linf[[5]]; C <- Linf[[4]]
    t0 <- Linf[[3]]; K <- Linf[[2]]
    Linf <- Linf[[1]] }
  Rt <- (C*K)/(2*pi)*sin(2*pi*(t-WP+0.5))
  Rto <- (C*K)/(2*pi)*sin(2*pi*(t0-WP+0.5))
  Linf*(1-exp(-K*(t-t0)-Rt+Rto))
}
SvonBertalanffy11 <- function(t,Linf,K,t0,C,WP) {
  Linf*(1-exp(-K*(t-t0)-(C*K)/(2*pi)*sin(2*pi*(t-WP+0.5))+(C*K)/(2*pi)*sin(2*pi*(t0-WP+0.5))))
}
msg_vonBertalanffy11 <- paste0("You have chosen paramaterization 11 (alt Somer's Seasonal) of ",
                               "the von Bertalanffy growth function.\n\n",
                               "  E[L|t] = Linf*(1-exp(-K*(t-to)-Rt+Rt0))\n\n",
                               "  where Rt = (CK/2*pi)*sin(2*pi*(t-WP+0.5)) and\n",
                               "       Rt0 = (CK/2*pi)*sin(2*pi*(t0-WP+0.5)) and\n\n",
                               "  and Linf = asymptotic mean length\n",
                               "         K = exponential rate of approach to Linf\n",
                               "        t0 = hypothetical time/age when mean length is 0\n",
                               "         C = proportional growth depression at 'winter peak'\n",
                               "        WP = 'winter peak' (point of slowest growth).\n\n")

# was Pauly
vonBertalanffy12 <- function(t,Linf,Kpr=NULL,t0=NULL,ts=NULL,NGT=NULL) {
  if (length(Linf)==5) {
    NGT <- Linf[[5]]; ts <- Linf[[4]]
    t0 <- Linf[[3]]; Kpr <- Linf[[2]]
    Linf <- Linf[[1]] }
  tpr <- iCalc_tpr(t,ts,NGT)
  q <- Kpr*(tpr-t0) +
    (Kpr*(1-NGT)/(2*pi))*sin((2*pi)/(1-NGT)*(tpr-ts)) -
    (Kpr*(1-NGT)/(2*pi))*sin((2*pi)/(1-NGT)*(t0-ts))
  Linf*(1-exp(-q))
}
SvonBertalanffy12 <- function(t,Linf,Kpr,t0,ts,NGT) {
  tpr <- iCalc_tpr(t,ts,NGT)
  q <- Kpr*(tpr-t0) +
    (Kpr*(1-NGT)/(2*pi))*sin((2*pi)/(1-NGT)*(tpr-ts)) -
    (Kpr*(1-NGT)/(2*pi))*sin((2*pi)/(1-NGT)*(t0-ts))
  Linf*(1-exp(-q))
}
msg_vonBertalanffy12 <- paste0("You have chosen paramaterization 12 (Paul's Seasonal Cessation) of ",
                               "the von Bertalanffy growth function.\n\n",
                               "  E[L|t] = Linf*(1-exp(-K'*(t'-to)-Vt'+Vt0))\n\n",
                               "  where Vt' = (K'(1-NGT)/2*pi)*sin(2*pi*(t'-ts)/(1-NGT)) and\n",
                               "        Vt0 = (K'(1-NGT)/2*pi)*sin(2*pi*(t0-ts)/(1-NGT)) and\n\n",
                               "  and Linf = asymptotic mean length\n",
                               "        K' = exponential rate of approach to Linf during growth period\n",
                               "        t0 = hypothetical time/age when mean length is 0\n",
                               "        ts = time from t=0 until the first growth oscillation begins\n",
                               "       NGT = length of no-growth period.\n\n")

# was Fabens
vonBertalanffy13 <- function(Lm,dt,Linf,K=NULL) {
  if (length(Linf)==2) {
    K <- Linf[[2]]
    Linf <- Linf[[1]] }
  (Linf-Lm)*(1-exp(-K*dt))
}
SvonBertalanffy13 <- function(Lm,dt,Linf,K) { (Linf-Lm)*(1-exp(-K*dt)) }
msg_vonBertalanffy13 <- paste0("You have chosen paramaterization 13 (Faben's Tag-Return) of ",
                               "the von Bertalanffy growth function.\n\n",
                               "  E[dL|Lm,dt] = (Linf-Lm)*(1-exp(-K*dt))\n\n",
                               "  where Linf = asymptotic mean length\n",
                               "           K = exponential rate of approach to Linf\n\n",
                               "  and the data are dL = change in length (from tagging to recapture)\n",
                               "                   Lm = length at time of tagging\n",
                               "                   dt = time between tagging and recapture.\n\n")

# was Fabens2
vonBertalanffy14 <- function(Lm,dt,Linf,K=NULL) {
  if (length(Linf)==2) {
    K <- Linf[[2]]
    Linf <- Linf[[1]] }
  Lm+(Linf-Lm)*(1-exp(-K*dt))
}
SvonBertalanffy14 <- function(Lm,dt,Linf,K) { Lm+(Linf-Lm)*(1-exp(-K*dt)) }
msg_vonBertalanffy14 <- paste0("You have chosen paramaterization 14 (alt Faben's Tag-Return) of ",
                               "the von Bertalanffy growth function.\n\n",
                               "  E[Lr|Lm,dt] = Lm + (Linf-Lm)*(1-exp(-K*dt))\n\n",
                               "  where Linf = asymptotic mean length\n",
                               "           K = exponential rate of approach to Linf\n\n",
                               "  and the data are Lr = length at time of recapture\n",
                               "                   Lm = length at time of tagging\n",
                               "                   dt = time between tagging and recapture.\n\n")

# was Wang
vonBertalanffy15 <- function(Lm,dt,Linf,K=NULL,b=NULL) {
  if (length(Linf)==3) {
    b <- Linf[[3]]
    K <- Linf[[2]]
    Linf <- Linf[[1]] }
  (Linf+b*(Lm-mean(Lm))-Lm)*(1-exp(-K*dt))
}
SvonBertalanffy15 <- function(Lm,dt,Linf,K,b) { (Linf+b*(Lm-mean(Lm))-Lm)*(1-exp(-K*dt)) }
msg_vonBertalanffy15 <- paste0("You have chosen paramaterization 15 (Wang's Tag-Return) of ",
                               "the von Bertalanffy growth function.\n\n",
                               "  E[dL|Lm,dt] = (Linf+b(Lm-E(Lm))-Lm)*(1-exp(-K*dt))\n\n",
                               "  where Linf = asymptotic mean length\n",
                               "           K = exponential rate of approach to Linf\n",
                               "           b = parameter\n\n",
                               "  and the data are dL = change in length (from taggin to recapture)\n",
                               "                   Lm = length at time of tagging\n",
                               "                   dt = time between tagging and recapture.\n\n",
                               "  and with E(Lm) = expectation (i.e., mean) of Lm.\n\n")

# was Wang2
vonBertalanffy16 <- function(Lm,dt,K,a=NULL,b=NULL) {
  if (length(K)==3) {
    b <- K[[3]]
    a <- K[[2]]
    K <- K[[1]] }
  (a+b*Lm)*(1-exp(-K*dt))
}
SvonBertalanffy16 <- function(Lm,dt,K,a,b) { (a+b*Lm)*(1-exp(-K*dt)) }
msg_vonBertalanffy16 <- paste0("You have chosen paramaterization 16 (alt Wang's Tag-Return) of ",
                               "the von Bertalanffy growth function.\n\n",
                               "  E[dL|Lm,dt] = (a+bLm)*(1-exp(-K*dt))\n\n",
                               "  where K = exponential rate of approach to Linf\n",
                               "     a, b = nuiscance parameters (no meaning)\n\n",
                               "  and the data are dL = change in length (from tagging to recapture)\n",
                               "                   Lm = length at time of marking\n",
                               "                   dt = time between tagging and recapture.\n\n")

# was Wang3
vonBertalanffy17 <- function(Lm,dt,K,a=NULL,b=NULL) {
  if (length(K)==3) {
    b <- K[[3]]
    a <- K[[2]]
    K <- K[[1]] }
  Lm+(a+b*Lm)*(1-exp(-K*dt))
}
SvonBertalanffy17 <- function(Lm,dt,K,a,b) { Lm+(a+b*Lm)*(1-exp(-K*dt)) }
msg_vonBertalanffy17 <- paste0("You have chosen paramaterization 17 (alt2 Wang's Tag-Return) of ",
                               "the von Bertalanffy growth function.\n\n",
                               "  E[Lr|Lm,dt] = Lm+(a+bLm)*(1-exp(-K*dt))\n\n",
                               "  where K = exponential rate of approach to Linf\n",
                               "     a, b = nuiscance parameters (no meaning)\n\n",
                               "  and the data are Lr = length at time of recapture\n",
                               "                   Lm = length at time of tagging\n",
                               "                   dt = time between tagging and recapture.\n\n")

# was Francis2
vonBertalanffy18 <- function(Lm,dt,g1,g2=NULL,L1,L2=NULL) {
  if (length(g1)==2) { g2 <- g1[[2]]; g1 <- g1[[1]] }
  if (length(L1)==2) { L2 <- L1[[2]]; L1 <- L1[[1]] }
  ((L2*g1-L1*g2)/(g1-g2)-Lm)*(1-(1+(g1-g2)/(L1-L2))^dt)
}
SvonBertalanffy18 <- function(Lm,dt,g1,g2,L1,L2) { 
  ((L2*g1-L1*g2)/(g1-g2)-Lm)*(1-(1+(g1-g2)/(L1-L2))^dt)
}
msg_vonBertalanffy18 <- paste0("You have chosen paramaterization 18 (Francis' Tag-Return) of ",
                               "the von Bertalanffy growth function.\n\n",
                               "  E[dL|Lm,dt] = ((L2g1-L1g2)/(g1-g2)-Lm)*(1-(1+(g1-g2)/(L1-L2))^dt)\n\n",
                               "  where g1 = mean growth rate at first (small) reference length L1\n",
                               "        g2 = mean growth rate at second (large) reference length L2\n\n",
                               "You must also give values (i.e., they are NOT model parameters) for\n",
                               "       L1 = the first (usually shorter) reference length\n",
                               "       L2 = the second (usually longer) reference length\n",
                               "The data are dL = change in length (from tagging to recapture)\n",
                               "             Lm = length at time of tagging\n",
                               "             dt = time between tagging and recapture.\n\n")

# was Francis3
vonBertalanffy19 <- function(Lm,t1,t2,g1,g2=NULL,w=NULL,u=NULL,L1,L2=NULL) {
  if (length(g1)==2) { g2 <- g1[[2]]; g1 <- g1[[1]] }
  if (length(L1)==2) { L2 <- L1[[2]]; L1 <- L1[[1]] }
  S1 <- u*sin(2*pi*(t1-w))/(2*pi)
  S2 <- u*sin(2*pi*(t2-w))/(2*pi)
  ((L2*g1-L1*g2)/(g1-g2)-Lm)*(1-(1+(g1-g2)/(L1-L2))^((t2-t1)+S2-S1))
}
SvonBertalanffy19 <- function(Lm,t1,t2,g1,g2,w,u,L1,L2) {
  S1 <- u*sin(2*pi*(t1-w))/(2*pi)
  S2 <- u*sin(2*pi*(t2-w))/(2*pi)
  ((L2*g1-L1*g2)/(g1-g2)-Lm)*(1-(1+(g1-g2)/(L1-L2))^((t2-t1)+S2-S1))
}
msg_vonBertalanffy19 <- paste0("You have chosen paramaterization 19 (Francis' Seasonal Tag-Return) of ",
                               "the von Bertalanffy growth function.\n\n",
                               "  E[dL|Lm,t1,t2] = ((L2g1-L1g2)/(g1-g2)-Lm)*(1-(1+(g1-g2)/(L1-L2))^((t2-t1)+S2-S1))\n\n",
                               "  where S1 = u*sin(2*pi*(t1-w))/(2*pi) and\n",
                               "        S2 = u*sin(2*pi*(t2-w))/(2*pi) and\n\n",
                               "  where g1 = mean growth rate at first (small) reference length L1\n",
                               "        g2 = mean growth rate at second (large) reference length L2\n",
                               "        w  = time of year when the growth rate is maximum\n",
                               "        u  = describes the extent of seasonality.\n\n",
                               "You must also give values (i.e., they are NOT model parameters) for\n",
                               "       L1 = the first (usually shorter) reference length\n",
                               "       L2 = the second (usually longer) reference length\n",
                               "The data are dL = change in length (from mark to recapture)\n",
                               "                   Lm = length at time of tagging\n",
                               "                   t1 = time at tagging\n",
                               "                   t2 = time at recapture.\n\n")

msgsGrow <- c("vonBertalanffy1"=msg_vonBertalanffy1,
              "vonBertalanffy2"=msg_vonBertalanffy2,
              "vonBertalanffy3"=msg_vonBertalanffy3,
              "vonBertalanffy4"=msg_vonBertalanffy4,
              "vonBertalanffy5"=msg_vonBertalanffy5,
              "vonBertalanffy6"=msg_vonBertalanffy6,
              "vonBertalanffy7"=msg_vonBertalanffy7,
              "vonBertalanffy8"=msg_vonBertalanffy8,
              "vonBertalanffy9"=msg_vonBertalanffy9,
              "vonBertalanffy10"=msg_vonBertalanffy10,
              "vonBertalanffy11"=msg_vonBertalanffy11,
              "vonBertalanffy12"=msg_vonBertalanffy12,
              "vonBertalanffy13"=msg_vonBertalanffy13,
              "vonBertalanffy14"=msg_vonBertalanffy14,
              "vonBertalanffy15"=msg_vonBertalanffy15,
              "vonBertalanffy16"=msg_vonBertalanffy16,
              "vonBertalanffy17"=msg_vonBertalanffy17,
              "vonBertalanffy18"=msg_vonBertalanffy18,
              "vonBertalanffy19"=msg_vonBertalanffy19)

#-------------------------------------------------------------------------------
#-- Gompertz parameterizations
#-------------------------------------------------------------------------------
# was Original
Gompertz1 <- function(t,Linf,gi=NULL,a1=NULL) {
  if (length(Linf)==3) {
    a1 <- Linf[[3]]
    gi <- Linf[[2]]
    Linf <- Linf[[1]] }
  Linf*exp(-exp(a1-gi*t))
}
SGompertz1 <-function(t,Linf,gi,a1) { Linf*exp(-exp(a1-gi*t)) }
msg_Gompertz1 <- paste0("You have chosen paramaterization 1 (original) of ",
                        "the Gompertz growth function.\n\n",
                        "  E[L|t] = Linf*exp(-exp(a-gi*t))\n\n",
                        "where Linf = asymptotic mean length\n",
                        "        gi = decrease in growth rate at inflection point\n",
                        "        a1 = nuisance parameter (no interpretation)\n\n")

# was Ricker1
Gompertz2 <- function(t,Linf,gi=NULL,ti=NULL) {
  if (length(Linf)==3) {
    ti <- Linf[[3]]
    gi <- Linf[[2]]
    Linf <- Linf[[1]] }
  Linf*exp(-exp(-gi*(t-ti)))
}
SGompertz2 <- function(t,Linf,gi,ti) { Linf*exp(-exp(-gi*(t-ti))) }
msg_Gompertz2 <- paste0("You have chosen paramaterization 2 of ",
                        "the Gompertz growth function.\n\n",
                        "  E[L|t] = Linf*exp(-exp(-gi*(t-ti)))\n\n",
                        "  where Linf = asymptotic mean length\n",
                        "          gi = instantaneous growth rate at inflection point\n",
                        "          ti = time at the inflection point\n\n")

# was QuinnDeriso1, Ricker2
Gompertz3 <- function(t,L0,gi=NULL,a2=NULL) {
  if (length(L0)==3) {
    a2 <- L0[[3]]
    gi <- L0[[2]]
    L0 <- L0[[1]] }
  L0*exp(a2*(1-exp(-gi*t)))
}
SGompertz3 <- function(t,L0,gi,a2) { L0*exp(a2*(1-exp(-gi*t))) }
msg_Gompertz3 <- paste0("You have chosen paramaterization 3 of ",
                        "the Gompertz growth function.\n\n",
                        "  E[L|t] = L0*exp(b*(1-exp(-gi*t)))\n\n",
                        "  where L0 = the mean length at age-0 (i.e., hatching or birth)\n",
                        "        gi = instantaneous growth rate at the inflection point\n",
                        "        a2 = nuisance parameter (no interpretation)\n\n")

# was QuinnDeriso2, Ricker3
Gompertz4 <-  function(t,Linf,gi=NULL,a2=NULL) {
  if (length(Linf)==3) {
    a2 <- Linf[[3]]
    gi <- Linf[[2]]
    Linf <- Linf[[1]] }
  Linf*exp(-a2*exp(-gi*t))
}
SGompertz4 <- function(t,Linf,gi,a2) { Linf*exp(-a2*exp(-gi*t)) }
msg_Gompertz4 <- paste0("You have chosen paramaterization 4 of ",
                        "the Gompertz growth function.\n\n",
                        "  E[L|t] = Linf*exp(-(a2/gi)*exp(-gi*t))\n\n",
                        "  where Linf = asymptotic mean length\n",
                        "          gi = instantaneous growth rate at inflection point\n",
                        "          a2 = nuisance parameter (no interpretation)\n\n")

# was QuinnDeriso3
Gompertz5 <- function(t,Linf,gi=NULL,t0=NULL) {
  if (length(Linf)==3) { 
    t0 <- Linf[[3]]
    gi <- Linf[[2]]
    Linf <- Linf[[1]] }
  Linf*exp(-(1/gi)*exp(-gi*(t-t0)))
}
SGompertz5 <- function(t,Linf,gi,t0) { Linf*exp(-(1/gi)*exp(-gi*(t-t0))) }
msg_Gompertz5 <- paste0("You have chosen paramaterization 5 of ",
                        "the Gompertz growth function.\n\n",
                        "  E[L|t] = Linf*exp(-(1/gi)*exp(-gi*(t-t0)))\n\n",
                        "  where Linf = asymptotic mean length\n",
                        "          gi = instantaneous growth rate at inflection point\n",
                        "          t0 = hypothetical time/age when mean length is 0\n\n")

# was Troynikov1
Gompertz6 <- function(Lm,dt,Linf,gi=NULL) {
  if (length(Linf)==2) {
    gi <- Linf[2]
    Linf <- Linf[1] }
  Linf*((Lm/Linf)^exp(-gi*dt))-Lm
}
SGompertz6 <- function(Lm,dt,Linf,gi) { Linf*((Lm/Linf)^exp(-gi*dt))-Lm }
msg_Gompertz6 <- paste0("You have chosen paramaterization 6 (Troynikov Tag-Return) of ",
                        "the Gompertz growth function.\n\n",
                        "  E[Lr-Lm|dt] = Linf*((Lm/Linf)^exp(-gi*dt))-Lm\n\n",
                        "  where Linf = asymptotic mean length\n",
                        "          gi = instantaneous growth rate at inflection point\n\n",
                        "  and the data are Lr = length at time of recapture\n",
                        "                   Lm = length at time of tagging\n",
                        "                   dt = time between tagging and recapture.\n\n")

# was Troynikov2
Gompertz7 <- function(Lm,dt,Linf,gi=NULL) {
  if (length(Linf)==2) {
    gi <- Linf[2]
    Linf <- Linf[1] }
  Linf*((Lm/Linf)^exp(-gi*dt))
}
SGompertz7 <- function(Lm,dt,Linf,gi) { Linf*((Lm/Linf)^exp(-gi*dt)) }
msg_Gompertz7 <- paste0("You have chosen paramaterization 7 (alt Troynikov Tag-Return) of ",
                        "the Gompertz growth function.\n\n",
                        "  E[Lr|dt] = Linf*((Lm/Linf)^exp(-gi*dt))\n\n",
                        "  where Linf = asymptotic mean length\n",
                        "          gi = instantaneous growth rate at inflection point\n\n",
                        "  and the data are Lr = length at time of recapture\n",
                        "                   Lm = length at time of tagging\n",
                        "                   dt = time between tagging and recapture.\n\n")

msgsGrow <- c(msgsGrow,
              "Gompertz1"=msg_Gompertz1,
              "Gompertz2"=msg_Gompertz2,
              "Gompertz3"=msg_Gompertz3,
              "Gompertz4"=msg_Gompertz4,
              "Gompertz5"=msg_Gompertz5,
              "Gompertz6"=msg_Gompertz6,
              "Gompertz7"=msg_Gompertz7)


#-------------------------------------------------------------------------------
#-- Logistic parameterizations
#-------------------------------------------------------------------------------
# was CampanaJones1
logistic1 <- function(t,Linf,gninf=NULL,ti=NULL) {
  if (length(Linf)==3) {
    ti <- Linf[[3]]
    gninf <- Linf[[2]]
    Linf <- Linf[[1]] }
  Linf/(1+exp(-gninf*(t-ti)))
}
Slogistic1 <- function(t,Linf,gninf,ti) { Linf/(1+exp(-gninf*(t-ti))) }
msg_logistic1 <- paste0("You have chosen paramaterization 1 of ",
                        "the logistic growth function.\n\n",
                        "  E[L|t] = Linf/(1+exp(-gninf*(t-ti)))\n\n",
                        "  where Linf = asymptotic mean length\n",
                        "      gninif = instantaneous growth rate at t=-infinity\n",
                        "          ti = time at inflection point\n\n")
# was CampanaJones2
logistic2 <- function(t,Linf,gninf=NULL,a=NULL) {
  if (length(Linf)==3) {
    a <- Linf[[3]]
    gninf <- Linf[[2]]
    Linf <- Linf[[1]] }
  Linf/(1+a*exp(-gninf*t))
}
Slogistic2 <- function(t,Linf,gninf,a) { Linf/(1+a*exp(-gninf*t)) }
msg_logistic2 <- paste0("You have chosen paramaterization 2 of ",
                        "the logistic growth function.\n\n",
                        "  E[L|t] = Linf/(1+a*exp(-gninf*t))\n\n",
                        "  where Linf = asymptotic mean length\n",
                        "      gninif = instantaneous growth rate at t=-infinity\n",
                        "           a = dimensionless, related to growth rate\n\n")

# was Karkach
logistic3 <- function(t,Linf,gninf=NULL,L0=NULL) {
  if (length(Linf)==3) {
    L0 <- Linf[[3]]
    gninf <- Linf[[2]]
    Linf <- Linf[[1]] }
  L0*Linf/(L0+(Linf-L0)*exp(-gninf*t))
}
Slogistic3 <- function(t,Linf,gninf,L0) { L0*Linf/(L0+(Linf-L0)*exp(-gninf*t)) }
msg_logistic3 <- paste0("You have chosen paramaterization 3 of ",
                        "the logistic growth function.\n\n",
                        "  E[L|t] = L0*Linf/(L0+(Linf-L0)*exp(-gninf*t))\n\n",
                        "  where Linf = asymptotic mean length\n",
                        "      gninif = instantaneous growth rate at t=-infinity\n",
                        "          L0 = mean length at time/age 0\n\n")

# was Haddon
logistic4 <- function(Lm,dLmax,L50=NULL,L95=NULL) {
  if (length(dLmax)==3) {
    L95 <- dLmax[3]
    L50 <- dLmax[2]
    dLmax <- dLmax[1] }
  dLmax/(1+exp(log(19)*((Lm-L50)/(L95-L50))))
}
Slogistic4 <- function(Lm,dLmax,L50,L95) { dLmax/(1+exp(log(19)*((Lm-L50)/(L95-L50)))) }
msg_logistic4 <- paste0("You have chosen paramaterization 4 (Haddon) of ",
                        "the logistic growth function.\n",
                        "for mark-recapture data.\n\n",
                        "  E[Lr-Lm|dt] = dLmax/(1+exp(log(19)*((Lm-L50)/(L95-L50))))\n\n",
                        "  where dLmax = maximum lenth increment during the study\n",
                        "          L50 = length at tagging for a growth increment of 0.5*dLmax",
                        "          L95 = length at tagging for a growth increment of 0.95*dLmax\n\n",
                        "  and the data are Lr = length at time of recapture\n",
                        "                   Lm = length at time of tagging\n")

msgsGrow <- c(msgsGrow,
              "logistic1"=msg_logistic1,
              "logistic2"=msg_logistic2,
              "logistic3"=msg_logistic3,
              "logistic4"=msg_logistic4)

#-------------------------------------------------------------------------------
#-- Richards parameterizations
#-------------------------------------------------------------------------------
# eqn 5 from Tjorve & Tjorve (2010)
Richards1 <- function(t,Linf,k=NULL,ti=NULL,b1=NULL) {
  if (length(Linf)==4) {
    b1 <- Linf[[4]]
    ti <- Linf[[3]]
    k <- Linf[[2]]
    Linf <- Linf[[1]] }
  Linf/((1+b1*exp(-k*(t-ti)))^(1/b1))
}
SRichards1 <- function(t,Linf,k,ti,b1) { Linf/((1+b1*exp(-k*(t-ti)))^(1/b1)) }
msg_Richards1 <- paste0("You have chosen paramaterization 1 of ",
                        "the Richards growth function.\n\n",
                        "  Linf/((1+b1*exp(-k*(t-ti)))^(1/b1))\n\n",
                        "  where Linf = asymptotic mean length\n",
                        "           k = controls the slope at inflection point\n",
                        "          ti = time/age at inflection point\n",
                        "          b1 = controls y- value of inflection point\n\n")

# eqn 3(alt) from Tjorve & Tjorve (2010)
Richards2 <- function(t,Linf,k=NULL,t0=NULL,b2=NULL) { 
  if (length(Linf)==4) {
    b2 <- Linf[[4]]
    t0 <- Linf[[3]]
    k <- Linf[[2]]
    Linf <- Linf[[1]] }
  Linf/((1+exp(-k*(t-t0)))^(-b2))
}
SRichards2 <- function(t,Linf,k,t0,b2) { Linf/((1+exp(-k*(t-t0)))^(-b2)) }
msg_Richards2 <- paste0("You have chosen paramaterization 2 of ",
                        "the Richards growth function.\n\n",
                        "  Linf/((1-*exp(-k*(t-t0)))^(-b2))\n\n",
                        "  where Linf = asymptotic mean length\n",
                        "           k = controls the slope at inflection point\n",
                        "          t0 = hypothetical time/age when mean length is 0\n",
                        "          b2 = controls y- value of inflection point\n\n")

# eqn 7 from Tjorve & Tjorve (2010)
Richards3 <- function(t,Linf,k=NULL,L0=NULL,b3=NULL) { 
  if (length(Linf)==4) {
    b3 <- Linf[[4]]
    L0 <- Linf[[3]]
    k <- Linf[[2]]
    Linf <- Linf[[1]] }
  Linf*(1+(((L0/Linf)^(1-b3))-1)*exp(-k*t))^(1/(1-b3))
}
SRichards3 <- function(t,Linf,k,L0,b3) { Linf*(1+(((L0/Linf)^(1-b3))-1)*exp(-k*t))^(1/(1-b3)) }
msg_Richards3 <- paste0("You have chosen paramaterization 3 of ",
                        "the Richards growth function.\n\n",
                        "  Linf*(1+(((L0/Linf)^(1-b3))-1)*exp(-k*t))^(1/(1-b3))\n\n",
                        "  where Linf = asymptotic mean length\n",
                        "           k = controls the slope at inflection point\n",
                        "          L0 = mean length at t=0\n",
                        "          b3 = controls y- value of inflection point\n\n")

# eqn 4 from Tjorve & Tjorve (2010)
Richards4 <- function(t,Linf,k=NULL,ti=NULL,b2=NULL) { 
  if (length(Linf)==4) { 
    b2 <- Linf[[4]]
    ti <- Linf[[3]]
    k <- Linf[[2]]
    Linf <- Linf[[1]] }
  Linf*(1-(1/b2)*exp(-k*(t-ti)))^b2
}
SRichards4 <- function(t,Linf,k,ti,b2) { Linf*(1-(1/b2)*exp(-k*(t-ti)))^b2 }
msg_Richards4 <- paste0("You have chosen paramaterization 4 of ",
                        "the Richards growth function.\n\n",
                        "  Linf*(1-(1/b2)*exp(-k*(t-ti)))^b2\n\n",
                        "  where Linf = asymptotic mean length\n",
                        "           k = controls slope at inflection point\n",
                        "          ti = time/age at inflection point\n",
                        "          b2 = controls y- value of inflection point\n\n")

#eqn 6 from Tjorve & Tjorve (2010)
Richards5 <- function(t,Linf,k=NULL,ti=NULL,b3=NULL) { 
  if (length(Linf)==4) {
    b3 <- Linf[[4]]
    ti <- Linf[[3]]
    k <- Linf[[2]]
    Linf <- Linf[[1]] }
  Linf*(1+(b3-1)*exp(-k*(t-ti)))^(1/(1-b3))
}
SRichards5 <- function(t,Linf,k,ti,b3) { Linf*(1+(b3-1)*exp(-k*(t-ti)))^(1/(1-b3)) }
msg_Richards5 <- paste0("You have chosen paramaterization 5 of ",
                        "the Richards growth function.\n\n",
                        "  Linf*(1+(b3-1)*exp(-k*(t-ti)))^(1/(1-b3))\n\n",
                        "  where Linf = asymptotic mean length\n",
                        "           k = controls slope at inflection point\n",
                        "          ti = time/age at inflection point\n",
                        "          b3 = controls y- value of inflection point\n\n")

msgsGrow <- c(msgsGrow,
              "Richards1"=msg_Richards1,
              "Richards2"=msg_Richards2,
              "Richards3"=msg_Richards3,
              "Richards4"=msg_Richards4,
              "Richards5"=msg_Richards5)

#-------------------------------------------------------------------------------
#-- Schnute function
#-------------------------------------------------------------------------------
Schnute <- function(t,L1,L3=NULL,a=NULL,b=NULL,t1,t3=NULL) {
  if (length(L1)==4) {
    b <- L1[[4]];  a <- L1[[3]]
    L3 <- L1[[2]]; L1 <- L1[[1]]
  }  
  if (length(t1)==2) { t3 <- t1[[2]]; t1 <- t1[[1]] }
  # Cases 1-4 in order by if
  if (a!=0 & b!=0) ((L1^b)+((L3^b)-(L1^b))*((1-exp(-a*(t-t1)))/(1-exp(-a*(t3-t1)))))^(1/b)
  else if (a!=0 & b==0) L1*exp(log(L3/L1)*((1-exp(-a*(t-t1)))/(1-exp(-a*(t3-t1)))))
  else if (a==0 & b!=0) ((L1^b)+((L3^b)-(L1^b))*((t-t1)/(t3-t1)))^(1/b)
  else if (a==0 & b==0) L1*exp(log(L3/L1)*((t-t1)/(t3-t1)))
}
SSchnute <- function(t,L1,L3,a,b,t1,t3) {
  # Cases 1-4 in order by if
  if (a!=0 & b!=0) ((L1^b)+((L3^b)-(L1^b))*((1-exp(-a*(t-t1)))/(1-exp(-a*(t3-t1)))))^(1/b)
  else if (a!=0 & b==0) L1*exp(log(L3/L1)*((1-exp(-a*(t-t1)))/(1-exp(-a*(t3-t1)))))
  else if (a==0 & b!=0) ((L1^b)+((L3^b)-(L1^b))*((t-t1)/(t3-t1)))^(1/b)
  else if (a==0 & b==0) L1*exp(log(L3/L1)*((t-t1)/(t3-t1)))
}
msg_Schnute <- paste0("You have chosen the Schnute growth function.\n\n",
                      "Details need to be added here!!","\n\n")

msgsGrow <- c(msgsGrow,"Schnute"=msg_Schnute)

#-------------------------------------------------------------------------------
#-- Schnute-Richards function
#-------------------------------------------------------------------------------
SchnuteRichards <- function(t,Linf,k=NULL,a=NULL,b=NULL,c=NULL) {
  if (length(Linf)==5) {
    c <- Linf[[5]]; b <- Linf[[4]]; a <- Linf[[3]]
    k <- Linf[[2]]; Linf <- Linf[[1]]
  }
  Linf*(1-a*exp(-k*t^c))^(1/b)
}

SSchnuteRichards <- function(t,Linf,k,a,b,c) { Linf*(1-a*exp(-k*t^c))^(1/b) }
msg_SchnuteRichards <- paste0("You have chosen the Schnute-Richards growth function.\n\n",
                        "  Linf*(1-a*exp(-k*t^c))^(1/b)\n\n",
                        "  where Linf = asymptotic mean length\n",
                        "           k = controls slope at inflection point\n",
                        "       a,b,c = nuisance (no meaning) parameters (b!=0)\n\n")

msgsGrow <- c(msgsGrow,"SchnuteRichards"=msg_SchnuteRichards)


#===== Internal function for handling parame, and pname in makeGrowthFun(),
#      showGrowthFun(), and findGrowthStarts()
iHndlGrowthModelParams <- function(type,param,pname,SGF=FALSE) {
  # Make a list of possible parameter names
  param_list <- list(
    "von Bertalanffy"=data.frame(pnum=c(1,1,1,2,2,2,3,4,5,6,6,7,8,9,9,10,11,12,
                                        13,14,15,16,17,18,19),
                                 pnms=c("typical","Typical","Beverton-Holt",
                                        "original","Original","von Bertalanffy",
                                        "Gallucci-Quinn","Mooij","Weisberg",
                                        "Ogle-Isermann","Ogle",
                                        "Schnute","Francis","Laslett","Polacheck",
                                        "Somers","Somers2","Pauly",
                                        "Fabens","Fabens2","Wang","Wang2","Wang3",
                                        "Francis2","Francis3")),
    "Gompertz"=data.frame(pnum=c(1,1,1,2,3,3,4,4,5,6,7),
                          pnms=c("original","Original","Gompertz",
                                 "Ricker1","Ricker2","Quinn-Deriso1",
                                 "Ricker3","Quinn-Deriso2","Quinn-Deriso3",
                                 "Troynikov1","Troynikov2")),
    "logistic"=data.frame(pnum=c(1,2,3,4),
                          pnms=c("Campana-Jones1","Campana-Jones2","Karkach","Haddon")),
    "Richards"=data.frame(pnum=c(1,2,3,4,5),
                          pnms=c("Tjorve5","Tjorve3","Tjorve7","Tjorve4","Tjorve6")))
  
  # If pname used, then convert name to param number
  if(!is.null(pname)) {
    if (length(pname)>1) STOP("Only one name can be given in 'pname'.")
    if (type %in% c("Schnute","Schnute-Richards"))
      STOP("'pname' not used with ",type," model; use 'param' instead.")
    if (!pname %in% param_list[[type]]$pnms)
      STOP("For ",type,"models, 'pname' must be one of: ",
           paste(param_list[[type]]$pnms,collapse=", "))
    param <- param_list[[type]]$pnum[param_list[[type]]$pnms==pname]
  }
  
  # Check that a possible 'param' was given
  max.param <- c("von Bertalanffy"=max(param_list$'von Bertalanffy'$pnum),
                 "Gompertz"=max(param_list$'Gompertz'$pnum),
                 "logistic"=max(param_list$'logistic'$pnum),
                 "Richards"=max(param_list$'Richards'$pnum),
                 "Schnute"=ifelse(SGF,4,1),
                 "Schnute-Richards"=1)
  if (param<1 | param>max.param[[type]]) {
    if (max.param[[type]]==1) STOP("'param' can only be 1 (the default) for ",type," model")
    else STOP(ifelse(type=="Schnute","'case'","'param'")," must be between 1 and ",
              max.param[[type]]," for ",type," model")
  }
  
  # param is irrelevant if type only has 1 param ... so set to NULL
  if (type %in% names(max.param)[max.param==1]) param <- NULL
  
  # Return param number
  param
}
