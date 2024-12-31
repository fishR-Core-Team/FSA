#' @title Estimate natural mortality from a variety of empirical methods.
#'
#' @description Several methods can be used to estimated natural mortality (M) from other types of data, including parameters from the von Bertalanffy growth equation, maximum age, and temperature. These relationships have been developed from meta-analyses of a large number of populations. Several of these methods are implemented in this function.
#'
#' @details One of several methods is chosen with \code{method}. The available methods can be seen with \code{Mmethods()} and are listed below with a brief description of where the equation came from. The sources (listed below) should be consulted for more specific information.
#'  \itemize{
#'    \item \code{method="HoenigNLS"}:  The \dQuote{modified Hoenig equation derived with a non-linear model} as described in Then \emph{et al.} (2015) on the third line of Table 3. This method was the preferred method suggested by Then \emph{et al.} (2015). Requires only \code{tmax}.
#'    \item \code{method="PaulyLNoT"}: The \dQuote{modified Pauly length equation} as described on the sixth line of Table 3 in Then \emph{et al.} (2015). Then \emph{et al.} (2015) suggested that this is the preferred method if maximum age (tmax) information was not available. Requires \code{K} and \code{Linf}.
#'    \item \code{method="PaulyL"}: The \dQuote{Pauly (1980) equation using fish lengths} from his equation 11. This is the most commonly used method in the literature. Note that Pauly used common logarithms as used here but the model is often presented in other sources with natural logarithms. Requires \code{K}, \code{Linf}, and \code{T}.
#'    \item \code{method="PaulyW"}: The \dQuote{Pauly (1980) equation for weights} from his equation 10. Requires \code{K}, \code{Winf}, and \code{T}.
#'    \item \code{method="HoeingO"}, \code{method="HoeingOF"}, \code{method="HoeingOM"}, \code{method="HoeingOC"}: The original \dQuote{Hoenig (1983) composite}, \dQuote{fish}, \dQuote{mollusc}, and \dQuote{cetacean} (fit with OLS) equations from the second column on page 899 of Hoenig (1983). Requires only \code{tmax}.
#'    \item \code{method="HoeingO2"}, \code{method="HoeingO2F"}, \code{method="HoeingO2M"}, \code{method="HoeingO2C"}: The original \dQuote{Hoenig (1983) composite}, \dQuote{fish}, \dQuote{mollusc}, and \dQuote{cetacean} (fit with Geometric Mean Regression) equations from the second column on page 537 of Kenchington (2014). Requires only \code{tmax}.
#'    \item \code{method="HoenigLM"}: The \dQuote{modified Hoenig equation derived with a linear model} as described in Then \emph{et al.} (2015) on the second line of Table 3. Requires only \code{tmax}.
#'    \item \code{method="HewittHoenig"}: The \dQuote{Hewitt and Hoenig (2005) equation} from their equation 8. Requires only \code{tmax}.
#'    \item \code{method="tmax1"}: The \dQuote{one-parameter tmax equation} from the first line of Table 3 in Then \emph{et al.} (2015). Requires only \code{tmax}.
#'    \item \code{method="K1"}:  The \dQuote{one-parameter K equation} from the fourth line of Table 3 in Then \emph{et al.} (2015). Requires only \code{K}.
#'    \item \code{method="K2"}: The \dQuote{two-parameter K equation} from the fifth line of Table 3 in Then \emph{et al.} (2015). Requires only \code{K}.
#'    \item \code{method="JensenK1"}: The \dQuote{Jensen (1996) one-parameter K equation}. Requires only \code{K}.
#'    \item \code{method="JensenK2"}: The \dQuote{Jensen (2001) two-parameter K equation} from their equation 8. Requires only \code{K}.
#'    \item \code{method="Gislason"}: The \dQuote{Gislason \emph{et al.} (2010) equation} from their equation 2. Requires \code{K}, \code{Linf}, and \code{L}.
#'    \item \code{method="AlversonCarney"}: The \dQuote{Alverson and Carney (1975) equation} as given in equation 10 of Zhang and Megrey (2006). Requires \code{tmax} and \code{K}.
#'    \item \code{method="Charnov"}: The \dQuote{Charnov \emph{et al.} (2013) equation} as given in the second column of page 545 of Kenchington (2014). Requires \code{K}, \code{Linf}, and \code{L}.
#'    \item \code{method="ZhangMegreyD"}, \code{method="ZhangMegreyP"}: The \dQuote{Zhang and Megrey (2006) equation} as given in their equation 8 but modified for demersal or pelagic fish. Thus, the user must choose the fish type with \code{group}. Requires \code{tmax}, \code{K}, \code{t0}, \code{t50}, and \code{b}.
#'    \item \code{method="RikhterEfanov1"}: The \dQuote{Rikhter and Efanov (1976) equation (#2)} as given in the second column of page 541 of Kenchington (2014) and in Table 6.4 of Miranda and Bettoli (2007). Requires only \code{t50}.
#'    \item \code{method="RikhterEfanov2"}: The \dQuote{Rikhter and Efanov (1976) equation (#1)} as given in the first column of page 541 of Kenchington (2014). Requires \code{t50}, \code{K}, \code{t0}, and \code{b}.
#'    \item \code{method="QuinnDeriso"}: The \dQuote{Quinn & Derison (1999)} equation as given in the FAMS manual as equation 4:18. Requires \code{PS} and \code{tmax}. Included only for use with \code{rFAMS} package.
#'    \item \code{method="ChenWatanabe"}: The \dQuote{Chen & Watanabe (1989)} equation as given in the FAMS manual as equation 4:24. As suggested in FAMS manual used \code{tmax} for final time and 1 as initial time. Requires \code{tmax}, \code{K}, and \code{t0}. Included only for use with \code{rFAMS} package.
#'    \item \code{method="PetersonWroblewski"}: The \dQuote{Peterson & Wroblewski (1984)} equation as given in the FAMS manual as equation 4:22. As suggested in FAMS manual used \code{Winf} for weight. Requires \code{Winf}. Included only for use with \code{rFAMS} package.
#'  } 
#' 
#' Conditional mortality (cm) is estimated from instantaneous natural mortality (M) with 1-exp(-M). It is returned with M here simply as a courtesy for those using the \code{rFAMS} package.
#' 
#' @param method A string that indicates what grouping of methods to return (defaults to all methods) in \code{Mmethods()} or which methods or equations to use in \code{metaM()}. See details.
#' @param tmax The maximum age for the population of fish.
#' @param K The Brody growth coefficient from the fit of the von Bertalanffy growth function.
#' @param Linf The asymptotic mean length (cm) from the fit of the von Bertalanffy growth function.
#' @param t0 The x-intercept from the fit of the von Bertalanffy growth function.
#' @param b The exponent from the weight-length relationship (slope from the logW-logL relationship).
#' @param L The body length of the fish (cm).
#' @param Temp The temperature experienced by the fish (C).
#' @param t50 The age (time) when half the fish in the population are mature.
#' @param Winf The asymptotic mean weight (g) from the fit of the von Bertalanffy growth function.
#' @param PS The proportion of the population that survive to \code{tmax}. Should usually be around 0.01 or 0.05.
#' @param verbose Logical for whether to include method name and given inputs in resultant data.frame. Defaults to \code{TRUE}.
#'
#' @return \code{Mmethods} returns a character vector with a list of methods.
#' 
#' \code{metaM} returns a data.frame with the following items:
#' \itemize{
#'    \item \code{M}: The estimated natural mortality rate.
#'    \item \code{cm}: The estimated conditional natural mortality rate (computed directly from \code{M}).
#'    \item \code{method}: The name for the method within the function (as given in \code{method}).
#'    \item \code{name}: A more descriptive name for the method.
#'    \item \code{givens}: A string that contains the input values required by the method to estimate M.
#'  }
#' 
#' @section Testing: Kenchington (2014) provided life history parameters for several stocks and used many models to estimate M. I checked the calculations for the \code{PaulyL}, \code{PaulyW}, \code{HoenigO}, \code{HoenigOF}, \code{HoenigO2}, \code{HoenigO2F}, \code{"JensenK1"}, \code{"Gislason"}, \code{"AlversonCarney"}, \code{"Charnov"}, \code{"ZhangMegrey"}, \code{"RikhterEfanov1"}, and \code{"RikhterEfanov2"} methods for three stocks. All results perfectly matched Kenchington's results for Chesapeake Bay Anchovy and Rio Formosa Seahorse. For the Norwegian Fjord Lanternfish, all results perfectly matched Kenchington's results except for \code{HoenigOF} and \code{HoenigO2F}.
#' 
#' Results for the Rio Formosa Seahorse data were also tested against results from \code{\link[fishmethods]{M.empirical}} from \pkg{fishmethods} for the \code{PaulyL}, \code{PaulyW}, \code{HoenigO}, \code{HoenigOF}, \code{"Gislason"}, and \code{"AlversonCarney"} methods (the only methods in common between the two packages). All results matched perfectly.
#' 
#' @author Derek H. Ogle, \email{DerekOgle51@gmail.com}
#' 
#' @section IFAR Chapter: 11-Mortality.
#' 
#' @seealso See \code{\link[fishmethods]{M.empirical}} in \pkg{fishmethods} for similar functionality.
#' 
#' @references Ogle, D.H. 2016. \href{https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
#' 
#' Alverson, D.L. and M.J. Carney. 1975. A graphic review of the growth and decay of population cohorts. Journal du Conseil International pour l'Exploration de la Mer. 36:133-143.
#' 
#' Chen, S. and S. Watanabe. 1989. Age dependence of natural mortality coefficient in fish population dynamics. Nippon Suisan Gakkaishi 55:205-208.
#' 
#' Charnov, E.L., H. Gislason, and J.G. Pope. 2013. Evolutionary assembly rules for fish life histories. Fish and Fisheries. 14:213-224.
#' 
#' Gislason, H., N. Daan, J.C. Rice, and J.G. Pope. 2010. Size, growth, temperature and the natural mortality of marine fish. Fish and Fisheries 11:149-158.
#' 
#' Hewitt, D.A. and J.M. Hoenig. 2005. Comparison of two approaches for estimating natural mortality based on longevity. Fishery Bulletin. 103:433-437. [Was (is?) from http://fishbull.noaa.gov/1032/hewitt.pdf.]
#' 
#' Hoenig, J.M. 1983. Empirical use of longevity data to estimate mortality rates. Fishery Bulletin. 82:898-903. [Was (is?) from http://www.afsc.noaa.gov/REFM/age/Docs/Hoenig_EmpiricalUseOfLongevityData.pdf.]
#' 
#' Jensen, A.L. 1996. Beverton and Holt life history invariants result from optimal trade-off of reproduction and survival. Canadian Journal of Fisheries and Aquatic Sciences. 53:820-822. [Was (is?) from .]
#' 
#' Jensen, A.L. 2001. Comparison of theoretical derivations, simple linear regressions, multiple linear regression and principal components for analysis of fish mortality, growth and environmental temperature data. Environometrics. 12:591-598. [Was (is?) from http://deepblue.lib.umich.edu/bitstream/handle/2027.42/35236/487_ftp.pdf.]
#' 
#' Kenchington, T.J. 2014. Natural mortality estimators for information-limited fisheries. Fish and Fisheries. 14:533-562.
#' 
#' Pauly, D. 1980. On the interrelationships between natural mortality, growth parameters, and mean environmental temperature in 175 fish stocks. Journal du Conseil International pour l'Exploration de la Mer. 39:175-192. [Was (is?) from http://innri.unuftp.is/pauly/On\%20the\%20interrelationships\%20betwe.pdf.]
#' 
#' Peterson, I. and J.S. Wroblewski. 1984. Mortality rate of fishes in the pelagic ecosystem. Canadian Journal of Fisheries and Aquatic Sciences. 41:1117-1120.
#' 
#' Quinn III, T.J. and R.B. Deriso. 1999. Quantitative Fish Dynamics. Oxford University Press, New York.
#' 
#' Rikhter, V.A., and V.N. Efanov. 1976. On one of the approaches for estimating natural mortality in fish populations (in Russian). ICNAF Research Document 76/IV/8, 12pp. 
#' 
#' Slipke, J.W. and M.J. Maceina. 2013. Fisheries Analysis and Modeling Simulator (FAMS 1.64). American Fisheries Society.
#' 
#' Then, A.Y., J.M. Hoenig, N.G. Hall, and D.A. Hewitt. 2015. Evaluating the predictive performance of empirical estimators of natural mortality rate using information on over 200 fish species. ICES Journal of Marine Science. 72:82-92.
#' 
#' Zhang, C-I and B.A. Megrey. 2006. A revised Alverson and Carney model for estimating the instantaneous rate of natural mortality. Transactions of the American Fisheries Society. 135-620-633. [Was (is?) from http://www.pmel.noaa.gov/foci/publications/2006/zhan0531.pdf.]
#' 
#' @keywords manip 
#'    
#' @aliases metaM Mmethods
#' 
#' @examples
#' ## List names for available methods
#' Mmethods()
#' Mmethods("tmax")
#' 
#' ## Simple Examples
#' metaM("tmax",tmax=20)
#' metaM("HoenigNLS",tmax=20)
#' metaM("HoenigNLS",tmax=20,verbose=FALSE)
#'  
#' ## Example Patagonian Sprat ... from Table 2 in Cerna et al. (2014)
#' ## http://www.scielo.cl/pdf/lajar/v42n3/art15.pdf
#' Temp <- 11
#' Linf <- 17.71
#' K <- 0.78
#' t0 <- -0.46
#' tmax <- t0+3/K
#' t50 <- t0-(1/K)*log(1-13.5/Linf)
#' metaM("RikhterEfanov1",t50=t50)
#' metaM("PaulyL",K=K,Linf=Linf,Temp=Temp)
#' metaM("HoenigNLS",tmax=tmax)
#' metaM("HoenigO",tmax=tmax)
#' metaM("HewittHoenig",tmax=tmax)
#' metaM("AlversonCarney",K=K,tmax=tmax)
#' 
#' ## Example of multiple calculations
#' metaM(c("RikhterEfanov1","PaulyL","HoenigO","HewittHoenig","AlversonCarney"),
#'      K=K,Linf=Linf,Temp=Temp,tmax=tmax,t50=t50)
#'
#' ## Example of multiple methods using Mmethods
#' # select some methods
#' metaM(Mmethods()[-c(15,20,22:24,26:29)],K=K,Linf=Linf,Temp=Temp,tmax=tmax,t50=t50)
#' # select just the Hoenig methods
#' metaM(Mmethods("Hoenig"),K=K,Linf=Linf,Temp=Temp,tmax=tmax,t50=t50)
#'  
#' ## Example of computing an average M
#' # select multiple models used in FAMS (example only, these are not best models)
#' ( res <- metaM(Mmethods("FAMS"),tmax=tmax,K=K,Linf=Linf,t0=t0,
#'                Temp=Temp,PS=0.01,Winf=30) )
#' ( meanM <- mean(res$M) )
#' ( meancm <- mean(res$cm) )
#' 
#' @rdname metaM
#' @export
Mmethods <- function(method=c("all","tmax","K","Hoenig","Pauly","FAMS")) {
  method <- match.arg(method)
  all_meth <- c("HoenigNLS","HoenigO","HoenigOF","HoenigOM","HoenigOC",
                "HoenigO2","HoenigO2F","HoenigO2M","HoenigO2C",
                "HoenigLM","HewittHoenig","tmax1",
                "PaulyLNoT","PaulyL","PaulyW",
                "K1","K2","JensenK1","JensenK2","Gislason",
                "AlversonCarney","Charnov",
                "ZhangMegreyD","ZhangMegreyP",
                "RikhterEfanov1","RikhterEfanov2",
                "QuinnDeriso","ChenWatanabe","PetersonWroblewski")
  H_meth <- all_meth[grep("Hoenig",all_meth)]
  P_meth <- 
  switch(method,
         all    = { meths <- all_meth },
         tmax   = { meths <- c("tmax1",H_meth)},
         K      = { meths <- c("K1","K2","JensenK1","JensenK2")},
         Hoenig = { meths <- H_meth},
         Pauly  = { meths <- all_meth[grep("Pauly",all_meth)] },
         FAMS   = { meths <- c("QuinnDeriso","HoenigOF","JensenK1",
                               "PetersonWroblewski","PaulyL","ChenWatanabe")}
         )
  meths
}

#' @rdname metaM
#' @export
metaM <- function(method=Mmethods(),
                  tmax=NULL,K=NULL,Linf=NULL,t0=NULL,b=NULL,
                  L=NULL,Temp=NULL,t50=NULL,Winf=NULL,PS=NULL,
                  verbose=TRUE) {
  ## Get method or methods
  method <- match.arg(method,several.ok=TRUE)
  ## Use apply to run all methods at once (even if only one)
  res <- lapply(method,metaM1,tmax,K,Linf,t0,b,L,Temp,t50,Winf,PS)
  ## Put together as a data.frame to return
  res <- as.data.frame(do.call(rbind,res))
  ## If not verbose then remove name and givens from data.frame
  if (!verbose) res <- res[,!names(res) %in% c("name","givens")]
  ## Return the result
  res
}
  
metaM1 <- function(method,tmax,K,Linf,t0,b,L,Temp,t50,Winf,PS,...) {
  switch(method,
         tmax1 = { ## from Then et al. (2015), Table 3, 1st line
           name <- "Then et al. (2015) tmax equation"
           iCheck_tmax(tmax)
           givens <- c(tmax=tmax)
           M <- 5.109/tmax },
         PaulyLNoT = { ## from Then et al. (2015), Table 3, 6th line
           name <- "Then et al. (2015) Pauly_NLS-T equation"
           iCheck_K(K)
           iCheck_Linf(Linf)
           givens <- c(K=K,Linf=Linf)
           M <- 4.118*(K^(0.73))*(Linf^(-0.33)) },
         PaulyL = { ## from Pauly (1980) Equation 11
           name <- "Pauly (1980) length equation"
           iCheck_K(K)
           iCheck_Linf(Linf)
           iCheck_Temp(Temp)
           givens <- c(K=K,Linf=Linf,Temp=Temp)
           M <- 10^(-0.0066-0.279*log10(Linf)+0.6543*log10(K)+0.4634*log10(Temp)) },
         PaulyW         = { ## from Pauly (1980) Equation 10
           iCheck_K(K)
           iCheck_Winf(Winf)
           iCheck_Temp(Temp)
           name <- "Pauly (1980) weight equation"
           givens <- c(K=K,Winf=Winf,Temp=Temp)
           M <- 10^(-0.2107-0.0824*log10(Winf)+0.6757*log10(K)+0.4627*log10(Temp)) },
         HoenigO = { ## from Hoenig (1983), 4th line, 2nd column, page 899
           name <- "Hoenig (1983) combined equation (OLS)"
           iCheck_tmax(tmax)
           givens <- c(tmax=tmax)
           M <- exp(1.44-0.982*log(tmax)) },
         HoenigOF = { ## from Hoenig (1983), 2nd line, 2nd column, page 899
           name <- "Hoenig (1983) fish equation (OLS)"
           iCheck_tmax(tmax)
           givens <- c(tmax=tmax)
           M <- exp(1.46-1.01*log(tmax)) },
         HoenigOM = { ## from Hoenig (1983), 1st line, 2nd column, page 899
           name <- "Hoenig (1983) mollusk equation (OLS)"
           iCheck_tmax(tmax)
           givens <- c(tmax=tmax)
           M <- exp(1.23-0.832*log(tmax)) },
         HoenigOC = { ## from Hoenig (1983), 3rd line, 2nd column, page 899
           name <- "Hoenig (1983) cetacean equation (OLS)"
           iCheck_tmax(tmax)
           givens <- c(tmax=tmax)
           M <- exp(0.941-0.873*log(tmax)) },
         HoenigO2 = { ## from Kenchington (2014) p.537 2nd column
           name <- "Hoenig (1983) combined equation (GM)"
           iCheck_tmax(tmax)
           givens <- c(tmax=tmax)
           M <- 5.52*tmax^(-1.08) },
         HoenigO2F = { ## from Kenchington (2014) p.537 2nd column
           name <- "Hoenig (1983) fish equation (GM)"
           iCheck_tmax(tmax)
           givens <- c(tmax=tmax)
           M <- 6.99*tmax^(-1.22) },
         HoenigO2M = {## from Kenchington (2014) p.537 2nd column
           name <- "Hoenig (1983) mollusk equation (GM)"
           iCheck_tmax(tmax)
           givens <- c(tmax=tmax)
           M <- 4.49*tmax^(-0.94) },
         HoenigO2C = { ## from Kenchington (2014) p.537 2nd column
           name <- "Hoenig (1983) cetacean equation (GM)"
           iCheck_tmax(tmax)
           givens <- c(tmax=tmax)
           M <- 5.20*tmax^(-1.04) },        
         HoenigLM = { ## from Then et al. (2015), Table 3, 2nd line
           iCheck_tmax(tmax)
           name <- "Then et al. (2015) Hoenig (LM) equation"
           givens <- c(tmax=tmax)
           M <- exp(1.717-1.01*log(tmax)) },
         HoenigNLS = { ## from Then et al. (2015), Table 3, 3rd line
           iCheck_tmax(tmax)
           name <- "Then et al. (2015) Hoenig (NLS) equation"
           givens <- c(tmax=tmax)
           M <- 4.899*tmax^(-0.916) },
         HewittHoenig = { ## from Hewitt and Hoenig (2005) equation 8
           iCheck_tmax(tmax)
           name <- "Hewitt & Hoenig (2005) tmax equation"
           givens <- c(tmax=tmax)
           M <- 4.22/tmax},
         K1 = { ## from Then et al. (2015), Table 3, 4th line
           iCheck_K(K)
           name <- "Then et al. (2015) one-parameter K equation"
           givens <- c(K=K)
           M <- 1.692*K },
         K2 = { ## from Then et al. (2015), Table 3, 5th line
           iCheck_K(K)
           name <- "Then et al. (2015) two-parameter K equation"
           givens <- c(K=K)
           M <- 0.098+1.55*K },
         JensenK1 = { ## from Jensen (1996), Kensington's "Jensen's Second Estimator"
           iCheck_K(K)
           name <- "Jensen (1996) one parameter K equation"
           givens <- c(K=K)
           M <- 1.5*K },
         JensenK2 = { ## from Jensen (2001) equation 8
           iCheck_K(K)
           name <- "Jensen (2001) two parameter K equation"
           givens <- c(K=K)
           M <- 0.21+1.47*K},         
         Gislason = { ## from Gislason et al. (2010) equation 2
           iCheck_K(K)
           iCheck_Linf(Linf)
           iCheck_L(L)
           name <- "Gislason et al. (2010) equation"
           givens <- c(K=K,Linf=Linf,L=L)
           M <- exp(0.55-1.61*log(L)+1.44*log(Linf)+log(K)) },
         AlversonCarney = {
           ## from Alverson and Carney (1975), eqn 10 in Zhang & Megrey (2006)
           iCheck_K(K)
           iCheck_tmax(tmax)
           name <- "Alverson & Carney (1975) equation"
           givens <- c(tmax=tmax,K=K)
           M <- (3*K)/(exp(K*(0.38*tmax))-1)},
         Charnov = {
           ## from Charnov et al. (2013) given on p. 545,
           ##   2nd column of Kenchington (2014)
           iCheck_K(K)
           iCheck_Linf(Linf)
           iCheck_L(L)
           name <- "Charnov et al. (2013) equation"
           givens <- c(K=K,Linf=Linf,L=L)
           M <- K*((Linf/L)^1.5)},
         ZhangMegreyD = { ## from Zhang and Megrey (2006) equation 8
           name <- "Zhang & Megrey (2006) Demersal equation"
           iCheck_K(K)
           iCheck_t0(t0)
           iCheck_tmax(tmax)
           iCheck_b(b)
           Ci <- 0.302
           givens <- c(tmax=tmax,t0=t0,K=K,b=b,Ci=Ci)
           M <- (b*K)/(exp(K*(Ci*tmax-t0))-1)},
         ZhangMegreyP = { ## from Zhang and Megrey (2006) equation 8
           name <- "Zhang & Megrey (2006) Pelagic equation"
           iCheck_K(K)
           iCheck_t0(t0)
           iCheck_tmax(tmax)
           iCheck_b(b)
           Ci <- 0.44
           givens <- c(tmax=tmax,t0=t0,K=K,b=b,Ci=Ci)
           M <- (b*K)/(exp(K*(Ci*tmax-t0))-1)},
         RikhterEfanov1 = {
           ## from Richter and Efanov (1976) as given on p. 541,
           ##   2nd column of Kenchington (2014) and in Table 6.4
           ##   of Miranda and Bettoli (2007)
           iCheck_t50(t50)
           name <- "Richter & Evanov (1976) equation #1"
           givens <- c(t50=t50)
           M <- (1.521/(t50^0.720))-0.155 },
         RikhterEfanov2 = {
           ## from Richter and Efanov (1976) as given on p. 541,
           ##   1st column of Kenchington (2014)
           iCheck_K(K)
           iCheck_t50(t50)
           iCheck_b(b)
           iCheck_t0(t0)
           name <- "Richter & Evanov (1976) equation #2"
           givens <- c(K=K,t0=t0,t50=t50,b=b)
           M <- (b*K)/(exp(K*(t50-t0))-1) },
         QuinnDeriso = {
           ## from Quinn & Deriso (1990) as described in FAMS manual
           ##   equation 4:18 in FAMS manual
           iCheck_PS(PS)
           name <- "Quinn & Deriso (1999) from FAMS"
           givens <- c(PS=PS,tmax=tmax)
           M <- -log(PS)/tmax
         },
         ChenWatanabe = {
           ## from Chen and Watanabe (1989) as described in FAMS manual
           ##   equation 4:24 in FAMS manual
           ##   here followed FAMS notes and used ti=1 and tf=tmax
           ti <- 1    # initial age
           tf <- tmax # final age
           iCheck_K(K)
           iCheck_t0(t0)
           iCheck_tmax(tmax)
           name <- "Chen & Watanabe (1989) from FAMS"
           givens <- c(tmax=tmax,K=K,t0=t0)
           M <- (1/(tf-ti))*log((exp(-K*tf)-exp(-K*t0))/(exp(-K*ti)-exp(-K*t0)))
         },
         PetersonWroblewski = {
           ## From Peterson & Wroblewski (1984) as described in FAMS manual
           ##   equation 4:22 in FAMS manual
           ##   here followed FAMS notes and used W=Winf
           W <- Winf
           iCheck_Winf(Winf)
           name <- "Peterson & Watanabe (1984) from FAMS"
           givens <- c(Winf=Winf)
           M <- 1.92*(W^(-0.25))
         }
  ) # end switch()
  ## Make givens into a string
  ### Round given values to default digits, and then convert to chracter
  givens <- sapply(givens,
                   FUN=function(x) as.character(round(x,digits=getOption("digits"))))
  ### Combine givens name with givens value
  tmpgivens <- paste0(names(givens),"=",givens)
  ### Separate multiple givens with a comma
  if (length(givens>1)) tmpgivens <- paste(tmpgivens,collapse=", ")
  
  ## Return data.frame
  data.frame(M=M,cm=1-exp(-M),method=method,name=name,givens=tmpgivens)
}

# ############################################################
# Internal methods
# ############################################################
iCheck_tmax <- function(tmax) {
  if (is.null(tmax)) STOP("A value must be given to 'tmax'.")  
  if (tmax <= 0) STOP("'tmax' must be positive.")
  if (tmax > 100) WARN("'tmax' value seems unreasonable.")
}

iCheck_K <- function(K) {
  if (is.null(K)) STOP("A value must be given to 'K'.")  
  if (K <= 0) STOP("'K' must be positive.")
  if (K > 3) WARN("'K' value seems unreasonable.")
}

iCheck_Linf <- function(Linf) {
  if (is.null(Linf)) STOP("A value must be given to 'Linf'.")  
  if (Linf <= 0) STOP("'Linf' must be positive.")
  if (Linf > 200) WARN("'Linf' value seems unreasonable. Make sure value is in centimeters.")
}

iCheck_L <- function(L) {
  if (is.null(L)) STOP("A value must be given to 'L'.")  
  if (L <= 0) STOP("'L' must be positive.")
  if (L > 200) WARN("'L' value seems unreasonable. Make sure value is in centimeters.")
}

iCheck_Winf <- function(Winf) {
  if (is.null(Winf)) STOP("A value must be given to 'Winf'.")  
  if (Winf <= 0) STOP("'Winf' must be positive.")
}

iCheck_t0 <- function(t0) {
  if (is.null(t0)) STOP("A value must be given to 't0'.")  
}

iCheck_Temp <- function(Temp) {
  if (is.null(Temp)) STOP("A value must be given to 'Temp'.")  
  if (Temp < 0 || Temp > 30) WARN("'Temp' value seems unreasonable. Make sure value is in celsius.")
}

iCheck_t50 <- function(t50) {
  if (is.null(t50)) STOP("A value must be given to 't50'.")  
  if (t50 <= 0) STOP("'t50' must be positive.")
  if (t50 > 100) WARN("'t50' value seems unreasonable.")
}

iCheck_b <- function(b) {
  if (is.null(b)) STOP("A value must be given to 'b'.")  
  if (b<1 || b>5) WARN("'b' value seems unreasonable.")
}

iCheck_PS <- function(PS) {
  if (is.null(PS)) STOP("A value must be given to 'PS'.")
  if (PS<0) STOP("'PS' must be greater than 0.")
  if (PS>1) STOP("'PS' should be proportion (e.g., 0.01).")
  if (PS>0.1) WARN("'PS' value seems unreasonable (FAMS suggests 0.01 or 0.05).")
}
