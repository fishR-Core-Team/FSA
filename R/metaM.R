#' @title Estimate natural mortality from a variety of empirical methods.
#'
#' @description Several methods can be used to estimated natural mortality (M) from other types of data, including parameters from the von Bertalanffy growth equation, maximum age, and temperature.  These relationships have been developed from meta-analyses of a large number of populations.  Several of these methods are implemented in this function.
#'
#' @details One of several methods is chosen with \code{method}.  The available methods can be seen with \code{Mmethods} and are listed below with a brief description of where the equation came from.  The sources (listed below) should be consulted for more specific information.
#'  \itemize{
#'    \item \code{method="tmax1"}: The \dQuote{one-parameter tmax equation} from the first line of Table 3 in Then et al. (2014).  This method was the method suggested by Then et al. (2014) and, thus, is the default method used.  Requires only \code{tmax}.
#'    \item \code{method="PaulyL"}: The \dQuote{Pauly (1980) equation using fish lengths} from his equation 11.  This is the most commonly used method in the literature.  Note that Pauly used common logarithms as used here but the model is often presented in other sources with natural logarithms.  Requires \code{K}, \code{Linf}, and \code{T}.
#'    \item \code{method="HoeingALL"}: The \dQuote{Hoenig (1983) composite equation} from the fourth line in the second column on page 899 of Hoenig (1983).  Requires only \code{tmax}.
#'    \item \code{method="HoenigLM"}:  The \dQuote{modified Hoenig equation derived with a linear model} as described in Then et al. (2014) on the second line of Table 3.  Requires only \code{tmax}.
#'    \item \code{method="HoenigNLS"}:  The \dQuote{modified Hoenig equation derived with a non-linear model} as described in Then et al. (2014) on the third line of Table 3.  Requires only \code{tmax}.
#'    \item \code{method="HoenigMollusks"}:  The \dQuote{Hoenig (1983) mollusks equation} from the first line in the second column on page 899 of Hoenig (1983).  Requires only \code{tmax}.
#'    \item \code{method="HoenigFish"}:  The \dQuote{Hoenig (1983) fish equation} from the second line in the second column on page 899 of Hoenig (1983).  Requires only \code{tmax}.
#'    \item \code{method="HoenigCetaceans"}:  The \dQuote{Hoenig (1983) cetaceans equation} from the third line in the second column on page 899 of Hoenig (1983).  Requires only \code{tmax}.
#'    \item \code{method="HewittHoenig"}:  The \dQuote{Hewitt and Hoenig (2005) equation} from their equation 8.  Requires only \code{tmax}.
#'    \item \code{method="K1"}:  The \dQuote{one-parameter K equation} from the fourth line of Table 3 in Then et al. (2014).  Requires only \code{K}.
#'    \item \code{method="K2"}: The \dQuote{two-parameter K equation} from the fifth line of Table 3 in Then et al. (2014).  Requires only \code{K}.
#'    \item \code{method="JensenK1"}: The \dQuote{Jensen (1996) one-parameter K equation}.  Requires only \code{K}.
#'    \item \code{method="JensenK2"}: The \dQuote{Jensen (2001) two-parameter K equation} from their equation 8.  Requires only \code{K}.
#'    \item \code{method="PaulyNoT"}: The \dQuote{modified Pauly length equation} as described on the sixth line of Table 3 in Then et al. (2014).  Then et al. (2014) suggested using this model of maximum age (tmax) information was not available.  Requires \code{K} and \code{Linf}.
#'    \item \code{method="Gislason"}: The \dQuote{Gislason et al. (2010) equation} from their equation 2.  Requires \code{K} and \code{Linf}.
#'    \item \code{method="AlversonCarney"}: The \dQuote{Alverson and Carney (1975) equation} as given in equation 10 of Zhang and Megrey (2006).  Requires \code{tmax} and \code{K}.
#'    \item \code{method="Charnov"}: The \dQuote{Charnov et al. (2013) equation} as given in the second column of pge 545 of Kenchington (2013).  Requires \code{K}, \code{Linf}, and \code{L}.
#'    \item \code{method="ZhangMegrey"}: The \dQuote{Zhang and Megrey (2006) equation} as given in their equation 8.  The formula differs depending on whether demersal or pelagic fish are being studied.  Thus, the user must choose the fish type with \R{group}.  Requires \code{tmax}, \code{K}, \code{t0}, \code{t50}, and \code{b}.
#'    \item \code{method="RikhterEfanov1"}: The \dQuote{Rikhter and Efanov (1976) equation (#2)} as given in the second column of pge 541 of Kenchington (2013) and in Table 6.4 of Miranda and Bettoli (2007).  Requires only \code{t50}.
#'    \item \code{method="RikhterEfanov2"}: The \dQuote{Rikhter and Efanov (1976) equation (#1)} as given in the first column of pge 541 of Kenchington (2013).  Requires \code{K}, \code{t0}, \code{t50}, and \code{b}.
#'    \item \code{method="PaulyW"}: The \dQuote{Pauly (1980) equation for weights} from his equation 10.  Requires \code{K}, \code{Winf}, and \code{T}.
#'  }  
#'    
#' @aliases metaM print.metaM Mmethods
#'
#' @param method A string that indicates which method or equation to use.  See details.
#' @param group A string that indicates whether the \code{ZhangMegrey} method should be modified for \code{"demersal"} or \code{"pelagic"} fish.
#' @param tmax The maximum age for the population of fish.
#' @param K The Brody growth coefficident from the fit of the von Bertalanffy growth function.
#' @param Linf The asymptotic mean length from the fit of the von Bertalanffy growth function.
#' @param t0 The x-intercept from the fit of the von Bertalanffy growth function.
#' @param b The exponent from the weight-length relationship (slope from the logW-logL relationship).
#' @param L The body length of the fish (cm).
#' @param T The temperature experienced by the fish (C).
#' @param t50 The age (time) when half the fish in the population are mature.
#' @param Winf The asymptotic mean weight from the fit of the von Bertalanffy growth function.
#' @param justM A logical that indicates whether just the estimate of M (\code{TRUE}) or a more descriptive list (Default) should be returned.
#' @param x A \code{metaM} object returned from \code{metaM}.
#' @param digits A numeric that controls the number of digits printed for the estimate of M.
#' @param \dots Additional arguments for methods.  Not implemented.
#'
#' @return A single numeric if \code{justM=TRUE}, otherwise a \code{metaM} object that is a list with the following items:
#' \itemize{
#'    \item \code{method}: The name for the method within the function (as given in \code{method}).
#'    \item \code{name}: A more descriptive name for the method.
#'    \item \code{givens}: A vector of values required by the method to estimate M.
#'    \item \code{M}: The estimated natural mortality rate.
#'  }
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @seealso See \code{M.empirical} in \pkg{fishmethods} for similar functionality.
#' 
#' @section fishR vignette: XXX
#' 
#' @references 
#' Alverson, D.L. and M.J. Carney.  1975.  A graphic review of the growth and decay of population cohorts.  Journal du Conseil International pour l'Exploration de la Mer. 36:133-143.
#' 
#' Charnov, E.L., H. Gislason, and J.G. Pope.  2013.  \href{http://www.sefsc.noaa.gov/sedar/download/S32_RD17_Charnov_etal _2012.pdf?id=DOCUMENT}{Evolutionary assembly rules for fish life histories}.  Fish and Fisheries.  14:213-224.
#' 
#' Gislason, H., N. Daan, J.C. Rice, and J.G. Pope.  2010.  Size, growth, temperature and the natural mortality of marine fish.  Fish and Fisheries 11:149-158.
#' 
#' Hewitt, D.A. and J.M. Hoenig.  2005.  \href{http://fishbull.noaa.gov/1032/hewitt.pdf}{Comparison of two approaches for estimating natural mortality based on longevity}.  Fishery Bulletin.  103:433-437.
#' 
#' Hoenig, J.M.  1983.  \href{http://www.afsc.noaa.gov/REFM/age/Docs/Hoenig_EmpiricalUseOfLongevityData.pdf}{Empirical use of longevity data to estimate mortality rates}.  Fishery Bulletin. 82:898-903.
#' 
#' Jensen, A.L.  1996.  Beverton and Holt life history invariants result from optimal trade-off of reproduction and survival.  Canadian Journal of Fisheries and Aquatic Sciences. 53:820-822.
#' 
#' Jensen, A.L.  2001.  \href{http://deepblue.lib.umich.edu/bitstream/handle/2027.42/35236/487_ftp.pdf}{Comparison of theoretical derivations, simple linear regressions, multiple linear regression and principal components for analysis of fish mortality, growth and environmental temperature data}.  Environometrics.  12:591-598.
#' 
#' Kenchington, T.J.  2014.  Natural mortality estimators for information-limited fisheries.  Fish and Fisheries.  14:533-562.
#' 
#' Pauly, D.  1980.  \href{http://innri.unuftp.is/pauly/On the interrelationships betwe.pdf}{On the interrelationships between natural mortality, growth parameters, and mean environmental temperature in 175 fish stocks}.  Journal du Conseil International pour l'Exploration de la Mer. 39:175-192.
#' 
#' Rikhter, V.A., and V.N. Efanov.  1976.  On one of the approaches for estimating natural mortality in fish populations (in Russian). ICNAF Research Document 76/IV/8, 12pp. 
#' 
#' Then, A.Y., J.M. Hoenig, N.G. Hall, and D.A. Hewitt.  2014.  Evaluating the predictive performance of empirical estimators of natural mortality rate using informatno on over 200 fish species.  ICES Journal of Marine Science.  XX:XXX-XXX.
#' 
#' Zhang, C-I and B.A. Megrey.  2006.  \href{http://www.pmel.noaa.gov/foci/publications/2006/zhan0531.pdf}{A revised Alverson and Carney model for estimating the instantaneous rate of natural mortality}.  Transactions of the American Fisheries Socity.  135-620-633.
#' 
#' @keywords manip
#' 
#' @examples
#' ## List names for available methods
#' Mmethods
#' 
#' ## Examples
#' metaM(tmax=20)
#' metaM(tmax=20,justM=TRUE)
#' 
#' # Example 8.11 (Atka Mackerel) from Quinn & Deriso (1999)
#' metaM("AlversonCarney",K=0.285,tmax=10)
#' metaM("AlversonCarney",K=0.285,tmax=12)
#' metaM("AlversonCarney",K=0.66,tmax=10)
#' metaM("AlversonCarney",K=0.66,tmax=12)
#' 
#' # Example 8.13 (Haddock) from Quinn & Deriso (1999)
#' metaM("PaulyL",K=0.233,Linf=71.8,T=3.9)
#' metaM("PaulyL",K=0.309,Linf=68.3,T=5.24)
#' 
#' # Example Patagonial Sprat ... from Table 2 in Cerna et al. (2014)
#' # http://www.scielo.cl/pdf/lajar/v42n3/art15.pdf
#' T <- 11
#' Linf <- 17.71
#' K <- 0.78
#' t0 <- -0.46
#' tmax <- t0+3/K
#' t50 <- t0-(1/K)*log(1-13.5/Linf)
#' metaM("RikhterEfanov1",t50=t50)
#' metaM("PaulyL",K=K,Linf=Linf,T=T)
#' metaM("HoenigAll",tmax=tmax)
#' metaM("HewittHoenig",tmax=tmax)
#' 
#' @rdname metaM
#' @export
Mmethods <- c("tmax1","PaulyL","HoenigAll",
              "HoenigLM","HoenigNLS","HoenigMollusks","HoenigFish",
              "HoenigCetaceans","HewittHoenig","K1","K2","JensenK1",
              "JensenK2","PaulyNoT","Gislason","AlversonCarney",
              "Charnov","ZhangMegrey","RikhterEfanov1",
              "RikhterEfanov2","PaulyW")

#' @rdname metaM
#' @export
metaM <- function(method=Mmethods,group=c("pelagic","demersal"),
                       tmax=NULL,K=NULL,Linf=NULL,t0=NULL,b=NULL,
                       L=NULL,T=NULL,t50=NULL,Winf=NULL,justM=FALSE,...) {
  method <- match.arg(method)
  switch(method,
         tmax1          = { ## from Then et al (2014), Table 3, 1st line
                            if (is.null(tmax)) stop("A value must be given to 'tmax'.",call.=FALSE)
                            name <- "Then et al. (2014) one-parameter tmax equation"
                            givens <- c(tmax=tmax)
                            M <- 5.109/tmax },
         HoenigLM       = { ## from Then et al (2014), Table 3, 2nd line
                            if (is.null(tmax)) stop("A value must be given to 'tmax'.",call.=FALSE)
                            name <- "Then et al. (2014) Hoenig_LM equation"
                            givens <- c(tmax=tmax)
                            M <- exp(1.717-1.01*log(tmax)) },
         HoenigNLS      = { ## from Then et al (2014), Table 3, 3rd line
                            if (is.null(tmax)) stop("A value must be given to 'tmax'.",call.=FALSE)
                            name <- "Then et al. (2014) Hoenig_NLS equation"
                            givens <- c(tmax=tmax)
                            M <- 4.899*tmax^(-0.916) },
         HoenigAll      = { ## from Hoenig (1983), 4th line, 2nd column, page 899
                            if (is.null(tmax)) stop("A value must be given to 'tmax'.",call.=FALSE)
                            name <- "Hoenig (1983) combined equation"
                            givens <- c(tmax=tmax)
                            M <- exp(1.44-0.982*log(tmax)) },
         HoenigMollusks = { ## from Hoenig (1983), 1st line, 2nd column, page 899
                            if (is.null(tmax)) stop("A value must be given to 'tmax'.",call.=FALSE)
                            name <- "Hoenig (1983) mollusk equation"
                            givens <- c(tmax=tmax)
                            M <- exp(1.23-0.832*log(tmax)) },
         HoenigFish     = { ## from Hoenig (1983), 2nd line, 2nd column, page 899
                            if (is.null(tmax)) stop("A value must be given to 'tmax'.",call.=FALSE)
                            name <- "Hoenig (1983) fish equation"
                            givens <- c(tmax=tmax)
                            M <- exp(1.46-1.01*log(tmax)) },
         HoenigCetaceans= { ## from Hoenig (1983), 3rd line, 2nd column, page 899
                            if (is.null(tmax)) stop("A value must be given to 'tmax'.",call.=FALSE)
                            name <- "Hoenig (1983) cetacean equation"
                            givens <- c(tmax=tmax)
                            M <- exp(0.941-0.873*log(tmax)) },
         HewittHoenig   = { ## from Hewitt and Hoenig (2005) equation 8
                            if (is.null(tmax)) stop("A value must be given to 'tmax'.",call.=FALSE)
                            name <- "Hewitt & Hoenig (2005) tmax equation"
                            givens <- c(tmax=tmax)
                            M <- 4.22/tmax},
         K1             = { ## from Then et al (2014), Table 3, 4th line
                            if (is.null(K)) stop("A value must be given to 'K'.",call.=FALSE)
                            name <- "Then et al. (2014) one-parameter K equation"
                            givens <- c(K=K)
                            M <- 1.692*K },
         K2             = { ## from Then et al (2014), Table 3, 5th line
                            if (is.null(K)) stop("A value must be given to 'K'.",call.=FALSE)
                            name <- "Then et al. (2014) two-parameter K equation"
                            givens <- c(K=K)
                            M <- 0.098+1.55*K },
         JensenK1       = { ## from Jensen (1996)
                            if (is.null(K)) stop("A value must be given to 'K'.",call.=FALSE)
                            name <- "Jensen (1996) one parameter K equation"
                            givens <- c(K=K)
                            M <- 1.60*K },
         JensenK2       = { ## from Jensen (2001) equation 8
                            if (is.null(K)) stop("A value must be given to 'K'.",call.=FALSE)
                            name <- "Jensen (2001) two parameter K equation"
                            givens <- c(K=K)
                            M <- 0.21+1.47*K},         
         PaulyLNoT      = { ## from Then et al (2014), Table 3, 6th line
                            if (is.null(K)) stop("A value must be given to 'K'.",call.=FALSE)
                            if (is.null(Linf)) stop("A value must be given to 'Linf'.",call.=FALSE)
                            name <- "Then et al. (2014) Pauly_NLS-T equation"
                            givens <- c(K=K,Linf=Linf)
                            M <- 4.118*(K^(0.73))*(Linf^(-0.33)) },
         Gislason       = { ## from Gislason et al. (2010) equation 2
                            if (is.null(K)) stop("A value must be given to 'K'.",call.=FALSE)
                            if (is.null(Linf)) stop("A value must be given to 'Linf'.",call.=FALSE)
                            name <- "Gislason et al. (2010) equation"
                            givens <- c(K=K,Linf=Linf)
                            M <- exp(0.55-1.61*log(L)+1.44*log(Linf)+log(K)) },
         AlversonCarney = { ## from Alverson and Carney (1975), equation 10 in Zhang & Megrey (2006)
                            if (is.null(K)) stop("A value must be given to 'K'.",call.=FALSE)
                            if (is.null(tmax)) stop("A value must be given to 'tmax'.",call.=FALSE)
                            name <- "Alverson & Carney (1975) equation"
                            givens <- c(tmax=tmax,K=K)
                            M <- (3*K)/(exp(K*(0.38*tmax))-1)},
         PaulyL         = { ## from Pauly (1980) Equation 11
                            if (is.null(K)) stop("A value must be given to 'K'.",call.=FALSE)
                            if (is.null(Linf)) stop("A value must be given to 'Linf'.",call.=FALSE)
                            if (is.null(T)) stop("A value must be given to 'T'.",call.=FALSE)
                            name <- "Pauly (1980) length equation"
                            givens <- c(K=K,Linf=Linf,T=T)
                            M <- 10^(-0.0066-0.279*log10(Linf)+0.6543*log10(K)+0.4634*log10(T)) },
         Charnov        = { ## from Charnov et al. (2013) given on p. 545,
                            ##   2nd column of Kenchington (2013)
                            if (is.null(K)) stop("A value must be given to 'K'.",call.=FALSE)
                            if (is.null(Linf)) stop("A value must be given to 'Linf'.",call.=FALSE)
                            if (is.null(L)) stop("A value must be given to 'L'.",call.=FALSE)
                            name <- "Charnov et al. (2013) equation"
                            given <- c(K=K,Linf=Linf,L=L)
                            M <- K*((Linf/L)^1.5)},
         ZhangMegrey    = { ## from Zhang and Megrey (2006) equation 8
                            name <- "Zhang & Megrey (2006) equation"
                            group <- match.arg(group)
                            if (is.null(K)) stop("A value must be given to 'K'.",call.=FALSE)
                            if (is.null(tmax)) stop("A value must be given to 'tmax'.",call.=FALSE)
                            if (is.null(b)) stop("A value must be given to 'b'.",call.=FALSE)
                            if (is.null(t0)) stop("A value must be given to 't0'.",call.=FALSE)
                            Ci <- ifelse(group=="Pelagic",0.44,0.38)
                            givens <- c(tmax=tmax,t0=t0,K=K,b=b,Ci=Ci)
                            M <- (b*K)/(exp(K*(Ci*tmax-t0))-1)},
         RikhterEfanov2 = { ## from Richter and Efanov (1976) as given on p. 541,
                            ##   1st column of Kenchington (2013)
                            if (is.null(K)) stop("A value must be given to 'K'.",call.=FALSE)
                            if (is.null(t50)) stop("A value must be given to 't50'.",call.=FALSE)
                            if (is.null(b)) stop("A value must be given to 'b'.",call.=FALSE)
                            if (is.null(t0)) stop("A value must be given to 't0'.",call.=FALSE)
                            name <- "Richter & Evanov (1976) equation #2"
                            givens <- c(K=K,t0=t0,t50=t50,b=b)
                            M <- (b*K)/(exp(K*(t50-t0))-1) },
         RikhterEfanov1 = { ## from Richter and Efanov (1976) as given on p. 541,
                            ##   2nd column of Kenchington (2013) and in Table 6.4
                            ##   of Miranda and Bettoli (2007)
                            if (is.null(t50)) stop("A value must be given to 't50'.",call.=FALSE)
                            name <- "Richter & Evanov (1976) equation #1"
                            givens <- c(t50=t50)
                            M <- (1.521/(t50^0.720))-0.155 },
         PaulyW         = { ## from Pauly (1980) Equation 10
                            if (is.null(K)) stop("A value must be given to 'K'.",call.=FALSE)
                            if (is.null(Winf)) stop("A value must be given to 'Winf'.",call.=FALSE)
                            if (is.null(T)) stop("A value must be given to 'T'.",call.=FALSE)
                            name <- "Pauly (1980) weight equation"
                            givens <- c(K=K,Winf=Winf,T=T)
                            M <- 10^(-0.2107-0.0824*log10(Winf)+0.6757*log10(K)+0.4627*log10(T)) },
  ) # end switch()
  if (justM) res <- M
    else {
      res <- list(method=method,name=name,givens=givens,M=M)
      class(res) <- "metaM"
    }
  res
}

#' @rdname metaM
#' @method print metaM
#' @export
print.metaM <- function(x,digits=4,...) {
  cat("M=",round(x$M,digits)," as estimated with ",x$name,"\n",sep="")
  tmp <- paste(names(x$givens),"=",x$givens,sep="")
  if (length(x$givens>1)) tmp <- paste(tmp,collapse=", ")
  cat("  with givens:",tmp,"\n")
}
