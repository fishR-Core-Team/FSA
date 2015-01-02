#' @title Kruskal-Wallis Multiple Comparisons.
#'
#' @description Performs Dunn, Nemenyi, or Siegel-Castellan tests of multiple comparisons following a significant Kruskal-Wallis test.
#'
#' @details This function is largely a wrapper for \code{dunn.test} from the \pkg{dunn.test} package, \code{posthoc.kruskal.nemenyi.test} from the \pkg{PMCMR} package, and \code{kruskalmc} from the \pkg{pgirmess} package.  It is intended to provide one interface for the most common methods for performing multiple comparisons following a significant Kruskal-Wallis test.  The data are provided to this function in the same way that they are provided to \code{kruskal.test}, which makes for more uniform use.
#' 
#' The user uses \code{type} to choose the \dQuote{Dunn}, \dQuote{Nemenyi}, or \dQuote{Siegel-Castellan} type of tests.  If \code{type="Dunn"} (Default) or \code{type="Nemenyi"} are chosen, then the user can choose a method to control the experimentwise error rate with \code{method}.  The \code{method="Tukey"} is only available for \code{type="Nemenyi"} and no other methods can be used with \code{type="Nemenyi"}.  The allowable methods for \code{type="Dunn"} are described in \code{p.adjust.methods} from \pkg{dunn.test}.  The default is to not control the experimentwise error rate.
#' 
#'   Other arguments to be sent to \code{dunn.test}, \code{posthoc.kruskal.nemenyi.test}, or \code{kruskalmc} are described in their respective help pages.
#' 
#' Consult the references below for the algorithms used in each method.
#'
#' @aliases kruskalMC kruskalMC.default kruskalMC.formula
#'
#' @param x A numeric vector of data values or a formula of the form x~g.
#' @param g A factor vector or a (non-numeric) vector that can be coerced to a factor vector.
#' @param data A data.frame that minimally contains \code{x} and \code{g}.
#' @param type A single string that identifies if the \dQuote{Dunn}, \dQuote{Nemenyi}, or \dQuote{Siegel-Castellan} multiple comparison method should be used.
#' @param method A single string that identifies the method to use to control the experimentwise error rate.  See details.
#' @param kw A logical that indicates whether the results of the Kruskal-Wallis test are reported.  Only used with \code{type="Dunn"}.  In contrast to \code{dunn.test()}, the default is \code{FALSE}.
#' @param alpha The nominal level of significance to be used.  This is mapped to the \code{probs} argument in \code{kruskalmc} if \code{type="SiegelCastellan"}.
#' @param \dots Arguments for \code{dunn.test} in \pkg{dunn.test} or \code{kruskalmc} in \pkg{pgirmess}.
#'
#' @return An object as defined in \code{dunn.test} in \pkg{dunn.test} or \code{kruskalmc} in \pkg{pgirmess} depending on which \code{type} of test was chosen.
#'
#' @seealso See \code{kruskal.test} and \code{dunn.test} in \pkg{dunn.test}, \code{posthoc.kruskal.nemenyi.test} from the \pkg{PMCMR}, and \code{kruskalmc} in \pkg{pgirmess}
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}, but largely a wrapper to the See Also functions.
#'
#' @references 
#' Dunn, O.J.  1964.  Multiple comparisons using rank sums. Technometrics 6:241-252.
#' 
#' Nemenyi, P.B.  1963.  Distribution-free multiple comparisons.  PhD dissertation, Princetion Univesity.
#' 
#' Siegel, S. and N.J. Castellan, Jr.  1988.  Nonparametric statistics for the behavioural sciences. MacGraw-Hill, New York. Relevant pages are 213-214 which are available at \url{http://giraudoux.pagesperso-orange.fr/#pgirmess}.
#' @keywords htest
#'
#' @examples
#' \dontrun{
#' ## Examples from dunn.test in dunn.test package
#' require(dunn.test)
#' data(homecare)
#' with(homecare,dunn.test(occupation,eligibility,method="hs"))
#' with(homecare,kruskalMC(occupation,eligibility,type="Dunn",method="hs"))
#' kruskalMC(occupation~eligibility,data=homecare,type="Dunn",method="hs")
#' 
#' with(airquality,dunn.test(Ozone, Month, kw=FALSE, method="bonferroni"))
#' with(airquality,dunn.test(Ozone, Month, kw=FALSE, method="hs"))
#' with(airquality,dunn.test(Ozone, Month, kw=FALSE, method="bh"))
#' kruskalMC(Ozone~Month,data=airquality,type="Dunn",method="bonferroni",kw=FALSE)
#' kruskalMC(Ozone~Month,data=airquality,type="Dunn",method="hs",kw=FALSE)
#' kruskalMC(Ozone~Month,data=airquality,type="Dunn",method="bh",kw=FALSE)
#' }
#' 
#' \dontrun{
#' ## examples from kruskalmc in pgirmess package
#' require(pgirmess)
#' df <- data.frame(
#'   resp=c(0.44,0.44,0.54,0.32,0.21,0.28,0.7,0.77,0.48,0.64,0.71,0.75,0.8,0.76,0.34,0.80,0.73,0.8),
#'   categ=as.factor(rep(c("A","B","C"),times=1,each=6))
#' )
#' kruskalmc(resp~categ,data=df)
#' kruskalmc(resp~categ,data=df,probs=0.01)
#' kruskalmc(resp~categ,data=df,cont="one-tailed")
#' kruskalmc(resp~categ,data=df,cont="two-tailed")
#' 
#' kruskalMC(resp~categ,data=df,type="Siegel")
#' kruskalMC(resp~categ,data=df,type="Siegel",alpha=0.01)
#' kruskalMC(resp~categ,data=df,type="Siegel",cont="one-tailed")
#' kruskalMC(resp~categ,data=df,type="Siegel",cont="two-tailed")
#' }
#'
#'\dontrun{
#' ## examples from posthoc.kruskal.nemenyi.test in the PMCMR package
#' require(PMCMR)
#' data(InsectSprays)
#' with(InsectSprays,posthoc.kruskal.nemenyi.test(count,spray))
#' with(InsectSprays,posthoc.kruskal.nemenyi.test(count,spray,"Chisq"))
#' kruskalMC(count~spray,data=InsectSprays,type="Nemenyi",method="Tukey")
#' kruskalMC(count~spray,data=InsectSprays,type="Nemenyi",method="Chisquare")
#' }
#' 
#' @rdname kruskalMC
#' @export
kruskalMC <- function (x,...) {
  UseMethod("kruskalMC") 
}

#' @rdname kruskalMC
#' @export
kruskalMC.default <- function(x,g,type=c("Dunn","Nemenyi","SiegelCastellan"),
                              method=c("none","bonferroni","sidak","holm","hs",
                                       "hochberg","bh","by","Tukey","Chisquare"),
                              kw=FALSE,alpha=0.05,...) {
  ## match the arguments
  type <- match.arg(type)
  method <- match.arg(method)
  ## perform some simple checks on the data
  if (!is.numeric(x)) stop("'x' must be numeric.",call.=FALSE)
  if (!is.factor(g)) {
    if (is.numeric(g)) stop("'g' must be coerceable to a factor.",call.=FALSE)
    g <- as.factor(g)
    warning("'g' variable was coerced to a factor.",call.=FALSE)
  }
  ## send to specific methods depending on type
  switch(type,
         Dunn={
           if (method=="Tukey") stop("method='Tukey' cannot be used with type='Dunn'",call.=FALSE)
           dunn.test::dunn.test(x=x,g=g,method=method,kw=kw,alpha=alpha,...)},
         Nemenyi={
           if (!method %in% c("Chisquare","Tukey")) stop("'method' must be 'Chisquare' or 'Tukey' when type='Nemenyi'",call.=FALSE)
           PMCMR::posthoc.kruskal.nemenyi.test(x=x,g=g,method=method)
         },
         SiegelCastellan={
           if (method!="none") warning("method changed to 'none' for when type='SiegelCastellan'",call.=FALSE)
           method <- "none"
           pgirmess::kruskalmc(resp=x,categ=g,probs=alpha,...)
           }
         ) # end switch 
}

#' @rdname kruskalMC
#' @export
kruskalMC.formula <- function(x,data=NULL,type=c("Dunn","Nemenyi","SiegelCastellan"),
                              method=c("none","bonferroni","sidak","holm","hs",
                                       "hochberg","bh","by","Tukey","Chisquare"),
                              kw=FALSE,alpha=0.05,...) {
  ## match the arguments
  type <- match.arg(type)
  method <- match.arg(method)
  ## get the dataframe of just the two variables
  tmp <- iHndlFormula(x,data,expNumR=1,expNumE=1)
  d <- tmp$mf
  ## perform some simple checks on the formula and variables
  if (!tmp$metExpNumR) stop("'kruskalMC' must have only one LHS variable.",call.=FALSE)
  if (!tmp$Rclass %in% c("numeric","integer")) stop("LHS variable must be numeric.",call.=FALSE)
  if (!tmp$metExpNumE) stop("'kruskalMC' must have only one RHS variable.",call.=FALSE)
  if (tmp$Eclass!="factor") {
    if (tmp$Eclass=="numeric") stop("RHS variable must be a factor.",call.=FALSE)
    d[,tmp$Enames] <- as.factor(d[,tmp$Enames])
    warning(tmp$Enames," was coerced to a factor.",call.=FALSE)
  }
  ## send the two variables to kruskalMC.default
  kruskalMC.default(d[,tmp$Rname],d[,tmp$Enames],type=type,method=method,kw=kw,alpha=alpha,...)
}
