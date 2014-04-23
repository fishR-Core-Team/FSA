\name{removal}
\alias{confint.removal}
\alias{removal}
\alias{summary.removal}
\title{Computes population estimates for k-, 3-, or 2-pass removal data.}
\usage{
removal(catch, type = c("Zippin", "CarleStrub", "Seber3", "Seber2",
  "RobsonRegier2"), alpha = 1, beta = 1, CS.se = c("Zippin",
  "Alternative"), just.ests = FALSE)

\method{summary}{removal}(object, ...)

\method{confint}{removal}(object, parm = c("both", "all", "No", "p"),
  level = conf.level, conf.level = 0.95, ...)
}
\arguments{
  \item{catch}{A numerical vector of catches of fish at
  each pass.}

  \item{type}{A single string that identifies the type of
  removal method to use for the calculations.  See
  details.}

  \item{alpha}{A single numeric value for the alpha
  parameter in the CarleStrub method (default is
  \code{1}).}

  \item{beta}{A single numeric value for the beta parameter
  in the CarleStrub method (default is \code{1}).}

  \item{CS.se}{A single string that identifies whether the
  SE in the CarleStrub method should be computed according
  to Seber or Zippin.}

  \item{object}{An object saved from \code{removal()}
  (i.e., of class \code{removal}).}

  \item{parm}{A specification of which parameters are to be
  given confidence intervals, either a vector of numbers or
  a vector of names.  If missing, all parameters are
  considered.}

  \item{level}{Same as \code{conf.level} but used for
  compatability with generic \code{confint} function.}

  \item{conf.level}{A single number representing the level
  of confidence to use for constructing confidence
  intervals.}

  \item{just.ests}{A logical that indicates whether just
  the estimates (\code{=TRUE}) or the return list
  (\code{=FALSE}; default; see below) is returned.}

  \item{\dots}{Additional arguments for methods.}
}
\value{
A vector that contains the estimaes and standard errors for
No and p if \code{just.ests=TRUE} or (default) a list with
the following items: \itemize{ \item catch the original
vector of observed catches. \item type The type of method
used (provided by the user). \item meth A label for the
type of method used. \item est A vector that contains the
estimates and standard errors for No and p. }
}
\description{
Computes estimates, with confidence intervals, of the
population size and probability of capture from the number
of fish removed in k-, 3-, or 2-passes in a closed
population.
}
\details{
The main function computes the estimates and associated
standard errors for the initial population size, No, and
probability of capture, p, for five possible methods chosen
with the \code{type} argument.  The types of methods that
can be used are listed below: \itemize{ \item
\code{type="Zippin"}: The general k-pass estimator
generally attributed to Zippin.  This function iteratively
solves for No in equation 3 of Carle and Strub (1978).
\item \code{type="CarleStrub"}: The general weighted k-pass
estimator proposed by Carle and Strub (1978).  This
function iteratively solves for No in equation 7 of Carle
and Strub (1978). \item \code{type="Seber3"}: The special
case for k=3 estimator shown by Seber(1982). \item
\code{type="Seber2"}: The special case for k=2 estimator
shown by Seber(1982). \item \code{type="RobsonRegier2"}:
The special case for k=2 estimator shown by Robson and
Regier (1968). }

Confidence intervals are computed using standard
large-sample normal distribution theory.  Note that the
confidence intervals for the 2- and 3-pass special cases
are only approximately correct if the estimated population
size is greater than 200.  If the estimated population size
is between 50 and 200 then a 95\% CI behaves more like a
90\% CI.

Note that, in the Carle Strub method, that if the resultant
No estimate is equal to the sum of the catches (T) then the
estimate of No that is returned will be the sum of the
catches.  In this instance and if the \dQuote{Seber} method
of computing the standard error is used then the SE will
not be estimable and the confidence intervals can not be
constructed.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl//Depletion.pdf}
}
\examples{
## First example -- 3 passes
ct3 <- c(77,50,37)

# Zippin (default) method
p1 <- removal(ct3)
summary(p1)
confint(p1)

# Carle Strub method
p2 <- removal(ct3,type="CarleStrub")
summary(p2)
confint(p2)

# Seber method
p3 <- removal(ct3,type="Seber3")
summary(p3)
confint(p3)

## Second example -- 2 passes
ct2 <- c(77,37)

# Seber method
p4 <- removal(ct2,type="Seber2")
summary(p4)
confint(p4)

# Robson-Regier method
p5 <- removal(ct2,type="RobsonRegier2")
summary(p5)
confint(p5)

## Demonstration of situation where estimates cannot be made
bad <- c(34,23,45)
bad1 <- removal(bad)


### Using lapply() to use removal on many different groups
## create a dummy data frame
lake <- factor(rep(c("Ash Tree","Bark","Clay"),each=5))
year <- factor(rep(c("2010","2011","2010","2011","2010","2011"),times=c(2,3,3,2,2,3)))
pass <- factor(c(1,2,1,2,3,1,2,3,1,2,1,2,1,2,3))
catch <- c(57,34,65,34,12,54,26,9,54,27,67,34,68,35,12)
d <- data.frame(lake,year,pass,catch)

## create a variable that indicates each different group
d$group <- with(d,interaction(lake,year))
d
## split the catch by the different groups (creates a list of catch vectors)
ds <- split(d$catch,d$group)
## apply removal() to each catch vector (i.e., different group)
res <- lapply(ds,removal,just.ests=TRUE)
res <- data.frame(t(data.frame(res,check.names=FALSE)))
## get rownames from above and split into separate columns
nms <- t(data.frame(strsplit(rownames(res),"\\\\.")))
attr(nms,"dimnames") <- NULL
fnl <- data.frame(nms,res)
## put names together with values
rownames(fnl) <- NULL
colnames(fnl)[1:2] <- c("Lake","Year")
fnl
## append approx. 95\% CIs
fnl$No.LCI <- fnl$No-1.96*fnl$No.se
fnl$No.UCI <- fnl$No+1.96*fnl$No.se
fnl
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\references{
Carle, F.L. and M.R. Strub. 1978. A new method for
estimating population size from removal data.  Biometrics,
34:621-630.

Seber, G.A.F. 1982. The Estimation of Animal Abundance.
Edward Arnold, second edition.

Robson, D.S., and H.A. Regier.  1968.  Estimation of
population number and mortality rates.  pp. 124-158 in
Ricker, W.E. (editor) Methods for Assessment of Fish
Production in Fresh Waters.  IBP Handbook NO. 3 Blackwell
Scientific Publications, Oxford.

Cowx, I.G.  1983.  Review of the methods for estimating
fish population size from survey removal data.  Fisheries
Management, 14:67-82.
}
\seealso{
\code{\link{depletion}}.
}
\keyword{manip}

