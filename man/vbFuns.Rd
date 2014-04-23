\name{vbFuns}
\alias{vbFuns}
\title{Creates a function for a specific von Bertalanffy parameterization.}
\usage{
vbFuns(type = c("typical", "original", "BevertonHolt", "Fabens", "Fabens2",
  "Francis", "GallucciQuinn", "Laslett", "Mooij", "Schnute", "Somers",
  "Somers2", "vonBertalanffy", "Wang", "Wang2"), simple = FALSE,
  msg = FALSE)
}
\arguments{
  \item{type}{A string that indicates the parameterization
  of the von Bertalanffy model.}

  \item{simple}{A logical that indicates whether the user
  should be allowed to send all parameter values in the
  first parameter argument (\code{=FALSE}; default) or
  whether all individual parameters must be specified
  (\code{=TRUE}).}

  \item{msg}{A logical that indicates whether a message
  about the model and parameter definitions should be
  output (\code{=TRUE}) or not (\code{=FALSE}; default).}
}
\value{
A function that can be used to predict fish length given a
vector of ages and values for the model parameters and, in
some parameterizations, values for some constants.  The
result should be saved to an object that can then be used
as a function name.  When the resulting function is used
the parameters are ordered as shown when the definitions of
the parameters are printed after the function is called (if
\code{msg=TRUE}).

If \code{simple=FALSE} was used then the values for all
three parameters can be included as a vector of length
three in the first paramater argument. Similarly, the
values for all constants can be included as a vector in the
first constant argument (i.e., \code{t1}).  If
\code{simple=TRUE} then all parameters and constants must
be declared individually.  In addition, if
\code{simple=TRUE} then the resulting function is somewhat
easier to read.
}
\description{
Creates a function for a specific von Bertalanffy model
parameterization.
}
\note{
The \sQuote{original} and \sQuote{vonBertalanffy} and the
\sQuote{typical} and \sQuote{BevertonHolt} versions are
synonomous.

Thanks to Gabor Grothendieck for a hint about using
\code{get()}.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/VonBertalanffy.pdf},
  \url{https://sites.google.com/site/fishrfiles/gnrl/VonBertalanffyExtra.pdf}
}
\examples{
## Simple Examples
( vb1 <- vbFuns() )              # typical parameterization
ages <- 0:20
plot(vb1(ages,Linf=20,K=0.3,t0=-0.2)~ages,type="b",pch=19)

( vb2 <- vbFuns("Francis") )     # Francis parameterization
predL <- vb2(ages,L1=10,L2=19,L3=20,t1=2,t3=18)
plot(predL~ages,type="b",pch=19)

( vb2s <- vbFuns("Francis",simple=TRUE) )   # compare to vb2

## Examples of fitting Von B models
##   After the last example a plot is constructed with three lines on top of each
##   other illustrating that the parameterizations all produce the same fitted
##   values.  However, observe the correlations in the summary() results.

# Fitting the typical paramaterization of the von B function
data(SpotVA1)
fit1 <- nls(tl~vb1(age,Linf,K,t0),data=SpotVA1,start=vbStarts(tl~age,data=SpotVA1))
summary(fit1,correlation=TRUE)
plot(tl~age,data=SpotVA1,pch=19)
curve(vb1(x,Linf=coef(fit1)[1],K=coef(fit1)[2],t0=coef(fit1)[3]),from=0,to=5,
 col="red",lwd=10,add=TRUE)

# Fitting the Francis paramaterization of the von B function
fit2 <- nls(tl~vb2(age,L1,L2,L3,t1=0,t3=5),data=SpotVA1,
 start=vbStarts(tl~age,data=SpotVA1,type="Francis",tFrancis=c(0,5)))
summary(fit2,correlation=TRUE)
## showing how the coefficients and t values can be sent to the first arguments
curve(vb2(x,L1=coef(fit2),t1=c(0,5)),from=0,to=5,col="blue",lwd=5,add=TRUE)

# Fitting the Schnute parameterization of the von B function
vb3 <- vbFuns("Schnute")
fit3 <- nls(tl~vb3(age,L1,L2,K,t1=0,t2=4),data=SpotVA1,
 start=vbStarts(tl~age,data=SpotVA1,type="Schnute"))
summary(fit3,correlation=TRUE)
curve(vb3(x,L1=coef(fit3),t1=c(0,4)),from=0,to=5,col="green",lwd=2,add=TRUE)
}
\references{
Fabens, A. 1965. Properties and fitting of the von
Bertalanffy growth curve. Growth 29:265-289.

Francis, R.I.C.C.  1988.  Are growth parameters estimated
from tagging and age-length data comparable?  Canadian
Journal of Fisheries and Aquatic Sciences, 45:936-942.

Gallucci, V.F. and T.J. Quinn II. 1979.  Reparameterizing,
fitting, and testing a simple growth model.  Transactions
of the American Fisheries Society, 108:14-25.

Mooij, W.M., J.M. Van Rooij, and S. Wijnhoven.  1999.
Analysis and comparison of fish growth from small samples
of length-at-age data: Detection of sexual dimorphism in
Eurasian perch as an example.  Transactions of the American
Fisheries Society 128:483-490.

Polacheck, T., J.P. Eveson, and G.M. Laslett.  2004.
Increase in growth rates of southern bluefin tuna
(\emph{Thunnus maccoyii}) over four decades: 1960 to 2000.
Canadian Journal of Fisheries and Aquatic Sciences,
61:307-322.

Schnute, J.  1981.  A versatile growth model with
statistically stable parameters. Canadian Journal of
Fisheries and Aquatic Sciences, 38:1128-1140.

Somers, I. F. 1988. On a seasonally oscillating growth
function. Fishbyte 6(1):8-11.

Wang, Y.-G.  1998.  An improved Fabens method for
estimation of growth parameters in the von Bertalanffy
model with individual asymptotes.  Canadian Journal of
Fisheries and Aquatic Sciences 55:397-400.
}
\seealso{
\code{\link{vbComp}} and \code{\link{growthModelSim}}.
}
\keyword{manip}

