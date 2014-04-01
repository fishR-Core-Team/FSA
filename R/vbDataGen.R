#'Randomly create a von Bertalanffy data set.
#'
#'Randomly creates a data set according to a typical von Bertalanffy model
#'following the methodology of Vigliola and Meekan (2009).
#'
#'This function follows the simulation methodology shown in Box 2 of Vigliola
#'and Meekan (2009) modified for the typical von Bertalanffy model (rather than
#'the exponential model illustrated in that Box).
#'
#'The results can be returned in one of four formats.  In \sQuote{wide} format
#'the data frame will contain a column for the fish's ID (\code{id}), simulated
#'age-at-capture (\code{agecap}), and simulated lengths at previous ages
#'(\code{anuX}).  In \sQuote{long} format the same data will be returned in a
#'data frame with a column for fish's ID, age-at-capture, age (\code{age}) and
#'length (\code{anu}) in the fish's history.  In this format the data for an
#'individual fish will be found in as many rows as the age of the fish.  Thus,
#'the data for a five-year-old fish will appear in five columns corresponding
#'to the ages and lengths at each of the fish's five years of life.  The
#'\sQuote{groupedData} format will look exactly like the long format but is a
#'\code{groupedData()} object (from the \pkg{nlme} package).  Finally, the
#'\sQuote{atCapture} format removes the longitudinal nature of the previous
#'three formats and returns just the fish's ID, age-at-capture, and final
#'length-at-capture.  The final length-at-capture is simply the last
#'\code{anuX} data in the three previous formats.
#'
#'@param n Number of individuals to simulate data for.
#'@param Linf The \sQuote{true} value of the Linf parameter.
#'@param K The \sQuote{true} value of the K parameter.
#'@param t0 The \sQuote{true} value of the t0 parameter.
#'@param minAge The minimum possible age for an individual in the data set.
#'@param maxAge The maximum possible age for an individual in the data set.
#'@param CV The coefficient of variation to be used when adding random error.
#'@param undf The uniform noise division factor for adding random noise to the
#'data.  See Box 2 in Vigliola and Meekan (2009).
#'@param dataType The type of data frame that should be returned.  See details.
#'@return A data frame.  See details.
#'@note This function will produce warnings about the random number generators
#'producing \code{NA}s.  This is a byproduct of the vectorization used in the
#'algorithm but does not cause any problems with the output.
#'@seealso \code{\link{vbComp}}, \code{growthModelSim} in \pkg{FSATeach}, and
#'\code{groupedData} in \pkg{nlme}
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/VonBertalanffy.pdf},
#'\url{https://sites.google.com/site/fishrfiles/gnrl/VonBertalanffyExtra.pdf}
#'@references Vigliola, L. and M.G. Meekan.  2009.  The back-calculation of
#'fish growth from otoliths.  Chapter 6 (pp. 174-211) in B.S. Green et al.
#'(eds.), Tropical Fish Otoliths: Information for Assessment, Management and
#'Ecology, Reviews: Methods and Technologies in Fish Biology and Fisheries 11.
#'@export
#'@keywords manip
#'@examples
#'vbwide <- vbDataGen(10,Linf=30,K=0.2,t0=-0.2)
#'head(vbwide)
#'vblong <- vbDataGen(10,Linf=30,K=0.2,t0=-0.2,dataType="long")
#'head(vblong)
#'vbgrp <- vbDataGen(10,Linf=30,K=0.2,t0=-0.2,dataType="groupedData")
#'head(vbgrp)
#'\dontrun{
#'library(lattice)
#'plot(vbgrp)
#'}
#'vbcap <- vbDataGen(10,Linf=30,K=0.2,t0=-0.2,dataType="atCapture")
#'head(vbcap)
#'
vbDataGen <- function(n,Linf,K,t0,minAge=1,maxAge=9,CV=0.10,undf=5,dataType=c("wide","long","groupedData","atCapture")) {
  # get typical von Bertalanffy model
  LVB <- vbFuns("typical")
  dataType <- match.arg(dataType)
  # suppress warnings about NAs
  options(warn= -1)
  ## assign random ages to each fish
  if (minAge<1) stop("minAge argument must be greater than or equal to 1.",.call=FALSE)
  if (minAge>maxAge) stop("minAge can NOT be greater than maxAge.",.call=FALSE)
  numAges <- maxAge
  ages <- sample(seq(minAge,maxAge),n,replace=TRUE) 
  ## Generate random parameters for each fish
  iLinf <- rnorm(n,Linf,Linf*CV) + runif(n,min=-Linf*(CV/undf),max=Linf*(CV/undf))
  iK <- rnorm(n,K,K*CV) + runif(n,min=-K*(CV/undf),max=K*(CV/undf))
  it0 <- rnorm(n,t0,abs(t0)*CV) + runif(n,min=-abs(t0)*(CV/undf),max=abs(t0)*(CV/undf))
  ## Generate size-at-age data
  # fill out a matrix of past measurements
  L <- matrix(NA,nrow=n,ncol=numAges)
  for (j in 1:n) {
    i <- 1:ages[j]
    if (length(i)<numAges) i <- c(i,rep(NA,numAges-length(i)))
    Ltemp <- LVB(i,iLinf[j],iK[j],it0[j])
    L[j,] <- Ltemp + rnorm(numAges,0,CV*Ltemp) + runif(numAges,-CV/undf*Ltemp,CV/undf*Ltemp)
  }
  # allow warnings to return
  options(warn=0)
  # put together as a data.frame
  d <- data.frame(1:n,ages,L)
  names(d) <- c("id","agecap",paste("anu",1:maxAge,sep=""))
  # How to output result
  if (dataType %in% c("long","groupedData")) d <- gReshape(d,in.pre="anu",val.name="anu")
  if (dataType=="groupedData") d <- groupedData(anu~prvAge|id,data=d,labels=list(x="Age",y="Length"))
  if (dataType=="atCapture") d <- data.frame(id=d$id,age=d$age,len=apply(d[,3:ncol(d)],1,max,na.rm=TRUE))  # finds max value in anuX to be length-at-capture and appends to ID and agecap
  d
}
