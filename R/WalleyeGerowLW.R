#'Raw length-weight data that can be used to compute the Ws for walleye as
#'performed in Gerow (2005).
#'
#'Raw length-weight data from a variety of populations used for computing the
#'standard weight (Ws) equation for walleye (\emph{Sander vitreus vitreus}).
#'
#'@name WalleyeGerowLW
#'@docType data
#'@format A data frame with 34734 observations on the following 3 variables:
#'\describe{
#' \item{popn}{A unique numeric identifier for each separate regression.}
#' \item{len}{Total length (mm) of fish.}
#' \item{wt}{Weight (g) of fish.} 
#'}
#'@section Topic(s): \itemize{
#' \item Relative weight
#' \item Standard weight
#' \item Length-weight
#'}
#'@concept Condition 'Relative Weight' 'Standard Weight' 'Length-Weight'
#'@seealso \code{\link{WSlit}} and \code{\link{wsVal}}, \code{\link{rlp}},
#'\code{\link{emp}}, and \code{\link{wsValidate}}.
#'@source From Ken Gerow.  Summarized in Gerow, K.G., R.C. Anderson-Sprecher,
#'and W.A. Hubert.  2005.  A new method to compute standard-weight equations
#'that reduces length-related bias.  North American Journal of Fisheries
#'Management 25:1288-1300.
#'@keywords datasets
#'@examples
#'data(WalleyeGerowLW)
#'str(WalleyeGerowLW)
#'head(WalleyeGerowLW)
#'## two of many examples
#'op <- par(mfrow=c(1,2),pch=19)
#'plot(wt~len,data=WalleyeGerowLW,subset=popn==1,main="Population #1")
#'plot(wt~len,data=WalleyeGerowLW,subset=popn==10,main="Population #10")
#'par(op)
#'
NULL
