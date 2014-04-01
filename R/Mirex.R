#'Mirex concentration, weight, capture year, and species of Lake Ontario salmon.
#'
#'Mirex concentration, weight, capture year, and species of Lake Ontario coho
#'and chinook salmon.
#'
#'The \code{year} variable should be converted to a factor as shown in the example.
#'
#'@name Mirex
#'@docType data
#'@format A data frame with 122 observations on the following 4 variables.
#'\describe{ \item{year}{a numeric vector of capture years.}
#'\item{weight}{a numeric vector of salmon weights (kg).}
#'\item{mirex}{a numeric vector of mirex concentration in the salmon
#'tissue (mg/kg).} \item{species}{a factor with levels \code{chinook}
#'\code{coho}.} }
#'@source actual data from Makarewicz, J.C., E.Damaske, T.W. Lewis, and M.
#'Merner.  2003.  Trend analysis reveals a recent reduction in mirex
#'concentrations in coho (\emph{Oncorhynchus kistuch}) and chinook (\emph{O.
#'tshawytscha}) salmon from Lake Ontario.  Environmental Science and
#'Technology, 37:1521-1527.
#'@keywords datasets
#'@examples
#'data(Mirex)
#'Mirex$year <- factor(Mirex$year)
#'lm1 <- lm(mirex~weight*year*species,data=Mirex)
#'anova(lm1)
#'
NULL
