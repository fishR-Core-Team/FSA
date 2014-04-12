\name{ageKeyPlot}
\alias{ageKeyPlot}
\title{Plots to visualize age-length keys.}
\usage{
ageKeyPlot(key, type = c("barplot", "area", "lines", "splines"),
  xlab = "Length", ylab = "Proportion", showLegend = FALSE,
  lbl.cex = 1.25, leg.cex = 1, lwd = 2, span = 0.25)
}
\arguments{
  \item{key}{A numeric matrix containing the age-length
  key.}

  \item{type}{A character indicating the type of plot to
  construct.  See examples.}

  \item{xlab}{A character label for the x-axis.}

  \item{ylab}{A character label for the y-axis.}

  \item{showLegend}{A logical indicating whether a legend
  should be displayed for when \code{type="barplot"} and
  \code{type="area"}.  See examples.}

  \item{lbl.cex}{A numeric character expansion value for
  labels inside the bars when \code{type="barplot"} and
  \code{showLegend=FALSE} or on the lines when
  \code{type="lines"} or \code{type="splines"}.}

  \item{leg.cex}{A numeric character expansion value for
  labels on the legend when \code{type="barplot"} or
  \code{type="area"} and \code{showLegend=TRUE}.}

  \item{lwd}{A numeric indicating the line width for when
  \code{type="lines"} or \code{type="splines"}.}

  \item{span}{A numerica indicating the span value to use
  in \code{loess} when \code{type="splines"}.}
}
\value{
None, but a plot is constructed.
}
\description{
Various plots to visualize the proportion of fish of
certain ages within length categories in an age-length key.
}
\details{
This is a first draft of this function.  Please send
suggestions to the author.
}
\section{fishR vignette}{
  \url{https://sites.google.com/site/fishrfiles/gnrl/AgeLengthKey.pdf}
}
\examples{
## Get data with length measurements and some assigned ages -- same as in ageKey()
data(WR79)
# get the age sample from the data frame
WR.age <- Subset(WR79, !is.na(age))
# add length categories
WR.age.mod <- lencat(~len,data=WR.age,startcat=35,w=5,drop.levels=TRUE)
# create age-length key
raw <- table(WR.age.mod$LCat, WR.age.mod$age)
( WR.key <- prop.table(raw, margin=1) )
ageKeyPlot(WR.key,"barplot")
ageKeyPlot(WR.key,"barplot",showLegend=TRUE)
ageKeyPlot(WR.key,"area",showLegend=TRUE)
ageKeyPlot(WR.key,"splines")
ageKeyPlot(WR.key,"lines")
}
\seealso{
\code{\link{ageKey}} and \code{\link{ageKeyPrep}}.
}
\keyword{plot}

