\name{ageKeyPlot}
\alias{ageKeyPlot}
\title{Plots to visualize age-length keys.}
\usage{
ageKeyPlot(key, type = c("barplot", "area", "lines", "splines", "bubble"),
  xlab = "Length", ylab = "Proportion", showLegend = FALSE,
  lbl.cex = 1.25, leg.cex = 1, lwd = 2, span = 0.25, grid = TRUE,
  col = "gray80", buf = 0.45, bubble.ylab = "Age", add = FALSE,
  xlim = NULL, ylim = NULL, ...)
}
\arguments{
  \item{key}{A numeric matrix containing the age-length
  key.}

  \item{type}{A character indicating the type of plot to
  construct.  See examples.}

  \item{xlab}{A character label for the x-axis.}

  \item{ylab}{A character label for the y-axis in all plots
  but when \code{type="bubble"} (see \code{bubble.ylab=)}.}

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

  \item{lwd}{A numeric that indicates the line width for
  when \code{type="lines"} or \code{type="splines"}.}

  \item{span}{A numeric that indicates the span value to
  use in \code{loess} when \code{type="splines"}.}

  \item{grid}{A logical that indicates whether a grid
  should be placed under the bubbles when
  \code{type="bubble"} or a character or appropriate vector
  that identifies a color for the grid.  See examples.}

  \item{col}{A string indicating the color of the bubbles
  when \code{type="bubble"}.}

  \item{buf}{A single numeric that indicates the relative
  width of the bubbles when \code{type="bubble"}. A value
  of 0.5 would mean that two full-width bubbles would touch
  either in the x- or y-direction (i.e., this would
  represent half of the minimum of the physical distance
  between values one-unit apart on the x- and y-axes).  Set
  this to a value less than 0.5 so that the bubble do not
  touch (the default is 0.45).}

  \item{bubble.ylab}{A string for labeling the y-axis when
  \code{type="bubble"}.}

  \item{add}{A logical indicating whether the data should
  be adding to an already existing plot.  May be useful for
  visually comparing age-length keys.  Only implemented
  when \code{type="bubble"}.}

  \item{xlim}{A numeric of length 2 that provide the limits
  for the x-axis.}

  \item{ylim}{A numeric of length 2 that provide the limits
  for the y-axis.}

  \item{\dots}{Additional arguments to pass to \code{plot},
  \code{barplot}, or \code{stackpoly}.}
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
ageKeyPlot(WR.key,"bubble")
ageKeyPlot(WR.key,"bubble",grid=FALSE)
ageKeyPlot(WR.key,"bubble",grid="blue")
ageKeyPlot(WR.key,"bubble",grid=rgb(0,0,0,0.2),col=rgb(0,0,0,0.5))
}
\seealso{
\code{\link{ageKey}} and \code{\link{ageKeyPrep}}.
}
\keyword{plot}

