#' @title Convert a "directional" position to "adj" values for labeling a point.
#'
#' @description Convert a \dQuote{directional} (e.g., \sQuote{NE}, \sQuote{S}, etc.) position to \dQuote{adj} values for labeling a point.
#'
#' @param pos A string that indicates a direction to place the label relative to a set of coordinates.
#' @param offset A value that controls how far the label will be from the point (0=label starts on point).
#'
#' @return A vector of length two that contains the x- and y-coordinate adjustment values such that a label will be placed according to the \dQuote{directional} position.  The results from this function would be set equal to the \code{adj=} argument in \code{text()}.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{text}.
#'
#' @keywords manip
#'
#' @examples
#' ## Make a dummy plot
#' plot(c(0.75,2,3.25),c(2,2,2),xlim=c(0,4),ylim=c(1,3),pch=16,xlab="x",ylab="y")
#'
#' ## Add some labels
#' text(2,2,"center",adj=pos2adj())
#' text(0.75,2,"north",adj=pos2adj("N"))
#' text(0.75,2,"south",adj=pos2adj("S"))
#' text(0.75,2,"east",adj=pos2adj("E"))
#' text(0.75,2,"west",adj=pos2adj("W"))
#' text(3.25,2,"NE",adj=pos2adj("NE"))
#' text(3.25,2,"NW",adj=pos2adj("NW"))
#' text(3.25,2,"SE",adj=pos2adj("SE"))
#' text(3.25,2,"SW",adj=pos2adj("SW"))
#'
#' @export
pos2adj <- function(pos=c("center","S","W","N","East","SW","NW","NE","SE",
                          "s","w","n","east","sw","nw","ne","se"),offset=0.5) {
  # spelled out East to attempt to avoid warning on RCMD check, but sill allow matching to "E"
  N <- S <- W <- NE <- NW <- SE <- SW <- NULL  # attempting to avoid bindings warning on RCMD CHECK
  pos <- match.arg(pos)
  switch(pos,
    center = { adj <- c(0.5,0.5) },
    N =, n = { adj <- c(0.5,0-offset) },
    S =, s = { adj <- c(0.5,1+offset) },
    East =, east = { adj <- c(0-offset,0.5) },
    W =, w = { adj <- c(1+offset,0.5) },
    NE=, ne = { adj <- c(0-offset,0-offset) },
    NW=, nw = { adj <- c(1+offset,0-offset) },
    SW=, sw = { adj <- c(1+offset,1+offset) },
    SE=, se = { adj <- c(0-offset,1+offset) }
  )
  adj
}
