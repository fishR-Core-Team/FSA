#' Prepares problematic age-length key matrices for use.
#' 
#' Used to prepare problematic age-length key matrices for use in \code{\link{ageKey}}.  Problems that can be fixed are the deletion of empty rows at the beginning or at the end of the age-length key matrix and the \sQuote{interpolation} of values in missing rows in the \sQuote{middle} of the age-length key.
#' 
#' This function can be used to prepare problematic age-length key matrices for use in \code{\link{ageKey}}.  Problems that can be fixed are the deletion of empty rows at the beginning or at the end of the age-length key matrix and the \sQuote{interpolation} of values in missing rows in the \sQuote{middle} of the age-length key.  In the case of interpolation, a cell in the missing row is computed by assuming a linear trend between the immediately adjacent rows with data both above and below the missing row.  This results in simply averaging the values in the adjacent rows if only one row is missing in the age-length key matrix.
#' 
#' Missing values can also be predicted using general linear or additive models as described in the references.
#' 
#' @param key A numeric matrix that contains the problematic age-length key.
#' @param show.msgs A logicical that indicates if process messages should be displayed (\code{TRUE}; default).
#' 
#' @return A matrix that contains the original age-length key matrix sent in \code{key} with missing rows either deleted if they were at the beginning or end of the matrix or replaced with interpolated values (see details) if in the middle of the matrix.
#' 
#'@author Derek H. Ogle, \email{dogle@@northland.edu} 
#'
#'@references 
#'Berg, C.W. and K. Kristensen.  2012.  Spatial age-length key modelling using continuation ratio logits.  Fisheries Research 129-130:119-126. \url{http://orbit.dtu.dk/fedora/objects/orbit:113964/datastreams/file_10214559/content}
#'
#'Gerritsen, H.D., D. McGrath, and C. Lordan.  2006.  A simple method for comparing age-length keys reveals significant regional differences within a single stock of haddock (\emph{Melanogrammus aeglefinus}).  ICSE Journal of Marine Science 63:1096-1100.  \url{http://icesjms.oxfordjournals.org/content/63/6/1096.full}
#'
#'Stari, T., K.F. Preedy, E. McKenzie, W.S.C. Gurney, M.R. Heath, P.A. Kunzlik, D.C. Speirs.  2010.  Smooth age length keys: Observations and implications for data collection on North Sea haddock.  Fisheries Research 105:2-12.  \url{http://www.sciencedirect.com/science/article/pii/S0165783610000512}.
#'
#'@seealso See \code{\link{ageKey}} for using an age-length key to assign ages to individual fish and \code{\link{ageKeyPlot}} to visualize age-length keys.
#'
#'Also see functions in the DATRAS package (avaiable at \url{http://www.rforge.net/DATRAS/index.html}).
#'
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/AgeLengthKey.pdf}
#'
#'@export
#'
#'@keywords manip
#'
#'@examples
#'# create a "good" small ALK matrix
#'alk <- matrix(c(0.4,0.3,0.3,0.0,
#'0.2,0.4,0.3,0.1,
#'0.1,0.2,0.4,0.3,
#'0.0,0.1,0.4,0.5,
#'0.0,0.0,0.2,0.8),
#'nrow=5,byrow=TRUE)
#'rownames(alk) <- c(10,20,30,40,50)
#'colnames(alk) <- c(2,3,4,5)
#'addmargins(alk,margin=2)
#'
#'# Create various "problematic" ALK matrics
#'alk1 <- alk2 <- alk5 <- alk24 <- alk23 <- alk12 <- alk45 <- alk135 <- alk
#'alk1[1,] <- NA
#'alk2[2,] <- NA
#'alk5[5,] <- NA
#'alk24[c(2,4),] <- NA
#'alk23[c(2,3),] <- NA
#'alk12[1:2,] <- NA
#'alk45[4:5,] <- NA
#'alk135[c(1,3,5),] <- NA
#'
#'# Show how the "problematic" matrices are "fixed"
#'alk1
#'ageKeyPrep(alk1)
#'alk5
#'ageKeyPrep(alk5)
#'alk2
#'ageKeyPrep(alk2)
#'alk24
#'ageKeyPrep(alk24)
#'alk23
#'ageKeyPrep(alk23)
#'alk12
#'ageKeyPrep(alk12)
#'alk45
#'ageKeyPrep(alk45)
#'alk135
#'ageKeyPrep(alk135)
#'
ageKeyPrep <- function(key,show.msgs=TRUE) {
  findMissingRows <- function(tmp) {
    rs <- rowSums(tmp)
    is.na(rs) | rs==0    
  } # end internal
  ## Begin Main Function
  # determine which rows are missing
  miss <- findMissingRows(key)
  # kick out if none are missing
  if (!any(miss)) warning("Ages were found for all lengths",call.=FALSE)
  else { # at least one length did not have any ages
    # if missing rows are initial rows, then delete those rows
    if (miss[1]) {
      if (show.msgs) message("Deleting some empty rows at the beginning of the key.")
      tmp <- which(!miss)[1]            # first non-missing row
      tmp <- tmp-1                      # last missing row at front end
      key <- key[-c(1:tmp),]            # delete first rows
      miss <- findMissingRows(key)      # re-find missing rows
    }
  }
  if (any(miss)) { # don't continue if no more missing rows
    # if missing rows are ending rows, then delete those rows
    if (miss[length(miss)]) {
      if (show.msgs) message("Deleting some empty rows at the end of the key.")
      tmp <- which(!miss)               # non-missing rows
      tmp <- tmp[length(tmp)]           # last non-missing row
      tmp <- tmp+1                      # first missing row at back end
      key <- key[-c(tmp:length(miss)),] # delete last rows
      miss <- findMissingRows(key)      # re-find missing rows
    }
  }
  if (any(miss)) { # don't continue if no more missing rows
    cmiss <- which(miss)
    cnmiss <- which(!miss)
    for (i in cmiss) {
      if (show.msgs) message(paste("Interpolating values for the",rownames(key)[i],"length row."))
      # Find closest rows that have age data
      tmp.ind <- findInterval(i,cnmiss)
      lwr.row <- cnmiss[tmp.ind]
      upr.row <- cnmiss[tmp.ind+1]
      # Interpolate the missing row
      for (j in 1:ncol(key)) {
        slp <- (key[lwr.row,j]-key[upr.row,j])/(lwr.row-upr.row)
        key[i,j] <- key[lwr.row,j]-slp*(lwr.row-i)
      }
      # Assure that the row sums to 1
      key[i,] <- key[i,]/sum(key[i,])
    }
  }
  key
}
