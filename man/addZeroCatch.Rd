\name{addZeroCatch}
\alias{addZeroCatch}
\title{Adds zero catch values for sampling events and species that were not captured.}
\usage{
addZeroCatch(df, eventvar, specvar, idvar = NULL, zerovar = NULL)
}
\arguments{
  \item{df}{A data frame that contains the capture summary
  data as described in the details.}

  \item{eventvar}{A string for the variable that identifies
  unique capture events.}

  \item{specvar}{A string for the variable that identifies
  the species captured.}

  \item{idvar}{A string or vector of strings for the
  variable(s) that are common to a unique capture event and
  should be repeated for each zero row added to the data
  frame (e.g., dates, gear type, etc.).  See details}

  \item{zerovar}{A string or vector of strings for the
  variable(s) that should be set equal to zero.  See
  details.}
}
\value{
Returns a data frame of the same type as \code{df} but with
rows of zero observation data appended and the columns
slightly reorganized.
}
\description{
Adds zero catch values for species that were not captured
in a sampling event but were captured in at least one other
sampling event (i.e., adds zeroes to the data frame for
capture events where a species was not observed).
}
\details{
The data frame in \code{df} must contain a column that
identifies a unique capture event (given to
\code{eventvar}), a column with the name of the species
captured (given to \code{specvar}), and a column that
contains the number of that species captured (potentially
given to \code{zerovar}; see details). This function finds
all event and species combinations where catch information
does not exist and then creates a new data frame that
contains a zero for the catch for all of these
combinations.  This new data frame is then appended to the
original data frame to construct a data frame that has
complete catch observation data in it -- i.e., including
zeroes for species that were not captured in some events.

The original data frame (\code{df}) may also contain
columns of data not discussed above.  For example, it may
include columns of data that are specific to the capture
event, such as date, gear type, habitat, etc.  Information
in these variables should be repeated specific to the
sampling event when the zero catch is added to the data
frame.  Thus, the variable names for these data can be
given to \code{idvar} so that these data can be repeated
with each zero added to the data frame.  In addition, the
data frame may contain other information related to the
catch, such as number of recaptured fish, number of fish
released, etc.  These variables should also have zeroes for
each species not caught in a sampling event (e.g., if the
catch of the species is zero, then the number of recaptures
must also be zero).  Thus, the variable names for this
information should be given to \code{zerovar} so that
zeroes can be added to these variables as well.  Only one
of \code{idvar} or \code{zerovar} needs to be given as the
function will assume that the rest of variable names in the
data frame (not including those given in \code{eventvar}
and \code{specvar}) will be of the other type.
}
\examples{
df <- data.frame(net=c(1,1,1,2,2,3),
                eff=c(1,1,1,1,1,1),
                species=c("BKT","LKT","RBT","BKT","LKT","RBT"),
                catch=c(3,4,5,5,4,3))
df

# example with ancillary data specific to the net number (e.g., effort)
addZeroCatch(df,"net","species",zerovar="catch")
# example with no ancillary data specific to the net number
addZeroCatch(df[,-2],"net","species",zerovar="catch")
}
\author{
Derek H. Ogle, \email{dogle@northland.edu}
}
\keyword{manip}

