#'Capture histories (3 samples) of cutthroat trout from Auke Lake.
#'
#'The capture histories of cutthroat trout (\emph{Oncorhynchus clarki}) in Auke
#'Lake, Alaska, from samples taken in 1998, 1999, and 2000.
#'
#'@name CutthroatAL
#'@docType data
#'@format A data frame with 517 observations on the following 3 variables.
#'\describe{ \item{ID}{Unique identification numbers for each fish.}
#'\item{first}{Indicator variable for whether the fish was captured in the
#'first sample (\code{1}=captured).} \item{second}{Indicator variable for
#'whether the fish was captured in the second sample (\code{1}=captured).}
#'\item{third}{Indicator variable for whether the fish was captured in the
#'third sample (\code{1}=captured).} }
#'@note The total number of captured fish in Table A.4 of the source is
#'incorrect given the capture histories shown in Table A.5.
#'@section Topic(s): \itemize{ \item Population size \item Abundance \item
#'Mark-recapture \item Jolly-Seber method \item Capture history }
#'@source Simulated from Appendix A.5 of Lum, J.L., J.D. Jones, and S.G.
#'Taylor. 2001.  Dolly varden and cutthroat trout population in Auke Lake,
#'Southeast Alaska, during 2000.  Alaska Department of Fish and Game Fisheries
#'Data Series 01-33.
#'@keywords datasets
#'@examples
#'data(CutthroatAL)
#'str(CutthroatAL)
#'head(CutthroatAL)
#'
NULL
