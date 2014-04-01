#'Summarize individual fish capture histories.
#'
#'Summarizes the capture histories of individual fish.  Results may be used in
#'the Lincoln-Petersent, Schnabel, Schumacher-Eschmeyer, or Jolly-Seber methods
#'for estimating population abundance.
#'
#'If the data.frame in \code{df} contains columns with non-capture history
#'information (e.g., fish ID, length, location, etc.) then the \code{cols} argument
#'should be used to identify which columns contain only the capture history
#'information.  Columns can be included by listing the column numbers (e.g., 
#'columns 2 through 7 could be included with \code{cols=2:7}).  Columns can be
#'excluded by including the column number preceded by a \dQuote{negative} sign
#'(e.g., columns 1 through 3 can be excluded with \code{cols=-(1:3)}.
#'
#'@param df A data.frame that contains the capture histories (and, perhaps, other
#'information).  These capture histories must be in \sQuote{FSA} format (see
#'\code{\link{capHistConvert}}).  See details.
#'@param cols A numerical vector of columns that contain the capture histories.
#'See details.
#'@return A list is returned with the following two components if only two
#'samples are provided.
#'\itemize{
#'\item \code{caphist} A vector summarizing the frequency of fish with each capture history.
#'\item \code{sum} A data frame that contains the number of marked fish from the first 
#'sample (\code{M}), the the number of captured fish in the second sample (\code{n}),
#'and the number of recaptured (i.e. previously marked) fish in the second sample
#'(\code{m}).
#'}
#'
#'However, if more than two samples are provided then a list with the following
#'four components is provided:
#'\itemize{
#'\item \code{caphist} A vector summarizing the frequency of fish with each capture history.
#'\item \code{sum} A data frame that contains the the number of captured fish in the ith sample
#'(\code{n}), the number of recaptured (i.e. previously marked) fish in the ith sample
#'(\code{m}), the number of marked fish returned to the population following the
#'ith sample (\code{R}; this will equal \code{n} as the function currently does
#'not handle mortalities); and the number of marked fish in the population prior
#'to the ith sample (\code{M}).
#'\item \code{methodB.top} A matrix that contains the top of the Method B table used for
#'the Jolly-Seber method (i.e., a contingency table of capture sample (columns)
#'and last seen sample (rows)).
#'\item \code{methodB.bot} A data.frame that contains the bottom of the Method B
#'table used for the Jolly-Seber method (i.e., the number of marked fish in the
#'sample (\code{m}), the number of unmarked fish in the sample (\code{u}), the
#'total number of fish in the sample (\code{n}), and the number of marked fish
#'returned to the population following the sample (\code{R}).
#'}
#'@note This function assumes that all unmarked captured fish are marked and
#'returned to the population (i.e., no losses at the time of marking are
#'allowed).
#'@seealso \code{\link{mrClosed}}, \code{\link{mrOpen}}, \code{\link{capHistConvert}}
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/MRClosed.pdf}, 
#'\url{https://sites.google.com/site/fishrfiles/gnrl/MROpen.pdf}
#'@export
#'@keywords manip
#'@examples
#'# data.frame with IDs in the first column
#'data(PikeNYPartial1)
#'capHistSum(PikeNYPartial1,cols=-1)
#'
#'# An examle with only two sample events (for demonstration only)
#'capHistSum(PikeNYPartial1,cols=-c(1,4:5))
#'
capHistSum <- function(df,cols=NULL) {
  sum.caphist <- function(ch) {                                 # Frequency of fish by unique capture history
    ch.lbl <- apply(ch,1,paste,collapse="")                     # Concatenate 0,1s to make a composite label
    table(as.factor(ch.lbl))                                    # Send back the frequency table by capture history
  }  # end internal sum.caphist

  sum.num <- function(ch,k) {                                   # Find number of marks, recaps, total catch by sample
    n <- apply(ch,2,sum)                                        # Total number (catch) in each sample
    m <- rep(0,k)                                               # Initiate number of marks
    chp <- ch[,1]                                               # Store first sample in a temp vector
    for (i in 2:k) {                                            # Cycle through remaining samples
      st <- table(chp,ch[,i])                                   #   Cross-tab previous with ith samples
      if (any(rownames(st)=="1") & any(colnames(st)=="1")) {    #   Marked fish seen in both samples (i.e., 1,1 group)
        m[i] <- st["1","1"]
      }
      chp <- as.numeric(chp+ch[,i]>0)                           #   Add ith sample to previous samples, 1s indicate previously caught
    }
    u <- n-m                                                    # Unmarked fish
    data.frame(n,m,u)                                           # Return results as a data.frame
  }  # end internal sum.num

  sum.num.petersen <- function(cht) {
    df <- data.frame(M=cht["10"]+cht["11"],
                     n=cht["01"]+cht["11"],
                     m=cht["11"])
    rownames(df) <- NULL
    df
  }  # end internal sum.num.petersen
        
  sum.num.schnabel <- function(df,k) {                          # Modify mark, recap, catch data to a Schnabel summary
    R <- df$n                                                   # Unmarked fish returned to the population after sample
    R[k] <- 0                                                   #   0 by default for last sample
    M <- c(0,cumsum(df$n-df$m)[1:(k-1)])                        # Marked fish prior to sample
    data.frame(n=df$n,m=df$m,R,M)                               # Return results as a data.frame
  }  # end internal sum.num.schnabel
    
  sum.num.jolly <- function(df,k) {                             # Modify mark, recap, catch data to a Jolly (bottom Method B) summary
    R <- df$n                                                   # Unmarked fish returned to the population after sample
    R[k] <- 0                                                   #   0 by default for last sample
    t(data.frame(m=df$m,u=df$u,n=df$n,R=R))                     # Essentially a re-ordering
  }  # end internal sum.num.jolly
    
  methodB.top <- function(ch,k) {
    # create a matrix to hold the Method B table (NA on lower triangle, 0 on upper)
    mb.top <- matrix(nrow=k,ncol=k)
    mb.top[upper.tri(mb.top)] <- 0
    # Loop through each possible recapture time
    for (i in 2:k) {
      # Reduce matrix to only fish sampled at time i (i.e., all 1s in ith column)
      ch.tmp.i <- ch[ch[,i]==1,,drop=FALSE]
      # Need to look for previous capture times
      for (j in (i-1):1) {
        # Find the matrix to fish sampled at both times i and j, but not between i and j
        #   i.e., all 1s in columns i and j
        ch.tmp.j <- ch.tmp.i[ch.tmp.i[,j]==1,,drop=FALSE]
        mb.top[j,i] <- nrow(ch.tmp.j)
        # Keep the rows where fish were not sampled on jth sample
        ch.tmp.i <- ch.tmp.i[ch.tmp.i[,j]==0,,drop=FALSE]
        # if no rows are left then nothing left to do
        if (nrow(ch.tmp.i)==0) break
      } # end j loop
    } # end i loop
    mb.top
  }  # end internal methodB.top
  # start of main capHistSum function  
  ifelse(is.null(cols),ch <- df, ch <- df[,cols])    
  k <- dim(ch)[2]                                                 # Number of samples
  res1 <- sum.caphist(ch)                                         # Capture history frequencies
  res2 <- sum.num(ch,k)                                           # Frequency of marked and unmarked fish in each sample
  if (k==2) {
    d <- list(caphist=res1,sum=sum.num.petersen(res1))
  } else {
    res2.schnabel <- sum.num.schnabel(res2,k)                     # Construct schnabel summary from marked & unmarked
    res2.jolly <- sum.num.jolly(res2,k)                           # Construct jolly bottom method B table from marked & unmarked
    res3 <- methodB.top(ch,k)                                     # Construct jolly top method B table
    d <- list(caphist=res1,sum=res2.schnabel,methodB.top=res3,methodB.bot=res2.jolly)
  }
  class(d) <- "CapHist"
  d
}
