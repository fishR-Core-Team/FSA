#'Expands a length frequency based on a subsample.
#'
#'Creates a list of lengths for the individuals not measured based on the
#'lengths measured in a subsample of individuals.
#'
#'This function can take either a vector of observed lengths or a data.frame in
#'which the variable containing the lengths is identified.  If a vector is
#'given in \code{df} then \code{cl} must be set to \code{NULL}.  If a
#'data.frame is given in \code{df} then \code{cl} must contain a valid name or
#'number of the column containing the observed lengths.
#'
#'The function will create length categories that begin with the value in
#'\code{startcat} and continue by values of \code{w} until a category value
#'greater than the largest observed length in \code{df}.  The code does not
#'allow categories of different widths.  See \code{\link{lencat}}.
#'
#'The resulting lengths are created by allocating individuals to each length
#'class based on the proportion of measured individuals in the subsample in
#'that length class.  Individuals within a length class are then assigned a
#'specific length within that length class based on a uniform distribution.
#'Because the expanded number of individuals in a length class is rounded down
#'based on the measured number per length class not all individuals will be
#'assigned a length value initially.  The remaining individuals are assigned to
#'a length class randomly according to weights based on the proportion of
#'individuals in the measured length classes.  Finally, these individuals are
#'assigned a specific length within the respective length class in the same
#'manner as above.
#'
#'The resulting length assignments are rounded to the number of decimals shown
#'in \code{decimal}.  If \code{decimals} is not set by the user then it will
#'default to the same number of decimals shown in the \code{w} value.  Care is
#'taken to make sure that the rounded result will not pass out of the given
#'length category (i.e., will not be allowed to round up to the next length
#'category).  Generally speaking, one will want to use more decimals then is
#'shown in \code{w}.  For example, one may want to create length categories
#'with a width of 1 inch (i.e., \code{w=1}) but have the results printed as if
#'measured to within 0.1 inch (i.e., \code{decimals=1}).
#'
#'@param x A numeric vector of length measurements.
#'@param startcat A number indicating the beginning of the first length-class.
#'@param w A number indicating the width of length classes to create.
#'@param additional The number of individuals that were not measured in the
#'sample (for which measurements should be determined).
#'@param total The total number of individuals in the sample (including those
#'that were measured in the subsample).
#'@param decimals A number indicating the number of decimals used in the output
#'vector of estimated lengths.
#'@param show.summary A logical indicating whether a summary of the process should
#'be shown at the end.
#'@param \dots Optional arguments to be passed to \code{\link{lencat}}.
#'@return Returns a vector that consists of measurements for the non-measured
#'individuals in the entire sample.
#'@seealso \code{\link{lencat}}.
#'@export
#'@keywords manip
#'@examples
#'## First example
#'# random lengths measured to nearest 0.1 unit -- values in a vector
#'len1 <- round(runif(50,0.1,9.9),1)
#'# assignment of integer lengths to 110 non-measured indivs
#'( new.len1a <- lenFreqExpand(len1,w=1,total=160) )
#'# assignment of lengths to 0.1 to 110 non-measured indivs
#'( new.len1b <- lenFreqExpand(len1,w=1,total=160,decimals=1) )
#'
#'## Second example -- if values are in a data.frame
#'# random lengths measured to nearest 0.1 unit
#'len2 <- data.frame(len=round(runif(50,10,117),1))
#'# assignment of lengths to 0.1 for 140 non-measured indivs
#'( new.len2a <- lenFreqExpand(len2$len,w=10,total=190,decimals=1) )
#'
#'## Third example
#'# hypothetically measured lengths
#'len <- c(6.7,6.9,7.3,7.4,7.5,8.2,8.7,8.9)
#'# find lengths for unmeasured fish assuming a total of 30
#'( newlen1 <- lenFreqExpand(len,w=0.5,total=30,decimals=1) )
#'# set a starting category
#'( newlen2 <- lenFreqExpand(len,w=0.5,startcat=6.2,total=30,decimals=1) )
#'
lenFreqExpand <- function(x,w,additional,startcat=NULL,total=additional+nrow(df),decimals=decs$wdec,show.summary=TRUE,...) {
  if (!is.vector(x)) stop("'x' must be a vector.",call.=FALSE)
  if (!is.numeric(x)) stop("'x' must be numeric.",call.=FALSE)
  # Find startcat if it is NULL
  if (is.null(startcat)) startcat <- floor(min(x,na.rm=TRUE)/w)*w
  # Find decimals in w and startcat, the decimals in w will be the default to round to
  decs <- checkStartcatW(startcat,w,x)
  if (total<length(x)) stop("\n Total number to expand to must be greater than number of fish supplied in 'df'.",call.=FALSE)
  # number to allocate
  num <- total-length(x)
  # find the length frequency of measured fish
  lcat <- lencat(x,w=w,startcat=startcat,...)
  lenfreq <- prop.table(table(lcat))
  # length frequency categories (lower limit of bin)
  cats <- as.numeric(rownames(lenfreq))
  # number of expanded individuals per length category
  reps <- floor(num*lenfreq)
  # expansion of lengths according to values in reps
  nrand.lens <- rep(cats,reps)
  # randomly allocate rest of indivs based on probabilities in length category
  rand.lens <- sample(cats,num-sum(reps),replace=TRUE,prob=lenfreq)
  # put expanded and randomly allocated indivs into one vector
  new.lens <- c(nrand.lens,rand.lens)
  # make sure that a length can't cross out of length category
  maxval <- w-1/(10^decimals)
  if (maxval>0) new.lens <- new.lens + runif(length(new.lens),min=0,max=maxval)
  new.lens <- round(new.lens,decimals)
  # if asked, print some summary values of what happened
  if (show.summary) {
    cat("Length Frequency Expansion using:\n")
    cat("Measured length frequency of",dim(df)[1],"individuals:")
    print(lenfreq)
    cat("\nNon-random allocations of",length(nrand.lens),"individuals by length category.")
    print(reps)
    cat("\nRandom allocations of",length(rand.lens),"individuals\n")
    cat("\nWith final length frequency table of:")
    final.lens <- lencat(new.lens,w=w,startcat=startcat,...)
    print(table(final.lens))
  }
  # return sorted vector of newly assigned lengths
  sort(new.lens)
}
