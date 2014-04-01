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
#'@param df A data.frame that (minimally) contains a column of length
#'measurements or a vector of observed lengths.  See details.
#'@param cl A number or name indicating which column the length measurements
#'occupy in the data.frame, \code{df}.  Set to \code{NULL} if \code{df} is a vector.
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
#'# random lengths measured to nearest 0.1 unit
#'len1 <- round(runif(50,0.1,9.9),1)
#'
#'# assignment of integer lengths to 110 non-measured indivs
#'new.len1a <- lenFreqExpand(len1,startcat=0,w=1,total=160)
#'new.len1a
#'
#'# assignment of lengths to 0.1 to 110 non-measured indivs
#'new.len1b <- lenFreqExpand(len1,startcat=0,w=1,total=160,decimals=1)
#'new.len1b
#'
#'
#'## Second example
#'# random lengths measured to nearest 0.1 unit
#'len2 <- data.frame(len=round(runif(50,10,117),1))
#'
#'# assignment of lengths to 0.1 for 140 non-measured indivs
#'new.len2a <- lenFreqExpand(len2,startcat=10,w=10,total=190,decimals=1)
#'new.len2a
#'
#'
#'## Third example
#'# hypothetically measured lengths
#'len <- c(6.7,6.9,7.3,7.4,7.5,8.2,8.7,8.9)
#'
#'# find lengths for unmeasured fish assuming a total of 30
#'newlen1 <- lenFreqExpand(len,startcat=6,w=0.5,total=30,decimals=1)
#'newlen1
#'# put together with measured lengths and make histogram
#'alllen <- c(len,newlen1)
#'hist(alllen,breaks=10,main="")
#'
lenFreqExpand <- function(df,cl=NULL,startcat,w,additional,total=additional+nrow(df),decimals=decs$wdec,show.summary=TRUE,...) {
  if (!is.null(cl)) {
    if (!is.data.frame(df)) stop("\n If a column name is given then df must be a data.frame")
    df <- df[,cl]
  }
  df <- as.data.frame(df)                                                       # turn into a data.frame
  cl <- names(df)                                                               # get the name of the one variable
  decs <- checkStartcatW(startcat,w,df)  
  if (total<nrow(df)) stop("\n Total number to expand to must be greater than number of fish supplied in 'df'.",call.=FALSE)
  df <- lencat(as.formula(paste("~",cl)),data=df,startcat,w,...)                                             # create length categories
  num <- total-dim(df)[1]                                                       # number to allocate
  lenfreq <- prop.table(table(df$LCat))                                         # length frequency of measured fish
  cats <- as.numeric(rownames(lenfreq))                                         # length frequency categories (lower limit of bin)
  reps <- floor(num*lenfreq)                                                    # number of expanded individuals per length category
  nrand.lens <- rep(cats,reps)                                                  # expansion of lengths according to values in reps
  rand.lens <- sample(cats,num-sum(reps),replace=TRUE,prob=lenfreq)             # randomly allocate rest of indivs based on probabilities in length category
  new.lens <- c(nrand.lens,rand.lens)                                           # put expanded and randomly allocated indivs into one vector
  maxval <- w-1/(10^decimals)                                                   # making sure that a length can't cross out of length category
  if (maxval>0) new.lens <- new.lens + runif(length(new.lens),min=0,max=maxval)
  new.lens <- round(new.lens,decimals)
  if (show.summary) {                                                           # print some summary values of what happened
    cat("Length Frequency Expansion using:\n")
    cat("Measured length frequency of",dim(df)[1],"individuals:")
    print(lenfreq)
    cat("\nNon-random allocations of",length(nrand.lens),"individuals by length category.")
    print(reps)
    cat("\nRandom allocations of",length(rand.lens),"individuals\n")
    cat("\nWith final length frequency table of:")
    final.lens <- as.data.frame(new.lens)
    final.lens <- lencat(as.formula(paste("~",names(final.lens))),data=final.lens,startcat,w,...)
    print(table(final.lens$LCat))
  }
  sort(new.lens)                                                                # return sorted vector of newly assigned lengths
}
