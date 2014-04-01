#'Reshapes a one-fish-per-line data frame to a one-measurement-per-line data frame.
#'
#'Converts growth data from the one-fish-per-line format to the
#'one-measurement-per-line format.  One-fish-per-line format is a common form
#'for collecting or storing growth data.  One-measurement-per-line format is
#'required for many statistical analyses.
#'
#'This function does NOT convert the data from radial to incremental or
#'incremental to radial measurements (see \code{\link{gConvert}}).
#'
#'The input data frame in \code{df} must have the following specific formats.
#'First, the measurements of annular increments or radii must be in
#'one-fish-per-line format.  The measurements must be contained in columns
#'that are named with a common prefix (e.g., \dQuote{anu}, \dQuote{inc}, or
#'\dQuote{rad}) followed by a number that represents the age of the fish when that
#'portion of the structure formed.  This prefix must be the same for all columns
#'containing measurements and \bold{must not be found in any other variable (as
#'a prefix or not)}.  For example, the first annular measurement should be in a
#'variable named \dQuote{anu1}, the second annular measurement in
#'\dQuote{anu2}, and so on.  The name of the prefix should be included in the
#'\code{in.pre=} argument.
#'
#'If \code{id.var} is left blank then the vector of variables that will not be
#'changed upon the reshaping will consist of all variables that do NOT contain
#'the \code{in.pre} prefix.
#'
#'Errors may occur if a particular variable in the original data frame, to be
#'included in the \code{id.var=} list, is of POSIX type.  A workaround for this
#'error is to include the name of that variable in \code{drop=}.
#'
#'The name of the variable in the reshaped output data frame containing the
#'measurements will be called the same as \code{in.pre} by default.  This can
#'be changed by including a new name as a string in \code{val.name}.
#'
#'@param df A data frame containing the growth measurement data in
#'one-fish-per-line format with specifs as defined in the details.
#'@param in.pre A string that represents the common part of the measurement
#'variable names.  See details.
#'@param id.var A vector of variables in \code{df} that do not change.  See details.
#'@param var.name A string indicating the name in the reshaped data frame that
#'represents the level of the measurement variables in the original data frame.
#'@param val.name A string indicating the name in the reshaped data frame that
#'represents the measurements in the orginal data frame.
#'@param last.plus A string that if non-null indicates that the last
#'measurement represents \dQuote{plus growth} and not an actual increment.  If
#'\dQuote{plus growth} is recorded then this string should indicate the name of
#'the variable in \code{df} that contains the age of the fish at capture.
#'@param na.rm A logical indicating whether \code{NA} values in the measurement
#'variables should be removed after reshaping.
#'@param drop A vector of variable names to drop before reshaping.
#'@return Returns a data frame in one-measurement-per-line format.  See details.
#'@seealso \code{\link{gConvert}} and \code{\link{reshape}}
#'@export
#'@keywords manip
#'@examples
#'data(SMBassWB)
#'head(SMBassWB)
#'
#'# convert radial measurements to increments
#'SMBi1 <- gConvert(SMBassWB,in.pre="anu",type="inc")
#'head(SMBi1)
#'
#'SMBi1a <- gReshape(SMBi1,in.pre="inc")
#'head(SMBi1a)
#'
#'# same as above but assume that last increment (in agecap variable) is plus-growth
#'SMBi2a <- gReshape(SMBi1,in.pre="inc",last.plus="agecap")
#'head(SMBi2a)
#'
#'# example of dropping some variables before reshaping
#'SMBi1b <- gReshape(SMBi1,in.pre="inc",drop=c("species","lake"))
#'head(SMBi1b)
#'
gReshape <- function(df,in.pre,id.var,var.name="prvAge",val.name=in.pre,last.plus=NULL,
                     na.rm=TRUE,drop=NULL) {
  if (missing(in.pre)) stop("\nYou must enter a prefix string in the in.pre argument.",call.=FALSE)
  # coerce df to be a data.frame
  df <- as.data.frame(df)
  # drop variables if given
  if (!is.null(drop)) df <- df[,-which(names(df) %in% drop)]
  # find measure.var by matching prefixes
  measure.var <- names(df)[grep(in.pre,names(df))]
  # if no id.var, then id.var is all not in measure.var
  if (missing(id.var)) id.var <- names(df)[-grep(in.pre,names(df))]
  # do the reshaping (new.row.names gets around error with duplicate rownames from reshape)
  ndf <- reshape(df,direction="long",idvar=id.var,varying=measure.var,
                 v.names=in.pre,timevar=var.name,new.row.names=1:100000)
  # remove all increments with NAs
  if (na.rm) ndf <- ndf[!is.na(ndf[,in.pre]),]
  # rename the rows
  rownames(ndf) <- 1:nrow(ndf)
  # handle the last.plus issues
  if (!is.null(last.plus)) {
    if(any(is.na(match(last.plus,names(df))))) stop("\nName in last.plus argument does not occur in the input data.frame.",call.=FALSE)
    # If last.plus not null then remove values where age exceeds age@cap (sent in last.plus)
    ndf <- ndf[(ndf[,var.name] <= ndf[,last.plus]),]
  }
  # strip attributes from reshape before returning
  attr(ndf,"reshapeLong") <- NULL
  ndf
}
