#'Add a total radius-at-capture variable to a data frame that contains 
#'one-fish-per-line increments data.
#'
#'Add a total radius-at-capture variable to a data frame that contains 
#'one-fish-per-line increments data (i.e., \dQuote{wide} format).
#'
#'The columns that contains the original increment data can be entered in a
#'variety of ways.  First, if all columns begin with the same prefix (and no
#'other columns contain that prefix) then the prefix string can be entered into
#'\code{in.pre}.  Second, a general sequence of column numbers can be entered into
#'\code{in.var} with the \code{#:#} form (if the columns are contiguous) or as a
#'concatenated vector if the columns are not contiguous.  Third, a concatenated
#'vector of column names can be entered into \code{in.var}.  Note that one of but
#'not both of \code{in.var} or \code{in.pre} must be declared by the user.
#'
#'Note that the computed total radius-at-capture will only be the actual total
#'radius-at-capture if all growth, including plus-growth in the current season,
#'is recorded in the input data frame.
#'
#'@param df A data frame that contains the growth increment data in one-fish-per-line
#' (i.e., \dQuote{wide}) format.
#'@param in.pre A string that represents the common part of the increment
#'variable names.  Default is \code{"inc"}.  See details.
#'@param in.var A vector of variables in \code{data} that do not change.  See details.
#'@param var.name A string that indicates the name of the total radius-at-capture
#'in the new data frame.
#'@return A data frame of increments in one-fish-per-line  (i.e., \dQuote{wide})
#'format with the total radius-at-capture appended in a variable named as given
#'in \code{var.name} (defaults to \dQuote{radcap}).
#'@seealso \code{\link{gConvert}}, \code{\link{gReshape}}
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
#'# add the radius-at-capture measurement
#'SMBi1a <- addRadCap(SMBi1,in.pre="inc",var.name="radcap2")
#'head(SMBi1a)
#'
addRadCap <- function(df,in.pre="inc",in.var,var.name="radcap") {
  if (missing(in.pre) & missing(in.var)) stop("You must use one of the in.pre or in.var arguments.",call.=FALSE)
  if (!missing(in.pre) & !missing(in.var)) warning("Both in.var and cols arguments were used.  Only the in.var argument will be used.",call.=FALSE)
  if (!missing(in.pre) & missing(in.var)) { in.var <- grep(in.pre,names(df)) }
  df1 <- df[,in.var]
  radcap <- apply(df1,MARGIN=1,FUN=sum,na.rm=TRUE)
  df <- data.frame(df,radcap)
  names(df)[ncol(df)] <- var.name
  df
}
