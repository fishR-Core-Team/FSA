#'Converts between growth measurement data types.
#'
#'Converts one-fish-per-line format growth data from radial to incremental or incremental to radial measurements.
#'
#'This function does NOT convert the data from one-fish-per-line to one-measurement-per-line format (see \code{\link{gReshape}}).  Furthermore, this function requires the data to be in one-fish-per-line format -- i.e., each lines contains all information and all of the growth measurements for an individual fish.
#'
#'This function assumes that the input data frame is of the opposite data type given in \code{type} (i.e., that a conversion is needed).  It does not check to see if this is true.
#'
#'The columns that contain the original measurement data can be entered in a variety of ways.  First, if all columns begin with the same prefix (and no other columns contain that prefix) then the prefix string can be entered into \code{in.pre=}.  Second, a general sequence of column numbers can be entered into \code{in.var=} with the \code{#:#} form (if the columns are contiguous) or as a concatenated vector form if the columns are not contiguous.  Third, a concatenated vector of column names can be entered into \code{in.var=}.  Note  that one of but not both of \code{in.var=} or \code{in.pre=} must be declared by the user.
#'
#'The newly computed data will be labeled with a prefix the same as \code{type=} (i.e., \code{"rad"} or \code{"inc"}) unless \code{out.pre=} is set by the user.  For example, if the data is convert to radial measurements then the output variables will be \dQuote{rad1}, \dQuote{rad2}, etc. unless \code{out.pre=} was changed from the default.  This function assumes that the measurements start with age-1.
#'
#' @param df A data frame that contain the growth measurement data in one-fish-per-line format.
#' @param in.pre A string that indicates the prefix for all variable names that contain the growth measurement data in the input data frame.  See details.
#' @param in.var A vector of column numbers or variable names that contain the growth measurement data in the input data frame.  See details.
#' @param type A string that identifies the output format data type (i.e., the format to convert to).  If \code{"inc} (the default) the output data frame will be incremental measurements.  If \code{"rad} the output data frame will contain radial measurements.
#' @param out.pre A string that indicates the prefix to use for the newly computed measurements in the output data frame.  Defaults to the same string as \code{type}.
#'
#' @return Returns a data frame with all columns, except for the \code{in.var} columns, from the input data frame retained as the left-most columns in the output data frame.  The original data in the \code{in.var} columns converted to data type \code{type} form the remaining columns in the output data frame and are labeled as described in the details.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{gReshape}}
#'
#' @keywords manip
#'
#' @examples
#'data(SMBassWB)
#'head(SMBassWB)     # to see column names & some data
#'SMBi1 <- gConvert(SMBassWB,in.pre="anu",type="inc")
#'head(SMBi1)
#'SMBi2 <- gConvert(SMBassWB,in.var=c("anu1","anu2","anu3","anu4","anu5","anu6",
#'  "anu7","anu8","anu9","anu10","anu11","anu12"),type="inc")
#'head(SMBi2)
#'SMBi3 <- gConvert(SMBassWB,in.var=8:19,type="inc")
#'head(SMBi3)
#'SMBr1 <- gConvert(SMBi1,in.pre="inc",type="rad")
#'head(SMBr1)
#'
#' @export
gConvert<-function(df,in.pre,in.var,type=c("inc","rad"),out.pre=type) {
  ## An internal function that can be used to find the accumulation of a series of numbers
   cadd <- function(x) Reduce("+",x,accumulate=TRUE)
  ## end of internal function
  
  if (missing(in.pre) & missing(in.var)) stop("You must use one of the in.var or in.pre arguments.",call.=FALSE)
  if (!missing(in.pre) & !missing(in.var)) warning("Both in.var and in.pre arguments were used.  Only the in.var argument will be used.",call.=FALSE)
  type <- match.arg(type)
  # coerce df as a data frame
  df <- as.data.frame(df)
  # Find variable column numbers if prefix was used.
  if (missing(in.var)) { in.var <- grep(in.pre,names(df)) }
    else { 
      # converts variable names to column numbers
      if (is.character(in.var)) in.var <- match(in.var,names(df)) 
    }
  # check to make sure all variable names are good
  if (any(is.na(in.var))) stop("At least one variable name is incorrect.",call.=FALSE)
  # number of increment or radial measurements
  num.mv <- length(in.var)
  # all parts of the original file except for increment/radial measurements
  df.1 <- df[,-in.var]
  # just the radial or increment measurements
  df.mv <- df[,in.var]
  switch(type,
    # create increments from radii
    inc= { df.2 <- data.frame(df.mv[,1],df.mv[,2:num.mv]-df.mv[,1:(num.mv-1)]) },
    # create radii from increments
    rad= { df.2 <- data.frame(t(apply(df.mv,1,cadd))) }
  )
  colnames(df.2) <- paste(rep(out.pre,num.mv),seq(1:num.mv),sep="") 
  data.frame(df.1,df.2)
}  
