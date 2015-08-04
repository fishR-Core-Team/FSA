#' @title Converts between types of measurements from calcified structures.
#'
#' @description Converts wide format growth data from radial to incremental or incremental to radial measurements.
#'
#' @details The data must be in WIDE format where each row contains all information (including all measurements from the calcified structure) for an individual fish.  It is assumed that the input data.frame is of the opposite data type given in \code{out.type} (i.e., that a conversion is needed).  It does not check to see if this is true.
#' 
#' The columns that contain the original measurement data can specified in a variety of ways.  First, if all columns begin with the same prefix (and no other columns contain that prefix), then the prefix string may be given to \code{in.pre=}.  Second, a sequence of column numbers may be given to \code{in.var=} with the \code{#:#} (if the columns are contiguous) or as a vector (if the columns are not contiguous).  Third, a vector of column names may be given to \code{in.var=}.  Note  that one, but not both, of \code{in.var=} or \code{in.pre=} must be specified by the user.
#'
#' The newly computed data will be labeled with a prefix the same as \code{out.type=} (i.e., \code{"rad"} or \code{"inc"}) unless \code{out.pre=} is set by the user.  For example, if the data are converted to radial measurements, then the output variables will be \dQuote{rad1}, \dQuote{rad2}, etc. unless \code{out.pre=} was changed from the default.  This function assumes that the measurements start with age-1.
#'
#' @param df A data.frame that contains the growth measurement data in wide or one-fish-per-line format.
#' @param in.pre A string that indicates the prefix for all variable names in the input data frame that contain the measurements from the calcified structures.  See details.
#' @param in.var A vector of column numbers or variable names in the input data frame that contain the measurements from the calcified structures.  See details.
#' @param out.type A string that identifies the output format data type (i.e., the format to convert to).  If \code{"inc} (the default) the output data frame will be incremental measurements.  If \code{"rad} the output data frame will be radial measurements.
#' @param out.pre A string that indicates the prefix to use for the newly computed measurements in the output data frame.  Defaults to the same string as \code{out.type}.
#'
#' @return A data.frame with all columns, except for those defined by \code{in.pre} or \code{in.var}, from the \code{df} retained as the left-most columns and the original data in the \code{in.var} columns converted to the \code{out.type} type as the remaining columns.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso See \code{\link{addRadCap}} for related functionality.
#'
#' @keywords manip
#'
#' @examples
#' ## Get data with radial measurements
#' data(SMBassWB)
#' headtail(SMBassWB)
#' 
#' ## Use in.pre= to convert to increments
#' SMBi1 <- gConvert(SMBassWB,in.pre="anu",out.type="inc")
#' headtail(SMBi1)
#' 
#' ## Use in.var= with column names to convert to increments
#' SMBi2 <- gConvert(SMBassWB,in.var=c("anu1","anu2","anu3","anu4","anu5","anu6",
#'                                     "anu7","anu8","anu9","anu10","anu11","anu12"),
#'                            out.type="inc")
#' headtail(SMBi2)
#' 
#' ## Use in.var with column numbers to convert to increments
#' SMBi3 <- gConvert(SMBassWB,in.var=8:19,out.type="inc")
#' headtail(SMBi3)
#' 
#' ## Convert back to radial measurements
#' SMBr1 <- gConvert(SMBi1,in.pre="inc",out.type="rad")
#' headtail(SMBr1)
#'
#' @export
gConvert<-function(df,in.pre=NULL,in.var=NULL,out.type=c("inc","rad"),out.pre=out.type) {
  ## Coerce to a data.frame
  df <- as.data.frame(df)
  ## Perform some checks and get the in.var variables
  in.var <- bcUtilChecker(df,in.pre,in.var)
  out.type <- match.arg(out.type)
  # number of increment or radial measurements
  num.mv <- length(in.var)
  # all parts of the original file except for increment/radial measurements
  df.1 <- df[,-in.var]
  # just the radial or increment measurements
  df.mv <- df[,in.var]
  switch(out.type,
    # create increments from radii
    inc= { df.2 <- data.frame(df.mv[,1],df.mv[,2:num.mv]-df.mv[,1:(num.mv-1)]) },
    # create radii from increments
    rad= { df.2 <- data.frame(t(apply(df.mv,1,cumsum))) }
  )
  # name the new colums
  colnames(df.2) <- paste(rep(out.pre,num.mv),seq(1:num.mv),sep="") 
  # put parts together and return
  data.frame(df.1,df.2)
}  



#' @title Add a total radius-at-capture variable to a data.frame that contains one-fish-per-line increments data.
#'
#' @description Add a total radius-at-capture variable to a data.frame that contains one-fish-per-line increments data (i.e., \dQuote{wide} format).
#'
#' @details #' The columns that contain the original measurement data can specified in a variety of ways.  First, if all columns begin with the same prefix (and no other columns contain that prefix), then the prefix string may be given to \code{in.pre=}.  Second, a sequence of column numbers may be given to \code{in.var=} with the \code{#:#} (if the columns are contiguous) or as a vector (if the columns are not contiguous).  Third, a vector of column names may be given to \code{in.var=}.  Note  that one, but not both, of \code{in.var=} or \code{in.pre=} must be specified by the user.
#'
#' Note that the computed total radius-at-capture will only be the actual total radius-at-capture if all growth, including plus-growth in the current season, is recorded in the input data frame.
#'
#' @param df A data.frame that contains the growth increment data in one-fish-per-line (i.e., \dQuote{wide}) format.
#' @param in.pre A string that represents the common part of the increment variable names.  See details.
#' @param in.var A vector of variables in \code{df} that do not change.  See details.
#' @param var.name A string that indicates the name for the new total radius-at-capture variable in the new data.frame.
#'
#' @return A data.frame of increments in one-fish-per-line  (i.e., \dQuote{wide}) format with the total radius-at-capture appended in a variable named as given in \code{var.name} (defaults to \dQuote{radcap}).
#'
#' @seealso See \code{\link{gConvert}} for related functionality.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @keywords manip
#'
#' @examples
#' ## Get data with radial measurements
#' data(SMBassWB)
#' headtail(SMBassWB)
#'
#' ## convert radial measurements to increments
#' SMBi1 <- gConvert(SMBassWB,in.pre="anu",out.type="inc")
#' headtail(SMBi1)
#'
#' ## add the radius-at-capture measurement
#' SMBi1a <- addRadCap(SMBi1,in.pre="inc",var.name="radcap2")
#' headtail(SMBi1a)
#'
#' @export
addRadCap <- function(df,in.pre=NULL,in.var=NULL,var.name="radcap") {
  ## Coerce to a data.frame
  df <- as.data.frame(df)
  ## Perform some checks and get the in.var variables
  in.var <- bcUtilChecker(df,in.pre,in.var)
  ## isolate the measurement data, compute the sum, append to
  ## original data.frame, and return the data.frame
  df[,var.name] <- rowSums(df[,in.var],na.rm=TRUE)
  df
}


##############################################################
# Internal Helper Files
##############################################################
bcUtilChecker <- function(df,in.pre,in.var) {
  ## Some checks
  if (is.null(in.pre) & is.null(in.var)) stop("You must use one of 'in.pre=' or 'in.var='.",call.=FALSE)
  if (!is.null(in.pre) & !is.null(in.var)) warning("Both 'in.var=' and 'in.pre=' were given;
                                                    only 'in.var=' will be used.",call.=FALSE)
  ## Find variables with measurements
  if (!is.null(in.var)) {
    if (is.character(in.var)) {
      if (!all(in.var %in% names(df))) stop("Not all 'in.var=' variables found.",call.=FALSE)
      # convert in.var names to column numbers
      in.var <- which(names(df) %in% in.var)
    } else {
      if (max(in.var)>ncol(df)) stop("Column numbers exceed number of columns.",call.=FALSE)
      if (min(in.var)<1) stop("Non-positive column number given.",call.=FALSE)
    }
  } else {
    in.var <- grep(paste0("^",in.pre),names(df)) 
    if (length(in.var)==0) stop("No variables start with 'in.pre' string.",call.=FALSE)
  }
  ## Return in.var
  in.var
}