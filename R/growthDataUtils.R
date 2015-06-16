#' @title Converts between growth measurement data types.
#'
#' @description Converts one-fish-per-line format growth data from radial to incremental or incremental to radial measurements.
#'
#' @details This function does NOT convert the data from one-fish-per-line to one-measurement-per-line format (see \code{\link{gReshape}}).  Furthermore, this function requires the data to be in one-fish-per-line format -- i.e., each lines contains all information and all of the growth measurements for an individual fish.
#'
#' This function assumes that the input data frame is of the opposite data type given in \code{type} (i.e., that a conversion is needed).  It does not check to see if this is true.
#'
#' The columns that contain the original measurement data can be entered in a variety of ways.  First, if all columns begin with the same prefix (and no other columns contain that prefix) then the prefix string can be entered into \code{in.pre=}.  Second, a general sequence of column numbers can be entered into \code{in.var=} with the \code{#:#} form (if the columns are contiguous) or as a concatenated vector form if the columns are not contiguous.  Third, a concatenated vector of column names can be entered into \code{in.var=}.  Note  that one of but not both of \code{in.var=} or \code{in.pre=} must be declared by the user.
#'
#' The newly computed data will be labeled with a prefix the same as \code{type=} (i.e., \code{"rad"} or \code{"inc"}) unless \code{out.pre=} is set by the user.  For example, if the data is convert to radial measurements then the output variables will be \dQuote{rad1}, \dQuote{rad2}, etc. unless \code{out.pre=} was changed from the default.  This function assumes that the measurements start with age-1.
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
#' data(SMBassWB)
#' head(SMBassWB)     # to see column names & some data
#' SMBi1 <- gConvert(SMBassWB,in.pre="anu",type="inc")
#' head(SMBi1)
#' SMBi2 <- gConvert(SMBassWB,in.var=c("anu1","anu2","anu3","anu4","anu5","anu6",
#'                                     "anu7","anu8","anu9","anu10","anu11","anu12"),
#'                            type="inc")
#' head(SMBi2)
#' SMBi3 <- gConvert(SMBassWB,in.var=8:19,type="inc")
#' head(SMBi3)
#' SMBr1 <- gConvert(SMBi1,in.pre="inc",type="rad")
#' head(SMBr1)
#'
#' @export
gConvert<-function(df,in.pre,in.var,type=c("inc","rad"),out.pre=type) {
  ## An internal function that can be used to find the accumulation of a series of numbers
   cadd <- function(x) Reduce("+",x,accumulate=TRUE)
  ## end of internal function
  
  ## Some checks
  if (missing(in.pre) & missing(in.var)) stop("You must use one of 'in.var=' or 'in.pre='.",call.=FALSE)
  if (!missing(in.pre) & !missing(in.var)) warning("Both 'in.var=' and 'in.pre=' were used;
                                                   only 'in.var=' will be used.",call.=FALSE)
  type <- match.arg(type)
  # coerce df as a data frame
  df <- as.data.frame(df)
  # Find variable column numbers if prefix was used.
  if (missing(in.var)) {
    in.var <- grep(paste0("^",in.pre),names(df))
    if (length(in.var)==0) stop("No variables start with 'in.pre' string.",call.=FALSE)
  } else { # converts variable names to column numbers
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


#' @title Reshapes a one-fish-per-line data frame to a one-measurement-per-line data frame.
#'
#' @description Converts growth data from the one-fish-per-line format to the one-measurement-per-line format.  One-fish-per-line format is a common form for collecting or storing growth data.  One-measurement-per-line format is required for many statistical analyses.
#'
#' @details This function does NOT convert the data from radial to incremental or incremental to radial measurements (see \code{\link{gConvert}}).
#'
#' The input data frame in \code{df} must have the following specific formats.  First, the measurements of annular increments or radii must be in one-fish-per-line format.  The measurements must be contained in columns that are named with a common prefix (e.g., \dQuote{anu}, \dQuote{inc}, or \dQuote{rad}) followed by a number that represents the age of the fish when that portion of the structure formed.  This prefix must be the same for all columns that contains measurements and \bold{must not be found in any other variable (as a prefix or not)}.  For example, the first annular measurement should be in a variable named \dQuote{anu1}, the second annular measurement in \dQuote{anu2}, and so on.  The name of the prefix should be included in the \code{in.pre=} argument.
#'
#' If \code{id.var} is left blank then the vector of variables that will not be changed upon the reshaping will consist of all variables that do NOT start with the \code{in.pre} prefix.
#'
#' Errors may occur if a particular variable in the original data frame, to be included in the \code{id.var=} list, is of POSIX type.  A workaround for this error is to include the name of that variable in \code{drop=}.
#'
#' The name of the variable in the reshaped output data frame that contains the measurements will be called the same as \code{in.pre} by default.  This can be changed by including a new name as a string in \code{val.name}.
#'
#' @param df A data frame that contains the growth measurement data in one-fish-per-line format with specifs as defined in the details.
#' @param in.pre A string that represents the common start to the measurement variable names.  See details.
#' @param id.var A vector of variables in \code{df} that do not change.  See details.
#' @param var.name A string that indicates the name in the reshaped data frame that represents the level of the measurement variables in the original data frame.
#' @param val.name A string that indicates the name in the reshaped data frame that represents the measurements in the orginal data frame.
#' @param last.plus A string that if non-null indicates that the last measurement represents \dQuote{plus growth} and not an actual increment.  If \dQuote{plus growth} is recorded then this string should indicate the name of the variable in \code{df} that contains the age of the fish at capture.
#' @param na.rm A logical that indicates whether \code{NA} values in the measurement variables should be removed after reshaping.
#' @param drop A vector of variable names to drop before reshaping.
#'
#' @return Returns a data frame in one-measurement-per-line format.  See details.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{gConvert}} and \code{\link{reshape}}
#'
#' @keywords manip
#'
#' @examples
#' data(SMBassWB)
#' head(SMBassWB)
#'
#' # convert radial measurements to increments
#' SMBi1 <- gConvert(SMBassWB,in.pre="anu",type="inc")
#' head(SMBi1)
#'
#' SMBi1a <- gReshape(SMBi1,in.pre="inc")
#' head(SMBi1a)
#'
#' # same as above but assume that last increment (in agecap variable) is plus-growth
#' SMBi2a <- gReshape(SMBi1,in.pre="inc",last.plus="agecap")
#' head(SMBi2a)
#'
#' # example of dropping some variables before reshaping
#' SMBi1b <- gReshape(SMBi1,in.pre="inc",drop=c("species","lake"))
#' head(SMBi1b)
#'
#' @export
gReshape <- function(df,in.pre,id.var,var.name="prvAge",val.name=in.pre,last.plus=NULL,
                     na.rm=TRUE,drop=NULL) {
  ## Some Checks
  if (missing(in.pre)) stop("\nYou must have a prefix string in 'in.pre='.",call.=FALSE)
  # coerce df to be a data.frame
  df <- as.data.frame(df)
  # drop variables if given
  if (!is.null(drop)) df <- df[,-which(names(df) %in% drop)]
  # find measure.var by matching prefixes (the ^ makes sure it is a prefish)
  measure.var <- names(df)[grepl(paste0("^",in.pre),names(df))]
  if (length(measure.var)==0) stop("No variables start with 'in.pre' string.",call.=FALSE)
  # if no id.var, then id.var is all not in measure.var
  if (missing(id.var)) id.var <- names(df)[!grepl(paste0("^",in.pre),names(df))]
  # do the reshaping (new.row.names gets around error with duplicate rownames from reshape)
  ndf <- reshape(df,direction="long",idvar=id.var,varying=measure.var,
                 v.names=in.pre,timevar=var.name,new.row.names=1:100000)
  # remove all increments with NAs
  if (na.rm) ndf <- ndf[!is.na(ndf[,in.pre]),]
  # rename the rows
  rownames(ndf) <- 1:nrow(ndf)
  # handle the last.plus issues
  if (!is.null(last.plus)) {
    if(any(is.na(match(last.plus,names(df))))) stop("'last.plus=' variable not found.",call.=FALSE)
    # If last.plus not null then remove values where age exceeds age@cap (sent in last.plus)
    ndf <- ndf[(ndf[,var.name] <= ndf[,last.plus]),]
  }
  # strip attributes from reshape before returning
  attr(ndf,"reshapeLong") <- NULL
  ndf
}


#' @title Add a total radius-at-capture variable to a data frame that contains one-fish-per-line increments data.
#'
#' @description Add a total radius-at-capture variable to a data frame that contains one-fish-per-line increments data (i.e., \dQuote{wide} format).
#'
#' @details The columns that contains the original increment data can be entered in a variety of ways.  First, if all columns begin with the same prefix (and no other columns contain that prefix) then the prefix string can be entered into \code{in.pre}.  Second, a general sequence of column numbers can be entered into \code{in.var} with the \code{#:#} form (if the columns are contiguous) or as a concatenated vector if the columns are not contiguous.  Third, a concatenated vector of column names can be entered into \code{in.var}.  Note that one of but not both of \code{in.var} or \code{in.pre} must be declared by the user.
#'
#' Note that the computed total radius-at-capture will only be the actual total radius-at-capture if all growth, including plus-growth in the current season, is recorded in the input data frame.
#'
#' @param df A data frame that contains the growth increment data in one-fish-per-line (i.e., \dQuote{wide}) format.
#' @param in.pre A string that represents the common part of the increment variable names.  Default is \code{"inc"}.  See details.
#' @param in.var A vector of variables in \code{data} that do not change.  See details.
#' @param var.name A string that indicates the name of the total radius-at-capture in the new data frame.
#'
#' @return A data frame of increments in one-fish-per-line  (i.e., \dQuote{wide}) format with the total radius-at-capture appended in a variable named as given in \code{var.name} (defaults to \dQuote{radcap}).
#'
#' @seealso \code{\link{gConvert}} and \code{\link{gReshape}}
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @keywords manip
#'
#' @examples
#' data(SMBassWB)
#' head(SMBassWB)
#'
#' # convert radial measurements to increments
#' SMBi1 <- gConvert(SMBassWB,in.pre="anu",type="inc")
#' head(SMBi1)
#'
#' # add the radius-at-capture measurement
#' SMBi1a <- addRadCap(SMBi1,in.pre="inc",var.name="radcap2")
#' head(SMBi1a)
#'
#' @export
addRadCap <- function(df,in.pre="inc",in.var,var.name="radcap") {
  if (missing(in.pre) & missing(in.var)) stop("You must use one of 'in.pre=' or 'in.var='.",call.=FALSE)
  if (!missing(in.pre) & !missing(in.var)) warning("Both 'in.var=' and 'in.pre=' were used;
                                                   only 'in.var=' will be used.",call.=FALSE)
  if (!missing(in.var)) {
    if (!all(in.var %in% names(df))) stop("Not all 'in.var=' variables found.",call.=FALSE)
  } else {
    in.var <- grep(paste0("^",in.pre),names(df)) 
    if (length(in.var)==0) stop("No variables start with 'in.pre' string.",call.=FALSE)
  }
  df1 <- df[,in.var]
  radcap <- apply(df1,MARGIN=1,FUN=sum,na.rm=TRUE)
  df <- data.frame(df,radcap)
  names(df)[ncol(df)] <- var.name
  df
}
