#' @title Adds zeroes for catches of species not collected in some sampling events.
#'
#' @description Adds zeroes for catches of species that were not captured in a sampling event but were captured in at least one other sampling event (i.e., adds zeroes to the data frame for capture events where a species was not observed).
#'
#' @details The data frame in \code{df} must contain a column that identifies a unique capture event (given in \code{eventvar}), a column with the name for the species captured (given in \code{specvar}), and a column that contains the number of that species captured (potentially given to \code{zerovar}; see details).  All sampling event and species combinations where catch information does not exist is identified and a new data frame that contains a zero for the catch for all of these combinations is created.  This new data frame is appended to the original data frame to construct a data frame that contains complete catch information -- i.e., including zeroes for species in events where that species was not captured.
#'
#' The original data frame (\code{df}) may also contain columns of data not discussed above.  For example, it may include columns of data that are specific to the capture event, such as date, gear type, habitat, etc.  The names for these variables can be given in \code{idvar} so that these data can be repeated with each row of zeroes added to the data frame.  In addition, the data frame may contain other information related to the catch, such as number of recaptured fish, number of fish released, etc.  These variables can be given in \code{zerovar} so that zeroes can be added to these variables as well (e.g., if the catch of the species is zero, then the number of recaptures must also be zero).  Only one of \code{idvar} or \code{zerovar} needs to be given as it will be assumed that the rest of variable names in the data frame (not including those given in \code{eventvar} and \code{specvar}) will be of the other type.
#' 
#' One should test the results of this function by creating a frequency table of the \code{eventvar} or \code{specvar}.  In either case, the table should contain the same value in each cell of the table.  See the examples.
#'
#' @param df A data frame that contains the capture summary data as described in the details.
#' @param eventvar A string for the variable that identifies unique capture events.
#' @param specvar A string for the variable that identifies the species captured.
#' @param idvar A string or vector of strings for the variable(s) that are common to a unique capture event and should be repeated for each zero row added to the data frame (e.g., dates, gear type, etc.).  See details
#' @param zerovar A string or vector of strings for the variable(s) that should be set equal to zero.  See details.
#' @return A data frame with the same structure as \code{df} but with rows of zero observation data appended.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @keywords manip
#'
#' @examples
#' ## Example Data #1 (some nets missing some fish, ancillary net data)
#' df1 <- data.frame(net=c(1,1,1,2,2,3),
#'                   eff=c(1,1,1,1,1,1),
#'                   species=c("BKT","LKT","RBT","BKT","LKT","RBT"),
#'                   catch=c(3,4,5,5,4,3))
#' df1
#' xtabs(~net+species,data=df1)                # not all 1s
#'
#' df1mod1 <- addZeroCatch(df1,"net","species",zerovar="catch")
#' df1mod1
#' xtabs(~net,data=df1mod1)                    # check, should all be 3
#' xtabs(~net+species,data=df1mod1)            # check, should all be 1
#' Summarize(catch~species,data=df1mod1)       # correct mean/sd of catches
#' Summarize(catch~species,data=df1)           # incorrect mean/sd of catches (no zeroes)
#'
#' # Same as example 1 but with no ancillary data specific to the net number
#' df2 <- df1[,-2]
#' df2
#' df1mod2 <- addZeroCatch(df2,"net","species",zerovar="catch")
#' df1mod2
#' xtabs(~net+species,data=df1mod2)   # check, should all be 1
#'
#' ## Example Data #3 (All nets have same species ... no zeroes needed)
#' df3 <- data.frame(net=c(1,1,1,2,2,2,3,3,3),
#'                   eff=c(1,1,1,1,1,1,1,1,1),
#'                   species=c("BKT","LKT","RBT","BKT","LKT","RBT","BKT","LKT","RBT"),
#'                   catch=c(3,4,5,5,4,3,3,2,7))
#' df3
#' xtabs(~net+species,data=df3)                # should all be 1 for this example
#'
#' df3mod1 <- addZeroCatch(df3,"net","species",zerovar="catch")
#' df3mod1
#' xtabs(~net+species,data=df3mod1)            # check, should still all be 1
#'
#' ## Example Data #4 (another variable that needs zeroes)
#' df4 <- df1
#' df4$recaps <- c(0,0,0,1,2,1)
#' df4
#' xtabs(~net+species,data=df4)                # not all 1s
#'
#' df4mod1 <- addZeroCatch(df4,"net","species",zerovar=c("catch","recaps"))
#' df4mod1                                     # note zeroes in both variables
#' xtabs(~net+species,data=df4mod1)            # check, should all be 1
#' Summarize(catch~species,data=df4)           # observe difference from next
#' Summarize(catch~species,data=df4mod1)
#' Summarize(recaps~species,data=df4)          # observe difference from next
#' Summarize(recaps~species,data=df4mod1)
#'
#' @export
addZeroCatch <- function(df,eventvar,specvar,idvar=NULL,zerovar=NULL) {
  df <- as.data.frame(df)
  dfnames <- names(df)
  ## checks
  if (is.null(idvar) & is.null(zerovar)) stop("One of 'idvar' or 'zerovar' must be non-null.",call.=FALSE)    
  if (!is.null(zerovar) & !is.null(idvar)) {
    if (length(names(df)) != (length(idvar)+length(zerovar)+2))
      stop("Combined lengths of 'eventvar', 'specvar', 'idvar', and 'zerovar' do not match number of columns in 'df'.",call.=FALSE)
  }
  # get vectors of event and species names
  tmp <- table(df[,eventvar],df[,specvar])
  events <- rownames(tmp)
  species <- colnames(tmp)
  # identify combos of events and species in df that need zeroes
  tmp <- expand.grid(events,species)
  colnames(tmp) <- c(eventvar,specvar)
  all.combos <- paste(tmp[,eventvar],tmp[,specvar],sep=":")
  combos.in.df <- paste(df[,eventvar],df[,specvar],sep=":")
  need0s <- tmp[which(!(all.combos %in% combos.in.df)),]
  
  # Catch if there are no need for zeroes
  if (nrow(need0s)==0) {
    warning("All 'eventvar' have all species in 'specvar'; thus, there are\n no zeroes to add.  The original data frame was returned.",call.=FALSE)
    df
  } else {
    # fills idvar or zerovar if null
    if (is.null(zerovar)) zerovar <- dfnames[!dfnames %in% c(eventvar,specvar,idvar)]
    if (length(zerovar)==0) stop("No variables are to be given zeroes.  Check that 'zerovar' will not be empty.",call.=FALSE)
    if (is.null(idvar)) idvar <- dfnames[!dfnames %in% c(eventvar,specvar,zerovar)]                 
    # create a vector full of zeroes for zerovar
    zeroes <- matrix(0,ncol=length(zerovar),nrow=1)  
  
    # Create new rows for the data frame that contain zeroes
    if (length(idvar)==0) { # idvar is empty
      # reorders columns for simplicity
      df <- df[,c(eventvar,specvar,zerovar)]
      # prepare an empty data frame to receive zeroes
      newdf <- df[FALSE,]
      for (i in 1:nrow(need0s)) {
        newrow <- data.frame(need0s[i,eventvar],need0s[i,specvar],zeroes)
        newdf <- rbind(newdf,newrow)                                                              
      }    
    } else { # idvar is not empty
      # reorders columns for simplicity
      df <- df[,c(eventvar,specvar,idvar,zerovar)]
      # prepare an empty data frame to receive zeroes
      newdf <- df[FALSE,]
      for (i in 1:nrow(need0s)) {
        newrow <- data.frame(need0s[i,eventvar],need0s[i,specvar],
                             unique(df[df[,eventvar]==need0s[i,eventvar],idvar]),
                             zeroes)
        newdf <- rbind(newdf,newrow)                                                              
      }    
    }
    names(newdf) <- names(df)
    # combine new zero rows with original rows
    df <- rbind(df,newdf)
    # puts the order of the variables back to the original order
    df[,dfnames]
  }
}