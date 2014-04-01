#'Adds zero catch values for sampling events and species that were not captured.
#'
#'Adds zero catch values for species that were not captured in a sampling event
#'but were captured in at least one other sampling event (i.e., adds zeroes to
#'the data frame for capture events where a species was not observed).
#'
#'The data frame in \code{df} must contain a column that identifies a unique
#'capture event (given to \code{eventvar}), a column with the name of the
#'species captured (given to \code{specvar}), and a column containing the number
#'of that species captured (potentially given to \code{zerovar}; see details).
#'This function finds all event and species combinations where catch information
#'does not exist and then creates a new data frame that contains a zero for the
#'catch for all of these combinations.  This new data frame is then appended to
#'the original data frame to construct a data frame that has complete catch
#'observation data in it -- i.e., including zeroes for species that were not
#'captured in some events.
#'
#'The original data frame (\code{df}) may also contain columns of data not
#'discussed above.  For example, it may include columns of data that are
#'specific to the capture event, such as date, gear type, habitat, etc.  Information
#'in these variables should be repeated specific to the sampling event when the
#'zero catch is added to the data frame.  Thus, the variable names for these data
#'can be given to \code{idvar} so that these data can be repeated with each zero
#'added to the data frame.  In addition, the data frame may contain other information
#'related to the catch, such as number of recaptured fish, number of fish released,
#'etc.  These variables should also have zeroes for each species not caught in a
#'sampling event (e.g., if the catch of the species is zero, then the number of
#'recaptures must also be zero).  Thus, the variable names for this information
#'should be given to \code{zerovar} so that zeroes can be added to these variables
#'as well.  Only one of \code{idvar} or \code{zerovar} needs to be given as the function
#'will assume that the rest of variable names in the data frame (not including those
#'given in \code{eventvar} and \code{specvar}) will be of the other type.
#'
#'@param df A data frame containing the capture summary data as described in the details.
#'@param eventvar A string indicating the variable that identifies unique capture events.
#'@param specvar A string indicating the variable that identifies the species captured.
#'@param idvar A string or vector of strings indicating the variable(s) that are
#'common to a unique capture event and should be repeated for each zero row added
#'to the data frame (e.g., dates, gear type, etc.).  See details
#'@param zerovar A string or vector of strings indicating the variable(s) that
#'should be set equal to zero.  See details.
#'@return Returns a data frame of the same type as \code{df} but with rows of
#'zero observation data appended and the columns slightly reorganized.
#'@export
#'@keywords manip
#'@examples
#'df <- data.frame(net=c(1,1,1,2,2,3),
#'                 eff=c(1,1,1,1,1,1),
#'                 species=c("BKT","LKT","RBT","BKT","LKT","RBT"),
#'                 catch=c(3,4,5,5,4,3))
#'df
#'
#'# example with ancillary data specific to the net number (e.g., effort)
#'addZeroCatch(df,"net","species",zerovar="catch")
#'# example with no ancillary data specific to the net number
#'addZeroCatch(df[,-2],"net","species",zerovar="catch")
#'
addZeroCatch <- function(df,eventvar,specvar,idvar=NULL,zerovar=NULL) {
  df <- as.data.frame(df)
  dfnames <- names(df)
  if (is.null(idvar) & is.null(zerovar)) stop("One of 'idvar' or 'zerovar' must be non-null.",call.=FALSE)    # checks
  if (!is.null(zerovar) & !is.null(idvar)) {
    if (length(names(df)) != (length(names(idvar))+length(names(zerovar))+2))
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