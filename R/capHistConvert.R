#' @title Convert between capture history data.frame formats.
#'
#' @description Use to convert between simple versions of several capture history data.frame formats -- \dQuote{individual}, \dQuote{frequency}, \dQuote{event}, \dQuote{MARK}, and \dQuote{RMark}.  The primary use is to convert to the \dQuote{individual} format for use in \code{\link{capHistSum}}.
#'
#' @details \code{\link{capHistSum}} requires capture histories to be recorded in the \dQuote{individual} format.  In this format, the data frame contains (at least) as many columns as sample events and as many rows as individually tagged fish.  Optionally, the data.frame may also contain a column with unique fish identifiers (e.g., tag numbers).  Each cell in the capture history portion of the data.frame contains a \sQuote{0} if the fish of that row was NOT seen in the event of that column and a \sQuote{1} if the fish of that row WAS seen in the event of that column.  For example, suppose that five fish were marked on four sampling events; fish \sQuote{17} was captured on the first two events; fish \sQuote{18} was captured on the first and third events; fish \sQuote{19} was captured on only the third event; fish \sQuote{20} was captured on only the fourth event; and fish \sQuote{21} was captured on the first and second events.  The \dQuote{individual} capture history date.frame for these data looks like:
#'
#' \tabular{ccccc}{
#' fish \tab event1 \tab event2 \tab event3 \tab event4 \cr
#' 17 \tab 1 \tab 1 \tab 0 \tab 0 \cr
#' 18 \tab 1 \tab 0 \tab 1 \tab 0 \cr
#' 19 \tab 0 \tab 0 \tab 1 \tab 0 \cr
#' 20 \tab 0 \tab 0 \tab 0 \tab 1 \cr 
#' 21 \tab 1 \tab 1 \tab 0 \tab 0 \cr
#' }
#'
#' The \dQuote{frequency} format data.frame (this format is used in \pkg{rcapture}) has unique capture histories in separate columns, as in the \dQuote{individual} format, but also includes a column with the frequency of individuals that had the capture history of that row.  It will not contain a fish identifier variable.  The same data from above looks like:
#'
#' \tabular{ccccc}{
#' event1 \tab event2 \tab event3 \tab event4 \tab freq \cr
#' 1 \tab 1 \tab 0 \tab 0 \tab 2 \cr
#' 1 \tab 0 \tab 1 \tab 0 \tab 1 \cr
#' 0 \tab 0 \tab 1 \tab 0 \tab 1 \cr
#' 0 \tab 0 \tab 0 \tab 1 \tab 1 \cr
#' }
#'
#' The \dQuote{event} format data.framed has a column with the unique fish identifier and a column with the event in which the fish of that row was observed.  The same data from above looks like:
#'
#' \tabular{cc}{
#' fish \tab event \cr
#' 17 \tab 1 \cr
#' 18 \tab 1 \cr
#' 21 \tab 1 \cr
#' 17 \tab 2 \cr
#' 21 \tab 2 \cr
#' 18 \tab 3 \cr
#' 19 \tab 3 \cr
#' 20 \tab 4 \cr
#'}
#'
#' MARK is the \dQuote{gold-standard} software for analyzing complex capture history information.  In the \dQuote{MARK} format the 0s and 1s of the capture histories are combined together as a string without any spaces.  Thus, the \dQuote{MARK} format has the capture history strings in one column with an additional column that contains the frequency of individuals that exhibited the capture history of that row.  The final column is ended with a semi-colon.  The same data from above looks like:
#'
#' \tabular{cc}{
#' ch \tab freq \cr
#' 0001 \tab 1; \cr
#' 0010 \tab 1; \cr
#' 1010 \tab 1; \cr
#' 1100 \tab 2; \cr
#' }
#'
#' \pkg{RMark} and \pkg{marked} are R packages used to replace or interact with MARK.  The \dQuote{RMark} or \dQuote{marked} format requires the capture histories as one string (must be a character string and called \sQuote{ch}), as in the \dQuote{MARK} format, but without the semicolon.  The data.frame may be augmented with an indentifier for individual fish OR with a frequency variable.  If augmented with a unique fish identification variable then the same data from above looks like:
#'
#' \tabular{cc}{
#' fish \tab ch \cr
#' 17 \tab 1100 \cr
#' 18 \tab 1010 \cr
#' 19 \tab 0010 \cr
#' 20 \tab 0001 \cr 
#' 21 \tab 1100 \cr
#' }
#' 
#' However, if augmented with a frequency variable then the same data from above looks like:
#'
#' \tabular{cc}{
#' ch \tab freq \cr
#' 0001 \tab 1 \cr
#' 0010 \tab 1 \cr
#' 1010 \tab 1 \cr
#' 1100 \tab 2 \cr
#' }
#'
#' Each of the formats can be used to convert from (i.e., in \code{in.type=}) or to convert to (i.e., in \code{out.type=}) with the exception that only the individual fish identifier version can be converted to when \code{out.type="RMark"}.
#' 
#' @param df A data.frame that contains the capture histories and, perhaps, a unique fish identifier or frequency variable.  See details.
#' @param cols2ignore A string or numeric vector that indicates columns in \code{df} to ignore.  Typical columns to ignore are those that are not either in \code{id=} or \code{freq=} or part of the capture history data).
#' @param in.type A single string that indicates the type of capture history format to convert \bold{FROM}.
#' @param out.type A single string that indicates the type of capture history format to convert \bold{TO}.
#' @param id A string or numeric that indicates the column in \code{df} that contains the unique identifier for an individual fish.  This argument is only used if \code{in.type="event"}, \code{in.type="individual"}, or, possibly, \code{in.type="RMark"}.
#' @param freq A string or numeric that indicates the column in \code{df} that contains the frequency of individual fish corresponding to a capture history.  This argument is only used if \code{in.type="MARK"}, \code{in.type="frequency"}, or, possibly, \code{in.type="RMark"}.
#' @param event.ord A string that contains a vector of ordered levels to be used when \code{in.type="event"}.  The default is to order alphabetically which may not be desirable if the events are labelled as \sQuote{first}, \sQuote{second}, \sQuote{third}, and \sQuote{fourth}.  In this case, use \code{event.ord=c("first","second","third","fourth")}.
#' @param var.lbls A string vector of labels for the columns that contain the returned individual or frequency capture histories.  If \code{var.lbls=NULL} or the length is different then the number of events then default labels using \code{var.lbls.pre} will be used.  This argument is only used if \code{out.type="frequency"} or \code{out.type="individual"}.
#' @param var.lbls.pre A single string used as a prefix for the labels of the columns that contain the returned individual or frequency capture histories.  This prefix will be appended with a number corresponding to the sample event.  This argument is only used if \code{out.type="frequency"} or \code{out.type="individual"} and will be ignored if a proper vector is given in \code{var.lbls}.
#' @param include.id A logical that indicates whether a unique fish identifier variable/column should be included in the output data.frame.  This argument is only used if \code{out.type="individual"} or \code{out.type="RMark"}.
#'
#' @return A data frame of the proper type given in \code{out.type} is returned.  See details.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @note The formats as used here are simple in the sense that one is only allowed to have the individual fish identifier or the frequency variable in addition to the capture history information.  More complex analyses may use a number of covariates.  For these more complex analyses, one should work directly with the \pkg{rcapture}, \pkg{RMark}, or \pkg{marked} packages.
#' 
#' This function also assumes that all unmarked captured fish are marked and returned to the population (i.e., no losses at the time of marking are allowed).
#'
#' @seealso \code{\link{capHistSum}} to summarize \dQuote{individual} capture histories into a format useable in \code{\link{mrClosed}} and \code{\link{mrOpen}}.  Also see \pkg{rcapture}, \pkg{RMark}, or \pkg{marked} packages.
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/MRClosed.pdf}, \url{https://sites.google.com/site/fishrfiles/gnrl/MROpen.pdf}
#'
#' @keywords manip
#'
#' @examples
#' ## A small example of 'event' format
#' ( ex1 <- data.frame(fish=c(17,18,21,17,21,18,19,20),yr=c(1987,1987,1987,1988,1988,1989,1989,1990)) )
#' # convert to 'individual' format
#' ( ex1.E2I <- capHistConvert(ex1,id="fish",in.type="event") )
#' # convert to 'frequency' format
#' ( ex1.E2F <- capHistConvert(ex1,id="fish",in.type="event",out.type="frequency") )
#' # convert to 'MARK' format
#' ( ex1.E2M <- capHistConvert(ex1,id="fish",in.type="event",out.type="MARK") )
#' # convert to 'RMark' format
#' ( ex1.E2R <- capHistConvert(ex1,id="fish",in.type="event",out.type="RMark") )
#' 
#' ## convert converted 'individual' format ...
#' # to 'frequency' format (must ignore "id")
#' ( ex1.I2F <- capHistConvert(ex1.E2I,id="fish",in.type="individual",out.type="frequency") )
#' # to 'MARK' format
#' ( ex1.I2M <- capHistConvert(ex1.E2I,id="fish",in.type="individual",out.type="MARK") )
#' # to 'RMark' format
#' ( ex1.I2R <- capHistConvert(ex1.E2I,id="fish",in.type="individual",out.type="RMark") )
#' # to 'event' format
#' ( ex1.I2E <- capHistConvert(ex1.E2I,id="fish",in.type="individual",out.type="event") )
#'
#'#' ## convert converted 'frequency' format ...
#' # to 'individual' format
#' ( ex1.F2I <- capHistConvert(ex1.E2F,freq="freq",in.type="frequency") )
#' # to 'Mark' format
#' ( ex1.F2M <- capHistConvert(ex1.E2F,freq="freq",in.type="frequency",out.type="MARK") )
#' # to 'RMark' format
#' ( ex1.F2R <- capHistConvert(ex1.E2F,freq="freq",in.type="frequency",out.type="RMark") )
#' # to 'event' format
#' ( ex1.F2E <- capHistConvert(ex1.E2F,freq="freq",in.type="frequency",out.type="event") )
#' 
#' ## convert converted 'MARK' format ...
#' # to 'individual' format
#' ( ex1.M2I <- capHistConvert(ex1.E2M,freq="freq",in.type="MARK") )
#' # to 'frequency' format
#' ( ex1.M2F <- capHistConvert(ex1.E2M,freq="freq",in.type="MARK",out.type="frequency") )
#' # to 'RMark' format
#' ( ex1.M2R <- capHistConvert(ex1.E2M,freq="freq",in.type="MARK",out.type="RMark") )
#' # to 'event' format
#' ( ex1.M2E <- capHistConvert(ex1.E2M,freq="freq",in.type="MARK",out.type="event") )
#'  
#' ## convert converted 'RMark' format ...
#' # to 'individual' format
#' ( ex1.R2I <- capHistConvert(ex1.E2R,id="fish",in.type="RMark") )
#' # to 'frequency' format
#' ( ex1.R2F <- capHistConvert(ex1.E2R,id="fish",in.type="RMark",out.type="frequency") )
#' # to 'MARK' format
#' ( ex1.R2M <- capHistConvert(ex1.E2R,id="fish",in.type="RMark",out.type="MARK") )
#' # to 'event' format
#' ( ex1.R2E <- capHistConvert(ex1.E2R,id="fish",in.type="RMark",out.type="event") )
#' 
#' ## Remove semi-colon from MARK format to make a RMark 'frequency' format
#' ex1.E2R1 <- ex1.E2M
#' ex1.E2R1$freq <- as.numeric(sub(";","",ex1.E2R1$freq))
#' ex1.E2R1
#' # convert this to 'individual' format
#' ( ex1.R2I1 <- capHistConvert(ex1.E2R1,freq="freq",in.type="RMark") )
#' # convert this to 'frequency' format
#' ( ex1.R2F1 <- capHistConvert(ex1.E2R1,freq="freq",in.type="RMark",out.type="frequency") )
#' # convert this to 'MARK' format
#' ( ex1.R2M1 <- capHistConvert(ex1.E2R1,freq="freq",in.type="RMark",out.type="MARK") )
#' # convert this to 'event' format
#' ( ex1.R2E1 <- capHistConvert(ex1.E2R1,freq="freq",in.type="RMark",out.type="event") )
#' 
#' 
#' #' ## A small example using character ids
#' ( ex2 <- data.frame(fish=c("id17","id18","id21","id17","id21","id18","id19","id20"),
#'                     yr=c(1987,1987,1987,1988,1988,1989,1989,1990)) )
#' # convert to 'individual' format
#' ( ex2.E2I <- capHistConvert(ex2,id="fish",in.type="event") )
#' # convert to 'frequency' format
#' ( ex2.E2F <- capHistConvert(ex2,id="fish",in.type="event",out.type="frequency") )
#' # convert to 'MARK' format
#' ( ex2.E2M <- capHistConvert(ex2,id="fish",in.type="event",out.type="MARK") )
#' # convert to 'RMark' format
#' ( ex2.E2R <- capHistConvert(ex2,id="fish",in.type="event",out.type="RMark") )
#' 
#' ## convert converted 'individual' format ...
#' # to 'frequency' format (must ignore "id")
#' ( ex2.I2F <- capHistConvert(ex2.E2I,id="fish",in.type="individual",out.type="frequency") )
#' # to 'MARK' format
#' ( ex2.I2M <- capHistConvert(ex2.E2I,id="fish",in.type="individual",out.type="MARK") )
#' # to 'RMark' format
#' ( ex2.I2R <- capHistConvert(ex2.E2I,id="fish",in.type="individual",out.type="RMark") )
#' # to 'event' format
#' ( ex2.I2E <- capHistConvert(ex2.E2I,id="fish",in.type="individual",out.type="event") )
#'
#'
#' ## ONLY RUN IN INTERACTIVE MODE
#' if (interactive()) {
#' 
#' ## A larger example of 'frequency' format
#' require(Rcapture)
#' data(bunting)
#' head(bunting)
#' # convert to 'individual' format
#' ex2a <- capHistConvert(bunting,in.type="frequency",freq="freq")
#' head(ex2a)
#' # convert to 'MARK' format
#' ex2b <- capHistConvert(bunting,id="id",in.type="frequency",freq="freq",out.type="MARK")
#' head(ex2b)
#' # convert converted 'individual' back to 'MARK' format
#' ex2c <- capHistConvert(ex2a,id="id",in.type="individual",out.type="MARK")
#' head(ex2c)
#' # convert converted 'individual' back to 'frequency' format
#' ex2d <- capHistConvert(ex2a,id="id",in.type="individual",out.type="frequency",var.lbls.pre="Sample")
#' head(ex2d)
#'
#' }
#' 
#'   
#' ## A small example of 'MARK' format -- capture history followed by frequency
#' ( ex3 <- data.frame(ch=c("10101;","10001;","01010;"),freq=c(3,1,2)) )
#' # convert to 'individual' format
#' ( ex3a <- capHistConvert(ex3,in.type="MARK",freq="freq") )
#' # convert to 'frequency' format
#' ( ex3b <- capHistConvert(ex3,in.type="MARK",freq="freq",out.type="frequency") )
#' 
#'
#' ## A small example of 'MARK' format with two groups -- males and females
#' ( ex4 <- data.frame(ch=c("100101;","010001;"),male=c(3,1),female=c(2,2)) )
#' # convert to 'individual' format
#' ( ex4m <- capHistConvert(ex4[,c("ch","male")],in.type="MARK",freq="male",include.id=FALSE) )
#' ( ex4f <- capHistConvert(ex4[,c("ch","female")],in.type="MARK",freq="female",include.id=FALSE) )
#' require(gdata)   # for combine()
#' ( ex4a <- combine(ex4m,ex4f,names=c("male","female")) )
#'
#' @export
capHistConvert <- function(df,cols2ignore=NULL,
                           in.type=c("frequency","event","individual","MARK","marked","RMark"),
                           out.type=c("individual","event","frequency","MARK","marked","RMark"),
                           id=NULL,event.ord=NULL,freq=NULL,
                           var.lbls=NULL,var.lbls.pre="event",
                           include.id=ifelse(is.null(id),FALSE,TRUE)) {
  # initial argument checks
  in.type <- match.arg(in.type)
  out.type <- match.arg(out.type)
  if (in.type==out.type) stop("'in.type' and 'out.type' cannot be the same.",call.=FALSE)
  # make sure df is a data.frame (could be sent as a matrix)
  df <- as.data.frame(df)
  # immediately remove cols2ignore cols
  if (!is.null(cols2ignore)) df <- df[,-cols2ignore]
  
  ## Convert from other forms to individual form
  switch(in.type,
         event=         { ch.df <- iEvent2Indiv(df,id,event.ord) },
         frequency=     { ch.df <- iFrequency2Indiv(df,freq)     },
         individual=    { ch.df <- iIndividual2Indiv(df,id)      },
         MARK=          { ch.df <- iMark2Indiv(df,freq)          },
         RMark=,marked= { ch.df <- iRMark2Indiv(df,id,freq)      }
         ) # end in.type switch

  ## Convert to the output types
  switch(out.type,
         event=         { ch.df <- iOutEvent(ch.df,id)                                        },
         individual=    { ch.df <- iOutIndividual(ch.df,id,var.lbls,var.lbls.pre,include.id)  },
         frequency=     { ch.df <- iOutFrequency(ch.df,var.lbls,var.lbls.pre)                 },
         MARK=          { ch.df <- iOutMARK(ch.df)                                            },
         RMark=,marked= { ch.df <- iOutRMark(ch.df,include.id)                                }
         ) # end out.type switch
  ## return the new data.frame
  ch.df
}



########################################################################
## Internal functions to convert from one format to an internal
##   individual format.  Thus, each function below returns a data.frame
##   with an id variable in the first column and individual capture
##   histories in all other columns.
########################################################################

iEvent2Indiv <- function(df,id,event.ord) {
  # See if there is an id variable
  if (is.null(id)) stop("No variable with unique fish identification information given in 'id'.",call.=FALSE)
  # All other variables are "events"
  event <- names(df)[which(names(df)!=id)]
  # Make a table of ids by events
  # Control the event order if told to do so by the user
  if (!is.null(event.ord)) {
    df$evento <- ordered(df[,event],levels=event.ord)
    ch.tab <- table(df[,id],df$evento)
  } else {
    ch.tab <- table(df[,id],df[,event])
  }
  # Turn the table into a data.frame and rename the variables
  tmp <- as.data.frame(ch.tab)
  names(tmp) <- c("id","event","freq")
  # Unstack and add rownames to data.frame
  tmp <- unstack(tmp,freq~event)
  tmp <- data.frame(rownames(ch.tab),tmp)
  if (is.null(id)) names(tmp)[1] <- "id"
  else names(tmp)[1] <- id
  # force id to be a character (rather than a factor as from as.data.frame)
  tmp[,1] <- as.character(tmp[,1])
  # return the new data.frame
  tmp
}

iFrequency2Indiv <- function(df,freq) {
  if (is.null(freq)) {
    warning("No 'freq' given; assumed frequencies were 1 for each capture history.",call.=FALSE)
    nfreq <- rep(1,nrow(df))
  } else {
    # isolate frequencies and create a df without them
    nfreq <- df[,freq]
    df <- df[,-which(names(df)==freq)]
  }
  tmp <- matrix(NA,ncol=ncol(df),nrow=sum(nfreq))
  for (i in 1:ncol(df)) tmp[,i] <- rep(df[,i],nfreq)
  tmp <- as.data.frame(tmp)
  # add an "id" variable in the first column
  tmp <- data.frame(1:nrow(tmp),tmp)
  names(tmp)[1] <- "id"
  tmp
}

iIndividual2Indiv <- function(df,id) {
  # make sure id is the first variable
  if (!is.null(id)) {
    tmp <- df[,c(which(names(df)==id),which(names(df)!=id))]
  } else {
    tmp <- data.frame(1:nrow(df),df)
    names(tmp)[1] <- "id"
  }
  tmp
}

iMark2Indiv <- function(df,freq) {
  # remove ";" from last column
  df[,ncol(df)] <- sub(";","",df[,ncol(df)])
  # isolate frequencies and capture histories
  if (is.null(freq)) {
    warning("No 'freq' given; assumed frequencies were 1 for each capture history.",call.=FALSE)
    nfreq <- rep(1,nrow(df))
  } else {
    # isolate frequencies and create a df without them
    # make sure frequencies are numeric and capture history is character
    nfreq <- as.numeric(df[,freq])
    df <- as.character(df[,-which(names(df)==freq)])
  }
  # separate the capture history string to variables
  chv <- rep(df,nfreq)
  tmp <- matrix(NA,ncol=nchar(chv[1]),nrow=length(chv))
  for (i in 1:length(chv)) {
    ch1 <- as.numeric(noquote(unlist(strsplit(chv[i],""))))
    tmp[i,] <- ch1
  }
  tmp <- as.data.frame(tmp)
  # add an "id" variable in the first column
  tmp <- data.frame(1:nrow(tmp),tmp)
  names(tmp)[1] <- "id"
  tmp
}

iRMark2Indiv <- function(df,id,freq) {
  # can't supply both id and freq
  if (!is.null(id) & !is.null(freq)) stop("Only one of 'id' or 'freq' can be used with the RMark format.",call.=FALSE)
  # if neither id nor freq then create a freq=1 column and use it
  if (is.null(id) & is.null(freq)) {
    warning("No 'freq' or 'id' given; assumed frequencies were 1 for each capture history.",call.=FALSE)
    df$freq <- rep(1,nrow(df))
    freq <- "freq"
  }
  # Handle when an id variable is given
  if (!is.null(id)) {
    # isolate id and ch variables
    v.id <- df[,id]
    v.ch <- as.character(df[,which(names(df)!=id)])
    # separate the capture history string to variables
    tmp <- matrix(NA,ncol=nchar(v.ch[1]),nrow=length(v.ch))
    for (i in 1:length(v.ch)) {
      ch1 <- as.numeric(noquote(unlist(strsplit(v.ch[i],""))))
      tmp[i,] <- ch1
    }
    tmp <- as.data.frame(tmp)
  } else {
    # Handle when a freq variable is given
    # isolate frequencies and ch variables
    nfreq <- df[,freq]
    v.ch <- as.character(df[,which(names(df)!=freq)])
    # separate the capture history string to variables
    v.ch <- rep(v.ch,nfreq)
    tmp <- matrix(NA,ncol=nchar(v.ch[1]),nrow=length(v.ch))
    for (i in 1:length(v.ch)) {
      ch1 <- as.numeric(noquote(unlist(strsplit(v.ch[i],""))))
      tmp[i,] <- ch1
    }
    tmp <- as.data.frame(tmp)
    # create an id variable to add on later
    v.id <- 1:nrow(tmp)
    id <- "id"
  }
  # add an "id" variable in the first column
  tmp <- data.frame(v.id,tmp)
  names(tmp)[1] <- id
  tmp
}


########################################################################
## Internal functions to convert from the internal individual format
##   returned by the in.type internal functions above to one of the
##   output formats.  Each function that begins with iOut returns a
##   data.frame in the proper format.  The other functions produce
##   intermediate objects.
########################################################################
iOutEvent <- function(ch.df,id) {
  # get total sample size as sum of all values in capture history
  n <- sum(ch.df[,-1])
  # get number of events
  n.ev <- ncol(ch.df)-1
  # initiate vectors for id and events
  v.id <- NULL
  v.ev <- NULL
  # Loop through events creating list of ids and event numbers
  for (i in 1:n.ev) {
    tmp <- ch.df[as.logical(ch.df[,i+1]),1]
    v.id <- c(v.id,tmp)
    v.ev <- c(v.ev,rep(i,length(tmp)))
  }
  # put ids and events together in a data.frame
  tmp <- data.frame(v.id,v.ev)
  # and name the colums
  if (is.null(id)) names(tmp) <- c("id","event")
  else names(tmp) <- c(id,"event")
  # return the new data.frame
  tmp
}

iOutFrequency <- function(ch.df,var.lbls,var.lbls.pre) {
  ch.df <- iPrepCapHistSum(ch.df)
  ch.df1 <- matrix(NA,ncol=nchar(as.character(ch.df[1,1])),nrow=nrow(ch.df))
  for (i in 1:nrow(ch.df)) {
    ch1 <- as.numeric(noquote(unlist(strsplit(as.character(ch.df[i,1]),""))))
    ch.df1[i,] <- ch1
  }
  ch.df <- data.frame(ch.df1,ch.df[,"freq"])
  if (length(var.lbls) >= (ncol(ch.df)-1)) {
    var.lbls <- var.lbls[1:(ncol(ch.df)-1)]
  } else {
    if (!is.null(var.lbls)) warning("Too few labels in 'var.lbls'; default labels will be used.",call.=FALSE)
    var.lbls <- c(paste(var.lbls.pre,1:(ncol(ch.df)-1),sep=""))
  }
  names(ch.df) <- c(var.lbls,"freq")
  ch.df
}

iOutIndividual <- function(ch.df,id,var.lbls,var.lbls.pre,include.id) {
  # names the variables
  names(ch.df) <- iMakeVarLabels(ch.df,id,var.lbls,var.lbls.pre)
  # decide to remove the id variable or not
  if (!include.id) ch.df <- ch.df[,-1]
  ch.df
}

iOutRMark <- function(ch.df,include.id) {
  # isolate the id variable if it is going to be included at the end
  if (include.id) idtmp <- ch.df[,1]
  # remove id column from ch.df
  chtmp <- ch.df[-1]
  # combine the capture histories into a string
  ch <- apply(as.matrix(chtmp),1,paste,sep="",collapse="")
  dftmp <- data.frame(ch=ch)
  dftmp$ch <- as.character(dftmp$ch)
  # add id variable back on if asked for
  if (include.id) {
    dftmp <- data.frame(idtmp,dftmp)
    names(dftmp)[1] <- names(ch.df)[1]
  }
  # return the new data.frame
  dftmp
}

iOutMARK <- function(ch.df) {
  ch.df <- iPrepCapHistSum(ch.df)
  ch.df[,ncol(ch.df)] <- paste(ch.df[,ncol(ch.df)],";",sep="")
  names(ch.df)[1] <- "ch"
  ch.df
}

iMakeVarLabels <- function(ch.df,id,var.lbls,var.lbls.pre) {
  # If var.lbls is longer than needed then truncate the vector
  if (length(var.lbls) >= ncol(ch.df)) {
    var.lbls <- var.lbls[1:ncol(ch.df)]
  } else {
    # Too few var.lbls given, warn user and then make new ones
    if (!is.null(var.lbls)) warning("Too few labels in 'var.lbls'; default labels will be used.",call.=FALSE)
    # Make default labels
    var.lbls <- c(names(ch.df)[1],paste(var.lbls.pre,1:(ncol(ch.df)-1),sep=""))
  }
}

iPrepCapHistSum <- function(ch.df) {
  # get capture history summary without the id column
  chsum <- capHistSum(ch.df,cols=2:ncol(ch.df))
  # convert to a data.frame and re-label columns
  ch.df <- as.data.frame(chsum$caphist)
  rownames(ch.df) <- 1:nrow(ch.df)
  colnames(ch.df) <- c("caphist","freq")
  # return the data.frame
  ch.df
}
