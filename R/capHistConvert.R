#' @title Convert between different capture history recording types.
#'
#' @description Use to convert between four capture history recording types -- \dQuote{event}, \dQuote{individual}, \dQuote{frequency}, and \dQuote{MARK}.  The primary usage is to convert to the \dQuote{individual} format from the other formats for use in \code{\link{capHistSum}}.
#'
#' @details \code{\link{capHistSum}} requires capture histories to be recorded in the \dQuote{individual} format.  In this format, the data frame contains (at least) as many columns as sample events and as many rows as individually tagged fish.  Each cell in the data frame contains a \sQuote{0} if the fish of that row was NOT seen in the event of that column and a \sQuote{1} if the fish of that row WAS seen in the event of that column.  For example, suppose that four fish were marked and four sampling events occurred.  Further suppose that fish \sQuote{17} was captured on the first two events, fish \sQuote{18} was captured on the first and third events, fish \sQuote{19} was captured on only the third event, fish \sQuote{20} was captured on only the fourth event, and fish \sQuote{21} was captured on the first and second events.  The \dQuote{individual} capture history for these data looks like:
#'
#' \tabular{ccccc}{
#' id \tab Event1 \tab Event2 \tab Event3 \tab Event4 \cr
#' 17 \tab 1 \tab 1 \tab 0 \tab 0 \cr
#' 18 \tab 1 \tab 0 \tab 1 \tab 0 \cr
#' 19 \tab 0 \tab 0 \tab 1 \tab 0 \cr
#' 20 \tab 0 \tab 0 \tab 0 \tab 1 \cr 
#' 21 \tab 1 \tab 1 \tab 0 \tab 0 \cr
#' }
#'
#' The data frame for the \dQuote{frequency} format has unique capture histories in separate columns as in the \dQuote{individual} format but it includes a column that contains the frequency of individuals with the capture history of that row.  The same data from above looks like:
#'
#' \tabular{ccccc}{
#' Event1 \tab Event2 \tab Event3 \tab Event4 \tab Freq \cr
#' 1 \tab 1 \tab 0 \tab 0 \tab 2 \cr
#' 1 \tab 0 \tab 1 \tab 0 \tab 1 \cr
#' 0 \tab 0 \tab 1 \tab 0 \tab 1 \cr
#' 0 \tab 0 \tab 0 \tab 1 \tab 1 \cr
#' }
#'
#' The data frame for the \dQuote{event} format consists of a column that corresponds to the individual fish (likely a tag number) and a column that identifies the event in which this fish was observed.  For example, the same data from above looks like:
#'
#' \tabular{cc}{
#' id \tab event \cr
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
#' MARK is the \dQuote{gold-standard} software for analyzing complex capture history information.  In the \dQuote{MARK} format the 0s and 1s of the capture histories are combined together as a string without any spaces and an ended with a semicolon.  Thus, the \dQuote{MARK} format has the capture history strings in one column with an additional column that contains the frequency of individuals that exhibited the various capture histories.  The sam data from above looks like:
#'
#' \tabular{cc}{
#' caphist \tab Freq \cr
#' 0001; \tab 1 \cr
#' 0010; \tab 1 \cr
#' 1010; \tab 1 \cr
#' 1100; \tab 2 \cr
#' }
#'
#' @param df A data.frame that contains the capture histories (and, perhaps, other information).  See details.
#' @param event A string or numeric that indicates the column in \code{df} that contains the capture event information.  This argument is only used if \code{in.type=="event"}.
#' @param id A string or numeric that indicates the column in \code{df} that contains the unique identification for an individual.  This argument is only used if \code{in.type=="event"}.
#' @param event.ord A string that contains the list of ordered levels in the \code{event} variable of \code{df} to be used when converting using \code{in.type=="event"}.
#' @param mch A string or numeric that indicates the column in \code{df} that contains the MARK capture history codes.  This argument is only used if \code{in.type=="MARK"}.
#' @param cols A string or numeric that indicates the columns in \code{df} that contain the frequency or individual capture history codes (each column is an individual sampling event -- see details).  This argument is only used if \code{in.type=="frequency"} or \code{in.type=="individual"}.
#' @param freq A string or numeric that indicates the columns in \code{df} that contain the frequency of individuals corresponding to a MARK or frequency capture history.  This argument is only used if \code{in.type=="MARK"} or \code{in.type=="frequency"}.
#' @param in.type A string that indicates the type of capture history format to convert FROM.
#' @param out.type A string that indicates the type of capture history format to convert TO.
#' @param var.lbls A vector of strings used to label the columns that contains the returned individual or frequency capture histories.  This argument is only used if \code{in.type=="frequency"} or \code{in.type=="individual"}.  If \code{var.lbls=NULL} or the length is different then the number of events then default labels using \code{var.lbls.pre} will be used.
#' @param var.lbls.pre A string used as a prefix for the labels of the columns that contains the returned individual or frequency capture histories.  This prefix will be appended with a number corresponding to the sample event.  This argument is only used if \code{in.type=="frequency"} or \code{in.type=="individual"} and will be ignored if a proper vector is given in \code{var.lbls}.
#'
#' @return A data frame of the proper type given in \code{out.type} is returned.  See details.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @note This function assumes that all unmarked captured fish are marked and returned to the population (i.e., no losses at the time of marking are allowed).  In addition it does not currently allow multiple frequencies as can be used in MARK.
#'
#' @seealso \code{\link{capHistSum}}, \code{\link{mrClosed}}, \code{\link{mrOpen}}
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/MRClosed.pdf}, \url{https://sites.google.com/site/fishrfiles/gnrl/MROpen.pdf}
#'
#' @keywords manip
#'
#' @examples
#' ## A small example of 'event' format -- fish ID followed by capture year
#' ( ex1 <- data.frame(id=c(17,18,21,17,21,18,19,20),yr=c(1987,1987,1987,1988,1988,1989,1989,1990)) )
#' # convert to 'individual' format
#' ( ex1a <- capHistConvert(ex1,event="yr",id="id") )
#' # convert to 'MARK' format
#' ( ex1b <- capHistConvert(ex1,event="yr",id="id",out.type="MARK") )
#' # convert to 'frequency' format
#' ( ex1c <- capHistConvert(ex1,event="yr",id="id",out.type="frequency") )
#'
#' ## A small example of 'MARK' format -- capture history followed by frequency
#' ( ex2 <- data.frame(mch=c("10101;","10001;","01010;"),freq=c(3,1,2)) )
#' # convert to 'individual' format
#' ( ex2a <- capHistConvert(ex2,in.type="MARK",mch="mch",freq="freq") )
#' # convert to 'frequency' format
#' ( ex2b <- capHistConvert(ex2,in.type="MARK",mch="mch",freq="freq",out.type="frequency") )
#'
#' ## ONLY RUN IN INTERACTIVE MODE
#' if (interactive()) {
#' 
#' ## A larger example of 'frequency' format
#' require(Rcapture)
#' data(bunting)
#' head(bunting)
#' # convert to 'individual' format
#' ex3a <- capHistConvert(bunting,in.type="frequency",cols=1:8,freq="freq")
#' head(ex3a)
#' # convert to 'MARK' format
#' ex3b <- capHistConvert(bunting,in.type="frequency",cols=1:8,freq="freq",out.type="MARK")
#' head(ex3b)
#' # convert converted 'individual' back to 'MARK' format
#' ex3c <- capHistConvert(ex3a,in.type="individual",cols=1:8,out.type="MARK")
#' head(ex3c)
#' # convert converted 'individual' back to 'frequency' format
#' ex3d <- capHistConvert(ex3a,in.type="individual",cols=1:8,out.type="frequency",var.lbls.pre="Sample")
#' head(ex3d)
#' 
#' }
#'
#' ## A small example of 'MARK' format with two groups -- males and females
#' ( ex4 <- data.frame(mch=c("100101;","100001;"),male=c(3,1),female=c(2,2)) )
#' # convert to 'individual' format
#' ( ex4m <- capHistConvert(ex4,in.type="MARK",mch="mch",freq="male") )
#' ( ex4f <- capHistConvert(ex4,in.type="MARK",mch="mch",freq="female") )
#' require(gdata)   # for combine()
#' ( ex4a <- combine(ex4m,ex4f,names=c("male","female")) )
#'
#' @export
capHistConvert <- function(df,event=NULL,id=NULL,event.ord=NULL,
                           cols=NULL,freq=NULL,mch=NULL,
                           in.type=c("event","frequency","individual","MARK"),
                           out.type=c("individual","frequency","MARK","RMark"),
                           var.lbls=NULL,var.lbls.pre="Event") {

  in.type <- match.arg(in.type)
  out.type <- match.arg(out.type)
  df <- as.data.frame(df)
  
 ### convert from event form to individual form
  switch(in.type,
         frequency={ ch.df <- iFrequency2Individual(df,cols,freq)      },
         event=    { ch.df <- iEvent2Individual(df,event,id,event.ord) },
         MARK=     { ch.df <- iMark2Individual(df,freq,mch)            }
         ) # end in.type switch
  
 ### output types
  if (out.type=="individual") {
    if (length(var.lbls) >= ncol(ch.df)) var.lbls <- var.lbls[1:ncol(ch.df)]
      else {
        if (!is.null(var.lbls)) warning("Too few variable labels sent in 'var.lbls', default labels will be used.",call.=FALSE)
        if (in.type=="event") var.lbls <- c(id,paste(var.lbls.pre,1:(ncol(ch.df)-1),sep=""))
          else var.lbls <- paste(var.lbls.pre,1:ncol(ch.df),sep="")
      }
    names(ch.df) <- var.lbls
  } else if (out.type=="RMark") { ## this was new code for converting to RMark
    if (in.type=="individual") ifelse(is.null(cols),ch.df <- df, ch.df <- df[,cols])
    ch <- apply(as.matrix(ch.df),1,paste,sep="",collapse="")
    ch.df <- data.frame(ch=ch)
    ch.df$ch <- as.character(ch.df$ch)
  } else {
    if (in.type=="individual") chsum <- capHistSum(df,cols=cols)
      else if(in.type=="event") chsum <- capHistSum(ch.df,cols=2:ncol(ch.df))
        else chsum <- capHistSum(ch.df,1:ncol(ch.df))
    ch.df <- as.data.frame(chsum$caphist)
    rownames(ch.df) <- 1:nrow(ch.df)
    colnames(ch.df)[1] <- "caphist"
    if (out.type=="MARK") ch.df[,1] <- paste(ch.df[,1],";",sep="")
      else {
        ch.df1 <- matrix(NA,ncol=nchar(as.character(ch.df[1,1])),nrow=nrow(ch.df))
        for (i in 1:nrow(ch.df)) {
          ch1 <- as.numeric(noquote(unlist(strsplit(as.character(ch.df[i,1]),""))))
          ch.df1[i,] <- ch1
        }
        ch.df <- data.frame(ch.df1,ch.df[,"Freq"])
        if (length(var.lbls) >= (ncol(ch.df)-1)) var.lbls <- var.lbls[1:(ncol(ch.df)-1)]
          else {
            if (!is.null(var.lbls)) warning("Too few variable labels sent in 'var.lbls', default labels will be used.",call.=FALSE)
            var.lbls <- c(paste(var.lbls.pre,1:(ncol(ch.df)-1),sep=""))
          }
        names(ch.df) <- c(var.lbls,"Freq")
      }
  }

ch.df
}



########################################################################
## Internal functions to convert from one format to individual format
##   Each function returns a data.frame of individual capture histories
########################################################################
iEvent2Individual <- function(df,event,id,event.ord) {
  if (is.null(event)) stop("No variable with capture event information given in 'event'.",call.=FALSE)
  if (is.null(id)) stop("No variable with unique fish identification information given in 'id'.",call.=FALSE)
  if (!is.null(event.ord)) {
    df$evento <- ordered(df[,event],levels=event.ord)
    ch.tab <- table(df[,id],df$evento)
  } else {
    ch.tab <- table(df[,id],df[,event])
  }
  tmp <- as.data.frame(ch.tab)
  names(tmp) <- c("id","event","freq")
  tmp <- unstack(tmp,freq~event)
  tmp <- data.frame(id=rownames(ch.tab),tmp)
}


iFrequency2Individual <- function(df,cols,freq) {
  tmp <- matrix(NA,ncol=length(cols),nrow=sum(df[,freq]))
  for (i in 1:length(cols)) tmp[,i] <- rep(df[,cols[i]],df[,freq])
  tmp <- as.data.frame(tmp)
}


iMark2Individual <- function(df,freq,mch) {
  if (is.null(mch)) stop("No capture history variable given in 'mch'.",call.=FALSE)
  if (is.null(freq)) {
    warning("No frequency variable given in 'freq', assumed frequencies were 1 for each capture history.",call.=FALSE)
    d$Freq <- rep(1,dim(df)[1])
    freq <- "freq"
  }
  # if ';' in ch then remove
  if (length(grep(";",df[,mch]))>0) df[,mch] <- sub(";","",df[,mch])
  chv <- rep(df[,mch],df[,freq])
  tmp <- matrix(NA,ncol=nchar(chv[1]),nrow=length(chv))
  for (i in 1:length(chv)) {
    ch1 <- as.numeric(noquote(unlist(strsplit(chv[i],""))))
    tmp[i,] <- ch1
  }
  tmp <- as.data.frame(tmp)
}
