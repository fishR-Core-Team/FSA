#'Convert between different capture history recording types.
#'
#'This function can be used to convert between four capture history recording
#'types -- by event, FSA, MARK, and Rcapture.  The primary usage is to convert
#'to the FSA format from these other formats for use in \code{\link{capHistSum}}.
#'
#'The \code{\link{capHistSum}} function requires capture histories to
#'be recorded in a specific format.  This format, called the \sQuote{FSA}
#'format, is in a data frame with (at least) as many columns as sample events
#'and as many rows as individually marked or tagged individuals.  Each cell in
#'the data frame contains a \sQuote{0} if the animal of that row was NOT seen
#'in the event of that column and a \sQuote{1} if the animal of that row WAS
#'seen in the event of that column.  For example, suppose that four fish were
#'marked and four sampling events occurred.  Further suppose that fish
#'\sQuote{17} was captured on the first two events, fish \sQuote{18} was
#'captured on the first and third events, fish \sQuote{19} was captured on only
#'the third event, fish \sQuote{20} was captured on only the fourth event, and
#'fish \sQuote{21} was captured on the first and second events.  The
#'\sQuote{FSA} capture history for these data would look like:
#'
#'\tabular{ccccc}{
#'id \tab Event1 \tab Event2 \tab Event3 \tab Event4 \cr
#'17 \tab 1 \tab 1 \tab 0 \tab 0 \cr
#'18 \tab 1 \tab 0 \tab 1 \tab 0 \cr
#'19 \tab 0 \tab 0 \tab 1 \tab 0 \cr
#'20 \tab 0 \tab 0 \tab 0 \tab 1 \cr 
#'21 \tab 1 \tab 1 \tab 0 \tab 0 \cr
#'}
#'
#'Another common format for capture histories is the \sQuote{'event'} format.  
#'This format consists of a column that corresponds to the individual animal
#'(likely a tag number) and a column that identifies the event in which this animal
#'was observed.  For example, the situation from above would look like:
#'
#'\tabular{cc}{
#'id \tab event \cr
#'17 \tab 1 \cr
#'18 \tab 1 \cr
#'21 \tab 1 \cr
#'17 \tab 2 \cr
#'21 \tab 2 \cr
#'18 \tab 3 \cr
#'19 \tab 3 \cr
#'20 \tab 4 \cr
#'}
#'
#'Program MARK is probably the \dQuote{gold-standard} software for analyzing
#'complex capture history information.  In the \sQuote{MARK} format the 0s and 1s
#'of the capture histories are combined together as a string without any spaces
#'and an ending semicolon.  Thus, the \sQuote{MARK} format has the capture history
#'strings in one column with an additional column that contains the frequency of
#'individuals that exhibited the various capture histories.  For example, the
#'situation from above would look like:
#'
#'\tabular{cc}{
#'caphist \tab Freq \cr
#'0001; \tab 1 \cr
#'0010; \tab 1 \cr
#'1010; \tab 1 \cr
#'1100; \tab 2 \cr
#'}
#'
#'Rcapture is a an R package that fits some of the same models found in Program
#'MARK.  The \sQuote{Rcapture} format has capture histories in separate columns
#'as in the \sQuote{FSA} format and the frequency of individuals with the various
#'capture histories recorded in a frequency column as in the \sQuote{MARK} format.
#'For example, the situation from above would look like:
#'
#'\tabular{ccccc}{
#'Event1 \tab Event2 \tab Event3 \tab Event4 \tab Freq \cr
#'1 \tab 1 \tab 0 \tab 0 \tab 2 \cr
#'1 \tab 0 \tab 1 \tab 0 \tab 1 \cr
#'0 \tab 0 \tab 1 \tab 0 \tab 1 \cr
#'0 \tab 0 \tab 0 \tab 1 \tab 1 \cr
#'}
#'
#'@param df A data.frame that contains the capture histories (and, perhaps, other
#'information).  See details.
#'@param event A string or numeric that indicates the column in \code{df} that
#'contains the capture event information.  This argument is only used if
#'\code{in.type=="event"}.
#'@param id A string or numeric that indicates the column in \code{df} that
#'contains the unique identification for an individual.  This argument is only
#'used if \code{in.type=="event"}.
#'@param event.ord A string that contains the list of ordered levels in the
#'\code{event} variable of \code{df} to be used when converting using
#'\code{in.type=="event"}.
#'@param mch A string or numeric that indicates the column in \code{df} that
#'contains the MARK capture history codes.  This argument is only used if
#'\code{in.type=="MARK"}.
#'@param cols A string or numeric that indicates the columns in \code{df} that
#'contain the Rcapture or FSA capture history codes (each column is an
#'individual sampling event -- see details).  This argument is only used if
#'\code{in.type=="Rcapture"} or \code{in.type=="FSA"}.
#'@param freq A string or numeric that indicates the columns in \code{df} that
#'contain the frequency of individuals corresponding to a MARK or Rcapture
#'capture history.  This argument is only used if \code{in.type=="MARK"} or
#'\code{in.type=="Rcapture"}.
#'@param in.type A string that indicates the type of capture history format to
#'convert FROM.
#'@param out.type A string that indicates the type of capture history format to
#'convert TO.
#'@param var.lbls A vector of strings used to label the columns that contains the
#'returned FSA or Rcapture capture histories.  This argument is only used if
#'\code{in.type=="Rcapture"} or \code{in.type=="FSA"}.  If \code{var.lbls=NULL}
#'or the length is different then the number of events then default labels using
#'\code{var.lbls.pre} will be used.
#'@param var.lbls.pre A string used as a prefix for the labels of the columns
#'that contains the returned FSA or Rcapture capture histories.  This prefix will
#'be appended with a number corresponding to the sample event.  This argument
#'is only used if \code{in.type=="Rcapture"} or \code{in.type=="FSA"} and will
#'be ignored if a proper vector is given in \code{var.lbls}.
#'@return A data frame of the proper type given in \code{out.type} is returned.
#'See details.
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@note This function assumes that all unmarked captured fish are marked and
#'returned to the population (i.e., no losses at the time of marking are
#'allowed).  In addition it does not currently allow multiple frequencies as
#'can be used in MARK.
#'@seealso \code{\link{capHistSum}}, \code{\link{mrClosed}}, \code{\link{mrOpen}}
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/MRClosed.pdf}, 
#'\url{https://sites.google.com/site/fishrfiles/gnrl/MROpen.pdf}
#'@export
#'@keywords manip
#'@examples
#'## A small example of 'event' format -- fish ID followed by capture year
#'( ex1 <- data.frame(id=c(17,18,21,17,21,18,19,20),yr=c(1987,1987,1987,1988,1988,1989,1989,1990)) )
#'# convert to 'FSA' format
#'( ex1a <- capHistConvert(ex1,event="yr",id="id") )
#'# convert to 'MARK' format
#'( ex1b <- capHistConvert(ex1,event="yr",id="id",out.type="MARK") )
#'# convert to 'Rcapture' format
#'( ex1c <- capHistConvert(ex1,event="yr",id="id",out.type="Rcapture") )
#'
#'## A small example of 'MARK' format -- capture history followed by frequency
#'( ex2 <- data.frame(mch=c("10101;","10001;","01010;"),freq=c(3,1,2)) )
#'# convert to 'FSA' format
#'( ex2a <- capHistConvert(ex2,in.type="MARK",mch="mch",freq="freq") )
#'# convert to 'Rcapture' format
#'( ex2b <- capHistConvert(ex2,in.type="MARK",mch="mch",freq="freq",out.type="Rcapture") )
#'
#'## A larger example of 'Rcapture' format
#'#require(Rcapture)
#'#data(bunting)
#'#head(bunting)
#'# convert to 'FSA' format
#'#ex3a <- capHistConvert(bunting,in.type="Rcapture",cols=1:8,freq="freq")
#'#head(ex3a)
#'# convert to 'MARK' format
#'#ex3b <- capHistConvert(bunting,in.type="Rcapture",cols=1:8,freq="freq",out.type="MARK")
#'#head(ex3b)
#'# convert converted 'FSA' back to 'MARK' format
#'#ex3c <- capHistConvert(ex3a,in.type="FSA",cols=1:8,out.type="MARK")
#'#head(ex3c)
#'# convert converted 'FSA' back to 'Rcapture' format
#'#ex3d <- capHistConvert(ex3a,in.type="FSA",cols=1:8,out.type="Rcap",var.lbls.pre="Sample")
#'#head(ex3d)
#'
#'
#'## A small example of 'MARK' format with two groups -- males and females
#'( ex4 <- data.frame(mch=c("100101;","100001;"),male=c(3,1),female=c(2,2)) )
#'# convert to 'FSA' format
#'( ex4m <- capHistConvert(ex4,in.type="MARK",mch="mch",freq="male") )
#'( ex4f <- capHistConvert(ex4,in.type="MARK",mch="mch",freq="female") )
#'require(gdata)   # for combine()
#'( ex4a <- combine(ex4m,ex4f,names=c("male","female")) )
#'
capHistConvert <- function(df,event=NULL,id=NULL,event.ord=NULL,mch=NULL,cols=NULL,freq=NULL,
                       in.type=c("event","MARK","Rcapture","FSA"),
                       out.type=c("FSA","MARK","RMark","Rcapture"),
                       var.lbls=NULL,var.lbls.pre="Event") {

  in.type <- match.arg(in.type)
  out.type <- match.arg(out.type)
  df <- as.data.frame(df)
  
 ### convert from by_event form to ind_ch form
  if (in.type=="event") {
    if (is.null(event)) stop("No capture event variable given in 'event'.",call.=FALSE)
    if (is.null(id)) stop("No animal unique identification variable given in 'id'.",call.=FALSE)
    if (!is.null(event.ord)) {
      df$evento <- ordered(df[,event],levels=event.ord)
      ch.tab <- table(df[,id],df$evento)
    } else ch.tab <- table(df[,id],df[,event])
    ch.df <- as.data.frame(ch.tab)
    names(ch.df) <- c("id","event","Freq")
    ch.df <- unstack(ch.df,Freq~event)
    ch.df <- data.frame(id=rownames(ch.tab),ch.df)
  }
  
 ### convert from MARK form to ind_ch form
  if (in.type=="MARK") {
    if (is.null(mch)) stop("No capture history variable given in 'mch'.",call.=FALSE)
    if (is.null(freq)) {
      warning("No frequency variable given in 'freq', assumed frequencies were 1 for each capture history.",call.=FALSE)
      d$Freq <- rep(1,dim(df)[1])
      freq <- "Freq"
    }
    if (length(grep(";",df[,mch]))>0) df[,mch] <- sub(";","",df[,mch])  # if ';' in ch then remove
    chv <- rep(df[,mch],df[,freq])
    ch.df <- matrix(NA,ncol=nchar(chv[1]),nrow=length(chv))
    for (i in 1:length(chv)) {
      ch1 <- as.numeric(noquote(unlist(strsplit(chv[i],""))))
      ch.df[i,] <- ch1
    }
    ch.df <- as.data.frame(ch.df)
  }
 
 ### convert from Rcapture form to ind_ch form
  if (in.type=="Rcapture") {
    ch.df <- matrix(NA,ncol=length(cols),nrow=sum(df[,freq]))
    for (i in 1:length(cols)) ch.df[,i] <- rep(df[,cols[i]],df[,freq])
    ch.df <- as.data.frame(ch.df)
  }      
  
 ### output types
  if (out.type=="FSA") {
    if (length(var.lbls) >= ncol(ch.df)) var.lbls <- var.lbls[1:ncol(ch.df)]
      else {
        if (!is.null(var.lbls)) warning("Too few variable labels sent in 'var.lbls', default labels will be used.",call.=FALSE)
        if (in.type=="event") var.lbls <- c(id,paste(var.lbls.pre,1:(ncol(ch.df)-1),sep=""))
          else var.lbls <- paste(var.lbls.pre,1:ncol(ch.df),sep="")
      }
    names(ch.df) <- var.lbls
  } else if (out.type=="RMark") { ## this was new code for converting to RMark
    if (in.type=="FSA") ifelse(is.null(cols),ch.df <- df, ch.df <- df[,cols])
    ch <- apply(as.matrix(ch.df),1,paste,sep="",collapse="")
    ch.df <- data.frame(ch=ch)
    ch.df$ch <- as.character(ch.df$ch) ##
  } else {
    if (in.type=="FSA") chsum <- capHistSum(df,cols=cols)
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
