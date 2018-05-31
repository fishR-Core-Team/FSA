#' @title Convert between capture history data.frame formats.
#'
#' @description Use to convert between simple versions of several capture history data.frame formats -- \dQuote{individual}, \dQuote{frequency}, \dQuote{event}, \dQuote{MARK}, and \dQuote{RMark}. The primary use is to convert to the \dQuote{individual} format for use in \code{\link{capHistSum}}.
#'
#' @details \code{\link{capHistSum}} requires capture histories to be recorded in the \dQuote{individual} format. In this format, the data frame contains (at least) as many columns as sample events and as many rows as individually tagged fish. Optionally, the data.frame may also contain a column with unique fish identifiers (e.g., tag numbers). Each cell in the capture history portion of the data.frame contains a \sQuote{0} if the fish of that row was NOT seen in the event of that column and a \sQuote{1} if the fish of that row WAS seen in the event of that column. For example, suppose that five fish were marked on four sampling events; fish \sQuote{17} was captured on the first two events; fish \sQuote{18} was captured on the first and third events; fish \sQuote{19} was captured on only the third event; fish \sQuote{20} was captured on only the fourth event; and fish \sQuote{21} was captured on the first and second events. The \dQuote{individual} capture history date.frame for these data looks like:
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
#' The \dQuote{frequency} format data.frame (this format is used in \pkg{Rcapture}) has unique capture histories in separate columns, as in the \dQuote{individual} format, but also includes a column with the frequency of individuals that had the capture history of that row. It will not contain a fish identifier variable. The same data from above looks like:
#'
#' \tabular{ccccc}{
#' event1 \tab event2 \tab event3 \tab event4 \tab freq \cr
#' 1 \tab 1 \tab 0 \tab 0 \tab 2 \cr
#' 1 \tab 0 \tab 1 \tab 0 \tab 1 \cr
#' 0 \tab 0 \tab 1 \tab 0 \tab 1 \cr
#' 0 \tab 0 \tab 0 \tab 1 \tab 1 \cr
#' }
#'
#' The \dQuote{event} format data.frame has a column with the unique fish identifier and a column with the event in which the fish of that row was observed. The same data from above looks like:
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
#' MARK (http://www.phidot.org/software/mark/index.html) is the \dQuote{gold-standard} software for analyzing complex capture history information. In the \dQuote{MARK} format the 0s and 1s of the capture histories are combined together as a string without any spaces. Thus, the \dQuote{MARK} format has the capture history strings in one column with an additional column that contains the frequency of individuals that exhibited the capture history of that row. The final column ends with a semi-colon. The same data from above looks like:
#'
#' \tabular{cc}{
#' ch \tab freq \cr
#' 0001 \tab 1; \cr
#' 0010 \tab 1; \cr
#' 1010 \tab 1; \cr
#' 1100 \tab 2; \cr
#' }
#'
#' The \code{\link[RMark:ABeginnersGuide]{RMark}} and \pkg{marked} are packages used to replace some of the functionality of MARK or to interact with MARK. The \dQuote{RMark} or \dQuote{marked} format requires the capture histories as one string (must be a character string and called \sQuote{ch}), as in the \dQuote{MARK} format, but without the semicolon. The data.frame may be augmented with an identifier for individual fish OR with a frequency variable. If augmented with a unique fish identification variable then the same data from above looks like:
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
#' @param df A data.frame that contains the capture histories and, perhaps, a unique fish identifier or frequency variable. See details.
#' @param cols2use A string or numeric vector that indicates columns in \code{df} to use. Negative numeric values will not use those columns. Cannot use both \code{cols2use} and \code{col2ignore}.
#' @param cols2ignore A string or numeric vector that indicates columns in \code{df} to ignore. Typical columns to ignore are those that are not either in \code{id=} or \code{freq=} or part of the capture history data. Cannot use both \code{cols2use} and \code{col2ignore}.
#' @param in.type A single string that indicates the type of capture history format to convert \bold{FROM}.
#' @param out.type A single string that indicates the type of capture history format to convert \bold{TO}.
#' @param id A string or numeric that indicates the column in \code{df} that contains the unique identifier for an individual fish. This argument is only used if \code{in.type="event"}, \code{in.type="individual"}, or, possibly, \code{in.type="RMark"}.
#' @param freq A string or numeric that indicates the column in \code{df} that contains the frequency of individual fish corresponding to a capture history. This argument is only used if \code{in.type="MARK"}, \code{in.type="frequency"}, or, possibly, \code{in.type="RMark"}.
#' @param event.ord A string that contains a vector of ordered levels to be used when \code{in.type="event"}. The default is to order alphabetically which may not be desirable if, for example, the events are labeled as \sQuote{first}, \sQuote{second}, \sQuote{third}, and \sQuote{fourth}. In this case, use \code{event.ord=c("first","second","third","fourth")}.
#' @param var.lbls A string vector of labels for the columns that contain the returned individual or frequency capture histories. If \code{var.lbls=NULL} or the length is different then the number of events then default labels using \code{var.lbls.pre} will be used. This argument is only used if \code{out.type="frequency"} or \code{out.type="individual"}.
#' @param var.lbls.pre A single string used as a prefix for the labels of the columns that contain the returned individual or frequency capture histories. This prefix will be appended with a number corresponding to the sample event. This argument is only used if \code{out.type="frequency"} or \code{out.type="individual"} and will be ignored if a proper vector is given in \code{var.lbls}.
#' @param include.id A logical that indicates whether a unique fish identifier variable/column should be included in the output data.frame. This argument is only used if \code{out.type="individual"} or \code{out.type="RMark"}.
#'
#' @return A data frame of the proper type given in \code{out.type} is returned. See details.
#'
#' @note The formats as used here are simple in the sense that one is only allowed to have the individual fish identifier or the frequency variable in addition to the capture history information. More complex analyses may use a number of covariates. For these more complex analyses, one should work directly with the \pkg{Rcapture}, \code{\link[RMark:ABeginnersGuide]{RMark}}, or \pkg{marked} packages.
#' 
#' This function also assumes that all unmarked captured fish are marked and returned to the population (i.e., no losses at the time of marking are allowed).
#' 
#' @section Warning: \code{capHistConvert} may give unwanted results if the data are \code{in.type="event"} but there are unused levels for the variable, as would result if the data.frame had been subsetted on the event variable. The unwanted results can be corrected by using \code{droplevels} before \code{capHistConvert}. See the last example for an example.
#'
#' @author Derek H. Ogle, \email{derek@@derekogle.com}
#'
#' @section IFAR Chapter: 9-Abundance from Capture-Recapture Data.
#'
#' @seealso See \code{\link{capHistSum}} to summarize \dQuote{individual} capture histories into a format usable in \code{\link{mrClosed}} and \code{\link{mrOpen}}. Also see \pkg{Rcapture}, \code{\link[RMark:ABeginnersGuide]{RMark}}, or \pkg{marked} packages for handling more complex analyses.
#' 
#' @references Ogle, D.H. 2016. \href{http://derekogle.com/IFAR}{Introductory Fisheries Analyses with R}. Chapman & Hall/CRC, Boca Raton, FL.
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
#' ( ex1.F2Ia <- capHistConvert(ex1.E2F,freq="freq",in.type="frequency",include.id=TRUE) )
#' # to 'Mark' format
#' ( ex1.F2M <- capHistConvert(ex1.E2F,freq="freq",in.type="frequency",
#'                             out.type="MARK") )
#' # to 'RMark' format
#' ( ex1.F2R <- capHistConvert(ex1.E2F,freq="freq",in.type="frequency",
#'                             out.type="RMark") )
#' ( ex1.F2Ra <- capHistConvert(ex1.E2F,freq="freq",in.type="frequency",
#'                              out.type="RMark",include.id=TRUE) )
#' # to 'event' format
#' ( ex1.F2E <- capHistConvert(ex1.E2F,freq="freq",in.type="frequency",
#'                             out.type="event") )
#' 
#' ## convert converted 'MARK' format ...
#' # to 'individual' format
#' ( ex1.M2I <- capHistConvert(ex1.E2M,freq="freq",in.type="MARK") )
#' ( ex1.M2Ia <- capHistConvert(ex1.E2M,freq="freq",in.type="MARK",include.id=TRUE) )
#' # to 'frequency' format
#' ( ex1.M2F <- capHistConvert(ex1.E2M,freq="freq",in.type="MARK",out.type="frequency") )
#' # to 'RMark' format
#' ( ex1.M2R <- capHistConvert(ex1.E2M,freq="freq",in.type="MARK",out.type="RMark") )
#' ( ex1.M2Ra <- capHistConvert(ex1.E2M,freq="freq",in.type="MARK",out.type="RMark",include.id=TRUE) )
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
#' ( ex1.R2I1a <- capHistConvert(ex1.E2R1,freq="freq",in.type="RMark",include.id=TRUE) )
#' # convert this to 'frequency' format
#' ( ex1.R2F1 <- capHistConvert(ex1.E2R1,freq="freq",in.type="RMark",out.type="frequency") )
#' # convert this to 'MARK' format
#' ( ex1.R2M1 <- capHistConvert(ex1.E2R1,freq="freq",in.type="RMark",out.type="MARK") )
#' # convert this to 'event' format
#' ( ex1.R2E1 <- capHistConvert(ex1.E2R1,freq="freq",in.type="RMark",out.type="event") )
#' 
#' 
#' ########################################################################
#' ## A small example using character ids
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
#' # to 'frequency' format
#' ( ex2.I2F <- capHistConvert(ex2.E2I,id="fish",in.type="individual",out.type="frequency") )
#' # to 'MARK' format
#' ( ex2.I2M <- capHistConvert(ex2.E2I,id="fish",in.type="individual",out.type="MARK") )
#' # to 'RMark' format
#' ( ex2.I2R <- capHistConvert(ex2.E2I,id="fish",in.type="individual",out.type="RMark") )
#' # to 'event' format
#' ( ex2.I2E <- capHistConvert(ex2.E2I,id="fish",in.type="individual",out.type="event") )
#' 
#' ## demo use of var.lbls
#' ( ex2.E2Ia <- capHistConvert(ex2,id="fish",in.type="event",var.lbls.pre="Sample") )
#' ( ex2.E2Ib <- capHistConvert(ex2,id="fish",in.type="event",
#'               var.lbls=c("first","second","third","fourth")) )
#'
#' ## demo use of event.ord
#' ( ex2.I2Ea <- capHistConvert(ex2.E2Ib,id="fish",in.type="individual",out.type="event") )
#' ( ex2.E2Ibad <- capHistConvert(ex2.I2Ea,id="fish",in.type="event") )
#' ( ex2.E2Igood <- capHistConvert(ex2.I2Ea,id="fish",in.type="event",
#'                  event.ord=c("first","second","third","fourth")) )
#' 
#' ## ONLY RUN IN INTERACTIVE MODE
#' if (interactive()) {
#' 
#' ########################################################################
#' ## A larger example of 'frequency' format (data from Rcapture package)
#' data(bunting,package="Rcapture")
#' head(bunting)
#' # convert to 'individual' format
#' bun.F2I <- capHistConvert(bunting,in.type="frequency",freq="freq")
#' head(bun.F2I)
#' # convert to 'MARK' format
#' bun.F2M <- capHistConvert(bunting,id="id",in.type="frequency",freq="freq",out.type="MARK")
#' head(bun.F2M)
#' # convert converted 'individual' back to 'MARK' format
#' bun.I2M <- capHistConvert(bun.F2I,id="id",in.type="individual",out.type="MARK")
#' head(bun.I2M)
#' # convert converted 'individual' back to 'frequency' format
#' bun.I2F <- capHistConvert(bun.F2I,id="id",in.type="individual",
#'            out.type="frequency",var.lbls.pre="Sample")
#' head(bun.I2F)
#'
#'
#' ########################################################################
#' ## A larger example of 'marked' or 'RMark' format, but with a covariate
#' ##   and when the covariate is removed there is no frequency or individual
#' ##   fish identifier.
#' data(dipper,package="marked")
#' head(dipper)
#' # isolate males and females
#' dipperF <- subset(dipper,sex=="Female")
#' dipperM <- subset(dipper,sex=="Male")
#' # convert females to 'individual' format
#' dipF.R2I <- capHistConvert(dipperF,cols2ignore="sex",in.type="RMark")
#' head(dipF.R2I)
#' # convert males to 'individual' format
#' dipM.R2I <- capHistConvert(dipperM,cols2ignore="sex",in.type="RMark")
#' head(dipM.R2I)
#' # add sex variable to each data.frame and then combine
#' dipF.R2I$sex <- "Female"
#' dipM.R2I$sex <- "Male"
#' dip.R2I <- rbind(dipF.R2I,dipM.R2I)
#' head(dip.R2I)
#' tail(dip.R2I)
#' 
#' } # end interactive
#'
#'
#' ## An example of problem with unused levels
#' ## Create a set of test data with several groups
#' ( df <- data.frame(fish=c("id17","id18","id21","id17","id21","id18","id19","id20","id17"),
#'                    group=c("B1","B1","B1","B2","B2","B3","B4","C1","C1")) )
#' #  Let's assume the user wants to subset the data from the "B" group
#' ( df1 <- subset(df,group %in% c("B1","B2","B3","B4")) )
#' #  Looks like capHistConvert() is still using the unused factor
#' #  level from group C
#' capHistConvert(df1,id="fish",in.type="event")
#' # use droplevels() to remove the unused groups and no problem
#' df1 <- droplevels(df1)
#' capHistConvert(df1,id="fish",in.type="event")
#'
#' @export
capHistConvert <- function(df,cols2use=NULL,cols2ignore=NULL,
                           in.type=c("frequency","event","individual",
                                     "MARK","marked","RMark"),
                           out.type=c("individual","event","frequency",
                                      "MARK","marked","RMark"),
                           id=NULL,event.ord=NULL,freq=NULL,
                           var.lbls=NULL,var.lbls.pre="event",
                           include.id=ifelse(is.null(id),FALSE,TRUE)) {
  # initial argument checks
  in.type <- match.arg(in.type)
  out.type <- match.arg(out.type)
  if (in.type==out.type) STOP("'in.type' and 'out.type' cannot be the same.")
  # make sure df is a data.frame (could be sent as a matrix)
  df <- as.data.frame(df)
  # change data.frame based on cols2use or cols2ignore
  df <- iHndlCols2UseIgnore(df,cols2use,cols2ignore)

  ## Convert from other forms to individual form
  switch(in.type,
         event=         { ch.df <- iEvent2Indiv(df,id,event.ord) },
         frequency=     { ch.df <- iFrequency2Indiv(df,freq)     },
         individual=    { ch.df <- iIndividual2Indiv(df,id)      },
         MARK=          { ch.df <- iMark2Indiv(df,freq)          },
         RMark=,marked= { ch.df <- iRMark2Indiv(df,id,freq)      }
         ) # end in.type switch

  # names the variables
  names(ch.df) <- iMakeVarLabels(ch.df,in.type,id,var.lbls,var.lbls.pre)
 
  ## Convert to the output types
  switch(out.type,
         event=         { ch.df <- iOutEvent(ch.df,id)                                        },
         individual=    { ch.df <- iOutIndividual(ch.df,include.id)  },
         frequency=     { ch.df <- iOutFrequency(ch.df)                 },
         MARK=          { ch.df <- iOutMARK(ch.df)                                            },
         RMark=,marked= { ch.df <- iOutRMark(ch.df,include.id)                                }
         ) # end out.type switch
  ## return the new data.frame
  ch.df
}

##############################################################
# Internal functions
##############################################################
#=============================================================
# Internal function to make default labels
#=============================================================
iMakeDefaultCHLabels <- function(ch.df,var.lbls.pre) {
  ## check var.lbls.pre
  # check of only one is given
  if (length(var.lbls.pre)>1) {
    WARN("'var.lbls.pre' contains more than one prefix, only first was used.") 
    var.lbls.pre <- var.lbls.pre[1]
  }
  # check that it does not start with a number
  if (!is.na(suppressWarnings(as.numeric(substring(var.lbls.pre,1,1))))) {
    WARN("'var.lbls.pre' cannot begin with a number, changed to 'event'.")
    var.lbls.pre <- "event"
  }
  ## make labels
  paste0(var.lbls.pre,seq_len(ncol(ch.df)-1))
}

#=============================================================
# Make labels for variable in output format
#=============================================================
iMakeVarLabels <- function(ch.df,in.type,id,var.lbls,var.lbls.pre) {
  ## var.lbls were given
  if (!is.null(var.lbls)) {
    ## too many var.lbls ... reduce to number needed
    if (length(var.lbls)>=(ncol(ch.df)-1)) {
      var.lbls <- var.lbls[seq_len(ncol(ch.df)-1)]
    } else {
      ## too few var.lbls ... warn and build default labels
      WARN("Too few labels in 'var.lbls'; default labels will be used.")
      var.lbls <- iMakeDefaultCHLabels(ch.df,var.lbls.pre)
    }
  } else { ## var.lbls not given
    ## for certain in.types, make var.lbls from ch column names
    if (in.type %in% c("individual","frequency","event")) {
      var.lbls <- names(ch.df)[-1]
    } else {
      ## make var.lbls from default values
      var.lbls <- iMakeDefaultCHLabels(ch.df,var.lbls.pre)
    }
  }
  # add id variable name
  c(ifelse(is.null(id),"id",id),var.lbls)
}

#=============================================================
## Internal functions to convert from one format to an internal
##   individual format. Thus, each function below returns a data.frame
##   with an id variable in the first column and individual capture
##   histories in all other columns.
#=============================================================
iEvent2Indiv <- function(df,id,event.ord) {
  # See if there is an id variable
  if (is.null(id)) STOP("No variable with unique fish identification information given in 'id'.")
  # All other variables are "events"
  event <- names(df)[which(names(df)!=id)]
  # Control the event order if told to do so by the user
  if (!is.null(event.ord)) df[,event] <- ordered(df[,event],levels=event.ord)
  # Make a table of ids by events
  ch.tab <- table(df[,id],df[,event])
  # Turn the table into a data.frame and rename the variables
  tmp <- as.data.frame(ch.tab)
  names(tmp) <- c("id","event","freq")
  # Unstack and add rownames to data.frame
  tmp <- utils::unstack(tmp,freq~event)
  tmp <- data.frame(rownames(ch.tab),tmp)
  names(tmp) <- c(ifelse(is.null(id),"id",id),levels(factor(df[,event])))
  # force id to be a character (rather than a factor as from as.data.frame)
  tmp[,1] <- as.character(tmp[,1])
  # return the new data.frame
  tmp
}

#=============================================================
#=============================================================
iFrequency2Indiv <- function(df,freq) {
  if (is.null(freq)) {
    tmp <- "No 'freq' given; assumed frequencies were 1 for each capture history."
    if (any(c("freq","Freq","FREQ") %in% names(df))) tmp <- paste0(tmp,"\nHowever, one variable appears to contain frequencies.")
    WARN(tmp)
    nfreq <- rep(1,nrow(df))
  } else {
    # isolate frequencies and create a df without them
    nfreq <- df[,freq]
    df <- df[,-which(names(df)==freq)]
  }
  tmp <- matrix(NA,ncol=ncol(df),nrow=sum(nfreq))
  for (i in seq_len(ncol(df))) tmp[,i] <- rep(df[,i],nfreq)
  tmp <- as.data.frame(tmp)
  # add an "id" variable in the first column
  tmp <- data.frame(seq_len(nrow(tmp)),tmp)
  names(tmp) <- c("id",names(df))
  tmp
}

#=============================================================
#=============================================================
iIndividual2Indiv <- function(df,id) {
  # make sure id is the first variable
  if (!is.null(id)) {
    tmp <- df[,c(which(names(df)==id),which(names(df)!=id))]
  } else {
    tmp <- data.frame(seq_len(nrow(df)),df)
    names(tmp)[1] <- "id"
  }
  tmp
}

#=============================================================
#=============================================================
iMark2Indiv <- function(df,freq) {
  # remove ";" from last column
  df[,ncol(df)] <- sub(";","",df[,ncol(df)])
  # isolate frequencies and capture histories
  if (is.null(freq)) {
    tmp <- "No 'freq' given; assumed frequencies were 1 for each capture history."
    if (any(c("freq","Freq","FREQ") %in% names(df))) tmp <- paste0(tmp,"\nHowever, one variable appears to contain frequencies.")
    WARN(tmp)
    nfreq <- rep(1,nrow(df))
  } else {
    # isolate frequencies and create a df without them
    # make sure frequencies are numeric and capture history is character
    nfreq <- as.numeric(df[,freq])
    df <- df[,-which(names(df)==freq)]
  }
  # expand the string 
  tmp <- iExpandCHString(df,nfreq)
  tmp
}

#=============================================================
#=============================================================
iRMark2Indiv <- function(df,id,freq) {
  # force to be a data.frame (likely comes as a vector)
  df <- as.data.frame(df)
  # can't supply both id and freq
  if (!is.null(id) & !is.null(freq)) STOP("Only one of 'id' or 'freq' can be used with the RMark format.")
  # if neither id nor freq then create a freq=1 column and use it
  if (is.null(id) & is.null(freq)) {
    tmp <- "No 'freq' or 'id' given; assumed frequencies were 1 for each capture history."
    if (any(c("freq","Freq","FREQ") %in% names(df))) tmp <- paste0(tmp,"\nHowever, one variable appears to contain frequencies.")
    WARN(tmp)
    df$freq <- rep(1,nrow(df))
    freq <- "freq"
  }
  # Handle when an id variable is given
  if (!is.null(id)) {
    # expland the string by first isolating the capture histories
    #   and id variables (first two arguments)
    tmp <- iExpandCHString(df[,which(names(df)!=id)],ids=df[,id],idname=id)
  } else {
    # expand the string by first isolating the capture histories
    #   and freq variables (first two arguments)
    tmp <- iExpandCHString(df[,which(names(df)!=freq)],df[,freq])
  }
  tmp
}

#=============================================================
## Internal function to expand the string of capture histories in the
##   MARK and RMark formats. Function takes a vector of the capture
##   history strings, optional vectors of the frequencies or unique
##   fish identifiers, and a name for the id variable.
## See use in iMark2Indiv() and iRMark2Indiv().
#=============================================================
iExpandCHString <- function(ch,nfreq=NULL,ids=NULL,idname="id") {
  # expand the capture histories if nfreq is provided
  if (!is.null(nfreq)) {
    nfreq <- as.numeric(nfreq)
    ch <- rep(ch,nfreq)
  }
  # make sure that ch is a character
  ch <- as.character(ch)
  # create a NA matrix to hold the separated capture histories
  tmp <- matrix(NA,ncol=nchar(ch[1]),nrow=length(ch))
  # expand each row in ch to fill a new row in tmp
  for (i in seq_along(ch)) {
    ch1 <- as.numeric(noquote(unlist(strsplit(ch[i],""))))
    tmp[i,] <- ch1
  }
  # make tmp a data.frame
  tmp <- as.data.frame(tmp)
  # add an "id" variable in the first column
  if (is.null(ids)) ids <- seq_len(nrow(tmp))
  tmp <- data.frame(ids,tmp)
  names(tmp)[1] <- idname
  tmp
}

#=============================================================
## Internal functions to convert from the internal individual format
##   returned by the in.type internal functions above to one of the
##   output formats. Each function that begins with iOut returns a
##   data.frame in the proper format. The other functions produce
##   intermediate objects.
#=============================================================
iOutEvent <- function(ch.df,id) {
  # get total sample size as sum of all values in capture history
  n <- sum(ch.df[,-1])
  # get event names and number
  events <- names(ch.df)[-1]
  n.ev <- length(events)
  # initiate vectors for id and events
  v.id <- NULL
  v.ev <- NULL
  # Loop through events creating list of ids and event numbers
  for (i in seq_len(n.ev)) {
    tmp <- ch.df[as.logical(ch.df[,i+1]),1]
    v.id <- c(v.id,tmp)
    v.ev <- c(v.ev,rep(events[i],length(tmp)))
  }
  # put ids and events together in a data.frame
  tmp <- data.frame(v.id,v.ev,stringsAsFactors=FALSE)
  # and name the colums
  if (is.null(id)) names(tmp) <- c("id","event")
  else names(tmp) <- c(id,"event")
  # return the new data.frame
  tmp
}

#=============================================================
#=============================================================
iOutFrequency <- function(ch.df) {
  var.lbls <- c(names(ch.df)[-1],"freq")
  ch.df <- iPrepCapHistSum(ch.df)
  ch.df1 <- matrix(NA,ncol=nchar(as.character(ch.df[1,1])),nrow=nrow(ch.df))
  for (i in seq_len(nrow(ch.df))) {
    ch1 <- as.numeric(noquote(unlist(strsplit(as.character(ch.df[i,1]),""))))
    ch.df1[i,] <- ch1
  }
  ch.df <- data.frame(ch.df1,ch.df[,"freq"],stringsAsFactors=FALSE)
  names(ch.df) <- var.lbls
  ch.df
}

#=============================================================
#=============================================================
iOutIndividual <- function(ch.df,include.id) {
  # decide to remove the id variable or not
  if (!include.id) ch.df <- ch.df[,-1]
  ch.df
}

#=============================================================
#=============================================================
iOutRMark <- function(ch.df,include.id) {
  # isolate the id variable if it is going to be included at the end
  if (include.id) idtmp <- ch.df[,1]
  # remove id column from ch.df
  chtmp <- ch.df[-1]
  # combine the capture histories into a string
  ch <- apply(as.matrix(chtmp),1,paste,sep="",collapse="")
  dftmp <- data.frame(ch=ch,stringsAsFactors=FALSE)
  dftmp$ch <- as.character(dftmp$ch)
  # add id variable back on if asked for
  if (include.id) {
    dftmp <- data.frame(idtmp,dftmp,stringsAsFactors=FALSE)
    names(dftmp)[1] <- names(ch.df)[1]
  }
  # return the new data.frame
  dftmp
}

#=============================================================
#=============================================================
iOutMARK <- function(ch.df) {
  ch.df <- iPrepCapHistSum(ch.df)
  ch.df[,ncol(ch.df)] <- paste0(ch.df[,ncol(ch.df)],";")
  names(ch.df)[1] <- "ch"
  ch.df
}

#=============================================================
#=============================================================
iPrepCapHistSum <- function(ch.df) {
  # get capture history summary without the id column
  chsum <- capHistSum(ch.df,cols2use=2:ncol(ch.df))
  # convert to a data.frame and re-label columns
  ch.df <- as.data.frame(chsum$caphist,stringsAsFactors=FALSE)
  rownames(ch.df) <- seq_len(nrow(ch.df))
  colnames(ch.df) <- c("caphist","freq")
  # return the data.frame
  ch.df
}
