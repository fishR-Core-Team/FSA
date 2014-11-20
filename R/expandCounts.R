#' @title Repeat individual fish data (including lengths) from tallied counts.
#' 
#' @description Repeat individual fish data, including lengths, from tallied counts and, optionally, add a random digit to length measurements to simulate actual length of fish in the bin.  This is useful as a precursor to summaries that require information, e.g., lengths, of individual fish (e.g., length frequency histograms, means lengths).
#' 
#' @details Fisheries data may be recorded as tallied counts in the field.  For example, field biologists may have simply recorded that there were 10 fish in one group, 15 in another, etc.  More specifically, the biologist may have recorded that there were 10 male Bluegill from the first sampling event between 100 and 124 mm, 15 male Bluegill from the first sampling event between 125 and 149 mm, and so on.  At times, it may be necessary to expand these counts such that the repeated information appears in individual rows in a new data.frame.  In this specific example, the tallied counts would be repeated such that the male, Bluegill, first sampling event, 100-124 mm information would be repeated 10 times; the male, Bluegill, first sampling event, 125-149 mm information would be repeated 15 times, and so on.  This function facilitates this type of expansion.
#' 
#' Length data has often been collected in a \dQuote{binned-and-tallied} format (e.g., 10 fish in the 100-124 mm group, 15 in the 125-149 mm group, etc.).  This type of data collection does not facilitate easy or precise calculations of summary statistics of length (i.e., mean and standard deviations of length).  Expanding the data as described above does not solve this problem because the length data are still essentially categorical (i.e., which group the fish belongs to rather than what it's actual length is).  To facilitate computation of summary statistics, the data can be expanded as described above and then a length can be randomly selected from within the recorded length bin to serve as a \dQuote{measured} length for that fish.  This function performs this type of expansion by randomly selecting the length from a uniform distribuion within the length bin (e.g., each value between 100 and 124 mm has the same probability of being selected).
#' 
#' @param cform A formula of the form \code{~countvar} where \code{countvar} generically represents the variable in \code{data} that contains the counts of individuals.  See details.
#' @param lform An optional formula of the form \code{~lowerbin+upperbin} where \code{lowerbin} and \code{upperbin} generically represent the variables in \code{data} that identify the lower- and upper-values of the length bins.  See details.
#' @param data A data.frame that contains variables in \code{cform} and \code{lform}.
#' @param removeCount A single logical that indicates if the variable that contains the counts of individuals (as given in \code{cform}) should be removed form the returned data.frame.  The default is \code{TRUE} such that the variable will be removed as the returned data.frame contains individuals and the counts of individuals in tallied bins is not relevant to an individual.
#' @param lprec A single numeric that controls the precision to which the random lengths are recorded.  See details.
#' @param new.name A single string that contains a name for the new length variable if random lengths are to be created.
#' @param verbose A logical indicating whether progress message should be printed or not.
#' @param \dots Not yet implemented.
#' 
#' @return A data.frame of the same structure as \code{data} except that the variable in \code{cform} may be deleted and the variable in \code{new.name} may be added.  The returned data.frame will have more rows than \code{data} because of the potential addition of new individuals expanded from the counts in \code{cform}.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @seealso See \code{\link{expandLenFreq}} for expanding length frequencies where individual fish measurements were made on individual fish in a subsample and the remaining fish were simply counted.
#' 
#' @keywords manip
#'
#' @examples
#' # all need expansion
#' ( d1 <- data.frame(name=c("Johnson","Johnson","Jones","Frank","Frank","Max"),
#'                    lwr.bin=c(15,15.5,16,16,17,17),
#'                    upr.bin=c(15.5,16,16.5,16.5,17.5,17.5),
#'                    freq=c(6,4,2,3,1,1)) )
#' expandCounts(d1,~freq)
#' expandCounts(d1,~freq,~lwr.bin+upr.bin)
#' 
#' # some need expansion
#' ( d2 <- data.frame(name=c("Johnson","Johnson","Jones","Frank","Frank","Max"),
#'                    lwr.bin=c(15,15.5,16,16,17.1,17.3),
#'                    upr.bin=c(15.5,16,16.5,16.5,17.1,17.3),
#'                    freq=c(6,4,2,3,1,1)) )
#' expandCounts(d2,~freq)
#' expandCounts(d2,~freq,~lwr.bin+upr.bin)
#' 
#' # none need expansion
#' ( d3 <- data.frame(name=c("Johnson","Johnson","Jones","Frank","Frank","Max"),
#'                    lwr.bin=c(15,15.5,16,16,17.1,17.3),
#'                    upr.bin=c(15,15.5,16,16,17.1,17.3),
#'                    freq=c(6,4,2,3,1,1)) )
#' expandCounts(d3,~freq)
#' expandCounts(d3,~freq,~lwr.bin+upr.bin)
#' 
#' # some need expansion, but different bin widths
#' ( d4 <- data.frame(name=c("Johnson","Johnson","Jones","Frank","Frank","Max"),
#'                    lwr.bin=c(15,  15,  16,  16,  17.1,17.3),
#'                    upr.bin=c(15.5,15.9,16.5,16.9,17.1,17.3),
#'                    freq=c(6,4,2,3,1,1)) )
#' expandCounts(d4,~freq)
#' expandCounts(d4,~freq,~lwr.bin+upr.bin)
#' 
#' \dontrun{
#' ##!!##!!## Change path to where example file is and then run to demo
#' 
#' ## Read in datafile (note periods in names)
#' df <- read.csv("c:/aaawork/consulting/R_WiDNR/Statewide/Surveysummaries2010.csv")
#' str(df) 
#' ## narrow variables for simplicity
#' df1 <- df[,c("County","Waterbody.Name","Survey.Year","Gear","Species","Number.of.Fish",
#'              "Length.or.Lower.Length.IN","Length.Upper.IN","Weight.Pounds","Gender")]
#' ## Sum the count to see how many fish there should be after expansion
#' sum(df1$Number.of.Fish)
#' 
#' ## Simple expansion
#' df2 <- expandCounts(df1,~Number.of.Fish)
#' 
#' ## Same expansion but include random component to lengths (thus new length variable)
#' ##   also note default lprec=0.1
#' df3 <- expandCounts(df1,~Number.of.Fish,~Length.or.Lower.Length.IN+Length.Upper.IN)
#' 
#' }
#' 
#' @export
expandCounts <- function(data,cform,lform=NULL,removeCount=TRUE,lprec=0.1,new.name="newlen",verbose=TRUE,...) {
  ## do some error checking on cform (cform changes from a pure formula)
  cform <- iHndlFormula(cform,data)
  if (cform$vnum>1) stop("'cform' must be only one variable.",call.=FALSE)
  if (!cform$vclass %in% c("integer","numeric")) stop("'cform' must be a 'numeric' or 'integer' variable.",call.=FALSE)
  ## find those fish with zero counts
  zerocounts <- which(data[,cform$vname]==0)
  if (length(zerocounts)>0 & verbose) {
    if (length(zerocounts)>5) message(paste0(length(zerocounts)," rows had zero counts in ",cform$vname,"."))
    else (message(paste0("Some rows (",paste(zerocounts,collapse=", "),") had zero counts in ",cform$vname,".")))
  }
  ## Expand the rows based on the counts
  # Which rows have a count of 1 ...
  onecounts <- which(data[,cform$vname]==1)
  if (verbose) message(paste(length(onecounts),"rows had an individual measurement."))
  # ... and rows where the count is >1
  morecounts <- which(data[,cform$vname]>1)
  tmp <- length(morecounts)
  # repeat the row numbers 'count' times for those with count > 1
  morecounts <- rep(morecounts,data[morecounts,cform$vname])
  if (verbose) message(paste(tmp,"rows with multiple measurements were expanded to",length(morecounts),"rows of individual measurements."))
  # create a new data.frame that has one of each onecounts rows of the original data.frame
  #   and morecounts rows of the morecount rows of the original data.frame
  newdf <- rbind(data[onecounts,],data[morecounts,],data[zerocounts,])
  # remove the counts variable if asked for
  if (removeCount) newdf <- newdf[,-which(names(data) == cform$vname)]
  # get integer rownames
  row.names(newdf) <- NULL

  ## do some error checking on lform (lform changes from a pure formula)
  if (!is.null(lform)) {
    lform <- iHndlFormula(lform,newdf,expNumR=0,expNumE=2,expNumENums=2)
    if (!lform$metExpNumR) stop("'lform' must not have a left-hand-side)",call.=FALSE)
    if (!lform$metExpNumE!=2) stop("'lform' must have two variables on the right-hand-side)",call.=FALSE)
    if (!lform$metExpNumENums) stop("'lform' must have two NUMERIC variables on the right-hand-side)",call.=FALSE)

    # isolate the lower and upper length variable names (assumed to be put in formula in order)
    lwr <- lform$Enames[1]
    upr <- lform$Enames[2]
    # error check that lwr>upr
    tmp <- which(data[,lwr]>data[,upr])
    if (length(tmp)>0) stop(paste0("Some rows (",tmp,") have '",lwr,"'' greater than '",upr,"'."),call.=FALSE)
    # separate fish with no upper length (either measured exactly or not measured)
    dfdontexpand <- newdf[is.na(newdf[,upr]),]
    # separate fish with an upper length
    tmp <- newdf[!is.na(newdf[,upr]),]
    # which fish have the same upper and lower lengths (if any)
    samelens <- which(tmp[,lwr]==tmp[,upr])
    # which fish have an upper length but no lower length (if any)
    upnolow <- which(is.na(tmp[,lwr]))
    # put together those with same lengths or no lower
    samelens <- c(samelens,upnolow)
    if (length(samelens)>0) {
      # fish with same upper and lower lengths (or no lower) don't need expansion
      dfdontexpand <- rbind(dfdontexpand,tmp[samelens,])
      # rest need expansion
      dfdoexpand <- tmp[-samelens,]
    } else {
      # no fish had same upper and lower lengths, thus all with upper length need expansion
      dfdoexpand <- tmp
    }
    # add a length variable to the data.frame that does not need expansion (equal to lower value)
    if (nrow(dfdontexpand)>0) {
      dfdontexpand[,new.name] <- dfdontexpand[,lwr]
      dfdontexpand[,"lennote"] <- "Observed length"
    }
    # add uniform random number to fish that need it
    if (nrow(dfdoexpand)>0) {
      dfdoexpand[,new.name] <- apply(as.matrix(dfdoexpand[,c(lwr,upr)]),1,function(x) sample(seq(x[1],x[2],lprec),1))
      dfdoexpand[,"lennote"] <- "Expanded length"
    }
    # put two data.frames back together
    if (nrow(dfdontexpand)==0) newdf <- dfdoexpand
      else if (nrow(dfdoexpand)==0) newdf <- dfdontexpand
        else newdf <- rbind(dfdontexpand,dfdoexpand)
  }
  ## return the new data.frame
  newdf
}
