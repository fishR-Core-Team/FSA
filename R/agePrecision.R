#' @title Compute measures of precision among sets of ages.
#'
#' @description Computes overall measures of precision for multiple age assignments made on the same individuals.  The age assignments may be from two or more readers of the same structure, one reader at two or more times, or two or more stuctures (e.g., scales, spines, otoliths).  Measures of precision include CV, APE, and various percentage difference values.
#'
#' @details If \code{what="precision"} in \code{summary} then a summary table that contains the following items will be printed:
#' \itemize{
#'   \item n Number of fish in \code{data}.
#'   \item R Number of age assessments given in \code{formula}.
#'   \item CV The mean coefficient of variation.  See the fishR vignette for calculational details.
#'   \item APE The mean average percent error.  See the fishR vignette for calculational details.
#'   \item PercAgree The percentage of fish for which all age assignments perfectly agree.
#' }
#'
#' If \code{what="difference"} is used in \code{summary} then a table that describes either the percentage (if \code{percent=TRUE}, default) or frequency of fish by the difference in paired age assignments.  This table has one row for each possible pair of age assignments.
#'
#' If \code{what="absolute difference"} is used in \code{summary} then a table that describes either the percentage (if \code{percent=TRUE}, default) or frequency of fish by the absolute value of the difference in paired age assignments.  This table has one row for each possible pair of age assignments.  The \dQuote{1} column, for example, represents age assignments that disagree by one year (in either direction).
#'
#' If \code{what="detail"} is used in \code{summary} then a data frame of the original \code{data} along with the intermediate caculations of the average age, standard deviation of age, APE, and CV for each individual will be printed.  These details are generally only used to check or to understand calculations.
#' 
#' @param formula A formula of the form \code{~var1+var2+var3+...} or, alternatively, \code{var1~var2+var3+...}, where the \code{varX} generically represent the variables that contain the age assignments.  The alternative formula allows for similar code as used in \code{\link{ageBias}} and can have only one variable on the left-hand side.
#' @param data A data.frame that minimally contains the variables in \code{formula}.
#' @param object An object of class \code{agePrec}, usuall a result from \code{agePrecision}.
#' @param what A single string that indicates what type of summary to print.  See details.
#' @param percent A single logical that indicates whether the difference table (see details) should be represented as percentages (\code{TRUE}; default) or frequency (\code{FALSE}) of fish.
#' @param digits A single numeric that indicates the minimum number of digits to print when using \code{summary}.
#' @param \dots Additional arguments for methods.
#' 
#' @return The main function returns a list with the following items:
#' \itemize{
#'   \item detail A data.frame with all data given in \code{data} and intermediate calculations for each fish.  See details
#'   \item rawdiff A frequency table of fish by differences for each pair of ages.
#'   \item absdiff A frequency table of fish by absolute differences for each pair of ages.
#'   \item APE The mean average percent error.
#'   \item CV The mean coefficient of variation.
#'   \item n Number of fish in \code{data}.
#'   \item R Number of age assessments for each fish given in \code{formula}.
#' }
#'
#' Nothing is returned by \code{summary}, but see details for what is printed.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/AgeComparisons.pdf}
#'
#' @seealso See \code{\link{ageBias}} for computation of the full age agreement table, along with tests and plots of age bias.
#' 
#' @references Beamish, R.J. and D.A. Fournier.  1981.  \href{http://www.pac.dfo-mpo.gc.ca/science/people-gens/beamish/PDF_files/compareagecjfas1981.pdf}{A method for comparing the precision of a set of age determinations.}  Canadian Journal of Fisheries and Aquatic Sciences 38:982-983.
#'
#'Campana, S.E.  1982.  \href{http://www.denix.osd.mil/nr/crid/Coral_Reef_Iniative_Database/References_for_Reef_Assessment_files/Campana,\%202001.pdf}{Accuracy, precision and quality control in age determination, including a review of the use and abuse of age validation methods.} Journal of Fish Biology 59:197-242.
#'
#'Campana, S.E., M.C. Annand, and J.I. McMillan. 1995.  \href{http://www.bio.gc.ca/otoliths/documents/Campana\%20et\%20al\%201995\%20TAFS.pdf}{Graphical and statistical methods for determining the consistency of age determinations.} Transactions of the American Fisheries Society 124:131-138.
#'
#'Chang, W.Y.B. 1982.  \href{http://www.nrcresearchpress.com/doi/abs/10.1139/f82-158}{A statistical method for evaluating the reproducibility of age determination.}  Canadian Journal of Fisheries and Aquatic Sciences 39:1208-1210.  
#'
#' @aliases agePrec plot.agePrec summary.agePrec
#'
#' @keywords htest manip
#' 
#' @examples
#' ## Example with just two age assignments
#' data(WhitefishLC)
#' ap1 <- agePrecision(~otolithC+scaleC,data=WhitefishLC)
#' summary(ap1)
#' summary(ap1,what="precision")
#' summary(ap1,what="difference")
#' summary(ap1,what="difference",percent=FALSE)
#' summary(ap1,what="absolute",percent=FALSE)
#'
#' barplot(ap1$rawdiff,ylab="Frequency",xlab="Otolith - Scale Age")
#' summary(ap1,what="detail")
#' summary
#'
#' ## Example with three age assignments
#' ap2 <- agePrecision(~otolithC+finrayC+scaleC,data=WhitefishLC)
#' summary(ap2)
#' summary(ap2,what="precision")
#' summary(ap2,what="difference")
#' summary(ap2,what="difference",percent=FALSE)
#' summary(ap2,what="absolute",percent=FALSE)
#' summary(ap2,what="detail")
#'
#' @rdname agePrecision
#' @export
agePrecision <- function(formula,data) {
  # change formula to have only a RHS
  tmp <- as.character(formula)[-1]
  formula <- as.formula(paste("~",paste(tmp,collapse="+")))
  tmp <- iHndlFormula(formula,data)
  
  if (!tmp$Etype=="numeric") stop("All variables must be numeric.",call.=FALSE)
  # sample size & number of structures
  n <- nrow(tmp$mf)
  R <- ncol(tmp$mf)
  # get dataframe of just ages (for simplicity)
  d <- tmp$mf
  
  ## Precision alculations (APE and CV) on each fish
  # Mean, SD of assigned ages
  age.avg <- apply(d,1,mean)
  age.sd <- apply(d,1,sd)
  # Summed absolute deviation
  tmp.adevs <- abs(apply(d,2,'-',age.avg))
  age.ad <- apply(tmp.adevs,1,sum)
  # APE & CV for each fish
  APE.j <- ((age.ad/age.avg)/R)*100
  CV.j <- (age.sd/age.avg)*100
  # Replaced NAs with 0 in APE.j and CV.j for when age.avg==0
  tmp <- which(age.avg==0)
  APE.j[tmp] <- 0
  CV.j[tmp] <- 0
  # Put results into a data.frame to return
  detail.df <- data.frame(d,avg=age.avg,sd=age.sd,APE=APE.j,CV=CV.j)
  ## Summary precision calculations (mean APE, CV, total agreement) for all fish
  APE <- mean(APE.j)
  CV <- mean(CV.j)
  # all ages agree if sd=0
  all.agree <- length(detail.df$sd[detail.df$sd==0])/n*100

  ## Raw age agreement summaries
  # find all pairs of comparisons
  prs <- t(combn(names(d),2))
  # maximum possible difference is max age - min age ... use this to set the levels
  #   for the agreement table.
  tmp <- max(d,na.rm=TRUE)-min(d,na.rm=TRUE)
  poss.lvls <- seq(-tmp,tmp,1)
  # create a matrix to contain the results of comparing each pair
  ragree <- matrix(NA,ncol=length(poss.lvls),nrow=nrow(prs))
  # cycle through each paired comparison putting results in agreement matrix
  for (i in 1:nrow(prs)) {
    tmp <- d[,prs[i,1]]-d[,prs[i,2]]
    ragree[i,] <- table(factor(tmp,levels=poss.lvls))
  }
  # relabel rows and columns of agreement table
  rownames(ragree) <- apply(prs,1,paste,collapse=" - ")
  colnames(ragree) <- poss.lvls
  # delete right- and left-most columns that contain all zeroes
  tmp <- c(which(rcumsum(colSums(ragree))==0),which(cumsum(colSums(ragree))==0))
  if (length(tmp>0)) ragree <- ragree[,-tmp]
  
  ## Absolute age agreement summaries
  # maximum possible difference is max age - min age ... use this to set the levels
  #   for the agreement table.
  poss.lvls <- 0:(max(d,na.rm=TRUE)-min(d,na.rm=TRUE))
  # create a matrix to contain the results of comparing each pair
  aagree <- matrix(NA,ncol=length(poss.lvls),nrow=nrow(prs))
  # cycle through each paired comparison putting results in agreement matrix
  for (i in 1:nrow(prs)) {
    tmp <- abs(d[,prs[i,1]]-d[,prs[i,2]])
    aagree[i,] <- table(factor(tmp,levels=poss.lvls))
  }
  # relabel rows and columns of agreement table
  rownames(aagree) <- apply(prs,1,paste,collapse=" v. ")
  colnames(aagree) <- poss.lvls
  # delete right-most columns that contain all zeroes
  tmp <- which(rcumsum(colSums(aagree))==0)
  if (length(tmp>0)) aagree <- aagree[,-tmp]
  
  ## Put together an output list
  d <- list(detail=detail.df,rawdiff=as.table(ragree),absdiff=as.table(aagree),
            APE=APE,CV=CV,PercAgree=all.agree,n=n,R=R)
  class(d) <- "agePrec"
  d 
}

#' @rdname agePrecision
#' @export
summary.agePrec <- function(object,what=c("precision","difference","absolute difference","detail"),
                            percent=TRUE,digits=4,...) {
  what <- match.arg(what,several.ok=TRUE)
  if ("precision" %in% what) {
    cat("Precision summary statistics\n")
    print(with(object,data.frame(n=n,R=R,CV=CV,APE=APE,PercAgree=PercAgree)),
          row.names=FALSE,digits=digits)
    what <- iHndlMultWhat(what,"precision")
  }
  if ("absolute difference" %in% what) {
    tmp <- object$absdiff
    msg <- "of fish by absolute differences in ages\n between pairs of assignments\n"
    if (percent) {
      msg <- paste("Percentage",msg)
      # need to check if 1-D, then handle as a vector
      if (length(dim(tmp))==1) tmp <- tmp/sum(tmp)*100 
      else tmp <- prop.table(tmp,margin=1)*100      
    } else msg <- paste("Frequency",msg)
    cat(msg)
    print(tmp,digits=digits)
    what <- iHndlMultWhat(what,"absolute difference")
  }  
  if ("difference" %in% what) {
    tmp <- object$rawdiff
    msg <- "of fish by differences in ages\n between pairs of assignments\n"
    if (percent) {
      msg <- paste("Percentage",msg)
      # need to check if 1-D, then handle as a vector
      if (length(dim(tmp))==1) tmp <- tmp/sum(tmp)*100
      else tmp <- prop.table(tmp,margin=1)*100      
    } else msg <- paste("Frequency",msg)
    cat(msg)
    print(tmp,digits=digits)
    what <- iHndlMultWhat(what,"difference")
  }
  if ("detail" %in% what) {
    cat("Intermediate calculations for each individual\n")
    print(object$detail,digits=digits)
    what <- iHndlMultWhat(what,"detail")
  }
}
