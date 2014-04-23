#'Computing precision between or among sets of ages.
#'
#'Computes overall measures of precision for age assignments made on the same
#'individuals.  The age assignments can consist of age measurements recorded for
#'two or more readers of the same structure, one reader at two or more times, or
#'for two or more stuctures (e.g., scales, spines, otoliths).
#'
#'The main function, \code{agePrecision}, requires a formula of the form
#'\code{~var1+var2+var3+...} where the \code{varX} generically represent the
#'variables that contain the age assignments.  Alternatively, the formula may be
#'of the form \code{var1~var2+var3+...} to allow similarity with \code{\link{ageBias}}.
#'Note that in this alternative formula that the RHS can have only one variable.
#'
#'If \code{what="precision"} (the default) in \code{summary} then a summary table
#'will be printed that contains the following items:
#'\itemize{
#'  \item n Number of fish in \code{data}.
#'  \item R Number of age assessments in \code{data}.
#'  \item CV The mean coefficient of variation.
#'  \item APE The mean average percent error.
#'  \item PercAgree The percentage of fish for which all age assignments perfectly agree.
#'}
#'
#'If \code{what="agreement"} in \code{summary} then a table that describes either
#'the percentage (if \code{percent=TRUE}, default) or frequency of fish by the
#'absolute difference in paired age assignments.  Thus, this table will have one
#'row for each possible pair of age assignments.  The \dQuote{0} column represents
#'perfect agreement between the two age assignements, the \dQuote{1} column
#'represents ages that disagreed by one year (in either direction), and so on.
#'
#'If \code{what="detail"} in \code{summary} then a data frame of the original data
#'plus the intermediate caculations of the average age, standard deviation of age,
#'APE, and CV for each individual fish will be printed.  These details are generally
#'only used to check or to understand calculations.
#'
#'See \code{\link{ageBias}} for computation of the full age agreement table, along
#'with tests and plots of age bias.
#'
#'@aliases agePrec plot.agePrec summary.agePrec
#'@param formula A formula of the form \code{~var1+var2+var3+...} where the
#'\code{varX} generically represent the variables that contain the age assignments.
#'See details.
#'@param data A data.frame that minimally contains the age assignments.  See 
#'\code{formula} above and details.
#'@param object An object saved from the \code{agePrecision} call (i.e., of
#'class \code{agePrec}).
#'@param what A string that indicates what type of summary to print.  See details.
#'@param percent A logical that indicates whether the agreement matrix should contain
#'percentages (\code{TRUE}; default) or raw numbers.  See details.
#'@param digits A numeric that indicates the minimum number of digits to print when
#'using \code{summary}.
#'@param \dots Additional arguments for methods.
#'@return \code{agePrec} returns a list with the following items:
#'\itemize{
#'  \item detail A data frame with all given and computed information about each fish.
#'  \item absdiff A table of number of fish by absolute differences for each pair of ages.
#'  \item APE The mean average percent error.
#'  \item CV The mean coefficient of variation.
#'  \item n Number of fish in \code{data}.
#'  \item R Number of age assessments in \code{data}.
#'  \item PercAgree The percentage of fish for which all age assignments perfectly agree.
#'}
#'
#'The \code{summary} function do not return anything.
#'
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/AgeComparisons.pdf}
#'
#'@seealso \code{\link{ageBias}}
#'
#'@references Beamish, R.J. and D.A. Fournier.  1981.  A method for comparing
#'the precision of a set of age determinations.  Canadian Journal of Fisheries
#'and Aquatic Sciences, 38:982-983.
#'
#'Campana, S.E.  1982.  Accuracy, precision and quality control in age
#'determination, including a review of the use and abuse of age validation
#'methods. Journal of Fish Biology, 59:197-242.
#'
#'Campana, S.E., M.C. Annand, and J.I. McMillan. 1995.  Graphical and
#'statistical methods for determining the consistency of age determinations.
#'Transactions of the American Fisheries Society, 124:131-138.
#'
#'Chang, W.Y.B. 1982.  A statistical method for evaluating the reproducibility
#'of age determination.  Canadian Journal of Fisheries and Aquatic Sciences,
#'39:1208-1210.
#'
#'@keywords htest manip
#'@examples
#'## Example with just two age assignments
#'data(WhitefishLC)
#'ap1 <- agePrecision(~otolithC+scaleC,data=WhitefishLC)
#'summary(ap1)
#'summary(ap1,what="precision")
#'summary(ap1,what="difference")
#'summary(ap1,what="difference",percent=FALSE)
#'summary(ap1,what="absolute",percent=FALSE)
#'barplot(ap1$rawdiff,ylab="Frequency",xlab="Otolith - Scale Age")
#'summary(ap1,what="detail")
#'
#'## Example with three age assignments
#'ap2 <- agePrecision(~otolithC+finrayC+scaleC,data=WhitefishLC)
#'summary(ap2)
#'summary(ap2,what="precision")
#'summary(ap2,what="difference")
#'summary(ap2,what="difference",percent=FALSE)
#'summary(ap2,what="absolute",percent=FALSE)
#'summary(ap2,what="detail")
#'
#'@rdname agePrec
#'@export
agePrecision <- function(formula,data) {
  # change formula to have only a RHS
  tmp <- as.character(formula)[-1]
  formula <- as.formula(paste("~",paste(tmp,collapse="+")))
  tmp <- hndlFormula(formula,data)
  
  if (!tmp$Etype=="numeric") stop("All variables must be numeric.",call.=FALSE)
  # sample size & number of structures
  n <- nrow(tmp$mf)
  R <- ncol(tmp$mf)
  # rename dataframe of just ages (for simplicity)
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


#'@rdname agePrec
#'@method summary agePrec
#'@S3method summary agePrec
summary.agePrec <- function(object,what=c("precision","difference","absolute difference","detail","agreement"),
                            percent=TRUE,digits=4,...) {
  what <- match.arg(what)
  if (what=="precision") {
    cat("Precision summary statistics\n")
    print(with(object,data.frame(n=n,R=R,CV=CV,APE=APE,PercAgree=PercAgree)),
          row.names=FALSE,digits=digits)
  }
  if (what %in% c("difference","absolute difference","agreement")) {
    if (what=="agreement") message("Use of what='agreement' is deprecated, instead use what='difference' or what='absolute difference'.")
    if (what=="difference") {
      tmp <- object$rawdiff
      msg <- "of fish by differences in ages between pairs of assignments\n"
    } else {
      tmp <- object$absdiff
      msg <- "of fish by absolute differences in ages between pairs of assignments\n"
    }
    if (percent) {
      msg <- paste("Percentage",msg)
      # need to check if it is a 1-D table and handle as a vector
      if (length(dim(tmp))==1) tmp <- tmp/sum(tmp)*100
       else tmp <- prop.table(tmp,margin=1)*100      
    } else msg <- paste("Frequency",msg)
    cat(msg)
    print(tmp,digits=digits)
  }
  if (what=="detail") {
    cat("Intermediate calculations for each individual.\n")
    print(object$detail,digits=digits)
  }
}
