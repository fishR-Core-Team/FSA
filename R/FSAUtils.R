#' @name FSAUtils
#' 
#' @title Capitalizes the first letter of first or all words in a string.
#' 
#' @description Capitalizes the first letter of first or all words in a string.
#' 
#' @param x A single string.
#' @param which A single string that indicates whether all (the default) or only the first words should be capitalized.
#'
#' @return A single string with the first letter of the first or all words capitalized.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @keywords manip
#'
#' @examples
#' ## Capitalize first letter of all words (the default)
#' capFirst("Derek Ogle")
#' capFirst("derek ogle")
#' capFirst("derek")
#'
#' ## Capitalize first letter of only the first words
#' capFirst("Derek Ogle",which="first")
#' capFirst("derek ogle",which="first")
#' capFirst("derek",which="first")

#' ## apply to all elements in a vector
#' vec <- c("Derek Ogle","derek ogle","Derek ogle","derek Ogle","DEREK OGLE")
#' capFirst(vec)
#' capFirst(vec,which="first")
#'
#' ## check class types
#' class(vec)
#' vec1 <- capFirst(vec)
#' class(vec1)
#' fvec <- factor(vec)
#' fvec1 <- capFirst(fvec)
#' class(fvec1)
#' 
#' @export
capFirst <- function(x,which=c("all","first")) {
  ## Get the class of the object
  cls <- class(x)
  ## Perform a check
  if (!(cls %in% c("character","factor"))) stop("'capFirst' only works with 'character' or 'class' objects.",call.=FALSE)
  ## Capitalize the one word or the words in the vector
  if (length(x)==1) x <- iCapFirst(x,which)
  else x <- apply(matrix(x),MARGIN=1,FUN=iCapFirst,which=which)
  ## Change the case to what the original was
  if (cls=="factor") x <- as.factor(x)
  ## Return the object
  x
}

## Internal Function
iCapFirst<- function(x,which=c("all","first")) {
  # See whether all or just the first word should have the first letter capitalized
  which <- match.arg(which)
  # convert entire string to lower case ...
  x <- tolower(x)
  # then split on space if more than one word
  s <- strsplit(x, " ")[[1]]
  if (which=="first") {
    # convert first letters of first word to upper-case    
    s1 <- toupper(substring(s, 1,1)[1])
    # attach capitalized first letter to rest of lower-cased original string
    paste(s1,substring(x,2),sep="",collapse=" ")
  } else {
    # convert first letters of all words to upper-case
    s1 <- toupper(substring(s, 1,1))
    # attach capitalized first letter to rest of lower-cased separated strings
    paste(s1,substring(s,2),sep="",collapse=" ")
  }
}


#' @name chooseColors
#' 
#' @title Create a list of colors from among a variety of color palettes.
#'
#' @description Create a list of colors from among a variety of color palettes.
#'
#' @param pal A character that is the name of a palette.  Must be one of \dQuote{rich}, \dQuote{cm}, \dQuote{default}, \dQuote{grey}, \dQuote{gray}, \dQuote{heat}, \dQuote{jet}, \dQuote{rainbow}, \dQuote{topo}, or \dQuote{terrain}, which are given in \code{paletteChoices}.
#' @param num The number of colors to be returned.
#' @param \dots Other arguments to the various palette functions.
#'
#' @return A vector of colors of length \code{num}.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso See \code{\link[gplots]{rich.colors}} in \pkg{gplots}, \code{\link{cm.colors}}, \code{\link{heat.colors}}, \code{\link{topo.colors}}, \code{\link{terrain.colors}}, \code{\link{rainbow}}, \code{\link{colorRampPalette}}, and \code{\link{colors}}.
#'
#' @keywords manip
#'
#' @examples
#' n <- 20
#' # Color Wheels
#' pie(rep(1,n), col=chooseColors("rich",n))
#' pie(rep(1,n), col=chooseColors("rainbow",n))
#' pie(rep(1,n), col=chooseColors("topo",n))
#' pie(rep(1,n), col=chooseColors("gray",n))
#' pie(rep(1,n), col=chooseColors("jet",n))
#'
NULL

#' @rdname chooseColors
#' @export
chooseColors <- function(pal=paletteChoices(),num,...) {
  ## Some checks
  pal <- match.arg(pal)
  if (!num>0) stop("'num' must be positive.",call.=FALSE)
  ## Generate jet and grey colors
  jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", 
                                   "yellow", "#FF7F00", "red", "#7F0000"))
  grey.colors <- colorRampPalette(c("grey20","grey80"))
  ## Get the colors according to the palette
  switch(pal,
         rich={clrs <- rich.colors(num,...)},
         cm={clrs <- cm.colors(num,...)},
         default={clrs <- 1:num},
         gray=,grey={clrs <- grey.colors(num)},
         heat={clrs <- heat.colors(num,...)},
         jet={clrs <- jet.colors(num)},
         rainbow={clrs <- rainbow(num,...)},
         topo={clrs <- topo.colors(num,...)},
         terrain={clrs <- terrain.colors(num,...)}
  )
  clrs
}

#' @rdname chooseColors
#' @export
paletteChoices <- function() c("rich","cm","default","grey","gray","heat","jet","rainbow","topo","terrain")


#' @title Converts "numeric" factor levels to numeric values.
#'
#' @description Converts \dQuote{numeric} factor levels to numeric values.
#'
#' @param object A vector with \dQuote{numeric} factor levels to be converted to numeric values.
#'
#' @return A numeric vector.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @keywords manip
#' 
#' @examples
#' junk <- factor(c(1,7,2,4,3,10))
#' str(junk)
#' junk2 <- fact2num(junk)
#' str(junk2)
#'
#' ## ONLY RUN IN INTERACTIVE MODE
#' if (interactive()) {
#' 
#' bad <- factor(c("A","B","C"))
#' # This will result in an error -- levels are not 'numeric'
#' bad2 <- fact2num(bad)
#' 
#' }  ## END IF INTERACTIVE MODE
#' 
#' @export
fact2num <- function(object) {
  ## Don't continue if object is not a factor or character 
  ## i.e., does not fit the purpose of this function
  if (!class(object) %in% c("factor","character")) stop("'object' is not a factor or character and does not fit the purpose of this function.",call.=FALSE)
  ## Convert factor to character and then numeric
  suppressWarnings(res <- as.numeric(as.character(object)))
  ## If all na's then stop because values were not numeric-like, else return
  if (all(is.na(res))) stop("Conversion aborted because all levels in 'object' are not 'numbers'.",call.=FALSE)
  else as.vector(res)
}


#' @title Opens web pages associated with the fishR website.
#'
#' @description Opens web pages associated with the \href{https://fishr.wordpress.com"}{fishR website} in a browser.  The user can open the main page or choose a specific page to open.
#'
#' @param where A string that indicates a particular page on the fishR website to open.
#' 
#' @return None, but a webpage will be opened in the default browswer.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @keywords misc
#' 
#' @examples
#' ## ONLY RUN IN INTERACTIVE MODE
#' if (interactive()) {
#' 
#' fishR()            # home page
#' fishR("IFAR")      # Introduction to Fisheries Analysis with R page
#' fishR("general")   # general vignettes page
#' fishR("books")     # books vignettes page
#' fishR("AIFFD")     # Analysis & Interpretation of Freshwater Fisheries Data page
#' fishR("posts")     # blog posts (some examples) page
#' 
#' } ## END IF INTERACTIVE MODE
#' 
#' @export
fishR <- function(where=c("home","IFAR","general","books","AIFFD","posts","news")) {
  where <- match.arg(where)
  tmp <- "https://fishr.wordpress.com"
  switch(where,
         home=   { tmp <- tmp },
         IFAR=   { tmp <- paste0(tmp,"/ifar/") },
         general={ tmp <- paste0(tmp,"/vignettes/") },
         books=  { tmp <- paste0(tmp,"/books/") },
         AIFFD=  { tmp <- paste0(tmp,"/books/aiffd/") },
         posts=,news=  { tmp <- paste0(tmp,"/news/") }
  )
  browseURL(tmp)
  invisible(tmp)
}


#' @title Read news and changes for the 'FSA' package.
#'
#' @description Opens up the \href{https://github.com/droglenc/FSA/blob/master/NEWS.md}{News.md GitHub file} for the \sQuote{FSA} package in an external browser.
#' 
#' @aliases fsaNews FSANews
#' 
#' @return None.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @keywords manip
#' 
#' @examples
#' ## ONLY RUN IN INTERACTIVE MODE
#' if (interactive()) {
#' 
#' fsaNews()
#' FSANews()
#'
#'}  ## END IF INTERACTIVE MODE
#' 
#' @rdname fsaNews
#' @export
fsaNews <- function () {
  browseURL("https://github.com/droglenc/FSA/blob/master/NEWS.md")
}

#' @rdname fsaNews
#' @export
FSANews <- function () {
  fsaNews()
}


#' Shows rows from the head and tail of a data frame or matrix.
#'
#' Shows rows from the head and tail of a data frame or matrix.
#'
#' @param x A data frame or matrix.
#' @param which A numeric or string vector that contains the column numbers or names to display.  Defaults to showing all columns.
#' @param n A single numeric that indicates the number of rows to display from each of the head and tail of structure.
#' @param addrownums If there are no row names for the MATRIX, then create them from the row numbers.
#' @param \dots Arguments to be passed to or from other methods.
#'
#' @return A matrix or data.frame with 2*n rows.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @note If \code{n} is larger than the number of rows in \code{x} then all of \code{x} is displayed.
#'
#' @keywords manip
#'
#' @examples
#' headtail(iris)
#' headtail(iris,10)
#' headtail(iris,which=c("Sepal.Length","Sepal.Width","Species"))
#' headtail(iris,which=grep("Sepal",names(iris)))
#' headtail(iris,n=200)
#'
#' ## Make a matrix for demonstration purposes only
#' miris <- as.matrix(iris[,1:4])
#' headtail(miris)
#' headtail(miris,10)
#' headtail(miris,addrownums=FALSE)
#' headtail(miris,10,which=2:4)
#'
#' ## Make a tbl_df type from dplyr ... note how headtail()
#' ## is not limited by the tbl_df restriction on number of
#' ## rows to show (but head() is).
#' if (require(dplyr)) {
#'   iris2 <- tbl_df(iris)
#'   class(iris2)
#'   headtail(iris2,n=15)
#'   head(iris2,n=30)
#' }
#' @export
headtail <- function(x,n=3L,which=NULL,addrownums=TRUE,...) {
  ## Some checks
  if (!(is.matrix(x) | is.data.frame(x))) stop("'x' must be a matrix or data.frame.",call.=FALSE)
  if (length(n)!=1L) stop("'n' must be a single number.",call.=FALSE)
  ## Remove tbl_df class if it exists
  if ("tbl_df" %in% class(x)) x <- as.data.frame(x)
  ## Process data.frame
  N <- nrow(x)
  n <- ifelse(n<0L,max(N+n,0L),min(n,N))
  if (n>=N) tmp <- x
  else {
    h <- head(x,n,...)
    if (addrownums) {
      if (is.null(rownames(x))) rownames(h) <- paste0("[",1:n,",]")
    } else rownames(h) <- NULL
    t <- tail(x,n,addrownums,...)
    tmp <- rbind(h,t)
  }
  if (!is.null(which)) tmp <- tmp[,which]
  tmp
}


#' @title Performs a hypothesis test that a linear model parameter is equal to a specific value.
#'
#' @description Performs a hypothesis test that a linear model parameter is equal to a specific value.  Useful for testing that a parameter is equal to a value other than 0.
#'
#' @details The \dQuote{direction} of the alternative hypothesis is identified by a string in the \code{alt} argument.  
#'
#' If the \code{lm} object is from a simple linear regression with an intercept then \code{term=1} will use the intercept and \code{term=2} will use the slope in the hypothesis test.
#'
#' @param object A \code{lm} object.
#' @param term A single numeric that indicates which term in the model to use in the hypothesis test.
#' @param bo The null hypothesized parameter value.
#' @param alt A string that identifies the \dQuote{direction} of the alternative hypothesis.  The strings may be \code{"less"} for a \dQuote{less than} alternative, \code{"greater"} for a \dQuote{greater than} alternative, or \code{"two.sided"} (DEFAULT) for a \dQuote{not equals} alternative.
#'
#' @return A matrix that contains the term number, hypothesized value, parameter estimate, standard error of the parameter estimate, t test statistic, degrees-of-freedom, and corresponding p-value.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{htest.nlsBoot}}.
#'
#' @keywords htest
#'
#' @examples
#' data(Mirex)
#' # Simple linear regression test HA:slope!=0.1
#' lm1 <- lm(mirex~weight, data=Mirex)
#' hoCoef(lm1,2,0.1)
#'
#' @export
hoCoef <- function(object,term=2,bo=0,alt=c("two.sided","less","greater")) {
  alt <- match.arg(alt)
  if (!"lm" %in% class(object)) stop("'object' must be from 'lm'.",call.=FALSE)
  if (!term>0) stop("'term' must be a positive number.",call.=FALSE)
  tmp <- summary(object)$coefficients
  if (term>length(rownames(tmp))) stop("'term' is greater than number of terms in the model.",call.=FALSE)
  est <- tmp[term,"Estimate"]
  se <- tmp[term,"Std. Error"]
  t <- (est-bo)/se
  df <- object$df.residual
  switch(alt,
         less=     { p.value <- pt(t,df,lower.tail=TRUE) },
         greater=  { p.value <- pt(t,df,lower.tail=FALSE) },
         two.sided={ p.value <- 2*pt(abs(t),df,lower.tail=FALSE) }
  )
  res <- cbind(term,bo,est,se,t,df,p.value)
  colnames(res) <- c("term","Ho Value","Estimate","Std. Error","T","df","p value")
  rownames(res) <- ""
  res
}



#' @title Ratio of lagged observations.
#'
#' @description Computes the ratio of lagged observations in a vector.
#'
#' @details This function behaves similarly to \code{diff()} except that it returns a vector or matrix of ratios rather than differences.
#'
#' @param x A numeric vector or matrix.
#' @param lag An integer representing the lag \sQuote{distance}.
#' @param direction A string that indicates the direction of calculation.  A \code{"backward"} induicates that \sQuote{latter} values are divided by \sQuote{former} values.  A \code{"forward"} induicates that \sQuote{former} values are divided by \sQuote{latter} values.  See examples.
#' @param recursion An integeer that indicates the level of recursion for the calculations.  A \code{1} will simply compute the ratios.  A \code{2}, for example, will compute the ratios, save the result, and then compute the ratios of the results using the same \code{lag}.  See examples.
#' @param differences Same as \code{recursion}.  Used for symmetry with \code{\link[base]{diff}}.
#' @param \dots Additional arguments to \code{diff()}.
#'
#' @return A vector or matrix of lagged ratios.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{diff}
#'
#' @keywords manip
#'
#' @examples
#' ## Backward lagged ratios
#' # no recursion
#' lagratio(1:10,1)
#' lagratio(1:10,2)
#' # with recursion
#' lagratio(1:10,1,2)
#' lagratio(1:10,2,2)
#' 
#' ## Forward lagged ratios
#' # no recursion
#' lagratio(10:1,1,direction="forward")
#' lagratio(10:1,2,direction="forward")
#' # with recursion
#' lagratio(10:1,1,2,direction="forward")
#' lagratio(10:1,2,2,direction="forward")
#'
#' @export
#'
lagratio <- function(x,lag=1L,recursion=1L,differences=recursion,direction=c("backward","forward"),...) {
  ## Some checks
  direction <- match.arg(direction)
  if(any(x==0)) stop("Will not work with zeroes in 'x'.",call.=FALSE)
  if(any(class(x) %in% c("POSIXt","POSIXct"))) stop("Function does not work for 'POSIXt' objects.",call.=FALSE)
  if (!recursion>0) stop("'recursion' value must be >0.",call.=FALSE)
  ## Flip vector if ratio direction is forward
  if (direction=="forward") x <- rev(x)
  ## Compute lagged ratio
  res <- exp(diff(log(x),lag=lag,differences=differences,...))
  ## Flip the resulting vector if direction is forward
  if (direction=="forward") res <- rev(res)
  ## Return the result
  res
}


#' @title Constructs the correction-factor used when back-transforming log-transformed values.
#'
#' @description Constructs the correction-factor used when back-transforming log-transformed values according to the method in Sprugel (1983).  Sprugel's main formula -- exp((syx^2)/2) -- is used when syx is estimated for natural log transformed data.  He noted that a formula for any based could be constructed by multiplying the syx term by log_e(base) to give exp(((log_e(base)*syx)^2)/2).  This more general formula is implemented in this function (where, of course, if the base is exp(1) then the general formula reduces to the original specific formula.)
#'
#' @param obj An object from \code{lm}.
#' @param base A single numerica value that indicates the base of the logarithm used.
#'
#' @return A numeric value that is the correction factor according to Sprugel (1983).
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @references Sprugel, D.G. 1983.  Correcting for bias in log-transformed allometric equations. Ecology 64:209-210.
#'
#' @keywords manip
#'
#' @examples
#' # toy data
#' df <- data.frame(y=rlnorm(10),x=rlnorm(10))
#' df$logey <- log(df$y)
#' df$log10y <- log10(df$y)
#' df$logex <- log(df$x)
#' df$log10x <- log10(df$x)
#' 
#' # model and predictions on loge scale
#' lme <- lm(logey~logex,data=df)
#' ( ploge <- predict(lme,data.frame(logex=log(10))) )
#' ( pe <- exp(ploge) )
#' ( cfe <- logbtcf(lme) )
#' ( cpe <- cfe*pe )
#' 
#' # model and predictions on log10 scale
#' lm10 <- lm(log10y~log10x,data=df)
#' plog10 <- predict(lm10,data.frame(log10x=log10(10)))
#' p10 <- 10^(plog10)
#' ( cf10 <- logbtcf(lm10,10) )
#' ( cp10 <- cf10*p10 )
#' 
#' # cfe and cf10, cpe and cp10 should be equal
#' all.equal(cfe,cf10)
#' all.equal(cpe,cp10)
#' 
#' @rdname logbtcf
#' @export
logbtcf <- function(obj,base=exp(1)) {
  if (!all(class(obj)=="lm")) stop("'obj' must be from lm().",call.=FALSE)
  exp(((log(base)*summary(obj)$sigma)^2)/2)
}


#' @name oddeven
#' 
#' @title Determine if a number is odd or even.
#' 
#' @description Determine if a number is odd or even.
#' 
#' @param x A numeric vector.
#' 
#' @return A logical vector of the same length as x.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @keywords manip
#' 
#' @examples
#' ## Individual values
#' is.odd(1)
#' is.odd(2)
#' is.even(3)
#' is.even(4)
#' 
#' ## Vector of values
#' d <- 1:8
#' data.frame(d,odd=is.odd(d),even=is.even(d))
NULL

#' @rdname oddeven
#' @export
is.odd <- function (x) iOddEven(x,1)

#' @rdname oddeven
#' @export
is.even <- function(x) iOddEven(x,0)


## Internal function
iOddEven <- function(x,checkval) {
  if (!is.vector(x)) stop("'x' must be a vector.",call.=FALSE)
  if (!is.numeric(x)) stop("'x' must be numeric.",call.=FALSE)
  x%%2 == checkval
}


#' @title Computes the percentage of values in a vector less than or greater than (and equal to) some value.
#'
#' @description Computes the percentage of values in a vector less than or greater than (and equal to) a user-supplied value.
#' 
#' @details This function is most useful when used with an apply-type of function.
#'
#' @param x A numeric vector.
#' @param val A single numeric value.
#' @param dir A string that indicates whether the percentage is for values in \code{x} that are \dQuote{greater than and equal} \code{"geq"}, \dQuote{greater than} \code{"gt"}, \dQuote{less than and equal} \code{"leq"}, \dQuote{less than} \code{"lt"} the value in \code{val}. 
#' @param na.rm A logical that indicates whether \code{NA} values should be removed (DEFAULT) from \code{x} or not.
#' @param digits A single numeric that indicates the number of decimals the percentage should be rounded to.
#'
#' @return A single numeric that is the percentage of values in \code{x} that meet the criterion in \code{dir} relative to \code{val}.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @keywords misc
#'
#' @examples
#' ## vector of values
#' ( tmp <- c(1:8,NA,NA) )
#' 
#' ## percentages excluding NA values
#' perc(tmp,5)
#' perc(tmp,5,"gt")
#' perc(tmp,5,"leq")
#' perc(tmp,5,"lt")
#' 
#' ## percentages including NA values
#' perc(tmp,5,na.rm=FALSE)
#' perc(tmp,5,"gt",na.rm=FALSE)
#' perc(tmp,5,"leq",na.rm=FALSE)
#' perc(tmp,5,"lt",na.rm=FALSE)
#' 
#' @export
perc <- function(x,val,dir=c("geq","gt","leq","lt"),na.rm=TRUE,digits=getOption("digits")) {
  ## Some checks
  dir <- match.arg(dir)
  if (!class(x) %in% c("numeric","integer")) stop("'perc' only works for numeric vectors.",call.=FALSE)
  if (length(val)>1) warning("Only the first value of 'val' was used.",call.=FALSE)
  ## Find sample size (don't or do include NA values)
  n <- ifelse(na.rm,length(x[!is.na(x)]),length(x))
  ## Compute percentage in dir(ection) of val(ue), but return
  ##   a NaN if the x has no valid values
  if (n==0) return(NaN)
  else { # find values that fit criterion
    switch(dir,
           geq= {tmp <- x[x>=val]},
           gt = {tmp <- x[x>val]},
           leq= {tmp <- x[x<=val]},
           lt = {tmp <- x[x<val]}
    ) # end switch
    ## must remove NA values (even if asked not to because they
    ## will appear to be less than val ... i.e., NAs were included
    ## in n above if asked for but they should not be included in
    ## the vector of values that fit the criterion) to find
    ## number that match the criterion
    tmp <- length(tmp[!is.na(tmp)])
  }
  round(tmp/n*100,digits)
}


#' @name rcumsum
#' 
#' @title Computes the prior to or reverse cumulative sum of a vector.
#'
#' @description Computes the prior-to (i.e., the cumulative sum prior to but not including the current value) or the reverse (i.e., the number that large or larger) cumulative sum of a vector.  Also works for 1-dimensional tables, matrices, and data.frames, though it is best used with vectors.
#'
#' @note An \code{NA} in the vector causes all returned values at and after the first \code{NA} for \code{pcumsum} and at and before the last \code{NA} for \code{rcumsum} to be \code{NA}.  See the examples.
#'
#' @param x a numeric object.
#'
#' @return A numeric vector that contains the prior-to or reverse cumulative sums.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{cumsum}}.
#'
#' @keywords misc
#'
#' @examples
#' ## Simple example
#' cbind(vals=1:10,
#'       cum=cumsum(1:10),
#'       pcum=pcumsum(1:10),
#'       rcum=rcumsum(1:10))
#'
#' ## Example with NA
#' vals <- c(1,2,NA,3)
#' cbind(vals,
#'       cum=cumsum(vals),
#'       pcum=pcumsum(vals),
#'       rcum=rcumsum(vals))
#'
#' ## Example with NA
#' vals <- c(1,2,NA,3,NA,4)
#' cbind(vals,
#'       cum=cumsum(vals),
#'       pcum=pcumsum(vals),
#'       rcum=rcumsum(vals))
#'       
#' ## Example with a matrix
#' mat <- matrix(c(1,2,3,4,5),nrow=1)
#' cumsum(mat)
#' pcumsum(mat)
#' rcumsum(mat)
#' 
#' ## Example with a table (must be 1-d)
#' df <- sample(1:10,100,replace=TRUE)
#' tbl <- table(df)
#' cumsum(tbl)
#' pcumsum(tbl)
#' rcumsum(tbl)
#' 
#' ## Example with a data.frame (must be 1-d)
#' df <- sample(1:10,100,replace=TRUE)
#' tbl <- as.data.frame(table(df))[,-1]
#' cumsum(tbl)
#' pcumsum(tbl)
#' rcumsum(tbl)
NULL

#' @rdname rcumsum
#' @export
rcumsum <- function(x) {
  iChkCumSum(x)
  rev(cumsum(rev(x)))
}

#' @rdname rcumsum
#' @export
pcumsum <- function(x) {
  iChkCumSum(x)
  cumsum(x)-x
}

## Internal function for Xcumsum()
iChkCumSum <- function(x) {
  tmp <- class(x)
  if ("matrix" %in% tmp | "data.frame" %in% tmp) {
    if (all(dim(x)!=1)) stop("'x' is not 1-dimensional.",call.=FALSE)
  }
  if ("table" %in% tmp | "xtabs" %in% tmp) {
    if (length(dim(x))>1) stop("'x' is not 1-dimensional.",call.=FALSE)
  }
  if (!is.numeric(x)) stop("'x' must be numeric.",call.=FALSE)
}



#' @title Computes standard error of the mean.
#'
#' @description Computes the standard error of the mean (i.e., standard deviation divided by the square root of the sample size).
#'
#' @param x A numeric vector.
#' @param na.rm A logical that indicates whether missing values should be removed before computing the standard error.
#' 
#' @return A single numeric that is the standard error of the mean of \code{x}.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso See \code{\link[sciplot]{se}} in \pkg{sciplot} for similar functionality.
#'
#' @keywords manip
#'
#' @examples
#' x <- 1:20
#' sd(x)/sqrt(length(x))
#' se(x)
#' 
#' # all return NA if missing values are not removed
#' x2 <- c(x,NA)
#' sd(x2)/sqrt(length(x2))
#' 
#' # Better if missing values are removed
#' se(x2,na.rm=FALSE)
#' sd(x2,na.rm=TRUE)/sqrt(length(x2[complete.cases(x2)]))
#' se(x2)
#' 
#' @export
se <- function (x,na.rm=TRUE) {
  if (!is.vector(x)) stop("'x' must be a vector.",call.=FALSE)
  if (!is.numeric(x)) stop("'x' must be numeric.",call.=FALSE)
  if (na.rm) x <- x[complete.cases(x)]
  sqrt(var(x)/length(x))
}


#' @name Subset
#' 
#' @title Subsets/filters a data frame and drops the unused levels.
#'
#' @description Subsets/filters a data frame and drops the unused levels.
#'
#' @details Newbie students using R expect that when a factor variable is subsetted with \code{\link{subset}} or filtered with \code{\link[dplyr]{filter}} that any original levels that are no longer used after the subsetting or flitering will be ignored.  This, however, is not the case and often results in tables with empty cells and figures with empty bars.  One remedy is to use \code{\link[gdata]{drop.levels}} from \pkg{gdata} immediately following the \code{\link{subset}} or \code{\link[dplyr]{filter}} call.  This generally becomes a repetitive sequence for most newbie students; thus, \code{Subset} and \code{filterD} incorporate these two functions into one function.
#' 
#' \code{Subset} is a wrapper to \code{\link{subset}} with a catch for non-data.frames and a specific call to \code{\link[gdata]{drop.levels}} just before the data.frame is returned.  I also added an argument to allow resetting the row names.  \code{filterD} is a wrapper for \code{\link[dplyr]{filter}} from \pkg{dplyr} followed by \code{\link[gdata]{drop.levels}} just before the data.frame is returned.  Otherwise, there is no new code here.
#' 
#' These functions are used only for data frames.
#' 
#' @param x A data frame.
#' @param subset A logical expression that indicates elements or rows to keep: missing values are taken as false.
#' @param select An expression, that indicates columns to select from a data frame.
#' @param drop passed on to \code{[} indexing operator.
#' @param resetRownames A logical that indicates if the rownames should be reset after the subsetting (\code{TRUE}; default).  Resetting rownames will simply number the rows from 1 to the number of rows in the result.
#' @param \dots further arguments to be passed to or from other methods.
#'
#' @return A data frame with the subsetted rows and selected variables.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#' 
#' @section IFAR Chapter: \href{https://fishr.wordpress.com/books/ifar/}{2-Basic Data Manipulations}.
#'
#' @seealso See \code{subset} and \code{\link[dplyr]{filter}} from \pkg{dplyr} for similar functionality.  See \code{\link[gdata]{drop.levels}} in \pkg{gdata} and \code{\link{droplevels}} for related functionality.
#'
#' @keywords misc
#'
#' @examples
#' ## The problem -- note use of unused level in the final table.
#' levels(iris$Species)
#' iris.set1 <- subset(iris,Species=="setosa" | Species=="versicolor")
#' levels(iris.set1$Species)
#' xtabs(~Species,data=iris)
#'
#' ## A simpler fix using Subset
#' iris.set2 <- Subset(iris,Species=="setosa" | Species=="versicolor")
#' levels(iris.set2$Species)
#' xtabs(~Species,data=iris.set2)
#' 
#' ## A simpler fix using filterD
#' iris.set3 <- filterD(iris,Species=="setosa" | Species=="versicolor")
#' levels(iris.set3$Species)
#' xtabs(~Species,data=iris.set3)
#'
NULL

#' @rdname Subset
#' @export
Subset <- function(x,subset,select,drop=FALSE,resetRownames=TRUE,...) {
  if (!is.data.frame(x)) stop("Subset should only be used with data frames.  See ?subset for other structures.",call.=FALSE)
  if (missing(subset)) r <- TRUE
  else {
    e <- substitute(subset)
    r <- eval(e, x, parent.frame())
    if (!is.logical(r)) stop("'subset' must evaluate to logical.",call.=FALSE)
    r <- r & !is.na(r)
  }
  if (missing(select)) vars <- TRUE
  else {
    nl <- as.list(1:ncol(x))
    names(nl) <- names(x)
    vars <- eval(substitute(select),nl,parent.frame())
  }
  res <- gdata::drop.levels(x[r,vars,drop = drop],reorder=FALSE)
  if (resetRownames) rownames(res) <- NULL
  if (nrow(res)==0) warning("The resultant data.frame has 0 rows.  Try str() on the result.\n",call.=FALSE)
  res
}

#' @rdname Subset
#' @export
filterD <- function(x,...) {
  res <- dplyr::filter(x,...)
  res <- gdata::drop.levels(res,reorder=FALSE)
  if (nrow(res)==0) warning("The resultant data.frame has 0 rows.  Try str() on the result.\n",call.=FALSE)
  res
}


#' @title Finds the number of valid (non-NA) values in a vector.
#'
#' @description Finds the number of valid (non-NA) values in a vector.
#'
#' @param object A vector.
#'
#' @return A single numeric value that is the number of non-\code{NA} values in a vector.
#' 
#' @seealso See \code{\link[plotrix]{valid.n}} in \pkg{plotrix} and \code{\link[gdata]{nobs}} in \pkg{gdata} for similar functionality.  See \code{\link{is.na}} for finding the missing values.
#' 
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @section IFAR Chapter: \href{https://fishr.wordpress.com/books/ifar/}{2-Basic Data Manipulations}.
#' 
#' @keywords manip
#' 
#' @examples
#' junk1 <- c(1,7,2,4,3,10,NA)
#' junk2 <- c("Derek","Hugh","Ogle","Santa","Claus","Nick",NA,NA)
#' junk3 <- factor(junk2)
#' junk4 <- c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,NA,NA)
#' 
#' validn(junk1)
#' validn(junk2)
#' validn(junk3)
#' validn(junk4)
#'  
#' @export
validn <- function(object) {
  ## Process easily if it is not a factor
  if (!is.factor(object)) {
    ## Check to make sure that it is a vector
    if (!is.vector(object)) stop("'object' must be a vector.",call.=FALSE)
    else return(sum(!is.na(object)))
  } else { # is a factor
    ## Make sure that it is a factor vector
    if (!is.null(dim(object))) stop("'object' must be a vector.",call.=FALSE)
    else return(sum(!is.na(object)))
  }
}
