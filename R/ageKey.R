#'Applies an age-length key to individuals in a length-sample.
#'
#'This function uses either the semi- or completely-random methods to assign
#'ages to individual fish in a length-sample according to the information in a
#'supplied age-length key.  The methods generally follow the algorithm outlined
#'by Isermann and Knight (2005).
#'
#'The age-length key sent in \code{key} must be constructed with length intervals
#'as rows and age values as columns.  The row names of \code{key} (i.e., \code{rownames(key)})
#'must contain the mininum values of each length interval (e.g., if an interval
#'is 100-109 then the corresponding row name must be 100).  The column names of
#'\code{key} (i.e., \code{colnames(key)}) must contain the age values (e.g., the
#'columns can NOT be named with \dQuote{age.1}, for example).
#'
#'The length intervals in the rows of the age-length key sent in \code{key} must
#'contain all of the length intervals present in the length sample to which the
#'age-length key is to be applied (i.e., sent in the \dQuote{length} portion of the
#'\code{formula}).  If this constraint is not met then the function will stop
#'with an error message. 
#'
#'If \code{len.breaks=NULL} then the length categories for the length sample
#'are determined with a starting category as the minimum value of the row names
#'and a width of the length interval categories as determined by the minimum
#'difference in adjacent row names of the age-length key matrix.  If length
#'categories of differing widths were used in construction of the age-length
#'key then the breaks used should be supplied to \code{len.breaks=}.  Use of the
#'\code{len.breaks=} argument may be useful when \dQuote{uneven} width length
#'categories must be used because the lengths in the length sample are not fully
#'represented in the age sample if \dQuote{narrower} widths or intervals are used.
#'See the examples below.
#'
#'Assigned ages will be stored in the column identified in the left-hand-side of
#'\code{formula} (if the formula has both a left- and right-hand-side.  If this
#'variable is missing in the formula then the new column wil be labeled with \code{age}.
#'
#'@param key A numeric matrix that contains the age-length key.
#'@param formula A formula of the form \code{age~length} where \dQuote{age}
#'generically represents a variable in \code{data} that will contain the estimated
#'age measurements once the key is applied (i.e., currently missing) and \dQuote{length}
#'generically represents a variable in \code{data} that contains known length 
#'measurements.  It is possible to use \code{~length} in which case a new variable
#'called \dQuote{age} will be created in the resulting data frame.
#'@param data A data.frame that minimally contains the length measurements and
#'possibly contains a variable that will receive the age estimates.  See description
#'for \code{formula}.
#'@param type A string that indicates whether to use the semi-random
#'(\code{type="SR"}, default) or completely-random (\code{type="CR"})
#'techniques for assigning ages to individual fish.
#'@param len.breaks A numeric vector of lower values for the break points of
#'the length categories.
#'@return The original data frame in \code{data} is returned with assigned ages
#'added to the column supplied in \code{formula} (see details) or in an additional
#'column labeled as \code{age}.
#'@references Isermann, D.A. and C.T. Knight.  2005.  A Computer Program for
#'Age-Length Keys Incorporating Age Assignment to Individual Fish.  North
#'American Journal of Fisheries Management, 25:1153-1160.
#'@seealso \code{alk}, \code{alkprop}, and \code{alkss} in \pkg{fishmethods} and
#'the entire \pkg{ALKr} package.
#'@section fishR vignette: \url{https://sites.google.com/site/fishrfiles/gnrl/AgeLengthKey.pdf}
#'@export
#'@keywords manip
#'@examples
#'## Get data with length measurements and some assigned ages
#'data(WR79)
#'# create the age and length samples from the data frame
#'WR.age <- Subset(WR79, !is.na(age))
#'WR.len <- Subset(WR79, is.na(age))
#'
#'## First Example -- Even breaks for length categories
#'# find smallest length in age sample -- create appropriate length categories
#'summary(WR.age$len)
#'WR.age.mod <- lencat(~len,data=WR.age,startcat=35,w=5,drop.levels=TRUE)
#'# create age-length key
#'raw <- table(WR.age.mod$LCat, WR.age.mod$age)
#'( WR.key <- prop.table(raw, margin=1) )
#'# apply age-length key to length sample
#'WR.len.mod <- ageKey(WR.key,age~len,data=WR.len)
#'head(WR.len.mod)
#'# combine original age sample with new length sample and summarize
#'WR.comb <- rbind(WR.age, WR.len.mod)
#'Summarize(len~age,data=WR.comb,digits=2)
#'
#'## Second Example -- Uneven breaks for length categories
#'# create breaks as 35-40, 40-45, ..., 95-100, 100-110, 110-130
#'brks <- c(seq(35,100,5),110,130)
#'WR.age.mod <- lencat(~len,data=WR.age,breaks=brks,drop.levels=TRUE)
#'# create age-length key
#'raw <- table(WR.age.mod$LCat, WR.age.mod$age)
#'( WR.key <- prop.table(raw, margin=1) )
#'# apply age-length key to length sample
#'WR.len.mod <- ageKey(WR.key,age~len,data=WR.len,len.breaks=brks)
#'head(WR.len.mod)
#'# combine original age sample with new length sample and summarize
#'WR.comb <- rbind(WR.age, WR.len.mod)
#'Summarize(len~age,data=WR.comb,digits=2)
#'
#'## Third Example -- length sample does not have an age variable (checking purposes only)
#'WR.len1 <- WR.len[,-3]
#'# apply age-length key to length sample
#'WR.len1.mod <- ageKey(WR.key,~len,data=WR.len1)
#'head(WR.len1.mod)

#'
ageKey <- function(key,formula,data,type=c("SR","CR"),len.breaks=NULL) {
  #### Semi-random assignment internal function
  ageKey.sr <- function(key,age.cats,data,data.len.cats,ca) {                        
    for (i in data.len.cats) {  ### Cycle through len categories in L sample                                              
      # Number in len interval from L sample
      len.n <- nrow(data[data$TMPLCAT==i,])
      # Conditional probability of age for len interval
      age.prob <- key[which(as.numeric(rownames(key))==i),]
      # Integer number of fish for each age
      age.freq <- floor(len.n*age.prob)
      # Vector of ages for integer counts
      ages <- rep(age.cats,age.freq)
      # Identify and deal with fractionality
      if (length(ages)<len.n) {
        # How many fish must be added?
        num.add <- len.n-length(ages)
        # Randomly ages for added fish
        ages.add <- sample(age.cats,num.add,replace=TRUE,prob=age.prob)
        # Add additional fish ages to end
        ages <- c(ages,ages.add)
      }
      # Randomly mix up the ages vector
      if (length(ages)>1) { ages <- sample(ages,length(ages),replace=FALSE) }
      # Replace rows of age col w/ assigned ages
      data[data$TMPLCAT==i,ca] <- ages                                               
    }
    data
  } ### end of ageKey.sr internal function
  
  
  ### Completely random assignment internal function
  ageKey.cr <- function(key,age.cats,data,ca) {                                    
    for (i in 1:dim(data)[1]) { #### Cycle through the fish
      # Conditional probability of age for length interval
      age.prob <- key[which(as.numeric(rownames(key))==data$TMPLCAT[i]),]
      data[i,ca] <- sample(age.cats,1,prob=age.prob)                              
    }
    data
  } ### end of ageKey.cr internal function


  ### Start of main function      
  type <- match.arg(type)
  cl <- getVarFromFormula(formula,data)
  if (length(cl)==1) ca <- "age"
    else {
      ca <- cl[1]
      cl <- cl[2]
    }
  # Check if key is proportions, if not change to proportions
  if (any(key>1,na.rm=TRUE)) key <- prop.table(key,1)
  # Remove rows with row sums of NA or 0 (i.e., only keeps lens with data in key)
  key.row.sum <- apply(key,1,sum)
  key.row.sum <- key.row.sum[!is.na(key.row.sum) & key.row.sum!=0]
  # Warn if row sums in key are not =1 (implies bad key)
  if (any(key.row.sum!=1)) warning("Key contains row that does not sum to 1.",call.=FALSE)
  # Find the length categories that are present in the key
  da.len.cats <- as.numeric(names(key.row.sum))
  # Check about min and max value in length sample relative to same on key
  if (min(data[,cl])<min(da.len.cats)) {
    stop(paste("The minimum observed length in the length sample (",min(data[,cl]),
               ")\n is less than the smallest length category in the age-length key (",
               min(da.len.cats),").\n  You should include fish of these lengths in
               your age sample or exclude fish of this length from your length sample.\n",
               sep=""),call.=FALSE)
  }
  if (max(data[,cl])>max(da.len.cats)) {
    warning(paste("The maximum observed length in the length sample (",max(data[,cl]),
                  ")\n is greater than the largest length category in the age-length key (",
                  max(da.len.cats),").\n Thus, the last length category will be treated as all-inclusive.\n",
                  sep=""),call.=FALSE)
  }
  # Create length categories var (TMPLCAT) for L sample
  if (is.null(len.breaks)) len.breaks <- da.len.cats
  options(warn=-1)  # suppress warnings from lencat()
  data <- lencat(as.formula(paste("~",cl)),data=data,breaks=len.breaks,as.fact=FALSE,vname="TMPLCAT")
  options(warn=1)
  # Find Vector of length cats present in L sample
  data.len.cats <- as.numeric(names(table(data$TMPLCAT)))                                  
  # Add variable for ages for L sample (initially NAs)
  if (!any(names(data)==ca)) {
    data <- data.frame(data,rep(NA,length(data[,cl])))
    names(data)[ncol(data)] <- ca
  }
  # Find vector of age categories in key
  age.cats <- as.numeric(colnames(key))
  # Perform the randomization depending on type chosen by user
  switch(type,
    SR=,Sr=,sr=,S=,s= {data <- ageKey.sr(key,age.cats,data,data.len.cats,ca)},
    CR=,Cr=,cr=,C=,c= {data <- ageKey.cr(key,age.cats,data,ca)}
  )
  # Remove length category column that was added
  data <- data[,-which(names(data)=="TMPLCAT")]
  data
}
