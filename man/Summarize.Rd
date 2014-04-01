\name{Summarize}
\alias{Summarize}
\alias{Summarize.default}
\alias{Summarize.formula}
\title{Summary statistics for a numeric or factor variable.}
\usage{
Summarize(object, ...)

\method{Summarize}{default}(object, digits = getOption("digits"),
  addtotal = TRUE, percent = TRUE, percdigs = 2, na.rm = TRUE,
  exclude = "", ...)

\method{Summarize}{formula}(object, data = NULL,
  digits = getOption("digits"), percent = c("row", "column", "total",
  "none"), percdigs = 2, addtotal = TRUE, na.rm = TRUE, exclude = "",
  ...)
}
\arguments{
  \item{object}{A vector of numeric or factor data.}

  \item{data}{An optional data frame containing the
  variables in the model.}

  \item{digits}{A numeric indicating the number of decimals
  to round the numeric summaries to.}

  \item{addtotal}{A logical indicating whether totals
  should be added to tables (\code{=TRUE}, default) or
  not.}

  \item{percent}{A logical that indicates whether frequency
  tables should include percentages (\code{=TRUE}, default)
  or not for a single categorical variable or a string
  indicating the type of percents to compute for a two-way
  table constructed wtih the formula.}

  \item{percdigs}{A numeric indicating the number of
  decimals to round the percentage summaries to.}

  \item{na.rm}{A logical that indicates whether numeric
  missing values (\code{NA}) should be removed
  (\code{=TRUE}, default) or not.}

  \item{exclude}{A string containing the code that should
  be excluded from the levels of the factor variable.}

  \item{...}{Other arguments to the generic \code{summary},
  \code{sd}, or \code{table} functions.}
}
\value{
A named vector or data frame (when a quantitative variable
is separted by one or two factor variables) of summary
statistics for numeric data and a matrix of frequencies
and, possibly, percentages for factor variables.
}
\description{
Summary statistics for a single numeric or factor variable,
possibly separated by the levels of a factor variable.
Very similar to \code{summary} for a numeric variables and
\code{table} for factor variables.
}
\details{
For numeric data this is the same as \code{summary} except
that \code{Summarize} includes the sample size, valid
sample size (sample size minus number of \code{NA}s) and
standard deviation (i.e., \code{sd}).  Also the output is
ordered slightly differently.

For a factor variable this function computes a frequency
table, a percentage table (if \code{percent=TRUE}), and a
valid percentage table (percentage if \dQuote{NA}s are
excluded; if \code{percent=TRUE}).  The tables will contain
a total row if \code{addtotal=TRUE}.

The \code{object} argument can be a formula of form
\code{y~x} where \code{y} can be either a numeric or factor
variable and \code{x} can be only a factor variable or
\code{y~x*z} where \code{z} is a second factor variable.
More complicated formulas are not supported.  When \code{y}
is numeric then the summary statistics of \code{y} will be
computed for each level in \code{x} or the combinations of
the levels of \code{x} and \code{z}.  When \code{y} is a
factor then a two-way table will be computed.  If
\code{addtotal=TRUE} then row totals only will be added.
If \code{percent=TRUE} then a row percentages table will be
computed such that the percentages represent the percent in
the levels of \code{x} for each level of \code{y}.
}
\note{
The \code{Summarize} function, when applied to a vector of
quantitative data, produces results of basic statistics
similar to that provided by \code{summary}.  The primary
addition in the results of \code{Summarize} is the
inclusion of the standard deviation and the (potential)
calculation of a valid sample size.  When applied to a
vector of categorical data, \code{Summarize} produces a
frequency table with (by default) percentages and (perhaps)
valid percentages.  The results for categorical data are
NOT meant to replace the \code{table} function but to
provide an alternative and to provide a useful result if
the student provides it with categorical data.

Students often need to look at basic statistics of a
quantitative variable separated for different levels of a
categorical variable.  This type of analysis can be made
with \code{tapply}, \code{by}, or \code{aggregate} (or a
few other functions in other packages) but the use of these
functions are not obvious to newbie students or return
results in a format that is not obvious to newbie students.
Thus, the formula method to the \code{Summarize} generic
function allows newbie students to use a common notation
(i.e., formula) to easily compute summary statistics for a
quantitative variable separated by the levels of a factor.
}
\examples{
## Create a numeric vector
y <- c(0,0,runif(98))

# typical output of summary()
summary(y)

# this function
Summarize(y)

# this function, but controlling the number of digits
Summarize(y,digits=3)

## Factor vector (excluding "NA"s in second call)
x <- factor(sample(c("A","B","C","NA"),100,replace=TRUE))
Summarize(x)
Summarize(x,exclude="NA")

## Factor vector with UNKNOWNs
z <- factor(sample(c("male","female","UNKNOWN"),100,replace=TRUE))
Summarize(z)
Summarize(z,exclude="UNKNOWN")

## Numeric vector by levels of a factor variable
Summarize(y~x,digits=3)
Summarize(y~x,digits=3,exclude="NA")
Summarize(y~z,digits=3)
Summarize(y~z,digits=3,exclude="UNKNOWN")

#### Using the data= argument
## create a data.frame with two extra quantitative variables
w <- sample(1:3,100,replace=TRUE)
v <- sample(1:3,100,replace=TRUE)
df <- data.frame(y,w,v,x,z)

## Single variables using formula notation
Summarize(y~1,data=df,digits=3)
Summarize(x~1,data=df,exclude="NA")

## Summarize quantitative by one or two factor variables
Summarize(y~z,data=df,digits=3,exclude="UNKNOWN")
Summarize(y~x*z,data=df,digits=3)
Summarize(y~x*z,data=df,digits=3,exclude="NA")
Summarize(y~x*z,data=df,digits=3,exclude=c("NA","UNKNOWN"))

## What happens if RHS of formula is not a factor
Summarize(y~w,data=df)
Summarize(y~w*v,data=df)

## Summarize factor variable by a factor variable
Summarize(x~z,data=df)
Summarize(x~z,data=df,exclude="NA")
Summarize(x~z,data=df,exclude=c("NA","UNKNOWN"))

## Summarizing all variables in a data frame
lapply(as.list(df),Summarize,digits=4)
}
\seealso{
\code{summary}, \code{table}, \code{tapply},
\code{summaryBy} in \pkg{doBy}, \code{describe} in
\pkg{psych}, \code{describe} in \pkg{prettyR}, and
\code{basicStats} in \pkg{fBasics}.
}
\keyword{misc}

