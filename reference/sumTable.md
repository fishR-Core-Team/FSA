# Creates a one- or two-way table of summary statistics.

Creates a one- or two-way table of summary statistics for a quantitative
variable.

## Usage

``` r
sumTable(formula, ...)

# S3 method for class 'formula'
sumTable(formula, data = NULL, FUN = mean, digits = getOption("digits"), ...)
```

## Arguments

- formula:

  A formula with a quantitative variable on the left-hand-side and one
  or two factor variables on the right-hand-side. See details.

- ...:

  Other arguments to pass through to `FUN`.

- data:

  An optional data frame that contains the variables in `formula`.

- FUN:

  A scalar function that identifies the summary statistics. Applied to
  the quantitative variable for all data subsets identified by the
  combination of the factor(s). Defaults to `mean`.

- digits:

  A single numeric that indicates the number of digits to be used for
  the result.

## Value

A one-way array of values if only one factor variable is supplied on the
right-hand-side of `formula`. A two-way matrix of values if two factor
variables are supplied on the right-hand-side of `formula`. These are
the same classes of objects returned by
[`tapply`](https://rdrr.io/r/base/tapply.html).

## Details

The formula must be of the form `quantitative~factor` or
`quantitative~factor*factor2` where `quantitative` is the quantitative
variable to construct the summaries for and `factor` and `factor2` are
factor variables that contain the levels for which separate summaries
should be constructed. If the variables on the right-hand-side are not
factors, then they will be coerced to be factors and a warning will be
issued.

This function is largely a wrapper to
[`tapply()`](https://rdrr.io/r/base/tapply.html), but only works for one
quantitative variable on the left-hand-side and one or two factor
variables on the right-hand-side. Consider using
[`tapply`](https://rdrr.io/r/base/tapply.html) for situations with more
factors on the right-hand-side.

## See also

See [`tapply`](https://rdrr.io/r/base/tapply.html) for a more general
implementation. See
[`Summarize`](https://fishr-core-team.github.io/FSA/reference/Summarize.md)
for a similar computation when only one factor variable is given.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## The same examples as in the old aggregate.table in gdata package
## but data in data.frame to illustrate formula notation
d <- data.frame(g1=sample(letters[1:5], 1000, replace=TRUE),
                g2=sample(LETTERS[1:3], 1000, replace=TRUE),
                dat=rnorm(1000))

sumTable(dat~g1*g2,data=d,FUN=length)       # get sample size
#> Warning: First RHS variable was converted to a factor.
#> Warning: Second RHS variable was converted to a factor.
#>    A  B  C
#> a 65 66 52
#> b 64 76 69
#> c 72 75 65
#> d 59 71 68
#> e 62 74 62
sumTable(dat~g1*g2,data=d,FUN=validn)       # get sample size (better way)
#> Warning: First RHS variable was converted to a factor.
#> Warning: Second RHS variable was converted to a factor.
#>    A  B  C
#> a 65 66 52
#> b 64 76 69
#> c 72 75 65
#> d 59 71 68
#> e 62 74 62
sumTable(dat~g1*g2,data=d,FUN=mean)         # get mean
#> Warning: First RHS variable was converted to a factor.
#> Warning: Second RHS variable was converted to a factor.
#>            A          B          C
#> a -0.1381336 -0.1197033  0.1430396
#> b  0.1027714 -0.0612817 -0.2037045
#> c  0.0456455  0.0692026 -0.1655908
#> d  0.2189534  0.1364768 -0.0439938
#> e  0.0646782  0.0521127 -0.0645313
sumTable(dat~g1*g2,data=d,FUN=sd)           # get sd
#> Warning: First RHS variable was converted to a factor.
#> Warning: Second RHS variable was converted to a factor.
#>           A        B         C
#> a 1.0159332 1.173373 0.9374071
#> b 0.9897397 1.023984 1.0252271
#> c 0.8883011 1.112907 1.0591669
#> d 0.8983995 1.015777 0.9749981
#> e 0.9292071 1.032470 1.0652494
sumTable(dat~g1*g2,data=d,FUN=sd,digits=1)  # show digits= argument
#> Warning: First RHS variable was converted to a factor.
#> Warning: Second RHS variable was converted to a factor.
#>     A   B   C
#> a 1.0 1.2 0.9
#> b 1.0 1.0 1.0
#> c 0.9 1.1 1.1
#> d 0.9 1.0 1.0
#> e 0.9 1.0 1.1

## Also demonstrate use in the 1-way example -- but see Summarize()
sumTable(dat~g1,data=d,FUN=validn)
#> Warning: RHS variable was converted to a factor.
#>   a   b   c   d   e 
#> 183 209 212 198 198 
sumTable(dat~g1,data=d,FUN=mean)
#> Warning: RHS variable was converted to a factor.
#>          a          b          c          d          e 
#> -0.0515904 -0.0580653 -0.0107865  0.0990734  0.0195224 

## Example with a missing value (compare to above)
d$dat[1] <- NA
sumTable(dat~g1,data=d,FUN=validn)  # note use of validn
#> Warning: RHS variable was converted to a factor.
#>   a   b   c   d   e 
#> 183 208 212 198 198 
sumTable(dat~g1,data=d,FUN=mean,na.rm=TRUE)
#> Warning: RHS variable was converted to a factor.
#>          a          b          c          d          e 
#> -0.0515904 -0.0637175 -0.0107865  0.0990734  0.0195224 
```
