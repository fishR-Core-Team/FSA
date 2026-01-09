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
#> a 54 60 69
#> b 68 79 61
#> c 81 65 67
#> d 52 81 63
#> e 68 78 54
sumTable(dat~g1*g2,data=d,FUN=validn)       # get sample size (better way)
#> Warning: First RHS variable was converted to a factor.
#> Warning: Second RHS variable was converted to a factor.
#>    A  B  C
#> a 54 60 69
#> b 68 79 61
#> c 81 65 67
#> d 52 81 63
#> e 68 78 54
sumTable(dat~g1*g2,data=d,FUN=mean)         # get mean
#> Warning: First RHS variable was converted to a factor.
#> Warning: Second RHS variable was converted to a factor.
#>           A          B          C
#> a 0.0148345  0.1074480 -0.1450652
#> b 0.1434694 -0.0873898  0.0415869
#> c 0.0112921  0.0896951 -0.0375250
#> d 0.1491436  0.0038763 -0.1765595
#> e 0.0074753 -0.0459891 -0.0141507
sumTable(dat~g1*g2,data=d,FUN=sd)           # get sd
#> Warning: First RHS variable was converted to a factor.
#> Warning: Second RHS variable was converted to a factor.
#>           A         B         C
#> a 0.9763261 0.9800017 1.0292878
#> b 1.0259764 1.1817831 1.1506830
#> c 0.9468045 1.1092575 0.9985559
#> d 0.9998303 0.9614649 0.8771359
#> e 1.0735484 0.9451659 0.9453150
sumTable(dat~g1*g2,data=d,FUN=sd,digits=1)  # show digits= argument
#> Warning: First RHS variable was converted to a factor.
#> Warning: Second RHS variable was converted to a factor.
#>     A   B   C
#> a 1.0 1.0 1.0
#> b 1.0 1.2 1.2
#> c 0.9 1.1 1.0
#> d 1.0 1.0 0.9
#> e 1.1 0.9 0.9

## Also demonstrate use in the 1-way example -- but see Summarize()
sumTable(dat~g1,data=d,FUN=validn)
#> Warning: RHS variable was converted to a factor.
#>   a   b   c   d   e 
#> 183 208 213 196 200 
sumTable(dat~g1,data=d,FUN=mean)
#> Warning: RHS variable was converted to a factor.
#>          a          b          c          d          e 
#> -0.0150905  0.0259083  0.0198623 -0.0155806 -0.0192148 

## Example with a missing value (compare to above)
d$dat[1] <- NA
sumTable(dat~g1,data=d,FUN=validn)  # note use of validn
#> Warning: RHS variable was converted to a factor.
#>   a   b   c   d   e 
#> 183 208 213 195 200 
sumTable(dat~g1,data=d,FUN=mean,na.rm=TRUE)
#> Warning: RHS variable was converted to a factor.
#>          a          b          c          d          e 
#> -0.0150905  0.0259083  0.0198623 -0.0124307 -0.0192148 
```
