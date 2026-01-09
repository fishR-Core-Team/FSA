# Summarize capture histories in individual fish format.

Use to summarize a capture history data file that is in the “individual”
fish format (see
[`capHistConvert`](https://fishr-core-team.github.io/FSA/reference/capHistConvert.md)
for a discussion of data file format types). Summarized capture history
results may be used in the Lincoln-Petersen, Schnabel,
Schumacher-Eschmeyer, or Jolly-Seber methods for estimating population
abundance (see
[`mrClosed`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md)
and
[`mrOpen`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md)).

## Usage

``` r
capHistSum(df, cols2use = NULL, cols2ignore = NULL)

is.CapHist(x)

# S3 method for class 'CapHist'
plot(x, what = c("u", "f"), pch = 19, cex.pch = 0.7, lwd = 1, ...)
```

## Arguments

- df:

  A data.frame that contains the capture histories (and, perhaps, other
  information) in “individual” fish format. See details.

- cols2use:

  A string or numeric vector that indicates columns in `df` that contain
  the capture histories. Negative numeric values will not use those
  columns. Cannot use both `cols2use` and `col2ignore`. See details.

- cols2ignore:

  A string or numeric vector that indicates columns in `df` that do not
  contain the capture histories and should be ignored. Cannot use both
  `cols2use` and `col2ignore`.

- x:

  An object from `capHistSum`.

- what:

  A string that indicates what type of diagnostic plot to construct with
  `plot`. See details.

- pch:

  A numeric that indicates the plotting character for the diagnostic
  plot.

- cex.pch:

  A numeric that indicates the character expansion value for the
  plotting characters in the diagnostic plot. The default is to be
  “slightly smaller” (i.e., `cex.pch=0.7`).

- lwd:

  A numeric that indicates the line width in the diagnostic plot.

- ...:

  Optional arguments to send to `plot`.

## Value

If the capture history data file represents only two samples, then a
list with the following two components is returned.

- `caphist` A vector summarizing the frequency of fish with each capture
  history.

- `sum` A data.frame that contains the number of marked fish from the
  first sample (`M`), the number of captured fish in the second sample
  (`n`), and the number of recaptured (i.e. previously marked) fish in
  the second sample (`m`).

If the capture history data file represents more than two samples, then
a list with the following five components is returned

- `caphist` A vector summarizing the frequency of fish with each capture
  history.

- `sum` A data frame that contains the the number of captured fish in
  the ith sample (`n`), the number of recaptured (i.e. previously
  marked) fish in the ith sample (`m`), the number of marked fish
  returned to the population following the ith sample (`R`; this will
  equal `n` as the function currently does not handle mortalities); the
  number of marked fish in the population prior to the ith sample (`M`);
  the number of fish first seen in the ith sample (`u`); the number of
  fish last seen in the ith sample (`v`); and the number of fish seen i
  times (`f`).

- `methodB.top` A matrix that contains the top of the Method B table
  used for the Jolly-Seber method (i.e., a contingency table of capture
  sample (columns) and last seen sample (rows)).

- `methodB.bot` A data.frame that contains the bottom of the Method B
  table used for the Jolly-Seber method (i.e., the number of marked fish
  in the sample (`m`), the number of unmarked fish in the sample (`u`),
  the total number of fish in the sample (`n`), and the number of marked
  fish returned to the population following the sample (`R`).

- `m.array` A matrix that contains the the so-called “m-array”. The
  first column contains the number of fish captured on the ith event.
  The columns labeled with “cX” prefix show the number of fish
  originally captured in the ith row that were captured in the Xth
  event. The last column shows the number of fish originally captured in
  the ith row that were never recaptured.

## Details

This function requires the capture history data file to be in the
“individual” fish format. See
[`capHistConvert`](https://fishr-core-team.github.io/FSA/reference/capHistConvert.md)
for a description of this (and other) formats and for methods to convert
from other formats to the “individual” fish format. In addition, this
function requires only the capture history portion of the data file.
Thus, if `df` contains columns with non-capture history information
(e.g., fish ID, length, location, etc.) then use `cols2use=` to identify
which columns contain only the capture history information. Columns to
use can be identified by listing the column numbers (e.g., columns 2
through 7 could be included with `cols2use=2:7`). In many instances it
may be easier to identify columns to *exclude* which can be done by
preceding the column number by a negative sign (e.g., columns 1 through
3 are excluded with `cols2use=-(1:3)`).

The object returned from this function can be used directly in
[`mrClosed`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md)
and
[`mrOpen`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md).
See examples of this functionality on the help pages for those
functions.

The `plot` function can be used to construct the two diagnostic plots
described by Baillargeon and Rivest (2007). The `what="f"` plot will
plot the log of the number of fish seen i times divided by `choose(t,i)`
against i. The `what="u"` plot will plot the log of the number of fish
seen for the first time on event i against i. Baillargeon and Rivest
(2007) provide a table that can be used to diagnosed types of
heterogeneities in capture probabilities from these plots.

## Note

This function assumes that all unmarked captured fish are marked and
returned to the population (i.e., no losses at the time of marking are
allowed).

## IFAR Chapter

9-Abundance from Capture-Recapture Data.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

Baillargeon, S. and Rivest, L.-P. (2007). Rcapture: Loglinear models for
capture-recapture in R. Journal of Statistical Software, 19(5):1-31.

## See also

See `descriptive` in Rcapture for `m.array` and some of the same values
in `sum`. See
[`capHistConvert`](https://fishr-core-team.github.io/FSA/reference/capHistConvert.md)
for a descriptions of capture history data file formats and how to
convert between them. See
[`mrClosed`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md)
and
[`mrOpen`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md)
for how to estimate abundance from the summarized capture history
information.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
# data.frame with IDs in the first column
head(PikeNYPartial1)
#>     id first second third fourth
#> 1 2001     1      0     0      0
#> 2 2002     1      0     0      0
#> 3 2003     1      0     0      0
#> 4 2004     1      0     0      0
#> 5 2005     1      0     0      0
#> 6 2006     1      0     0      0

# Three ways to ignore first column of ID numbers
( ch1 <- capHistSum(PikeNYPartial1,cols2use=-1) )
#> $caphist
#> 
#> 0001 0010 0011 0100 0101 0110 1000 1001 1010 1100 
#>    5    8    2   12    1    2   21    1    2    3 
#> 
#> $sum
#>    n m  R  M  u  v  f
#> 1 27 0 27  0 27 21 46
#> 2 18 3 18 27 18 15 11
#> 3 14 4 14 42 10 14  0
#> 4  9 4  0 52  5  9  0
#> 
#> $methodB.top
#>     i=1 i=2 i=3 i=4
#> j=1  NA   3   2   1
#> j=2  NA  NA   2   1
#> j=3  NA  NA  NA   2
#> j=4  NA  NA  NA  NA
#> 
#> $methodB.bot
#>   i=1 i=2 i=3 i=4
#> m   0   3   4   4
#> u  27  15  10   5
#> n  27  18  14   9
#> R  27  18  14   0
#> 
#> $m.array
#>     ni c2 c3 c4 not recapt
#> i=1 27  3  2  1         21
#> i=2 18 NA  2  1         15
#> i=3 14 NA NA  2         12
#> i=4  9 NA NA NA          9
#> 
#> attr(,"class")
#> [1] "CapHist"
( ch1 <- capHistSum(PikeNYPartial1,cols2ignore=1) )
#> $caphist
#> 
#> 0001 0010 0011 0100 0101 0110 1000 1001 1010 1100 
#>    5    8    2   12    1    2   21    1    2    3 
#> 
#> $sum
#>    n m  R  M  u  v  f
#> 1 27 0 27  0 27 21 46
#> 2 18 3 18 27 18 15 11
#> 3 14 4 14 42 10 14  0
#> 4  9 4  0 52  5  9  0
#> 
#> $methodB.top
#>     i=1 i=2 i=3 i=4
#> j=1  NA   3   2   1
#> j=2  NA  NA   2   1
#> j=3  NA  NA  NA   2
#> j=4  NA  NA  NA  NA
#> 
#> $methodB.bot
#>   i=1 i=2 i=3 i=4
#> m   0   3   4   4
#> u  27  15  10   5
#> n  27  18  14   9
#> R  27  18  14   0
#> 
#> $m.array
#>     ni c2 c3 c4 not recapt
#> i=1 27  3  2  1         21
#> i=2 18 NA  2  1         15
#> i=3 14 NA NA  2         12
#> i=4  9 NA NA NA          9
#> 
#> attr(,"class")
#> [1] "CapHist"
( ch1 <- capHistSum(PikeNYPartial1,cols2ignore="id") )
#> $caphist
#> 
#> 0001 0010 0011 0100 0101 0110 1000 1001 1010 1100 
#>    5    8    2   12    1    2   21    1    2    3 
#> 
#> $sum
#>    n m  R  M  u  v  f
#> 1 27 0 27  0 27 21 46
#> 2 18 3 18 27 18 15 11
#> 3 14 4 14 42 10 14  0
#> 4  9 4  0 52  5  9  0
#> 
#> $methodB.top
#>     i=1 i=2 i=3 i=4
#> j=1  NA   3   2   1
#> j=2  NA  NA   2   1
#> j=3  NA  NA  NA   2
#> j=4  NA  NA  NA  NA
#> 
#> $methodB.bot
#>   i=1 i=2 i=3 i=4
#> m   0   3   4   4
#> u  27  15  10   5
#> n  27  18  14   9
#> R  27  18  14   0
#> 
#> $m.array
#>     ni c2 c3 c4 not recapt
#> i=1 27  3  2  1         21
#> i=2 18 NA  2  1         15
#> i=3 14 NA NA  2         12
#> i=4  9 NA NA NA          9
#> 
#> attr(,"class")
#> [1] "CapHist"

# diagnostic plots
plot(ch1)

plot(ch1,what="f")

plot(ch1,what="u")


# An examle with only two sample events (for demonstration only)
( ch2 <- capHistSum(PikeNYPartial1,cols2use=-c(1,4:5)) )
#> $caphist
#> 
#> 00 01 10 11 
#> 15 15 24  3 
#> 
#> $sum
#>    M  n m
#> 1 27 18 3
#> 
#> attr(,"class")
#> [1] "CapHist"
( ch2 <- capHistSum(PikeNYPartial1,cols2use=2:3) )
#> $caphist
#> 
#> 00 01 10 11 
#> 15 15 24  3 
#> 
#> $sum
#>    M  n m
#> 1 27 18 3
#> 
#> attr(,"class")
#> [1] "CapHist"
( ch2 <- capHistSum(PikeNYPartial1,cols2ignore=c(1,4:5)) )
#> $caphist
#> 
#> 00 01 10 11 
#> 15 15 24  3 
#> 
#> $sum
#>    M  n m
#> 1 27 18 3
#> 
#> attr(,"class")
#> [1] "CapHist"
```
