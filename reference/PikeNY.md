# Summarized multiple mark-recapture data for all Northern Pike from Buckhorn Marsh, NY.

Summary results of capture histories (number captured, number of
recaptured fish, and number of unmarked fish that were marked) for all
Buckhorn Marsh Northern Pike (*Esox lucius*).

## Format

A data frame with 21 observations on the following 4 variables:

- date:

  Capture date

- n:

  Total fish captured in each sample

- m:

  Marked fish captured in each sample

- R:

  Marked fish returned to the population

## Source

New York Power Authority. 2004. Use of Buckhorn Marsh and Grand Island
tributaries by Northern Pike for spawning and as a nursery. Technical
report, New York Power Authority, January 2004. Niagara Power Project
(FERC No. 2216). [CSV
file](https://raw.githubusercontent.com/fishR-Core-Team/FSA/master/data-raw/PikeNY.csv)

## Topic(s)

- Population Size

- Abundance

- Mark-Recapture

- Capture-Recapture

- Schnabel

- Schumacher-Eschmeyer

## See also

Used in
[`mrClosed`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md)
examples. Also see
[`PikeNYPartial1`](https://fishr-core-team.github.io/FSA/reference/PikeNYPartial1.md).

## Examples

``` r
str(PikeNY)
#> 'data.frame':    21 obs. of  4 variables:
#>  $ date: Factor w/ 21 levels "1-Apr","1-May",..: 15 16 19 20 1 12 17 21 3 4 ...
#>  $ n   : int  2 3 2 3 20 18 14 9 17 6 ...
#>  $ m   : int  0 0 0 0 2 3 4 4 14 5 ...
#>  $ R   : int  2 3 2 3 20 18 13 9 17 6 ...
head(PikeNY)
#>     date  n m  R
#> 1 28-Mar  2 0  2
#> 2 29-Mar  3 0  3
#> 3 30-Mar  2 0  2
#> 4 31-Mar  3 0  3
#> 5  1-Apr 20 2 20
#> 6  2-Apr 18 3 18
```
