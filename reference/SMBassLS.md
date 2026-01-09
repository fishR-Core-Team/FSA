# Catch-effort data for Little Silver Lake (Ont) Smallmouth Bass.

Catch-effort data for Smallmouth Bass (*Micropterus dolomieu*) in Little
Silver Lake, Ont.

## Format

A data frame with 10 observations on the following 3 variables:

- day:

  Day of the catch

- catch:

  Number of smallmouth bass caught

- effort:

  Number of traps set per day

## Source

From Omand, D.N. 1951. A study of populations of fish based on
catch-effort statistics. Journal of Wildlife Management, 15:88-98. [CSV
file](https://raw.githubusercontent.com/fishR-Core-Team/FSA/master/data-raw/SMBassLS.csv)

## Topic(s)

- Population size

- Abundance

- Depletion methods

- Leslie method

- DeLury method

- Catchability

## See also

Used in
[`depletion`](https://fishr-core-team.github.io/FSA/reference/depletion.md)
examples.

## Examples

``` r
str(SMBassLS)
#> 'data.frame':    10 obs. of  3 variables:
#>  $ day   : int  1 2 3 4 5 6 7 8 9 10
#>  $ catch : int  131 69 99 78 56 76 49 42 63 47
#>  $ effort: int  7 7 7 7 7 7 7 7 7 7
head(SMBassLS)
#>   day catch effort
#> 1   1   131      7
#> 2   2    69      7
#> 3   3    99      7
#> 4   4    78      7
#> 5   5    56      7
#> 6   6    76      7
```
