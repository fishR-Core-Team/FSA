# Catch-at-age for Tobin Harbor, Isle Royale Brook Trout.

Catch-at-age in fyke nets from 1996-1998 for “Coaster” Brook Trout
(*Salvelinus fontinalis*) in Tobin Harbor, Isle Royale, Lake Superior.

## Format

A data frame with 7 observations on the following 2 variables.

- age:

  A numeric vector of assigned ages

- catch:

  A numeric vector of number of Brook Trout caught

## Source

Quinlan, H.R. 1999. Biological Characteristics of Coaster Brook Trout at
Isle Royale National Park, Michigan, 1996-98. U.S. Fish and Wildlife
Service Ashland Fishery Resources Office report. November 1999. [CSV
file](https://raw.githubusercontent.com/fishR-Core-Team/FSA/master/data-raw/BrookTroutTH.csv)

## Topic(s)

- Mortality

- Catch Curve

- Chapman-Robson

## See also

Used in
[`catchCurve`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md)
and
[`chapmanRobson`](https://fishr-core-team.github.io/FSA/reference/chapmanRobson.md)
examples.

## Examples

``` r
str(BrookTroutTH)
#> 'data.frame':    7 obs. of  2 variables:
#>  $ age  : int  0 1 2 3 4 5 6
#>  $ catch: int  39 93 112 45 58 12 8
head(BrookTroutTH)
#>   age catch
#> 1   0    39
#> 2   1    93
#> 3   2   112
#> 4   3    45
#> 5   4    58
#> 6   5    12
plot(log(catch)~age,data=BrookTroutTH)

```
