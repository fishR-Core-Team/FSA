# Assigned ages from two readers on three structures for Lake Whitefish from Lake Champlain.

Assigned ages from two readers on three structures for Lake Whitefish
(*Coregonus clupeaformis*) from Lake Champlain in 2009.

## Format

A data frame with 151 observations on the following 11 variables:

- fishID:

  A unique fish identification number

- tl:

  Total length (in mm)

- scale1:

  Assessed age from scales by first reader

- scale2:

  Assessed age from scales by second reader

- scaleC:

  Consensus age from scales by both reader

- finray1:

  Assessed age from fin rays by first reader

- finray2:

  Assessed age from fin rays by second reader

- finrayC:

  Consensus age from fin rays by both reader

- otolith1:

  Assessed age from otoliths by first reader

- otolith2:

  Assessed age from otoliths by second reader

- otolithC:

  Consensus age from otoliths by both reader

## Source

Data from Herbst, S.J. and J.E. Marsden. 2011. Comparison of precision
and bias of scale, fin ray, and otolith age estimates for lake whitefish
(*Coregonus clupeaformis*) in Lake Champlain. Journal of Great Lakes
Research. 37:386-389. Contributed by Seth Herbst. **Do not use for other
than educational purposes without permission from the author.** [CSV
file](https://raw.githubusercontent.com/fishR-Core-Team/FSA/master/data-raw/WhitefishLC.csv)

## Topic(s)

- Age

- Ageing Error

- Precision

- Bias

- Age Comparisons

## See also

Used in
[`ageBias`](https://fishr-core-team.github.io/FSA/reference/ageBias.md)
and
[`agePrecision`](https://fishr-core-team.github.io/FSA/reference/agePrecision.md)
examples.

## Examples

``` r
str(WhitefishLC)
#> 'data.frame':    151 obs. of  11 variables:
#>  $ fishID  : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ tl      : int  345 334 348 300 330 316 508 475 340 173 ...
#>  $ scale1  : int  3 4 7 4 3 4 6 4 3 1 ...
#>  $ scale2  : int  3 3 5 3 3 4 7 5 3 1 ...
#>  $ scaleC  : int  3 4 6 4 3 4 7 5 3 1 ...
#>  $ finray1 : int  3 3 3 3 4 2 6 9 2 2 ...
#>  $ finray2 : int  3 3 3 2 3 3 6 9 3 1 ...
#>  $ finrayC : int  3 3 3 3 4 3 6 9 3 1 ...
#>  $ otolith1: int  3 3 3 3 3 6 9 11 3 1 ...
#>  $ otolith2: int  3 3 3 3 3 5 10 12 4 1 ...
#>  $ otolithC: int  3 3 3 3 3 6 10 11 4 1 ...
head(WhitefishLC)
#>   fishID  tl scale1 scale2 scaleC finray1 finray2 finrayC otolith1 otolith2
#> 1      1 345      3      3      3       3       3       3        3        3
#> 2      2 334      4      3      4       3       3       3        3        3
#> 3      3 348      7      5      6       3       3       3        3        3
#> 4      4 300      4      3      4       3       2       3        3        3
#> 5      5 330      3      3      3       4       3       4        3        3
#> 6      6 316      4      4      4       2       3       3        6        5
#>   otolithC
#> 1        3
#> 2        3
#> 3        3
#> 4        3
#> 5        3
#> 6        6
```
