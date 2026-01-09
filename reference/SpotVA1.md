# Age and length of spot.

Ages (from otoliths) and lengths of Virginia Spot (*Leiostomus
xanthurus*).

## Format

A data frame of 403 observations on the following 2 variables:

- tl:

  Measured total lengths (in inches)

- age:

  Ages assigned from examination of otoliths

## Source

Extracted from Table 1 in Chapter 8 (Spot) of the VMRC Final Report on
Finfish Ageing, 2002 by the Center for Quantitative Fisheries Ecology at
Old Dominion University. [CSV
file](https://raw.githubusercontent.com/fishR-Core-Team/FSA/master/data-raw/SpotVA1.csv)

## Details

Final length measurements were simulated by adding a uniform error to
the value at the beginning of the length category.

## Topic(s)

- Growth

- von Bertalanffy

## See also

Used in
[`vbFuns`](https://fishr-core-team.github.io/FSA/reference/growthModels.md),
[`vbStarts`](https://fishr-core-team.github.io/FSA/reference/vbStarts.md),
and
[`nlsTracePlot`](https://fishr-core-team.github.io/FSA/reference/nlsTracePlot.md)
examples. Also see
[`SpotVA2`](https://fishr-core-team.github.io/FSAdata/reference/SpotVA2.html)
in FSAdata for related data.

## Examples

``` r
str(SpotVA1)
#> 'data.frame':    403 obs. of  2 variables:
#>  $ tl : num  6.5 6.3 7.4 7.1 7.7 7.1 7.9 7.3 7.5 7.3 ...
#>  $ age: int  0 0 0 0 0 0 0 0 0 0 ...
head(SpotVA1)
#>    tl age
#> 1 6.5   0
#> 2 6.3   0
#> 3 7.4   0
#> 4 7.1   0
#> 5 7.7   0
#> 6 7.1   0
plot(tl~age,data=SpotVA1)

```
