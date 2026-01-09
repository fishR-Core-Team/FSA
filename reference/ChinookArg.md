# Lengths and weights for Chinook Salmon from three locations in Argentina.

Lengths and weights for Chinook Salmon from three locations in
Argentina.

## Format

A data frame with 112 observations on the following 3 variables:

- tl:

  Total length (cm)

- w:

  Weight (kg)

- loc:

  Capture location (`Argentina`, `Petrohue`, `Puyehue`)

## Source

From Figure 4 in Soto, D., I. Arismendi, C. Di Prinzio, and F. Jara.
2007. Establishment of Chinook Salmon (*Oncorhynchus tshawytscha*) in
Pacific basins of southern South America and its potential ecosystem
implications. Revista Chilena d Historia Natural, 80:81-98. \[Was (is?)
from http://www.scielo.cl/pdf/rchnat/v80n1/art07.pdf.\] [CSV
file](https://raw.githubusercontent.com/fishR-Core-Team/FSA/master/data-raw/ChinookArg.csv)

## Topic(s)

- Weight-Length

## See also

Used in
[`lwCompPreds`](https://fishr-core-team.github.io/FSA/reference/lwCompPreds.md)
examples.

## Examples

``` r
str(ChinookArg)
#> 'data.frame':    112 obs. of  3 variables:
#>  $ tl : num  120 115 111 110 110 ...
#>  $ w  : num  17.9 17.2 16.8 15.8 14.3 13.8 12.8 11.7 12.8 14.8 ...
#>  $ loc: Factor w/ 3 levels "Argentina","Petrohue",..: 1 1 1 1 1 1 1 1 1 1 ...
head(ChinookArg)
#>      tl    w       loc
#> 1 120.1 17.9 Argentina
#> 2 115.0 17.2 Argentina
#> 3 111.2 16.8 Argentina
#> 4 110.2 15.8 Argentina
#> 5 110.0 14.3 Argentina
#> 6 109.7 13.8 Argentina
op <- par(mfrow=c(2,2),pch=19,mar=c(3,3,0.5,0.5),mgp=c(1.9,0.5,0),tcl=-0.2)
plot(w~tl,data=ChinookArg,subset=loc=="Argentina")
plot(w~tl,data=ChinookArg,subset=loc=="Petrohue")
plot(w~tl,data=ChinookArg,subset=loc=="Puyehue")
par(op)

```
