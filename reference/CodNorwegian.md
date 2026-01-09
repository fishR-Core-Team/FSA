# Stock and recruitment data for Norwegian cod, 1937-1960.

Norwegian cod (*Gadus morhua*) stock and recruitment by year, 1937-1960.

## Format

A data frame of 24 observations on the following 3 variables:

- year:

  Year of data

- recruits:

  Recruits – year-class strength index

- stock:

  Spawning stock index

## Source

From Garrod, D.J. 1967. Population dynamics of the Arcto-Norwegian Cod.
Journal of the Fisheries Research Board of Canada, 24:145-190. [CSV
file](https://raw.githubusercontent.com/fishR-Core-Team/FSA/master/data-raw/CodNorwegian.csv)

## Topic(s)

- Stock-Recruit

- Recruitment

## See also

Used in
[`srStarts`](https://fishr-core-team.github.io/FSA/reference/srStarts.md),
[`srFuns`](https://fishr-core-team.github.io/FSA/reference/srFuns.md),
and
[`nlsTracePlot`](https://fishr-core-team.github.io/FSA/reference/nlsTracePlot.md)
examples.

## Examples

``` r
str(CodNorwegian)
#> 'data.frame':    24 obs. of  3 variables:
#>  $ year    : int  1937 1938 1939 1940 1941 1942 1943 1944 1945 1946 ...
#>  $ recruits: int  146 31 17 26 43 58 113 75 99 70 ...
#>  $ stock   : int  118 164 180 172 151 139 122 114 139 140 ...
head(CodNorwegian)
#>   year recruits stock
#> 1 1937      146   118
#> 2 1938       31   164
#> 3 1939       17   180
#> 4 1940       26   172
#> 5 1941       43   151
#> 6 1942       58   139
op <- par(mfrow=c(1,2),pch=19,mar=c(3,3,0.5,0.5),mgp=c(1.9,0.5,0),tcl=-0.2)
plot(recruits~year,data=CodNorwegian,type="l")
plot(recruits~stock,data=CodNorwegian)

par(op)
```
