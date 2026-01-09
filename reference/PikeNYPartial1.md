# Capture histories (4 samples), in capture history format, of a subset of Northern Pike from Buckhorn Marsh, NY.

Each line consists of the capture history over four samples of Northern
Pike (*Esox lucius*) in Buckhorn Marsh. This file contains the capture
histories for only those pike captured from April 1-4.

## Format

A data frame with 57 observations on the following 4 variables.

- id:

  A unique identification numbers

- first:

  Indicator variable for the first sample (1=captured)

- second:

  Indicator variable for the second sample (1=captured)

- third:

  Indicator variable for the third sample (1=captured)

- fourth:

  Indicator variable for the fourth sample (1=captured)

## Source

Summary values taken from Table C-1 of New York Power Authority. 2004.
Use of Buckhorn Marsh and Grand Island tributaries by Northern Pike for
spawning and as a nursery. Technical report, New York Power Authority,
January 2004. Niagara Power Project (FERC No. 2216). [CSV
file](https://raw.githubusercontent.com/fishR-Core-Team/FSA/master/data-raw/PikeNYPartial1.csv)

## Topic(s)

- Population Size

- Abundance

- Mark-Recapture

- Capture-Recapture

- Schnabel

- Schumacher-Eschmeyer

- Capture History

## See also

Used in
[`capHistSum`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md)
and
[`mrClosed`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md)
examples. Also see
[`PikeNY`](https://fishr-core-team.github.io/FSA/reference/PikeNY.md).

## Examples

``` r
str(PikeNYPartial1)
#> 'data.frame':    57 obs. of  5 variables:
#>  $ id    : int  2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 ...
#>  $ first : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ second: int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ third : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ fourth: int  0 0 0 0 0 0 0 0 0 0 ...
head(PikeNYPartial1)
#>     id first second third fourth
#> 1 2001     1      0     0      0
#> 2 2002     1      0     0      0
#> 3 2003     1      0     0      0
#> 4 2004     1      0     0      0
#> 5 2005     1      0     0      0
#> 6 2006     1      0     0      0
```
