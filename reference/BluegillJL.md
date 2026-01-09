# Capture histories (2 samples) of Bluegill from Jewett Lake, MI.

Each line consists of the capture history over two samples of Bluegill
(*Lepomis macrochirus*) in Jewett Lake (MI). This file contains the
capture histories for only Bluegill larger than 6-in.

## Format

A data frame with 277 observations on the following 2 variables.

- first:

  a numeric vector of indicator variables for the first sample
  (1=captured)

- second:

  a numeric vector of indicator variables for the second sample
  (1=captured)

## Source

From example 8.1 in Schneider, J.C. 1998. Lake fish population estimates
by mark-and-recapture methods. Chapter 8 in Schneider, J.C. (ed.) 2000.
Manual of fisheries survey methods II: with periodic updates. Michigan
Department of Natural Resources, Fisheries Special Report 25, Ann Arbor.
\[Was (is?) from
http://www.michigandnr.com/publications/pdfs/IFR/manual/SMII%20Chapter08.pdf.\]
[CSV
file](https://raw.githubusercontent.com/fishR-Core-Team/FSA/master/data-raw/BluegillJL.csv)

## Topic(s)

- Population Size

- Abundance

- Mark-Recapture

- Capture-Recapture

- Petersen

- Capture History

## See also

Used in
[`mrClosed`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md)
examples.

## Examples

``` r
str(BluegillJL)
#> 'data.frame':    277 obs. of  2 variables:
#>  $ first : int  1 0 1 0 1 1 1 1 1 1 ...
#>  $ second: int  0 1 0 1 0 0 0 0 0 0 ...
head(BluegillJL)
#>   first second
#> 1     1      0
#> 2     0      1
#> 3     1      0
#> 4     0      1
#> 5     1      0
#> 6     1      0
```
