# Capture histories (9 samples) of Cutthroat Trout from Auke Lake.

Individual capture histories of Cutthroat Trout (*Oncorhynchus clarki*)
in Auke Lake, Alaska, from samples taken in 1998-2006.

## Format

A data frame with 1684 observations on the following 10 variables.

- id:

  Unique identification numbers for each fish

- y1998:

  Indicator variable for whether the fish was captured in 1998
  (`1`=captured)

- y1999:

  Indicator variable for whether the fish was captured in 1999
  (`1`=captured)

- y2000:

  Indicator variable for whether the fish was captured in 2000
  (`1`=captured)

- y2001:

  Indicator variable for whether the fish was captured in 2001
  (`1`=captured)

- y2002:

  Indicator variable for whether the fish was captured in 2002
  (`1`=captured)

- y2003:

  Indicator variable for whether the fish was captured in 2003
  (`1`=captured)

- y2004:

  Indicator variable for whether the fish was captured in 2004
  (`1`=captured)

- y2005:

  Indicator variable for whether the fish was captured in 2005
  (`1`=captured)

- y2006:

  Indicator variable for whether the fish was captured in 2006
  (`1`=captured)

## Source

From Appendix A.3 of Harding, R.D., C.L. Hoover, and R.P. Marshall.
2010. Abundance of Cutthroat Trout in Auke Lake, Southeast Alaska, in
2005 and 2006. Alaska Department of Fish and Game Fisheries Data Series
No. 10-82. \[Was (is?) from
http://www.sf.adfg.state.ak.us/FedAidPDFs/FDS10-82.pdf.\] [CSV
file](https://raw.githubusercontent.com/fishR-Core-Team/FSA/master/data-raw/CutthroatAL.csv)

## Note

Entered into “RMark” format (see
[`CutthroatALf`](https://fishr-core-team.github.io/FSAdata/reference/CutthroatALf.html)
in FSAdata) and then converted to individual format with
[`capHistConvert`](https://fishr-core-team.github.io/FSA/reference/capHistConvert.md)

## Topic(s)

- Population Size

- Abundance

- Mark-Recapture

- Capture-Recapture

- Jolly-Seber

- Capture History

## See also

Used in
[`mrOpen`](https://fishr-core-team.github.io/FSA/reference/mrOpen.md)
examples.

## Examples

``` r
str(CutthroatAL)
#> 'data.frame':    1684 obs. of  10 variables:
#>  $ id   : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ y1998: int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ y1999: int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ y2000: int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ y2001: int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ y2002: int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ y2003: int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ y2004: int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ y2005: int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ y2006: int  1 1 1 1 1 1 1 1 1 1 ...
head(CutthroatAL)
#>   id y1998 y1999 y2000 y2001 y2002 y2003 y2004 y2005 y2006
#> 1  1     0     0     0     0     0     0     0     0     1
#> 2  2     0     0     0     0     0     0     0     0     1
#> 3  3     0     0     0     0     0     0     0     0     1
#> 4  4     0     0     0     0     0     0     0     0     1
#> 5  5     0     0     0     0     0     0     0     0     1
#> 6  6     0     0     0     0     0     0     0     0     1
```
