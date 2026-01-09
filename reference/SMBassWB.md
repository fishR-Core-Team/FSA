# Growth increment data for West Bearskin Lake, MN, Smallmouth Bass.

Growth data from Smallmouth Bass (*Micropterus dolomieu*) captured in
West Bearskin Lake, MN. Five samples were collected over three years
(1988-1990) with two gears (fall – trapnets, spring – electrofishing).

## Format

A data frame of 445 observations on the following 20 variables:

- species:

  Species of the fish (`SMB` for each fish in this file)

- lake:

  Lake fish was captured in (`WB` for each fish in this file)

- gear:

  Gear used to capture the fish (`T`=Trapnet and `E`=Electrofishing)

- yearcap:

  Year fish was captured (`1988`, `1989`, or `1990`)

- fish:

  A unique identifier for each fish

- agecap:

  Assigned age-at-capture for the fish (from scales)

- lencap:

  Total length-at-capture for the fish (mm)

- anu1:

  Magnified scale radius (mm) to the 1st annulus

- anu2:

  Magnified scale radius (mm) to the 2nd annulus

- anu3:

  Magnified scale radius (mm) to the 3rd annulus

- anu4:

  Magnified scale radius (mm) to the 4th annulus

- anu5:

  Magnified scale radius (mm) to the 5th annulus

- anu6:

  Magnified scale radius (mm) to the 6th annulus

- anu7:

  Magnified scale radius (mm) to the 7th annulus

- anu8:

  Magnified scale radius (mm) to the 8th annulus

- anu9:

  Magnified scale radius (mm) to the 9th annulus

- anu10:

  Magnified scale radius (mm) to the 10th annulus

- anu11:

  Magnified scale radius (mm) to the 11th annulus

- anu12:

  Magnified scale radius (mm) to the 12th annulus

- radcap:

  Total scale radius at time of capture

## Source

Data from the linear growth modeling software distributed in support of
Weisberg, S. 1993. Using hard-part increment data to estimate age and
environmental effects. Canadian Journal of Fisheries and Aquatic
Sciences 50:1229-1237. [CSV
file](https://raw.githubusercontent.com/fishR-Core-Team/FSA/master/data-raw/SMBassWB.csv)

## Note

Data are in one-fish-per-line format.

## Topic(s)

- Growth increment analysis

- Weisberg linear growth model

- Back-Calculation

## See also

Used in
[`capHistSum`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md)
and
[`mrClosed`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md)
examples. Also see `wblake` from alr4 for the same dataset with only the
`agecap`, `lencap`, and `radcap` variables.

## Examples

``` r
str(SMBassWB)
#> 'data.frame':    445 obs. of  20 variables:
#>  $ species: Factor w/ 1 level "SMB": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ lake   : Factor w/ 1 level "WB": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ gear   : Factor w/ 2 levels "E","T": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ yearcap: int  1988 1988 1988 1988 1988 1988 1989 1990 1990 1990 ...
#>  $ fish   : num  5 3 2 4 6 7 50 482 768 428 ...
#>  $ agecap : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ lencap : int  71 64 57 68 72 80 55 75 75 71 ...
#>  $ anu1   : num  1.91 1.88 1.09 1.32 1.59 ...
#>  $ anu2   : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ anu3   : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ anu4   : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ anu5   : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ anu6   : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ anu7   : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ anu8   : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ anu9   : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ anu10  : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ anu11  : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ anu12  : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ radcap : num  1.91 1.88 1.1 1.33 1.59 ...
head(SMBassWB)
#>   species lake gear yearcap fish agecap lencap    anu1 anu2 anu3 anu4 anu5 anu6
#> 1     SMB   WB    E    1988    5      1     71 1.90606   NA   NA   NA   NA   NA
#> 2     SMB   WB    E    1988    3      1     64 1.87707   NA   NA   NA   NA   NA
#> 3     SMB   WB    E    1988    2      1     57 1.09227   NA   NA   NA   NA   NA
#> 4     SMB   WB    E    1988    4      1     68 1.31848   NA   NA   NA   NA   NA
#> 5     SMB   WB    E    1988    6      1     72 1.59283   NA   NA   NA   NA   NA
#> 6     SMB   WB    E    1988    7      1     80 1.91602   NA   NA   NA   NA   NA
#>   anu7 anu8 anu9 anu10 anu11 anu12  radcap
#> 1   NA   NA   NA    NA    NA    NA 1.90606
#> 2   NA   NA   NA    NA    NA    NA 1.87707
#> 3   NA   NA   NA    NA    NA    NA 1.09736
#> 4   NA   NA   NA    NA    NA    NA 1.33108
#> 5   NA   NA   NA    NA    NA    NA 1.59283
#> 6   NA   NA   NA    NA    NA    NA 1.91602
```
