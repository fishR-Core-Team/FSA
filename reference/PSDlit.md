# Gabelhouse five-cell length categories for various species.

Cutoffs for the Gabelhouse five-cell length categories for a variety of
species.

## Format

A data frame of 58 observations on the following 11 variables:

- species:

  Species name.

- group:

  Sub-group name (e.g., `"landlocked"` or `"lotic"`).

- substock.in:

  Zero inches.

- stock.in:

  Stock length in inches.

- quality.in:

  Quality length in inches.

- preferred.in:

  Preferred length in inches.

- memorable.in:

  Memorable length in inches.

- trophy.in:

  Trophy length in inches.

- substock.cm:

  Zero cm.

- stock.cm:

  Stock length in cm.

- quality.cm:

  Quality length in cm.

- preferred.cm:

  Preferred length in cm.

- memorable.cm:

  Memorable length in cm.

- trophy.cm:

  Trophy length in cm.

- source:

  Literature source for the length entries.

## Source

Original summary table from Dr. Michael Hansen, University of
Wisconsin-Stevens Point. Additional species have been added by the
package author from the literature.

## Details

Entries for some species (e.g., “Muskellunge” and “Walleye”) have been
duplicated for sub-groups to facilitate use with relative weight
calculations. For example, entries for “Muskellunge (overall)”,
“Muskellunge (female)”, and “Muskellunge (male)” are duplicates of the
entry for “Muskellunge”; i.e., these entries in `PSDlit` are not
necessarily just for those sub-groups but this allows for seamless
similar computations of relative weights for these sub-groups.

## Topic(s)

- Size structure

- Proportional size structure

- Relative stock density

- Proportional stock density

## IFAR Chapter

6-Size Structure.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

## See also

See
[`psdVal`](https://fishr-core-team.github.io/FSA/reference/psdVal.md),
[`psdCalc`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md),
[`psdPlot`](https://fishr-core-team.github.io/FSA/reference/psdPlot.md),
[`psdAdd`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md),
and
[`tictactoe`](https://fishr-core-team.github.io/FSA/reference/tictactoe.md)
for related functionality.

## Examples

``` r
str(PSDlit)
#> 'data.frame':    96 obs. of  15 variables:
#>  $ species     : chr  "Alabama Bass" "Arctic Grayling" "Bighead Carp" "Bigmouth Buffalo" ...
#>  $ group       : chr  NA NA NA NA ...
#>  $ substock.in : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ stock.in    : num  7 8 11.8 11 6 ...
#>  $ quality.in  : num  11 12 21.2 18 9 ...
#>  $ preferred.in: num  14 16 26.8 24 12 ...
#>  $ memorable.in: num  17 20 35 30 15 46.5 12 35 10 20 ...
#>  $ trophy.in   : num  20 22 43.8 37 18 ...
#>  $ substock.cm : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ stock.cm    : num  18 20 30 28 15 40 13 30 8 20 ...
#>  $ quality.cm  : int  28 30 54 46 23 72 20 51 15 30 ...
#>  $ preferred.cm: int  35 40 68 61 30 90 25 76 20 40 ...
#>  $ memorable.cm: int  43 50 89 76 39 118 30 89 25 50 ...
#>  $ trophy.cm   : num  51 55 111 94 46 148 38 114 30 60 ...
#>  $ source      : chr  "Sammons et al. (2025)" "Hyatt (2000)" "Phelps and Willis (2013)" "Bister et al. (2000)" ...
head(PSDlit)
#>            species group substock.in stock.in quality.in preferred.in
#> 1     Alabama Bass  <NA>           0     7.00      11.00        14.00
#> 2  Arctic Grayling  <NA>           0     8.00      12.00        16.00
#> 3     Bighead Carp  <NA>           0    11.75      21.25        26.75
#> 4 Bigmouth Buffalo  <NA>           0    11.00      18.00        24.00
#> 5   Black Bullhead  <NA>           0     6.00       9.00        12.00
#> 6       Black Carp  <NA>           0    15.75      28.25        35.50
#>   memorable.in trophy.in substock.cm stock.cm quality.cm preferred.cm
#> 1         17.0     20.00           0       18         28           35
#> 2         20.0     22.00           0       20         30           40
#> 3         35.0     43.75           0       30         54           68
#> 4         30.0     37.00           0       28         46           61
#> 5         15.0     18.00           0       15         23           30
#> 6         46.5     58.25           0       40         72           90
#>   memorable.cm trophy.cm                   source
#> 1           43        51    Sammons et al. (2025)
#> 2           50        55             Hyatt (2000)
#> 3           89       111 Phelps and Willis (2013)
#> 4           76        94     Bister et al. (2000)
#> 5           39        46       Gabelhouse (1984a)
#> 6          118       148 Phelps and Willis (2013)
```
