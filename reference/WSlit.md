# All known standard weight equations.

Parameters for all known standard weight equations.

## Format

A data frame with observations on the following 13 variables:

- species:

  Species name. Use
  [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md)
  to see the list of available species.

- group:

  Sub-group name (e.g., `"female"` or `"lotic"`).

- units:

  Units of measurements. `Metric` uses lengths in mm and weight in
  grams. `English` uses lengths in inches and weight in pounds.

- ref:

  Reference quartile (`75`, `50`, or `25`).

- measure:

  The type of length measurement used – total length (`TL`) or fork
  length (`FL`).

- method:

  The type of method used to derive the equation (Regression Line
  Percentile (`RLP`; see Murphy *et al.* (1990) and Murphy *et al.*
  (1991)), Empirical Percentile (`EmP`; see Gerow *et al.* (2005)), or
  `Other`).

- min.len:

  Minimum total length (mm or in, depending on `units`) for which the
  equation should be applied.

- max.len:

  Maximum total length (mm or in, depending on `units`) for which the
  equation should be applied.

- int:

  The intercept for the model.

- slope:

  The slope for the linear equation or the linear coefficient for the
  quadratic equation.

- quad:

  The quadratic coefficient in the quadratic equation.

- source:

  Source of the equation. These match the sources given in Neumann *et
  al.* (2012).

- comment:

  Comments about use of equation.

## Source

Most of these equations can be found in Neumann *et al.* (2012). Species
not in Neumann *et al.* (2012) are noted as such in the `comments`
variable.

## Details

The minimum TL for the English units were derived by rounding the
converted minimum TL for the metric units to what seemed like common
units (inches, half inches, or quarter inches).

Entries for “Chinook Salmon (landlocked)” and “Striped Bass
(landlocked)” are the same as for “Chinook Salmon” and “Striped Bass”
but were added to facilitate use with PSD calculations as Gabelhouse
lengths are only published for the landlocked sub-group; i.e., these
entries in `WSlit` are not necessarily just for landlocked populations.

## Topic(s)

- Relative weight

- Standard weight

- Condition

## IFAR Chapter

8-Condition.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

Gerow, K.G., R.C. Anderson-Sprecher, and W.A. Hubert. 2005. A new method
to compute standard weight equations that reduces length-related bias.
North American Journal of Fisheries Management 25:1288–1300.

Murphy, B.R., M.L. Brown, and T.A. Springer. 1990. Evaluation of the
relative weight (Wr) index, with new applications to walleye. North
American Journal of Fisheries Management 10:85–97.

Murphy, B. R., D. W. Willis, and T. A. Springer. 1991. The relative
weight index in fisheries management: Status and needs. Fisheries
(Bethesda) 16(2):30–38.

Neumann, R.M., C.S. Guy, and D.W. Willis. 2012. Length, Weight, and
Associated Indices. Chapter 14 in Zale, A.V., D.L. Parrish, and T.M.
Sutton, editors. Fisheries Techniques. American Fisheries Society,
Bethesda, MD.

## See also

See [`wsVal`](https://fishr-core-team.github.io/FSA/reference/wsVal.md)
and [`wrAdd`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)
for related functionality.

## Examples

``` r
str(WSlit)
#> 'data.frame':    257 obs. of  13 variables:
#>  $ species: chr  "Aegean Chub" "African Sharptooth Catfish" "Alabama Bass" "Alabama Bass" ...
#>  $ group  : chr  NA NA NA NA ...
#>  $ measure: chr  "TL" "TL" "TL" "TL" ...
#>  $ units  : chr  "metric" "metric" "metric" "English" ...
#>  $ ref    : int  75 75 75 75 75 75 75 75 50 75 ...
#>  $ method : chr  "EmP" "EmP" "EmP" "EmP" ...
#>  $ min.len: num  70 180 150 6 100 70 150 150 160 6 ...
#>  $ max.len: num  220 450 550 22 NA 240 NA NA NA NA ...
#>  $ int    : num  -3.8 -3.67 -5.62 -3.53 -5.6 ...
#>  $ slope  : num  1.78 1.89 3.28 3.17 3.29 ...
#>  $ quad   : num  0.329 0.209 NA NA NA ...
#>  $ source : chr  "Giannetto et al. (2012)" "Emiroglu et al. (2018)" "Sammons et al. (2025)" "Sammons et al. (2025)" ...
#>  $ comment: chr  "none" "only from Sakarya River Basin (Turkey)" "RLP and EmP (quadratic) models not recommended" "RLP and EmP (quadratic) models not recommended" ...
head(WSlit)
#>                      species group measure   units ref method min.len max.len
#> 1                Aegean Chub  <NA>      TL  metric  75    EmP      70     220
#> 2 African Sharptooth Catfish  <NA>      TL  metric  75    EmP     180     450
#> 3               Alabama Bass  <NA>      TL  metric  75    EmP     150     550
#> 4               Alabama Bass  <NA>      TL English  75    EmP       6      22
#> 5    Alabama Bass (original)  <NA>      TL  metric  75    RLP     100      NA
#> 6                Ankara Nase  <NA>      TL  metric  75    EmP      70     240
#>        int  slope    quad                  source
#> 1  -3.8010 1.7830  0.3290 Giannetto et al. (2012)
#> 2  -3.6680 1.8850  0.2087  Emiroglu et al. (2018)
#> 3  -5.6189 3.2840      NA   Sammons et al. (2025)
#> 4  -3.5339 3.1750      NA   Sammons et al. (2025)
#> 5  -5.5980 3.2904      NA   Dicenzo et al. (1995)
#> 6 -10.0170 7.4020 -0.9710  Emiroglu et al. (2020)
#>                                                                   comment
#> 1                                                                    none
#> 2                                  only from Sakarya River Basin (Turkey)
#> 3                          RLP and EmP (quadratic) models not recommended
#> 4                          RLP and EmP (quadratic) models not recommended
#> 5 HAS BEEN REVISED; min.len not made clear (assumed same as Spotted Bass)
#> 6                                                        only from Turkey
```
