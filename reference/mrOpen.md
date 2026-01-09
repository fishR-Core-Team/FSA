# Jolly-Seber analysis from multiple mark-recapture events from an open population.

This function takes the two parts of a Method B table and uses the
Jolly-Seber method to estimate the population size at each possible
sample period and the apparent survival rate and number of additional
individuals added to the population between possible sample periods.
This method assumes that the population is open.

## Usage

``` r
jolly(...)

mrOpen(
  mb.top,
  mb.bot = NULL,
  type = c("Jolly", "Manly"),
  conf.level = 0.95,
  phi.full = TRUE
)

# S3 method for class 'mrOpen'
summary(object, parm = c("N", "phi", "B", "M"), verbose = FALSE, ...)

# S3 method for class 'mrOpen'
confint(
  object,
  parm = c("N", "phi", "B"),
  level = NULL,
  conf.level = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- ...:

  Additional arguments for methods.

- mb.top:

  A matrix that contains the “top” of the Method B table (i.e., a
  contingency table of capture sample (columns) and last seen sample
  (rows)) or an object of class `CapHist` from
  [`capHistSum`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md).
  See details.

- mb.bot:

  A data frame that contains the “bottom” of the Method B table (i.e.,
  the number of marked fish in the sample (`m`), the number of unmarked
  fish in the sample (`u`), the total number of fish in the sample
  (`n`), and the number of marked fish returned to the population
  following the sample (`R`)).

- type:

  A string that indicates whether the large sample (normal theory)
  method of Jolly (`type="Jolly"`) or the “arbitrary” method of Manly
  (`type="Manly"`) should be used to construct confidence intervals.

- conf.level:

  A single numeric that indicates the level of confidence to use for
  constructing confidence intervals (default is 0.95). See details.

- phi.full:

  A logical that indicates whether the standard error for phi should
  include only sampling variability (`phi.full=FALSE`) or sampling and
  individual variability (`phi.full=TRUE`,default).

- object:

  An object from `mrOpen` (i.e., of class `mrOpen`).

- parm:

  A string that identifies the model parameters for which to return
  summaries or confidence intervals. By default, all parameters are
  returned.

- verbose:

  A logical that indicates if the observables and other notes should be
  printed in `summary` and if the type of confidence interval used
  should be printed in `confint`. See details.

- level:

  Same as `conf.level` but used for compatibility with generic `confint`
  function.

## Value

A list with the following items:

- df A data frame that contains observable summaries from the data and
  estimates of the number of extant marked fish (M), population size for
  each possible sample period (N), apparent survival rate between each
  possible pair of sample periods (phi), and the number of additional
  individuals added to the population between each possible pair of
  sample periods (B). In addition to the estimates, values of the
  standard errors and the lower and upper confidence interval bounds for
  each parameter are provided (however, see the details above).

- type The provided type of confidence intervals that was used.

- phi.full The provided logical that indicates the type of standard
  error for phi that was used.

- conf.level The provided level of confidence that was used.

## Details

`jolly` is just a convenience wrapper that produces the exact same
results as `mrOpen`.

If `mb.top` contains an object from the
[`capHistSum`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md)
function then `mb.bot` can be left missing. In this case, the function
will extract the needed data from the `methodB.top` and `methodB.bot`
portions of the `CapHist` class object.

If `mb.top` is a matrix then it must be square, must have non-negative
and no NA values in the upper triangle, and all NA values on the lower
triangle and diagonal. If `mb.bot` is a matrix then it must have four
rows named `m`, `u`, `n`, and `R` (see
[`capHistSum`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md)
for definitions), all values must be non-NA, and the first value of `m`
must be 0. The last value of `R` can either be 0 or some positive number
(it is ultimately ignored in all calculations).

All parameter estimates are performed using equations 4.6-4.9 from
Pollock *et al.* (1990) and from page 204 in Seber 2002. If
`type="Jolly"` then all standard errors (square root of the variances)
are from equations 4.11, 4.12, and 4.14 in Pollock *et al.* (1990)
(these are different than those in Seber (2002) ... see Pollock *et
al.*'s note on page 21). If `type="Jolly"` and `phi.full=TRUE` then the
full variance for the phi parameter is given as in eqn 4.18 in Pollock
*et al.* (1990), otherwise eqn 4.13 from Pollock *et al.* (1990) is
used. When `type="Jolly"` the confidence interval are produced using
normal theory (i.e., estimate +/- z\*SE). If `type="Manly"` then the
confidence intervals for N and phi (none will be produced for B) are
constructed using the methods of Manly (1984) and as described in
2.24-2.33 of Krebs (1989). No standard errors are returned when
`type="Manly"`.

The `summary` function returns estimates of M, N, phi, B, and their
associated standard errors and, if `verbose=TRUE` the intermediate
calculations of “observables” from the data – n, m, R, r, and z.

The level of confidence is not set in the `confint` function, in
contrast to most `confint` functions. Rather the confidence level is set
in the main `mrOpen` function.

## Testing

The formulas have been triple-checked against formulas in Pollock *et
al.* (1990), Manly (1984), and Seber (2002).

The results for the
[`CutthroatAL`](https://fishr-core-team.github.io/FSA/reference/CutthroatAL.md)
data file (as analyzed in the example) was compared to results from the
JOLLY program available at
http://www.mbr-pwrc.usgs.gov/software/jolly.html. The r and z values
matched, all M and N estimates match at one decimal place, all phi are
within 0.001, and all B are within 0.7. The SE match for M except for
two estimates that are within 0.1, match for N except for one estimate
that is within 0.1, are within 0.001 for phi, and are within 1.3 for B
(except for for the first estimate which is dramatically off).

The results of `mrOpen` related to Table 4.4 of Pollock *et al.* (1990)
match (to one decimal place) except for three estimates that are within
0.1% for N, match (to two decimal places) for phi except for where
Pollock set phi\>1 to phi=1, match for B except for Pollock set B\<0 to
B=0. The SE match (to two decimal places) for N except for N15 (which is
within 0.5, \<5%), match (to three decimal places) for phi except for
phi15 (which is within 0.001, \<0.5%), match (to two decimal places) for
B except for B17 and B20 which are within 0.2 (\<0.2%)

All point estimates of M, N, phi, and B and the SE of phi match the
results in Table 2.3 of Krebs (1989) (within minimal rounding error for
a very small number of results). The SE of N results are not close to
those of Krebs (1989) (who does not provide a formula for SE so the
discrepancy cannot be explored). The SE of B results match those of
Krebs (1989) for 5 of the 8 values and are within 5% for 2 of the other
3 values (the last estimate is off by 27%).

For comparing to Jolly's data as presented in Tables 5.1 and 5.2 of
Seber (2002), M was within 4 (less than 1.5%), N was within 3% (except
N2 which was within 9%), phi was within 0.01 (less than 1.5

## IFAR Chapter

9-Abundance from Capture-Recapture Data and 11-Mortality.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

Jolly, G.M. 1965. Explicit estimates from capture-recapture data with
both death and immigration – stochastic model. Biometrika, 52:225-247.

Krebs, C.J. 1989. Ecological Methodology. Harper & Row Publishers, New
York.

Leslie, P.H. and D. Chitty. 1951. The estimation of population
parameters from data obtained by means of the capture-recapture method.
I. The maximum likelihood equations for estimating the death-rate.
Biometrika, 38:269-292.

Manly, B.F.J. 1984. Obtaining confidence limits on parameters of the
Jolly-Seber model for capture-recapture data. Biometrics, 40:749-758.

Pollock, K.H., J.D. Nichols, C. Brownie, and J.E. Hines. 1991.
Statistical inference for capture-recapture experiments. Wildlife
Monographs, 107:1-97.

Seber, G.A.F. 1965. A note on the multiple recapture census. Biometrika
52:249-259.

Seber, G.A.F. 2002. The Estimation of Animal Abundance. Edward Arnold,
second edition (reprinted).

## See also

[`capHistSum`](https://fishr-core-team.github.io/FSA/reference/capHistSum.md),
[`mrClosed`](https://fishr-core-team.github.io/FSA/reference/mrClosed.md)

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## First example -- capture histories summarized with capHistSum()
ch1 <- capHistSum(CutthroatAL,cols2use=-1)  # ignore first column of fish ID
ex1 <- mrOpen(ch1)
summary(ex1)
#>         M M.se     N  N.se   phi phi.se     B  B.se
#> i=1    NA   NA    NA    NA 0.411  0.088    NA    NA
#> i=2  36.6  6.4 561.1 117.9 0.349  0.045 198.6  48.2
#> i=3 127.8 13.4 394.2  44.2 0.370  0.071 526.3 119.7
#> i=4 120.7 20.8 672.2 138.8 0.218  0.031 154.1  30.2
#> i=5  68.3  4.1 301.0  21.8 0.437  0.041 304.7  25.4
#> i=6 117.6  7.3 436.1  30.3 0.451  0.069 357.2  61.2
#> i=7 175.1 24.6 553.7  84.3 0.268  0.072 106.9  36.2
#> i=8 100.2 24.7 255.3  65.4    NA     NA    NA    NA
#> i=9    NA   NA    NA    NA    NA     NA    NA    NA
summary(ex1,verbose=TRUE)
#> Observables:
#>      m   n   R   r  z
#> i=1  0  89  89  26 NA
#> i=2 22 352 352  96  4
#> i=3 94 292 292  51  6
#> i=4 41 233 233  46 16
#> i=5 58 259 259 100  4
#> i=6 99 370 370  99  5
#> i=7 91 290 290  44 13
#> i=8 52 134 134  13  5
#> i=9 18 140   0  NA NA
#> 
#> Estimates (phi.se includes sampling and individual variability):
#>         M M.se     N  N.se   phi phi.se     B  B.se
#> i=1    NA   NA    NA    NA 0.411  0.088    NA    NA
#> i=2  36.6  6.4 561.1 117.9 0.349  0.045 198.6  48.2
#> i=3 127.8 13.4 394.2  44.2 0.370  0.071 526.3 119.7
#> i=4 120.7 20.8 672.2 138.8 0.218  0.031 154.1  30.2
#> i=5  68.3  4.1 301.0  21.8 0.437  0.041 304.7  25.4
#> i=6 117.6  7.3 436.1  30.3 0.451  0.069 357.2  61.2
#> i=7 175.1 24.6 553.7  84.3 0.268  0.072 106.9  36.2
#> i=8 100.2 24.7 255.3  65.4    NA     NA    NA    NA
#> i=9    NA   NA    NA    NA    NA     NA    NA    NA
summary(ex1,parm="N")
#>         N  N.se
#> i=1    NA    NA
#> i=2 561.1 117.9
#> i=3 394.2  44.2
#> i=4 672.2 138.8
#> i=5 301.0  21.8
#> i=6 436.1  30.3
#> i=7 553.7  84.3
#> i=8 255.3  65.4
#> i=9    NA    NA
summary(ex1,parm=c("N","phi"))
#>         N  N.se   phi phi.se
#> i=1    NA    NA 0.411  0.088
#> i=2 561.1 117.9 0.349  0.045
#> i=3 394.2  44.2 0.370  0.071
#> i=4 672.2 138.8 0.218  0.031
#> i=5 301.0  21.8 0.437  0.041
#> i=6 436.1  30.3 0.451  0.069
#> i=7 553.7  84.3 0.268  0.072
#> i=8 255.3  65.4    NA     NA
#> i=9    NA    NA    NA     NA
confint(ex1)
#>     N.lci N.uci phi.lci phi.uci B.lci B.uci
#> i=1    NA    NA   0.237   0.584    NA    NA
#> i=2 330.0 792.1   0.261   0.436 104.0 293.1
#> i=3 307.6 480.8   0.231   0.509 291.7 760.8
#> i=4 400.2 944.2   0.159   0.278  95.0 213.2
#> i=5 258.3 343.6   0.356   0.517 254.9 354.6
#> i=6 376.7 495.5   0.316   0.585 237.2 477.3
#> i=7 388.4 719.1   0.127   0.409  36.0 177.8
#> i=8 127.2 383.4      NA      NA    NA    NA
#> i=9    NA    NA      NA      NA    NA    NA
confint(ex1,parm="N")
#>     N.lci N.uci
#> i=1    NA    NA
#> i=2 330.0 792.1
#> i=3 307.6 480.8
#> i=4 400.2 944.2
#> i=5 258.3 343.6
#> i=6 376.7 495.5
#> i=7 388.4 719.1
#> i=8 127.2 383.4
#> i=9    NA    NA
confint(ex1,parm=c("N","phi"))
#>     N.lci N.uci phi.lci phi.uci
#> i=1    NA    NA   0.237   0.584
#> i=2 330.0 792.1   0.261   0.436
#> i=3 307.6 480.8   0.231   0.509
#> i=4 400.2 944.2   0.159   0.278
#> i=5 258.3 343.6   0.356   0.517
#> i=6 376.7 495.5   0.316   0.585
#> i=7 388.4 719.1   0.127   0.409
#> i=8 127.2 383.4      NA      NA
#> i=9    NA    NA      NA      NA
confint(ex1,verbose=TRUE)
#> The Jolly method was used to construct confidence intervals.
#>     N.lci N.uci phi.lci phi.uci B.lci B.uci
#> i=1    NA    NA   0.237   0.584    NA    NA
#> i=2 330.0 792.1   0.261   0.436 104.0 293.1
#> i=3 307.6 480.8   0.231   0.509 291.7 760.8
#> i=4 400.2 944.2   0.159   0.278  95.0 213.2
#> i=5 258.3 343.6   0.356   0.517 254.9 354.6
#> i=6 376.7 495.5   0.316   0.585 237.2 477.3
#> i=7 388.4 719.1   0.127   0.409  36.0 177.8
#> i=8 127.2 383.4      NA      NA    NA    NA
#> i=9    NA    NA      NA      NA    NA    NA

## Second example - Jolly's data -- summarized data entered "by hand"
s1 <- rep(NA,13)
s2 <- c(10,rep(NA,12))
s3 <- c(3,34,rep(NA,11))
s4 <- c(5,18,33,rep(NA,10))
s5 <- c(2,8,13,30,rep(NA,9))
s6 <- c(2,4,8,20,43,rep(NA,8))
s7 <- c(1,6,5,10,34,56,rep(NA,7))
s8 <- c(0,4,0,3,14,19,46,rep(NA,6))
s9 <- c(0,2,4,2,11,12,28,51,rep(NA,5))
s10 <- c(0,0,1,2,3,5,17,22,34,rep(NA,4))
s11 <- c(1,2,3,1,0,4,8,12,16,30,rep(NA,3))
s12 <- c(0,1,3,1,1,2,7,4,11,16,26,NA,NA)
s13 <- c(0,1,0,2,3,3,2,10,9,12,18,35,NA)
jolly.top <- cbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13)

n <- c(54,146,169,209,220,209,250,176,172,127,123,120,142)
R <- c(54,143,164,202,214,207,243,175,169,126,120,120,0)
m <- c(0,10,37,56,53,77,112,86,110,84,77,72,95)
u <- n-m
jolly.bot <- rbind(m,u,n,R)

ex2 <- mrOpen(jolly.top,jolly.bot)
summary(ex2,verbose=TRUE)
#> Observables:
#>      m   n   R   r   z
#> 1    0  54  54  24  NA
#> 2   10 146 143  80  14
#> 3   37 169 164  70  57
#> 4   56 209 202  71  71
#> 5   53 220 214 109  89
#> 6   77 209 207 101 121
#> 7  112 250 243 108 110
#> 8   86 176 175  99 132
#> 9  110 172 169  70 121
#> 10  84 127 126  58 107
#> 11  77 123 120  44  88
#> 12  72 120 120  35  60
#> 13  95 142   0  NA  NA
#> 
#> Estimates (phi.se includes sampling and individual variability):
#>        M M.se     N  N.se   phi phi.se     B  B.se
#> 1     NA   NA    NA    NA 0.646  0.110    NA    NA
#> 2   34.9  4.8 466.2 133.3 1.009  0.110 290.5 164.3
#> 3  169.5 17.9 758.1 125.6 0.864  0.107 293.0 134.2
#> 4  256.2 27.1 943.8 137.7 0.564  0.063 400.2 118.1
#> 5  227.0 17.4 928.8 120.2 0.834  0.074 101.5 110.0
#> 6  323.7 23.8 871.6  93.9 0.790  0.071 109.2  74.0
#> 7  358.2 24.9 795.7  72.0 0.651  0.057 134.4  54.7
#> 8  318.3 20.4 647.6  59.3 0.981  0.096 -11.6  52.4
#> 9  399.7 33.3 623.0  60.1 0.685  0.081  48.5  34.2
#> 10 314.3 27.6 473.3  48.8 0.880  0.121  82.8  39.1
#> 11 313.6 34.9 498.6  63.0 0.767  0.128  73.3  39.0
#> 12 273.7 36.2 453.6  66.6    NA     NA    NA    NA
#> 13    NA   NA    NA    NA    NA     NA    NA    NA
confint(ex2,verbose=TRUE)
#> The Jolly method was used to construct confidence intervals.
#>    N.lci  N.uci phi.lci phi.uci  B.lci B.uci
#> 1     NA     NA   0.430   0.862     NA    NA
#> 2  205.0  727.5   0.793   1.226  -31.4 612.5
#> 3  512.0 1004.3   0.654   1.074   30.0 556.0
#> 4  673.9 1213.8   0.441   0.687  168.8 631.6
#> 5  693.2 1164.5   0.689   0.980 -114.1 317.1
#> 6  687.6 1055.7   0.650   0.929  -35.8 254.1
#> 7  654.6  936.8   0.538   0.763   27.3 241.6
#> 8  531.3  763.9   0.794   1.169 -114.2  91.1
#> 9  505.2  740.8   0.526   0.844  -18.6 115.6
#> 10 377.6  569.0   0.644   1.116    6.2 159.5
#> 11 375.1  622.0   0.516   1.019   -3.2 149.8
#> 12 323.1  584.1      NA      NA     NA    NA
#> 13    NA     NA      NA      NA     NA    NA

ex3 <- mrOpen(jolly.top,jolly.bot,type="Manly")
summary(ex3,verbose=TRUE)
#> Observables:
#>      m   n   R   r   z
#> 1    0  54  54  24  NA
#> 2   10 146 143  80  14
#> 3   37 169 164  70  57
#> 4   56 209 202  71  71
#> 5   53 220 214 109  89
#> 6   77 209 207 101 121
#> 7  112 250 243 108 110
#> 8   86 176 175  99 132
#> 9  110 172 169  70 121
#> 10  84 127 126  58 107
#> 11  77 123 120  44  88
#> 12  72 120 120  35  60
#> 13  95 142   0  NA  NA
#> 
#> Estimates (phi.se includes sampling and individual variability):
#>        M     N   phi     B
#> 1     NA    NA 0.646    NA
#> 2   34.9 466.2 1.009 290.5
#> 3  169.5 758.1 0.864 293.0
#> 4  256.2 943.8 0.564 400.2
#> 5  227.0 928.8 0.834 101.5
#> 6  323.7 871.6 0.790 109.2
#> 7  358.2 795.7 0.651 134.4
#> 8  318.3 647.6 0.981 -11.6
#> 9  399.7 623.0 0.685  48.5
#> 10 314.3 473.3 0.880  82.8
#> 11 313.6 498.6 0.767  73.3
#> 12 273.7 453.6    NA    NA
#> 13    NA    NA    NA    NA
confint(ex3,verbose=TRUE)
#> Manly did not provide a method for constructing confidence intervals for B.
#> The Manly method was used to construct confidence intervals.
#>    N.lci  N.uci phi.lci phi.uci
#> 1     NA     NA   0.389   0.783
#> 2  301.3  979.4   0.705   1.100
#> 3  564.0 1210.0   0.627   1.028
#> 4  725.2 1427.8   0.422   0.658
#> 5  731.0 1353.0   0.629   0.901
#> 6  703.2 1219.3   0.602   0.866
#> 7  663.3 1060.8   0.499   0.712
#> 8  529.5  888.6   0.741   1.094
#> 9  506.4  863.4   0.513   0.821
#> 10 375.7  681.9   0.638   1.096
#> 11 384.5  751.9   0.535   1.034
#> 12 342.0  712.0      NA      NA
#> 13    NA     NA      NA      NA

## demonstrate use of jolly()
ex3a <- jolly(jolly.top,jolly.bot)
```
