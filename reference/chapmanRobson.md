# Computes Chapman-Robson estimates of S and Z.

Computes the Chapman-Robson estimates of annual survival rate (S) and
instantaneous mortality rate (Z) from catch-at-age data on the
descending limb of a catch-curve. Method functions extract estimates
with associated standard errors and confidence intervals. A plot method
highlights the descending-limb, shows the linear model on the descending
limb, and, optionally, prints the estimated Z and A.

## Usage

``` r
chapmanRobson(x, ...)

# Default S3 method
chapmanRobson(
  x,
  catch,
  ages2use = age,
  zmethod = c("Smithetal", "Hoenigetal", "original"),
  ...
)

# S3 method for class 'formula'
chapmanRobson(
  x,
  data,
  ages2use = age,
  zmethod = c("Smithetal", "Hoenigetal", "original"),
  ...
)

# S3 method for class 'chapmanRobson'
summary(
  object,
  parm = c("all", "both", "Z", "S"),
  verbose = FALSE,
  as.df = FALSE,
  ...
)

# S3 method for class 'chapmanRobson'
coef(object, parm = c("all", "both", "Z", "S"), as.df = FALSE, ...)

# S3 method for class 'chapmanRobson'
confint(
  object,
  parm = c("all", "both", "S", "Z"),
  level = conf.level,
  conf.level = 0.95,
  as.df = FALSE,
  incl.est = FALSE,
  ...
)

# S3 method for class 'chapmanRobson'
plot(
  x,
  pos.est = "topright",
  cex.est = 0.95,
  round.est = c(3, 1),
  ylab = "Catch",
  xlab = "Age",
  ylim = NULL,
  col.pt = "gray30",
  axis.age = c("both", "age", "recoded age"),
  ...
)
```

## Arguments

- x:

  A numerical vector of the assigned ages in the catch curve or a
  formula of the form `catch~age` when used in `chapmanRobson`. An
  object saved from `chapmanRobson` (i.e., of class `chapmanRobson`)
  when used in the methods.

- ...:

  Additional arguments for methods.

- catch:

  A numerical vector of the catches or CPUEs for the ages in the catch
  curve. Not used if `x` is a formula.

- ages2use:

  A numerical vector of the ages that define the descending limb of the
  catch curve.

- zmethod:

  A string that indicates the method to use for estimating Z. See
  details.

- data:

  A data frame from which the variables in the `x` formula can be found.
  Not used if `x` is not a formula.

- object:

  An object saved from the `chapmanRobson` call (i.e., of class
  `chapmanRobson`).

- parm:

  A numeric or string (of parameter names) vector that specifies which
  parameters are to be given confidence intervals If missing, all
  parameters are considered.

- verbose:

  A logical that indicates whether the method should return just the
  estimate (`FALSE`; default) or a more verbose statement.

- as.df:

  A logical that indicates whether the results of `coef`, `confint`, or
  `summary` should be returned as a data.frame. Ignored in `summary` if
  `parm="lm"`.

- level:

  Same as `conf.level`. Used for compatibility with the generic
  `confint` function.

- conf.level:

  A number representing the level of confidence to use for constructing
  confidence intervals.

- incl.est:

  A logical that indicated whether the parameter point estimate should
  be included in the results from `confint`. Defaults to `FALSE`.

- pos.est:

  A string to identify where to place the estimated mortality rates on
  the plot. Can be set to one of `"bottomright"`, `"bottom"`,
  `"bottomleft"`, `"left"`, `"topleft"`, `"top"`, `"topright"`,
  `"right"` or `"center"` for positioning the estimated mortality rates
  on the plot. Typically `"bottomleft"` (DEFAULT) and `"topright"` will
  be “out-of-the-way” placements. Set `pos.est` to `NULL` to remove the
  estimated mortality rates from the plot.

- cex.est:

  A single numeric character expansion value for the estimated mortality
  rates on the plot.

- round.est:

  A numeric that indicates the number of decimal place to which Z (first
  value) and S (second value) should be rounded. If only one value then
  it will be used for both Z and S.

- ylab:

  A label for the y-axis (`"Catch"` is the default).

- xlab:

  A label for the x-axis (`"Age"` is the default).

- ylim:

  A numeric for the limits of the y-axis. If `NULL` then will default to
  0 or the lowest catch and a maximum of the maximum catch. If a single
  value then it will be the maximum of the y-axis. If two values then
  these will the minimum and maximum values of the y-axis.

- col.pt:

  A string that indicates the color of the plotted points.

- axis.age:

  A string that indicates the type of x-axis to display. The `age` will
  display only the original ages, `recoded age` will display only the
  recoded ages, and `both` (DEFAULT) displays the original ages on the
  main axis and the recoded ages on the secondary axis.

## Value

A list with the following items:

- age the original vector of assigned ages.

- catch the original vector of observed catches or CPUEs.

- age.e a vector of assigned ages used to estimate mortalities.

- catch.e a vector of catches or CPUEs used to estimate mortalities.

- age.r a vector of recoded ages used to estimate mortalities. See
  references.

- n a numeric holding the intermediate calculation of n. See references.

- T a numeric holding the intermediate calculation of T. See references.

- est A 2x2 matrix that contains the estimates and standard errors for S
  and Z.

## Details

The default is to use all ages in the age vector. This is only
appropriate if the age and catch vectors contain only the ages and
catches on the descending limb of the catch curve. Use `ages2use` to
isolate only the catch and ages on the descending limb.

The Chapman-Robson method provides an estimate of the annual survival
rate, with the annual mortality rate (A) determined by 1-S. The
instantaneous mortality rate is often computed as -log(S). However,
Hoenig *et al.* (1983) showed that this produced a biased (over)estimate
of Z and provided a correction. The correction is applied by setting
`zmethod="Hoenigetal"`. Smith *et al.* (2012) showed that the Hoenig *et
al.* method should be corrected for a variance inflation factor. This
correction is applied by setting `zmethod="Smithetal"` (which is the
default behavior). Choose `zmethod="original"` to use the original
estimates for Z and it's SE as provided by Chapman and Robson.

## Testing

Tested the results of chapmanRobson against the results in Miranda and
Bettoli (2007). The point estimates of S matched perfectly but the SE of
S did not because Miranda and Bettoli used a rounded estimate of S in
the calculation of the SE of S but chapmanRobson does not.

Tested the results against the results from `agesurv` in fishmethods
using the `rockbass` data.frame in fishmethods. Results for Z and the SE
of Z matched perfectly for non-bias-corrected results. The estimate of
Z, but not the SE of Z, matched for the bias-corrected (following Smith
*et al.* (2012)) results. FSA uses equation 2 from Smith *et al.* (2012)
whereas fishmethods appears to use equation 5 from the same source to
estimate the SE of Z.

## IFAR Chapter

11-Mortality.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

Chapman, D.G. and D.S. Robson. 1960. The analysis of a catch curve.
Biometrics. 16:354-368.

Hoenig, J.M. and W.D. Lawing, and N.A. Hoenig. 1983. Using mean age,
mean length and median length data to estimate the total mortality rate.
International Council for the Exploration of the Sea, CM 1983/D:23,
Copenhagen.

Ricker, W.E. 1975. Computation and interpretation of biological
statistics of fish populations. Technical Report Bulletin 191, Bulletin
of the Fisheries Research Board of Canada. \[Was (is?) from
http://www.dfo-mpo.gc.ca/Library/1485.pdf.\]

Robson, D.S. and D.G. Chapman. 1961. Catch curves and mortality rates.
Transactions of the American Fisheries Society. 90:181-189.

Smith, M.W., A.Y. Then, C. Wor, G. Ralph, K.H. Pollock, and J.M. Hoenig.
2012. Recommendations for catch-curve analysis. North American Journal
of Fisheries Management. 32:956-967.

## See also

See [`agesurv`](https://rdrr.io/pkg/fishmethods/man/agesurv.html) in
fishmethods for similar functionality. See
[`catchCurve`](https://fishr-core-team.github.io/FSA/reference/catchCurve.md)
and [`agesurvcl`](https://rdrr.io/pkg/fishmethods/man/agesurvcl.html) in
fishmethods for alternative methods. See
[`metaM`](https://fishr-core-team.github.io/FSA/reference/metaM.md) for
empirical methods to estimate natural mortality.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
plot(catch~age,data=BrookTroutTH,pch=19)


## demonstration of formula notation
cr1 <- chapmanRobson(catch~age,data=BrookTroutTH,ages2use=2:6)
summary(cr1)
#>     Estimate Std. Error
#> S 49.4600432  2.3260749
#> Z  0.7018264  0.1153428
summary(cr1,verbose=TRUE)
#> Intermediate statistics: n=235; T=229
#>     Estimate Std. Error
#> S 49.4600432  2.3260749
#> Z  0.7018264  0.1153428
coef(cr1)
#>          S          Z 
#> 49.4600432  0.7018264 
confint(cr1)
#>      95% LCI    95% UCI
#> S 44.9010202 54.0190662
#> Z  0.4757586  0.9278941
confint(cr1,incl.est=TRUE)
#>          Est    95% LCI    95% UCI
#> S 49.4600432 44.9010202 54.0190662
#> Z  0.7018264  0.4757586  0.9278941
plot(cr1)

plot(cr1,axis.age="age")

plot(cr1,axis.age="recoded age")

summary(cr1,parm="Z")
#>    Estimate Std. Error
#> Z 0.7018264  0.1153428
coef(cr1,parm="Z")
#>         Z 
#> 0.7018264 
confint(cr1,parm="Z",incl.est=TRUE)
#>         Est   95% LCI   95% UCI
#> Z 0.7018264 0.4757586 0.9278941

## demonstration of excluding ages2use
cr2 <- chapmanRobson(catch~age,data=BrookTroutTH,ages2use=-c(0,1))
summary(cr2)
#>     Estimate Std. Error
#> S 49.4600432  2.3260749
#> Z  0.7018264  0.1153428
plot(cr2)


## demonstration of ability to work with missing age classes
age <- c(  2, 3, 4, 5, 7, 9,12)
ct  <- c(100,92,83,71,56,35, 1)
cr3 <- chapmanRobson(age,ct,4:12)
#> Warning: Some 'ages2use' not in observed ages.
summary(cr3)
#>     Estimate Std. Error
#> S 63.2683658  1.8679976
#> Z  0.4569234  0.1465991
plot(cr3)


## Demonstration of computation for multiple groups
##   only ages on the descending limb for each group are in the data.frame
# Get example data
data(FHCatfish,package="FSAdata")
FHCatfish
#>       river age abundance
#> 1     Coosa   2        25
#> 2     Coosa   3        24
#> 3     Coosa   4        18
#> 4     Coosa   5        17
#> 5     Coosa   6        29
#> 6     Coosa   7        14
#> 7     Coosa   8         5
#> 8     Coosa   9        10
#> 9     Coosa  10        15
#> 10    Coosa  11         7
#> 11    Coosa  12        11
#> 12    Coosa  13        10
#> 13    Coosa  14         5
#> 14    Coosa  15         1
#> 15    Coosa  16         4
#> 16    Coosa  17         2
#> 17    Coosa  18         2
#> 18    Coosa  20         1
#> 19    Coosa  25         1
#> 20 Ocmulgee   2        32
#> 21 Ocmulgee   3        15
#> 22 Ocmulgee   4        13
#> 23 Ocmulgee   5         8
#> 24 Ocmulgee   6         6
#> 25 Ocmulgee   7        11
#> 26 Ocmulgee   8         7
#> 27 Ocmulgee   9         8
#> 28 Ocmulgee  10         2
#> 29 Ocmulgee  11         1
#> 30 Ocmulgee  12         1
#> 31 Ocmulgee  13         1
#> 32 Ocmulgee  16         1
#> 33  Satilla   2        94
#> 34  Satilla   3        77
#> 35  Satilla   4        36
#> 36  Satilla   5         2
#> 37  Satilla   6         1
#> 38  Satilla   7         1
#> 39  Satilla  10         1

# Note use of incl.est=TRUE and as.df=TRUE
if (require(dplyr)) {
  res <- FHCatfish %>%
    dplyr::group_by(river) %>%
    dplyr::group_modify(~confint(chapmanRobson(abundance~age,data=.x),
                                 incl.est=TRUE,as.df=TRUE)) %>%
    as.data.frame() # removes tibble and grouping structure
  res
}
#>      river        S    S_LCI    S_UCI         Z     Z_LCI     Z_UCI
#> 1    Coosa 83.76623 81.70625 85.82621 0.1769846 0.1291486 0.2248206
#> 2 Ocmulgee 74.20147 69.94560 78.45735 0.2975508 0.2315192 0.3635824
#> 3  Satilla 44.90862 39.92065 49.89658 0.7973861 0.4608940 1.1338782

## Demonstration of computation for multiple groups
##   ages not on descending limb are in the data.frame, but use same
##     ages.use= for each group
# Get example data
data(WalleyeKS,package="FSAdata")

# Note use of incl.est=TRUE and as.df=TRUE
if (require(dplyr)) {
  res <- WalleyeKS %>%
    dplyr::group_by(reservoir) %>%
    dplyr::group_modify(~confint(chapmanRobson(catch~age,data=.x,ages2use=2:10),
                                 incl.est=TRUE,as.df=TRUE)) %>%
    as.data.frame() # removes tibble and grouping structure
  res
}
#> Warning: Some 'ages2use' not in observed ages.
#> Warning: Some 'ages2use' not in observed ages.
#> Warning: Some 'ages2use' not in observed ages.
#> Warning: Some 'ages2use' not in observed ages.
#> Warning: Some 'ages2use' not in observed ages.
#> Warning: Some 'ages2use' not in observed ages.
#> Warning: Some 'ages2use' not in observed ages.
#> Warning: Some 'ages2use' not in observed ages.
#>     reservoir        S    S_LCI    S_UCI         Z     Z_LCI     Z_UCI
#> 1 Cedar.Bluff 46.92308 41.96379 51.88237 0.7538037 0.6615871 0.8460203
#> 2      Cheney 53.47432 48.09274 58.85590 0.6233886 0.1617918 1.0849854
#> 3  Glen.Elder 45.06713 42.71023 47.42404 0.7963078 0.6651496 0.9274661
#> 4      Kirwin 39.04110 31.10067 46.98152 0.9302788 0.7316175 1.1289400
#> 5    Lovewell 58.62069 55.71150 61.52988 0.5334457 0.2137564 0.8531351
#> 6      Marion 59.19679 56.46571 61.92787 0.5237522 0.3083083 0.7391961
#> 7     Webster 57.73196 50.76275 64.70117 0.5457089 0.3605679 0.7308498
#> 8      Wilson 47.08249 42.68974 51.47525 0.7510342 0.5833127 0.9187556
```
