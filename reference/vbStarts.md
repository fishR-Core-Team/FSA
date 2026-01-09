# Find reasonable starting values for a von Bertalanffy growth function.

DEPRECATED (as of v0.10.0). Finds reasonable starting values for the
parameters in a specific parameterization of the von Bertalanffy growth
function.

## Usage

``` r
vbStarts(
  formula,
  data = NULL,
  param = c("Typical", "typical", "Traditional", "traditional", "BevertonHolt",
    "Original", "original", "vonBertalanffy", "GQ", "GallucciQuinn", "Mooij", "Weisberg",
    "Ogle", "Schnute", "Francis", "Somers", "Somers2", "Pauly"),
  type = param,
  fixed = NULL,
  meth0 = c("yngAge", "poly"),
  methLinf = c("Walford", "oldAge", "longFish", "poly"),
  num4Linf = 1,
  ages2use = NULL,
  methEV = c("means", "poly"),
  valOgle = NULL,
  plot = FALSE,
  col.mdl = "gray70",
  lwd.mdl = 3,
  lty.mdl = 1,
  cex.main = 0.9,
  col.main = "red",
  dynamicPlot = FALSE,
  ...
)
```

## Arguments

- formula:

  A formula of the form `len~age`.

- data:

  A data frame that contains the variables in `formula`.

- type, param:

  A string that indicates the parameterization of the von Bertalanffy
  model.

- fixed:

  A named list that contains user-defined rather than automatically
  generated (i.e., fixed) starting values for one or more parameters.
  See details.

- meth0:

  A string that indicates how the t0 and L0 parameters should be
  derived. See details.

- methLinf:

  A string that indicates how Linf should be derived. See details.

- num4Linf:

  A single numeric that indicates how many of the longest fish (if
  `methLinf="longFish"`) or how any of the oldest ages (if
  `methLinf="oldAge"`) should be averaged to estimate a starting value
  for Linf.

- ages2use:

  A numerical vector of the two ages to be used in the Schnute or
  Francis parameterizations. See details.

- methEV:

  A string that indicates how the lengths of the two ages in the Schnute
  parameterization or the three ages in the Francis parameterization
  should be derived. See details.

- valOgle:

  A single named numeric that is the set Lr or tr value for use in
  `type="Ogle"`. See details.

- plot:

  A logical that indicates whether a plot of the data with the
  superimposed model fit at the starting values should be created.

- col.mdl:

  A color for the model when `plot=TRUE`.

- lwd.mdl:

  A line width for the model when `plot=TRUE`.

- lty.mdl:

  A line type for the model when `plot=TRUE`.

- cex.main:

  A character expansion value for the main title when `plot=TRUE`.

- col.main:

  A color for the main title when `plot=TRUE`.

- dynamicPlot:

  DEPRECATED.

- ...:

  Further arguments passed to the methods.

## Value

A list that contains reasonable starting values. Note that the
parameters will be listed in the same order and with the same names as
listed in
[`vbFuns`](https://fishr-core-team.github.io/FSA/reference/growthModels.md).

## Details

DEPRECATED ... use
[`findGrowthStarts`](https://fishr-core-team.github.io/FSA/reference/findGrowthStarts.md)
instead.

This function attempts to find reasonable starting values for a variety
of parameterizations of the von Bertalanffy growth function. There is no
guarantee that these starting values are the ‘best’ starting values. One
should use them with caution and should perform sensitivity analyses to
determine the impact of different starting values on the final model
results.

If `methLinf="Walford"`, then the Linf and K parameters are estimated
via the concept of the Ford-Walford plot. If `methLinf="oldAge"` then
Linf is estimated as the mean length of the `num4Linf` longest observed
lengths.

The product of the starting values for Linf and K is used as a starting
value for omega in the GallucciQuinn and Mooij parameterizations. The
result of log(2) divided by the starting value for K is used as the
starting value for t50 in the Weisberg parameterization.

If `meth0="yngAge"`, then a starting value for t0 or L0 is found by
algebraically solving the typical or original parameterization,
respectively, for t0 or L0 using the mean length of the first age with
more than one data point as a “known” quantity. If `meth0="poly"` then a
second-degree polynomial model is fit to the mean length-at-age data.
The t0 starting value is set equal to the root of the polynomial that is
closest to zero. The L0 starting value is set equal to the mean length
at age-0 predicted from the polynomial function.

Starting values for the L1 and L3 parameters in the Schnute
parameterization and the L1, L2, and L3 parameters in the Francis
parameterization may be found in two ways. If `methEV="poly"`, then the
starting values are the predicted length-at-age from a second-degree
polynomial fit to the mean lengths-at-age data. If `methEV="means"` then
the observed sample means at the corresponding ages are used. In the
case where one of the supplied ages is fractional, then the value
returned will be linearly interpolated between the mean lengths of the
two closest ages. The ages to be used for L1 and L3 in the Schnute and
Francis parameterizations are supplied as a numeric vector of length 2
in `ages2use=`. If `ages2use=NULL` then the minimum and maximum observed
ages will be used. In the Francis method, L2 will correspond to the age
half-way between the two ages in `ages2use=`. A warning will be given if
L2\<L1 for the Schnute method or if L2\<L1 or L3\<L2 for the Francis
method.

Starting values for the Somers and Pauly parameterizations are the same
as the traditional parameterization for Linf, K, and t0. However, for
the Pauly parameterization the starting value for Kpr is the starting
value for K divided by 1 minus the starting value of NGT. The starting
values of C, ts, WP, and NGT are set at constants that are unlikely to
work for all species. Thus, the user should use the `fixed` argument to
fix starting values for these parameters that are more likely to result
in a reliable fit.

## Note

The ‘original’ and ‘vonBertalanffy’ and the ‘typical’ and ‘BevertonHolt’
parameterizations are synonymous.

## IFAR Chapter

12-Individual Growth.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

See references in
[`vbFuns`](https://fishr-core-team.github.io/FSA/reference/growthModels.md).

## See also

See
[`growthFunShow`](https://fishr-core-team.github.io/FSA/reference/growthModels.md)
to display the equations for the parameterizations used in FSA and
[`vbFuns`](https://fishr-core-team.github.io/FSA/reference/growthModels.md)
for functions that represent the von Bertalanffy parameterizations. See
[`nlsTracePlot`](https://fishr-core-team.github.io/FSA/reference/nlsTracePlot.md)
for help troubleshooting nonlinear models that don't converge.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## Simple examples of some parameterization
vbStarts(tl~age,data=SpotVA1)
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> $Linf
#> [1] 13.26773
#> 
#> $K
#> [1] 0.4114688
#> 
#> $t0
#> [1] -2.124367
#> 
vbStarts(tl~age,data=SpotVA1,type="Original")
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> $Linf
#> [1] 13.26773
#> 
#> $K
#> [1] 0.4114688
#> 
#> $L0
#> [1] 7.732
#> 
vbStarts(tl~age,data=SpotVA1,type="Francis",ages2use=c(0,5))
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> $L1
#> [1] 7.732
#> 
#> $L2
#> [1] 11.60785
#> 
#> $L3
#> [1] 12.4
#> 
vbStarts(tl~age,data=SpotVA1,type="Somers")
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> $Linf
#> [1] 13.26773
#> 
#> $K
#> [1] 0.4114688
#> 
#> $t0
#> [1] -2.124367
#> 
#> $C
#> [1] 0.5
#> 
#> $ts
#> [1] 0.3
#> 
vbStarts(tl~age,data=SpotVA1,type="Ogle",valOgle=c(tr=0))
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> $Linf
#> [1] 13.26773
#> 
#> $K
#> [1] 0.4114688
#> 
#> $Lr
#> [1] 7.274961
#> 
vbStarts(tl~age,data=SpotVA1,type="Ogle",valOgle=c(Lr=8))
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> $Linf
#> [1] 13.26773
#> 
#> $K
#> [1] 0.4114688
#> 
#> $tr
#> [1] 0.7521688
#> 

## Using a different method to find Linf
vbStarts(tl~age,data=SpotVA1,methLinf="oldAge")
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> $Linf
#> [1] 12.4
#> 
#> $K
#> [1] 0.4114688
#> 
#> $t0
#> [1] -2.374338
#> 

## Using a different method to find t0 and L0
vbStarts(tl~age,data=SpotVA1,meth0="yngAge")
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> $Linf
#> [1] 13.26773
#> 
#> $K
#> [1] 0.4114688
#> 
#> $t0
#> [1] -2.124367
#> 
vbStarts(tl~age,data=SpotVA1,type="original",meth0="yngAge")
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> $Linf
#> [1] 13.26773
#> 
#> $K
#> [1] 0.4114688
#> 
#> $L0
#> [1] 7.732
#> 

## Using a different method to find the L1, L2, and L3
vbStarts(tl~age,data=SpotVA1,type="Francis",ages2use=c(0,5),methEV="means")
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> $L1
#> [1] 7.732
#> 
#> $L2
#> [1] 11.60785
#> 
#> $L3
#> [1] 12.4
#> 
vbStarts(tl~age,data=SpotVA1,type="Schnute",ages2use=c(0,5),methEV="means")
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> $L1
#> [1] 7.732
#> 
#> $L3
#> [1] 12.4
#> 
#> $K
#> [1] 0.4114688
#> 

## Examples with a Plot
vbStarts(tl~age,data=SpotVA1,plot=TRUE)
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> Warning: 'vbFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.

#> $Linf
#> [1] 13.26773
#> 
#> $K
#> [1] 0.4114688
#> 
#> $t0
#> [1] -2.124367
#> 
vbStarts(tl~age,data=SpotVA1,type="Francis",ages2use=c(0,5),plot=TRUE)
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> Warning: 'vbFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.

#> $L1
#> [1] 7.732
#> 
#> $L2
#> [1] 11.60785
#> 
#> $L3
#> [1] 12.4
#> 
vbStarts(tl~age,data=SpotVA1,type="Somers",plot=TRUE)
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> Warning: 'vbFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.

#> $Linf
#> [1] 13.26773
#> 
#> $K
#> [1] 0.4114688
#> 
#> $t0
#> [1] -2.124367
#> 
#> $C
#> [1] 0.5
#> 
#> $ts
#> [1] 0.3
#> 

## Examples where some parameters are fixed by the user
vbStarts(tl~age,data=SpotVA1,fixed=list(Linf=15))
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> $Linf
#> [1] 15
#> 
#> $K
#> [1] 0.4114688
#> 
#> $t0
#> [1] -1.760933
#> 
vbStarts(tl~age,data=SpotVA1,fixed=list(Linf=15,K=0.3,t0=-1),plot=TRUE)
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> Warning: 'vbFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.

#> $Linf
#> [1] 15
#> 
#> $K
#> [1] 0.3
#> 
#> $t0
#> [1] -1
#> 
vbStarts(tl~age,data=SpotVA1,type="Pauly",fixed=list(t0=-1.5),plot=TRUE)
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
#> Warning: 'vbFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.

#> $Linf
#> [1] 13.26773
#> 
#> $Kpr
#> [1] 0.5878125
#> 
#> $t0
#> [1] -1.5
#> 
#> $ts
#> [1] 0.3
#> 
#> $NGT
#> [1] 0.3
#> 

## See examples in vbFuns() for use of vbStarts() when fitting Von B models
```
