# DEPRECATED (as of v0.10.0). Creates a function for a specific parameterization of the von Bertalanffy, Gompertz, Richards, and logistic growth functions.

DEPRECATED (as of v0.10.0). Creates a function for a specific
parameterizations of the von Bertalanffy, Gompertz, Richards, and
logistic growth functions. Use `growthFunShow()` to see the equations
for each growth function.

## Usage

``` r
vbFuns(
  param = c("Typical", "typical", "Traditional", "traditional", "BevertonHolt",
    "Original", "original", "vonBertalanffy", "GQ", "GallucciQuinn", "Mooij", "Weisberg",
    "Ogle", "Schnute", "Francis", "Laslett", "Polacheck", "Somers", "Somers2", "Pauly",
    "Fabens", "Fabens2", "Wang", "Wang2", "Wang3", "Francis2", "Francis3"),
  simple = FALSE,
  msg = FALSE
)

GompertzFuns(
  param = c("Ricker1", "Ricker2", "Ricker3", "QuinnDeriso1", "QuinnDeriso2",
    "QuinnDeriso3", "QD1", "QD2", "QD3", "Original", "original", "Troynikov1",
    "Troynikov2"),
  simple = FALSE,
  msg = FALSE
)

RichardsFuns(param = 1, simple = FALSE, msg = FALSE)

logisticFuns(
  param = c("CJ1", "CJ2", "Karkach", "Haddon", "CampanaJones1", "CampanaJones2"),
  simple = FALSE,
  msg = FALSE
)

growthFunShow(
  type = c("vonBertalanffy", "Gompertz", "Richards", "Logistic", "Schnute",
    "SchnuteRichards"),
  param = NULL,
  case = param,
  plot = FALSE,
  ...
)
```

## Arguments

- param:

  A string (for von Bertalanffy, Gompertz, and logistic) or numeric (for
  Richards) that indicates the specific parameterization of the growth
  function. See details.

- simple:

  A logical that indicates whether the function will accept all
  parameter values in the first parameter argument (`=FALSE`; DEFAULT)
  or whether all individual parameters must be specified in separate
  arguments (`=TRUE`).

- msg:

  A logical that indicates whether a message about the growth function
  and parameter definitions should be output (`=TRUE`) or not (`=FALSE`;
  DEFAULT).

- type:

  A string (in `growthFunShow`) that indicates the type of growth
  function to show.

- case:

  A numeric that indicates the specific case of the Schnute function to
  use. See details.

- plot:

  A logical that indicates whether the growth function expression should
  be shown as an equation in a simple plot.

- ...:

  Not implemented.

## Value

The functions ending in `xxxFuns` return a function that can be used to
predict fish size given a vector of ages and values for the growth
function parameters and, in some parameterizations, values for
constants. The result should be saved to an object that is then the
function name. When the resulting function is used, the parameters are
ordered as shown when the definitions of the parameters are printed
after the function is called (if `msg=TRUE`). If `simple=FALSE`
(DEFAULT), then the values for all parameters may be included as a
vector in the first parameter argument (but in the same order).
Similarly, the values for all constants may be included as a vector in
the first constant argument (i.e., `t1`). If `simple=TRUE`, then all
parameters and constants must be declared individually. The resulting
function is somewhat easier to read when `simple=TRUE`, but is less
general for some applications.

An expression of the equation for each growth function may be created
with `growthFunShow`. In this function `type=` is used to select the
major function type (e.g., von Bertalanffy, Gompertz, Richards,
Logistic, Schnute) and `param=` is used to select a specific
parameterization of that growth function. If `plot=TRUE`, then a simple
graphic will be created with the equation using
[`plotmath`](https://rdrr.io/r/grDevices/plotmath.html) for a pretty
format.

## Details

DEPRECATED ... use
[`makeGrowthFun`](https://fishr-core-team.github.io/FSA/reference/makeGrowthFun.md)
and
[`showGrowthFun`](https://fishr-core-team.github.io/FSA/reference/showGrowthFun.md)
instead.

## Note

Take note of the following for parameterizations (i.e., `param`) of each
growth function:

- von Bertalanffy

  - The ‘Original’ and ‘vonBertalanffy’ are synonymous as are ‘Typical’,
    ‘Traditional’, and ‘BevertonHolt’. Further note that the ‘Ogle’
    parameterization has the ‘Original’/‘vonBertalanffy’ and
    ‘Typical’/‘Traditional’/‘BevertonHolt’ parameterizations as special
    cases.

- Gompertz

  - The ‘Ricker2’ and ‘QuinnDeriso1’ are synonymous, as are ‘Ricker3’
    and ‘QuinnDeriso2’.

  - The parameterizations and parameters for the Gompertz function are
    varied and confusing in the literature. I have attempted to use a
    uniform set of parameters in these functions, but that makes a
    direct comparison to the literature difficult. Common sources for
    Gompertz models are listed in the references below. I make some
    comments here to aid comparisons to the literature.

  - Within FSA, L0 is the mean length at age 0, Linf is the mean
    asymptotic length, ti is the age at the inflection point, gi is the
    instantaneous growth rate at the inflection point, t0 is a the
    hypothetical age at a mean length of 0, and a, b, and c are nuisance
    parameters with no real-world interpretations.

  - The function in Ricker (1975)\[p. 232\] is the same as ‘Ricker2’
    where the a parameter here is equal to G there and the gi parameter
    here is equal to the g parameter there. Also note that their w is L
    here.

  - In the Ricker (1979)\[p. 705\] functions (the ‘RickerX’ functions),
    the a parameter here is equal to k there and the gi parameter here
    is equal to the g parameter there. Also note that their w is L here.
    In the Ricker (1979) functions as presented in Campana and Jones
    (1992), the a parameter here is equal to k parameter there and the
    gi parameter here is equal to the G parameter there. Also note that
    their X is L here.

  - In the Quinn and Deriso (1999) functions (the ‘QuinnDerisoX’
    functions), the a parameter here is equal to lambda/K there and the
    gi parameter here is equal to the K parameter there. Also note that
    their Y is L here.

  - The function in Quist *et al.* (2012)\[p. 714\] is the same as
    ‘Ricker1’ where the gi parameter here is equal to the G parameter
    there and the ti parameter here is equal to the t0 parameter there.

  - The function in Katsanevakis and Maravelias (2008) is the same as
    ‘Ricker1’ where the gi parameter here is equal to the k2 parameter
    there and the ti parameter here is equal to the t2 parameter there.

- Richards

  - Only 4-parameter parameterizations from Tjorve and Tjorve (2010)
    that seemed useful for modeling fish growth are provided here.

  - Within FSA, Linf is the mean asymptotic length; ti is the age at the
    inflection point; k controls the slope at the inflection point
    (maximum relative growth rate); a is dimensionless but related to
    the horizontal position (i.e., age) of the inflection point; b, c,
    and d are dimensionless but related to the vertical position (i.e.,
    size) of the inflection point; and L0 is the mean length at age-0.

  - The parameterizations (1-5) correspond to functions/equations 5,
    3(alt), 7, 4, and 6, respectively, in Tjorve and Tjorve (2010). Note
    that their A, S, k are Linf, a, k and their d is b, c, and d,
    respectively, here (in FSA).

- logistic

  - Within FSA, L0 is the mean length at age 0, Linf is the mean
    asymptotic length, ti is the age at the inflection point, and gninf
    is the instantaneous growth rate at negative infinity.

## IFAR Chapter

12-Individual Growth.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

Campana, S.E. and C.M. Jones. 1992. Analysis of otolith microstructure
data. Pages 73-100 In D.K. Stevenson and S.E. Campana, editors. Otolith
microstructure examination and analysis. Canadian Special Publication of
Fisheries and Aquatic Sciences 117. \[Was (is?) from
https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/141734.pdf.\]

Fabens, A. 1965. Properties and fitting of the von Bertalanffy growth
curve. Growth 29:265-289.

Francis, R.I.C.C. 1988. Are growth parameters estimated from tagging and
age-length data comparable? Canadian Journal of Fisheries and Aquatic
Sciences, 45:936-942.

Gallucci, V.F. and T.J. Quinn II. 1979. Reparameterizing, fitting, and
testing a simple growth model. Transactions of the American Fisheries
Society, 108:14-25.

Garcia-Berthou, E., G. Carmona-Catot, R. Merciai, and D.H. Ogle. A
technical note on seasonal growth models. Reviews in Fish Biology and
Fisheries 22:635-640.

Gompertz, B. 1825. On the nature of the function expressive of the law
of human mortality, and on a new mode of determining the value of life
contingencies. Philosophical Transactions of the Royal Society of
London. 115:513-583.

Haddon, M., C. Mundy, and D. Tarbath. 2008. Using an inverse-logistic
model to describe growth increments of blacklip abalone (*Haliotis
rubra*) in Tasmania. Fishery Bulletin 106:58-71. \[Was (is?) from
https://spo.nmfs.noaa.gov/sites/default/files/pdf-content/2008/1061/haddon.pdf.\]

Karkach, A. S. 2006. Trajectories and models of individual growth.
Demographic Research 15:347-400. \[Was (is?) from
https://www.demographic-research.org/volumes/vol15/12/15-12.pdf.\]

Katsanevakis, S. and C.D. Maravelias. 2008. Modeling fish growth:
multi-model inference as a better alternative to a priori using von
Bertalanffy equation. Fish and Fisheries 9:178-187.

Mooij, W.M., J.M. Van Rooij, and S. Wijnhoven. 1999. Analysis and
comparison of fish growth from small samples of length-at-age data:
Detection of sexual dimorphism in Eurasian perch as an example.
Transactions of the American Fisheries Society 128:483-490.

Polacheck, T., J.P. Eveson, and G.M. Laslett. 2004. Increase in growth
rates of southern bluefin tuna (*Thunnus maccoyii*) over four decades:
1960 to 2000. Canadian Journal of Fisheries and Aquatic Sciences,
61:307-322.

Quinn, T. J. and R. B. Deriso. 1999. Quantitative Fish Dynamics. Oxford
University Press, New York, New York. 542 pages.

Quist, M.C., M.A. Pegg, and D.R. DeVries. 2012. Age and growth. Chapter
15 in A.V. Zale, D.L Parrish, and T.M. Sutton, editors. Fisheries
Techniques, Third Edition. American Fisheries Society, Bethesda, MD.

Richards, F. J. 1959. A flexible growth function for empirical use.
Journal of Experimental Biology 10:290-300.

Ricker, W.E. 1975. Computation and interpretation of biological
statistics of fish populations. Technical Report Bulletin 191, Bulletin
of the Fisheries Research Board of Canada. \[Was (is?) from
https://publications.gc.ca/collections/collection_2015/mpo-dfo/Fs94-191-eng.pdf.\]

Ricker, W.E. 1979. Growth rates and models. Pages 677-743 In W.S. Hoar,
D.J. Randall, and J.R. Brett, editors. Fish Physiology, Vol. 8:
Bioenergetics and Growth. Academic Press, New York, NY. \[Was (is?) from
https://books.google.com/books?id=CB1qu2VbKwQC&pg=PA705&lpg=PA705&dq=Gompertz+fish&source=bl&ots=y34lhFP4IU&sig=EM_DGEQMPGIn_DlgTcGIi_wbItE&hl=en&sa=X&ei=QmM4VZK6EpDAgwTt24CABw&ved=0CE8Q6AEwBw#v=onepage&q=Gompertz%20fish&f=false.\]

Schnute, J. 1981. A versatile growth model with statistically stable
parameters. Canadian Journal of Fisheries and Aquatic Sciences,
38:1128-1140.

Somers, I. F. 1988. On a seasonally oscillating growth function.
Fishbyte 6(1):8-11. \[Was (is?) from
https://www.fishbase.us/manual/English/fishbaseSeasonal_Growth.htm.\]

Tjorve, E. and K. M. C. Tjorve. 2010. A unified approach to the
Richards-model family for use in growth analyses: Why we need only two
model forms. Journal of Theoretical Biology 267:417-425. \[Was (is?)
from
https://www.researchgate.net/profile/Even_Tjorve/publication/46218377_A_unified_approach_to_the_Richards-model_family_for_use_in_growth_analyses_why_we_need_only_two_model_forms/links/54ba83b80cf29e0cb04bd24e.pdf.\]

Tjorve, K. M. C. and E. Tjorve. 2017. The use of Gompertz models in
growth analyses, and new Gompertz-model approach: An addition to the
Unified-Richards family. PLOS One. \[Was (is?) from
https://doi.org/10.1371/journal.pone.0178691.\]

Troynikov, V. S., R. W. Day, and A. M. Leorke. Estimation of seasonal
growth parameters using a stochastic Gompertz model for tagging data.
Journal of Shellfish Research 17:833-838. \[Was (is?) from
https://www.researchgate.net/profile/Robert_Day2/publication/249340562_Estimation_of_seasonal_growth_parameters_using_a_stochastic_gompertz_model_for_tagging_data/links/54200fa30cf203f155c2a08a.pdf.\]

Vaughan, D. S. and T. E. Helser. 1990. Status of the Red Drum stock of
the Atlantic coast: Stock assessment report for 1989. NOAA Technical
Memorandum NMFS-SEFC-263, 117 p. \[Was (is?) from
https://repository.library.noaa.gov/view/noaa/5927/noaa_5927_DS1.pdf.\]

Wang, Y.-G. 1998. An improved Fabens method for estimation of growth
parameters in the von Bertalanffy model with individual asymptotes.
Canadian Journal of Fisheries and Aquatic Sciences 55:397-400.

Weisberg, S., G.R. Spangler, and L. S. Richmond. 2010. Mixed effects
models for fish growth. Canadian Journal of Fisheries And Aquatic
Sciences 67:269-277.

Winsor, C.P. 1932. The Gompertz curve as a growth curve. Proceedings of
the National Academy of Sciences. 18:1-8. \[Was (is?) from
https://pmc.ncbi.nlm.nih.gov/articles/PMC1076153/pdf/pnas01729-0009.pdf.\]

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>, thanks to Gabor Grothendieck for
a hint about using [`get()`](https://rdrr.io/r/base/get.html).

## Examples

``` r
###########################################################
## Simple Examples -- Von B
( vb1 <- vbFuns() )
#> Warning: 'vbFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (t, Linf, K = NULL, t0 = NULL) 
#> {
#>     if (length(Linf) == 3) {
#>         K <- Linf[[2]]
#>         t0 <- Linf[[3]]
#>         Linf <- Linf[[1]]
#>     }
#>     Linf * (1 - exp(-K * (t - t0)))
#> }
#> <bytecode: 0x561ef60531c0>
#> <environment: 0x561ef5f99088>
ages <- 0:20
plot(vb1(ages,Linf=20,K=0.3,t0=-0.2)~ages,type="b",pch=19)

( vb2 <- vbFuns("Francis") )
#> Warning: 'vbFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (t, L1, L2 = NULL, L3 = NULL, t1, t3 = NULL) 
#> {
#>     if (length(L1) == 3) {
#>         L2 <- L1[[2]]
#>         L3 <- L1[[3]]
#>         L1 <- L1[[1]]
#>     }
#>     if (length(t1) == 2) {
#>         t3 <- t1[[2]]
#>         t1 <- t1[[1]]
#>     }
#>     r <- (L3 - L2)/(L2 - L1)
#>     L1 + (L3 - L1) * ((1 - r^(2 * ((t - t1)/(t3 - t1))))/(1 - 
#>         r^2))
#> }
#> <bytecode: 0x561ef5ffe3e8>
#> <environment: 0x561efab89348>
plot(vb2(ages,L1=10,L2=19,L3=20,t1=2,t3=18)~ages,type="b",pch=19)

( vb2c <- vbFuns("Francis",simple=TRUE) )   # compare to vb2
#> Warning: 'vbFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (t, L1, L2, L3, t1, t3) 
#> {
#>     r <- (L3 - L2)/(L2 - L1)
#>     L1 + (L3 - L1) * ((1 - r^(2 * ((t - t1)/(t3 - t1))))/(1 - 
#>         r^2))
#> }
#> <bytecode: 0x561ef5ffc340>
#> <environment: 0x561efa388478>

## Simple Examples -- Gompertz
( gomp1 <- GompertzFuns() )
#> Warning: 'GompertzFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (t, Linf, gi = NULL, ti = NULL) 
#> {
#>     if (length(Linf) == 3) {
#>         gi <- Linf[[2]]
#>         ti <- Linf[[3]]
#>         Linf <- Linf[[1]]
#>     }
#>     Linf * exp(-exp(-gi * (t - ti)))
#> }
#> <bytecode: 0x561efae32898>
#> <environment: 0x561efb6c93f0>
plot(gomp1(ages,Linf=800,gi=0.5,ti=5)~ages,type="b",pch=19)

( gomp2 <- GompertzFuns("Ricker2") )
#> Warning: 'GompertzFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (t, L0, b = NULL, gi = NULL) 
#> {
#>     if (length(L0) == 3) {
#>         b <- L0[[2]]
#>         gi <- L0[[3]]
#>         L0 <- L0[[1]]
#>     }
#>     L0 * exp(b * (1 - exp(-gi * t)))
#> }
#> <bytecode: 0x561efae315c8>
#> <environment: 0x561efadd3988>
plot(gomp2(ages,L0=2,b=6,gi=0.5)~ages,type="b",pch=19)

( gomp2c <- GompertzFuns("Ricker2",simple=TRUE) )   # compare to gomp2
#> Warning: 'GompertzFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (t, L0, b, gi) 
#> {
#>     L0 * exp(b * (1 - exp(-gi * t)))
#> }
#> <bytecode: 0x561efb6c3540>
#> <environment: 0x561ef5513690>
( gompT <- GompertzFuns("Troynikov1"))
#> Warning: 'GompertzFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (Lm, dt, Linf, gi = NULL) 
#> {
#>     if (length(Linf) == 2) {
#>         gi <- Linf[2]
#>         Linf <- Linf[1]
#>     }
#>     Linf * ((Lm/Linf)^exp(-gi * dt)) - Lm
#> }
#> <bytecode: 0x561efb6c4350>
#> <environment: 0x561ef3a49358>

## Simple Examples -- Richards
( rich1 <- RichardsFuns(1) )
#> Warning: 'RichardsFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (t, Linf, k = NULL, ti = NULL, b1 = NULL) 
#> {
#>     if (length(Linf) == 4) {
#>         k <- Linf[[2]]
#>         ti <- Linf[[3]]
#>         b1 <- Linf[[4]]
#>         Linf <- Linf[[1]]
#>     }
#>     Linf/((1 + b1 * exp(-k * (t - ti)))^(1/b1))
#> }
#> <bytecode: 0x561ef7e6d198>
#> <environment: 0x561ef94de850>
plot(rich1(ages,Linf=800,k=0.5,ti=3,b1=0.15)~ages,type="b",pch=19)

( rich2 <- RichardsFuns(2) )
#> Warning: 'RichardsFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (t, Linf, k = NULL, t0 = NULL, b2 = NULL) 
#> {
#>     if (length(Linf) == 4) {
#>         k <- Linf[[2]]
#>         t0 <- Linf[[3]]
#>         b2 <- Linf[[4]]
#>         Linf <- Linf[[1]]
#>     }
#>     Linf/((1 + exp(-k * (t - t0)))^(-b2))
#> }
#> <bytecode: 0x561ef7e6f470>
#> <environment: 0x561ef99b1e70>
plot(rich2(ages,Linf=800,k=0.5,t0=-1,b2=6)~ages,type="b",pch=19)

( rich3 <- RichardsFuns(3) )
#> Warning: 'RichardsFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (t, Linf, k = NULL, L0 = NULL, b3 = NULL) 
#> {
#>     if (length(Linf) == 4) {
#>         k <- Linf[[2]]
#>         L0 <- Linf[[3]]
#>         b3 <- Linf[[4]]
#>         Linf <- Linf[[1]]
#>     }
#>     Linf * (1 + (((L0/Linf)^(1 - b3)) - 1) * exp(-k * t))^(1/(1 - 
#>         b3))
#> }
#> <bytecode: 0x561ef94d52a8>
#> <environment: 0x561efa82e0a8>
plot(rich3(ages,Linf=800,k=0.5,L0=50,b3=1.5)~ages,type="b",pch=19)

( rich4 <- RichardsFuns(4) )
#> Warning: 'RichardsFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (t, Linf, k = NULL, ti = NULL, b2 = NULL) 
#> {
#>     if (length(Linf) == 4) {
#>         k <- Linf[[2]]
#>         ti <- Linf[[3]]
#>         b2 <- Linf[[4]]
#>         Linf <- Linf[[1]]
#>     }
#>     Linf * (1 - (1/b2) * exp(-k * (t - ti)))^b2
#> }
#> <bytecode: 0x561ef94daa80>
#> <environment: 0x561efa5708b8>
plot(rich4(ages,Linf=800,k=0.5,ti=3,b2=6)~ages,type="b",pch=19)

( rich5 <- RichardsFuns(5) )
#> Warning: 'RichardsFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (t, Linf, k = NULL, ti = NULL, b3 = NULL) 
#> {
#>     if (length(Linf) == 4) {
#>         k <- Linf[[2]]
#>         ti <- Linf[[3]]
#>         b3 <- Linf[[4]]
#>         Linf <- Linf[[1]]
#>     }
#>     Linf * (1 + (b3 - 1) * exp(-k * (t - ti)))^(1/(1 - b3))
#> }
#> <bytecode: 0x561ef94d9040>
#> <environment: 0x561efb5ecdf0>
plot(rich5(ages,Linf=800,k=0.5,ti=3,b3=0.95)~ages,type="b",pch=19)
lines(rich5(ages,Linf=800,k=0.5,ti=3,b3=1.5)~ages,type="b",pch=19,col="blue")

( rich2c <- RichardsFuns(2,simple=TRUE) ) # compare to rich2
#> Warning: 'RichardsFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (t, Linf, k, t0, b2) 
#> {
#>     Linf/((1 + exp(-k * (t - t0)))^(-b2))
#> }
#> <bytecode: 0x561ef7e7cf88>
#> <environment: 0x561efb017f20>

## Simple Examples -- Logistic
( log1 <- logisticFuns() )
#> Warning: 'logisticFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (t, Linf, gninf = NULL, ti = NULL) 
#> {
#>     if (length(Linf) == 3) {
#>         gninf <- Linf[[2]]
#>         ti <- Linf[[3]]
#>         Linf <- Linf[[1]]
#>     }
#>     Linf/(1 + exp(-gninf * (t - ti)))
#> }
#> <bytecode: 0x561efb0a2170>
#> <environment: 0x561efb0aa4b8>
plot(log1(ages,Linf=800,gninf=0.5,ti=5)~ages,type="b",pch=19)

( log2 <- logisticFuns("CJ2") )
#> Warning: 'logisticFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (t, Linf, gninf = NULL, a = NULL) 
#> {
#>     if (length(Linf) == 3) {
#>         gninf <- Linf[[2]]
#>         a <- Linf[[3]]
#>         Linf <- Linf[[1]]
#>     }
#>     Linf/(1 + a * exp(-gninf * t))
#> }
#> <bytecode: 0x561efb0a4c98>
#> <environment: 0x561efc90c8d8>
plot(log2(ages,Linf=800,gninf=0.5,a=10)~ages,type="b",pch=19)

( log2c <- logisticFuns("CJ2",simple=TRUE) ) # compare to log2
#> Warning: 'logisticFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (t, Linf, gninf, a) 
#> {
#>     Linf/(1 + a * exp(-gninf * t))
#> }
#> <bytecode: 0x561efb0a4020>
#> <environment: 0x561efca8f2e8>
( log3 <- logisticFuns("Karkach") )
#> Warning: 'logisticFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (t, Linf, L0 = NULL, gninf = NULL) 
#> {
#>     if (length(Linf) == 3) {
#>         L0 <- Linf[[2]]
#>         gninf <- Linf[[3]]
#>         Linf <- Linf[[1]]
#>     }
#>     L0 * Linf/(L0 + (Linf - L0) * exp(-gninf * t))
#> }
#> <bytecode: 0x561efb0a3a38>
#> <environment: 0x561efcb439f8>
plot(log3(ages,L0=10,Linf=800,gninf=0.5)~ages,type="b",pch=19)

( log4 <- logisticFuns("Haddon") )
#> Warning: 'logisticFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
#> function (Lm, dLmax, L50 = NULL, L95 = NULL) 
#> {
#>     if (length(dLmax) == 3) {
#>         L50 <- dLmax[2]
#>         L95 <- dLmax[3]
#>         dLmax <- dLmax[1]
#>     }
#>     dLmax/(1 + exp(log(19) * ((Lm - L50)/(L95 - L50))))
#> }
#> <bytecode: 0x561efb0a81a8>
#> <environment: 0x561efcd0d350>


###########################################################
## Examples of fitting
##   After the last example a plot is constructed with three
##   or four lines on top of each other illustrating that the
##   parameterizations all produce the same fitted values.
##   However, observe the correlations in the summary() results.

## Von B
plot(tl~age,data=SpotVA1,pch=19)

# Fitting the typical parameterization of the von B function
fit1 <- nls(tl~vb1(age,Linf,K,t0),data=SpotVA1,
            start=vbStarts(tl~age,data=SpotVA1))
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
summary(fit1,correlation=TRUE)
#> 
#> Formula: tl ~ vb1(age, Linf, K, t0)
#> 
#> Parameters:
#>      Estimate Std. Error t value Pr(>|t|)    
#> Linf 16.79818    1.93455   8.683  < 2e-16 ***
#> K     0.22493    0.06922   3.250  0.00125 ** 
#> t0   -2.55738    0.47496  -5.384 1.24e-07 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.053 on 400 degrees of freedom
#> 
#> Correlation of Parameter Estimates:
#>    Linf  K    
#> K  -0.99      
#> t0 -0.94  0.98
#> 
#> Number of iterations to convergence: 10 
#> Achieved convergence tolerance: 5.538e-06
#> 
curve(vb1(x,Linf=coef(fit1)),from=0,to=5,col="red",lwd=10,add=TRUE)

# Fitting the Francis parameterization of the von B function
fit2 <- nls(tl~vb2c(age,L1,L2,L3,t1=0,t3=5),data=SpotVA1,
            start=vbStarts(tl~age,data=SpotVA1,type="Francis",ages2use=c(0,5)))
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
summary(fit2,correlation=TRUE)
#> 
#> Formula: tl ~ vb2c(age, L1, L2, L3, t1 = 0, t3 = 5)
#> 
#> Parameters:
#>    Estimate Std. Error t value Pr(>|t|)    
#> L1  7.34803    0.18749   39.19   <2e-16 ***
#> L2 11.41275    0.08344  136.78   <2e-16 ***
#> L3 13.72912    0.32219   42.61   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.053 on 400 degrees of freedom
#> 
#> Correlation of Parameter Estimates:
#>    L1    L2   
#> L2 -0.43      
#> L3  0.48  0.20
#> 
#> Number of iterations to convergence: 7 
#> Achieved convergence tolerance: 9.456e-06
#> 
curve(vb2c(x,L1=coef(fit2)[1],L2=coef(fit2)[2],L3=coef(fit2)[3],t1=0,t3=5),
      from=0,to=5,col="blue",lwd=5,add=TRUE)

# Fitting the Schnute parameterization of the von B function
vb3 <- vbFuns("Schnute")
#> Warning: 'vbFuns()' is deprecated as of v0.10.0. Please use 'makeGrowthFun()' instead as it will be continuously updated in the future.
fit3 <- nls(tl~vb3(age,L1,L3,K,t1=0,t3=4),data=SpotVA1,
            start=vbStarts(tl~age,data=SpotVA1,type="Schnute",ages2use=c(0,4)))
#> Warning: 'vbStarts()' is deprecated as of v0.10.0. Please use 'findGrowthStarts()' instead as it provides starting values based on a better theoretical approach which should generally work better than those provided by vbStarts().
summary(fit3,correlation=TRUE)
#> 
#> Formula: tl ~ vb3(age, L1, L3, K, t1 = 0, t3 = 4)
#> 
#> Parameters:
#>    Estimate Std. Error t value Pr(>|t|)    
#> L1  7.34803    0.18749   39.19  < 2e-16 ***
#> L3 12.95500    0.18740   69.13  < 2e-16 ***
#> K   0.22494    0.06922    3.25  0.00125 ** 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.053 on 400 degrees of freedom
#> 
#> Correlation of Parameter Estimates:
#>    L1    L3   
#> L3  0.30      
#> K  -0.80 -0.71
#> 
#> Number of iterations to convergence: 8 
#> Achieved convergence tolerance: 8.268e-06
#> 
curve(vb3(x,L1=coef(fit3),t1=c(0,4)),from=0,to=5,col="green",lwd=2,add=TRUE)


```
