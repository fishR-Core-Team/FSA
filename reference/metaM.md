# Estimate natural mortality from a variety of empirical methods.

Several methods can be used to estimated natural mortality (M) from
other types of data, including parameters from the von Bertalanffy
growth equation, maximum age, and temperature. These relationships have
been developed from meta-analyses of a large number of populations.
Several of these methods are implemented in this function.

## Usage

``` r
Mmethods(method = c("all", "tmax", "K", "Hoenig", "Pauly", "FAMS"))

metaM(
  method = Mmethods(),
  tmax = NULL,
  K = NULL,
  Linf = NULL,
  t0 = NULL,
  b = NULL,
  L = NULL,
  Temp = NULL,
  t50 = NULL,
  Winf = NULL,
  PS = NULL,
  verbose = TRUE
)
```

## Arguments

- method:

  A string that indicates what grouping of methods to return (defaults
  to all methods) in `Mmethods()` or which methods or equations to use
  in `metaM()`. See details.

- tmax:

  The maximum age for the population of fish.

- K:

  The Brody growth coefficient from the fit of the von Bertalanffy
  growth function.

- Linf:

  The asymptotic mean length (cm) from the fit of the von Bertalanffy
  growth function.

- t0:

  The x-intercept from the fit of the von Bertalanffy growth function.

- b:

  The exponent from the weight-length relationship (slope from the
  logW-logL relationship).

- L:

  The body length of the fish (cm).

- Temp:

  The temperature experienced by the fish (C).

- t50:

  The age (time) when half the fish in the population are mature.

- Winf:

  The asymptotic mean weight (g) from the fit of the von Bertalanffy
  growth function.

- PS:

  The proportion of the population that survive to `tmax`. Should
  usually be around 0.01 or 0.05.

- verbose:

  Logical for whether to include method name and given inputs in
  resultant data.frame. Defaults to `TRUE`.

## Value

`Mmethods` returns a character vector with a list of methods.

`metaM` returns a data.frame with the following items:

- `M`: The estimated natural mortality rate.

- `cm`: The estimated conditional natural mortality rate (computed
  directly from `M`).

- `method`: The name for the method within the function (as given in
  `method`).

- `name`: A more descriptive name for the method.

- `givens`: A string that contains the input values required by the
  method to estimate M.

## Details

One of several methods is chosen with `method`. The available methods
can be seen with `Mmethods()` and are listed below with a brief
description of where the equation came from. The sources (listed below)
should be consulted for more specific information.

- `method="HoenigNLS"`: The “modified Hoenig equation derived with a
  non-linear model” as described in Then *et al.* (2015) on the third
  line of Table 3. This method was the preferred method suggested by
  Then *et al.* (2015). Requires only `tmax`.

- `method="PaulyLNoT"`: The “modified Pauly length equation” as
  described on the sixth line of Table 3 in Then *et al.* (2015). Then
  *et al.* (2015) suggested that this is the preferred method if maximum
  age (tmax) information was not available. Requires `K` and `Linf`.

- `method="PaulyL"`: The “Pauly (1980) equation using fish lengths” from
  his equation 11. This is the most commonly used method in the
  literature. Note that Pauly used common logarithms as used here but
  the model is often presented in other sources with natural logarithms.
  Requires `K`, `Linf`, and `T`.

- `method="PaulyW"`: The “Pauly (1980) equation for weights” from his
  equation 10. Requires `K`, `Winf`, and `T`.

- `method="HoeingO"`, `method="HoeingOF"`, `method="HoeingOM"`,
  `method="HoeingOC"`: The original “Hoenig (1983) composite”, “fish”,
  “mollusc”, and “cetacean” (fit with OLS) equations from the second
  column on page 899 of Hoenig (1983). Requires only `tmax`.

- `method="HoeingO2"`, `method="HoeingO2F"`, `method="HoeingO2M"`,
  `method="HoeingO2C"`: The original “Hoenig (1983) composite”, “fish”,
  “mollusc”, and “cetacean” (fit with Geometric Mean Regression)
  equations from the second column on page 537 of Kenchington (2014).
  Requires only `tmax`.

- `method="HoenigLM"`: The “modified Hoenig equation derived with a
  linear model” as described in Then *et al.* (2015) on the second line
  of Table 3. Requires only `tmax`.

- `method="HewittHoenig"`: The “Hewitt and Hoenig (2005) equation” from
  their equation 8. Requires only `tmax`.

- `method="tmax1"`: The “one-parameter tmax equation” from the first
  line of Table 3 in Then *et al.* (2015). Requires only `tmax`.

- `method="HamelCope"`: The equation 7 from Hamel and Cope (2022).
  Requires only `tmax`.

- `method="K1"`: The “one-parameter K equation” from the fourth line of
  Table 3 in Then *et al.* (2015). Requires only `K`.

- `method="K2"`: The “two-parameter K equation” from the fifth line of
  Table 3 in Then *et al.* (2015). Requires only `K`.

- `method="JensenK1"`: The “Jensen (1996) one-parameter K equation”.
  Requires only `K`.

- `method="JensenK2"`: The “Jensen (2001) two-parameter K equation” from
  their equation 8. Requires only `K`.

- `method="Gislason"`: The “Gislason *et al.* (2010) equation” from
  their equation 2. Requires `K`, `Linf`, and `L`.

- `method="AlversonCarney"`: The “Alverson and Carney (1975) equation”
  as given in equation 10 of Zhang and Megrey (2006). Requires `tmax`
  and `K`.

- `method="Charnov"`: The “Charnov *et al.* (2013) equation” as given in
  the second column of page 545 of Kenchington (2014). Requires `K`,
  `Linf`, and `L`.

- `method="ZhangMegreyD"`, `method="ZhangMegreyP"`: The “Zhang and
  Megrey (2006) equation” as given in their equation 8 but modified for
  demersal or pelagic fish. Thus, the user must choose the fish type
  with `group`. Requires `tmax`, `K`, `t0`, `t50`, and `b`.

- `method="RikhterEfanov1"`: The “Rikhter and Efanov (1976) equation
  (#2)” as given in the second column of page 541 of Kenchington (2014)
  and in Table 6.4 of Miranda and Bettoli (2007). Requires only `t50`.

- `method="RikhterEfanov2"`: The “Rikhter and Efanov (1976) equation
  (#1)” as given in the first column of page 541 of Kenchington (2014).
  Requires `t50`, `K`, `t0`, and `b`.

- `method="QuinnDeriso"`: The “Quinn & Derison (1999)” equation as given
  in the FAMS manual as equation 4:18. Requires `PS` and `tmax`.
  Included only for use with `rFAMS` package.

- `method="ChenWatanabe"`: The “Chen & Watanabe (1989)” equation as
  given in the FAMS manual as equation 4:24. As suggested in FAMS manual
  used `tmax` for final time and 1 as initial time. Requires `tmax`,
  `K`, and `t0`. Included only for use with `rFAMS` package.

- `method="PetersonWroblewski"`: The “Peterson & Wroblewski (1984)”
  equation as given in the FAMS manual as equation 4:22. As suggested in
  FAMS manual used `Winf` for weight. Requires `Winf`. Included only for
  use with `rFAMS` package.

Conditional mortality (cm) is estimated from instantaneous natural
mortality (M) with 1-exp(-M). It is returned with M here simply as a
courtesy for those using the `rFAMS` package.

## Testing

Kenchington (2014) provided life history parameters for several stocks
and used many models to estimate M. I checked the calculations for the
`"PaulyL"`, `"PaulyW"`, `"HoenigO"`, `"HoenigOF"`, `"HoenigO2"`,
`"HoenigO2F"`, `"JensenK1"`, `"Gislason"`, `"AlversonCarney"`,
`"Charnov"`, `"ZhangMegrey"`, `"RikhterEfanov1"`, and `"RikhterEfanov2"`
methods for three stocks. All results perfectly matched Kenchington's
results for Chesapeake Bay Anchovy and Rio Formosa Seahorse. For the
Norwegian Fjord Lanternfish, all results perfectly matched Kenchington's
results except for `"HoenigOF"` and `"HoenigO2F"`.

Results for the Rio Formosa Seahorse data were also tested against
results from
[`M.empirical`](https://rdrr.io/pkg/fishmethods/man/M.empirical.html)
from fishmethods for the `"PaulyL"`, `"PaulyW"`, `"HoenigO"`,
`"HoenigOF"`, `"Gislason"`, and `"AlversonCarney"` methods (the only
methods in common between the two packages). All results matched
perfectly.

## IFAR Chapter

11-Mortality.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

Alverson, D.L. and M.J. Carney. 1975. A graphic review of the growth and
decay of population cohorts. Journal du Conseil International pour
l'Exploration de la Mer. 36:133-143.

Chen, S. and S. Watanabe. 1989. Age dependence of natural mortality
coefficient in fish population dynamics. Nippon Suisan Gakkaishi
55:205-208.

Charnov, E.L., H. Gislason, and J.G. Pope. 2013. Evolutionary assembly
rules for fish life histories. Fish and Fisheries. 14:213-224.

Gislason, H., N. Daan, J.C. Rice, and J.G. Pope. 2010. Size, growth,
temperature and the natural mortality of marine fish. Fish and Fisheries
11:149-158.

Hamel, O. and J. M. Cope. 2022. Development and considerations for
application of a longevity-based prior for the natural mortality rate.
Fisheries Research 256:106477 \[Was (is? from
https://www.researchgate.net/publication/363595336_Development_and_considerations_for_application_of_a_longevity-based_prior_for_the_natural_mortality_rate).\]

Hewitt, D.A. and J.M. Hoenig. 2005. Comparison of two approaches for
estimating natural mortality based on longevity. Fishery Bulletin.
103:433-437. \[Was (is?) from
http://fishbull.noaa.gov/1032/hewitt.pdf.\]

Hoenig, J.M. 1983. Empirical use of longevity data to estimate mortality
rates. Fishery Bulletin. 82:898-903. \[Was (is?) from
http://www.afsc.noaa.gov/REFM/age/Docs/Hoenig_EmpiricalUseOfLongevityData.pdf.\]

Jensen, A.L. 1996. Beverton and Holt life history invariants result from
optimal trade-off of reproduction and survival. Canadian Journal of
Fisheries and Aquatic Sciences. 53:820-822. \[Was (is?) from .\]

Jensen, A.L. 2001. Comparison of theoretical derivations, simple linear
regressions, multiple linear regression and principal components for
analysis of fish mortality, growth and environmental temperature data.
Environometrics. 12:591-598. \[Was (is?) from
http://deepblue.lib.umich.edu/bitstream/handle/2027.42/35236/487_ftp.pdf.\]

Kenchington, T.J. 2014. Natural mortality estimators for
information-limited fisheries. Fish and Fisheries. 14:533-562.

Pauly, D. 1980. On the interrelationships between natural mortality,
growth parameters, and mean environmental temperature in 175 fish
stocks. Journal du Conseil International pour l'Exploration de la Mer.
39:175-192. \[Was (is?) from
http://innri.unuftp.is/pauly/On%20the%20interrelationships%20betwe.pdf.\]

Peterson, I. and J.S. Wroblewski. 1984. Mortality rate of fishes in the
pelagic ecosystem. Canadian Journal of Fisheries and Aquatic Sciences.
41:1117-1120.

Quinn III, T.J. and R.B. Deriso. 1999. Quantitative Fish Dynamics.
Oxford University Press, New York.

Rikhter, V.A., and V.N. Efanov. 1976. On one of the approaches for
estimating natural mortality in fish populations (in Russian). ICNAF
Research Document 76/IV/8, 12pp.

Slipke, J.W. and M.J. Maceina. 2013. Fisheries Analysis and Modeling
Simulator (FAMS 1.64). American Fisheries Society.

Then, A.Y., J.M. Hoenig, N.G. Hall, and D.A. Hewitt. 2015. Evaluating
the predictive performance of empirical estimators of natural mortality
rate using information on over 200 fish species. ICES Journal of Marine
Science. 72:82-92.

Zhang, C-I and B.A. Megrey. 2006. A revised Alverson and Carney model
for estimating the instantaneous rate of natural mortality. Transactions
of the American Fisheries Society. 135-620-633. \[Was (is?) from
http://www.pmel.noaa.gov/foci/publications/2006/zhan0531.pdf.\]

## See also

See
[`M.empirical`](https://rdrr.io/pkg/fishmethods/man/M.empirical.html) in
fishmethods for similar functionality and the "Natural Mortality Tool"
Shiny app on-line.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## List names for available methods
Mmethods()
#>  [1] "HoenigNLS"          "HoenigO"            "HoenigOF"          
#>  [4] "HoenigOM"           "HoenigOC"           "HoenigO2"          
#>  [7] "HoenigO2F"          "HoenigO2M"          "HoenigO2C"         
#> [10] "HoenigLM"           "HewittHoenig"       "tmax1"             
#> [13] "HamelCope"          "PaulyLNoT"          "PaulyL"            
#> [16] "PaulyW"             "K1"                 "K2"                
#> [19] "JensenK1"           "JensenK2"           "Gislason"          
#> [22] "AlversonCarney"     "Charnov"            "ZhangMegreyD"      
#> [25] "ZhangMegreyP"       "RikhterEfanov1"     "RikhterEfanov2"    
#> [28] "QuinnDeriso"        "ChenWatanabe"       "PetersonWroblewski"
Mmethods("tmax")
#>  [1] "HamelCope"    "tmax1"        "HoenigNLS"    "HoenigO"      "HoenigOF"    
#>  [6] "HoenigOM"     "HoenigOC"     "HoenigO2"     "HoenigO2F"    "HoenigO2M"   
#> [11] "HoenigO2C"    "HoenigLM"     "HewittHoenig"

## Simple Examples
metaM("HamelCope",tmax=20)
#>      M        cm    method                                name  givens
#> 1 0.27 0.2366205 HamelCope Hamel and Cope (2022) tmax equation tmax=20
metaM("HoenigNLS",tmax=20)
#>           M        cm    method                                     name
#> 1 0.3150387 0.2702394 HoenigNLS Then et al. (2015) Hoenig (NLS) equation
#>    givens
#> 1 tmax=20
metaM("HoenigNLS",tmax=20,verbose=FALSE)
#>           M        cm    method
#> 1 0.3150387 0.2702394 HoenigNLS
 
## Example Patagonian Sprat ... from Table 2 in Cerna et al. (2014)
## http://www.scielo.cl/pdf/lajar/v42n3/art15.pdf
Temp <- 11
Linf <- 17.71
K <- 0.78
t0 <- -0.46
tmax <- t0+3/K
t50 <- t0-(1/K)*log(1-13.5/Linf)
metaM("RikhterEfanov1",t50=t50)
#>          M        cm         method                                name
#> 1 1.050009 0.6500656 RikhterEfanov1 Richter & Evanov (1976) equation #1
#>          givens
#> 1 t50=1.3818805
metaM("PaulyL",K=K,Linf=Linf,Temp=Temp)
#>         M        cm method                         name
#> 1 1.14058 0.6803665 PaulyL Pauly (1980) length equation
#>                        givens
#> 1 K=0.78, Linf=17.71, Temp=11
metaM("HoenigNLS",tmax=tmax)
#>          M        cm    method                                     name
#> 1 1.602862 0.7986805 HoenigNLS Then et al. (2015) Hoenig (NLS) equation
#>           givens
#> 1 tmax=3.3861538
metaM("HoenigO",tmax=tmax)
#>          M        cm  method                                  name
#> 1 1.274125 0.7203245 HoenigO Hoenig (1983) combined equation (OLS)
#>           givens
#> 1 tmax=3.3861538
metaM("HewittHoenig",tmax=tmax)
#>          M        cm       method                                 name
#> 1 1.246252 0.7124193 HewittHoenig Hewitt & Hoenig (2005) tmax equation
#>           givens
#> 1 tmax=3.3861538
metaM("AlversonCarney",K=K,tmax=tmax)
#>         M        cm         method                              name
#> 1 1.35398 0.7417895 AlversonCarney Alverson & Carney (1975) equation
#>                   givens
#> 1 tmax=3.3861538, K=0.78

## Example of multiple calculations
metaM(c("RikhterEfanov1","PaulyL","HoenigO","HewittHoenig","AlversonCarney"),
     K=K,Linf=Linf,Temp=Temp,tmax=tmax,t50=t50)
#>          M        cm         method                                  name
#> 1 1.050009 0.6500656 RikhterEfanov1   Richter & Evanov (1976) equation #1
#> 2 1.140580 0.6803665         PaulyL          Pauly (1980) length equation
#> 3 1.274125 0.7203245        HoenigO Hoenig (1983) combined equation (OLS)
#> 4 1.246252 0.7124193   HewittHoenig  Hewitt & Hoenig (2005) tmax equation
#> 5 1.353980 0.7417895 AlversonCarney     Alverson & Carney (1975) equation
#>                        givens
#> 1               t50=1.3818805
#> 2 K=0.78, Linf=17.71, Temp=11
#> 3              tmax=3.3861538
#> 4              tmax=3.3861538
#> 5      tmax=3.3861538, K=0.78

## Example of multiple methods using Mmethods
# select some methods
metaM(Mmethods()[-c(16,21,23:25,27:30)],K=K,Linf=Linf,Temp=Temp,tmax=tmax,t50=t50)
#>            M        cm         method
#> 1  1.6028619 0.7986805      HoenigNLS
#> 2  1.2741252 0.7203245        HoenigO
#> 3  1.2562214 0.7152721       HoenigOF
#> 4  1.2401272 0.7106526       HoenigOM
#> 5  0.8835623 0.5866920       HoenigOC
#> 6  1.4786176 0.7720474       HoenigO2
#> 7  1.5784652 0.7937085      HoenigO2F
#> 8  1.4266652 0.7598917      HoenigO2M
#> 9  1.4625421 0.7683534      HoenigO2C
#> 10 1.6243510 0.8029605       HoenigLM
#> 11 1.2462517 0.7124193   HewittHoenig
#> 12 1.5087915 0.7788229          tmax1
#> 13 1.5947297 0.7970366      HamelCope
#> 14 1.3304645 0.7356455      PaulyLNoT
#> 15 1.1405804 0.6803665         PaulyL
#> 16 1.3197600 0.7328006             K1
#> 17 1.3070000 0.7293693             K2
#> 18 1.1700000 0.6896331       JensenK1
#> 19 1.3566000 0.7424651       JensenK2
#> 20 1.3539801 0.7417895 AlversonCarney
#> 21 1.0500095 0.6500656 RikhterEfanov1
#>                                           name                      givens
#> 1     Then et al. (2015) Hoenig (NLS) equation              tmax=3.3861538
#> 2        Hoenig (1983) combined equation (OLS)              tmax=3.3861538
#> 3            Hoenig (1983) fish equation (OLS)              tmax=3.3861538
#> 4         Hoenig (1983) mollusk equation (OLS)              tmax=3.3861538
#> 5        Hoenig (1983) cetacean equation (OLS)              tmax=3.3861538
#> 6         Hoenig (1983) combined equation (GM)              tmax=3.3861538
#> 7             Hoenig (1983) fish equation (GM)              tmax=3.3861538
#> 8          Hoenig (1983) mollusk equation (GM)              tmax=3.3861538
#> 9         Hoenig (1983) cetacean equation (GM)              tmax=3.3861538
#> 10     Then et al. (2015) Hoenig (LM) equation              tmax=3.3861538
#> 11        Hewitt & Hoenig (2005) tmax equation              tmax=3.3861538
#> 12            Then et al. (2015) tmax equation              tmax=3.3861538
#> 13         Hamel and Cope (2022) tmax equation              tmax=3.3861538
#> 14     Then et al. (2015) Pauly_NLS-T equation          K=0.78, Linf=17.71
#> 15                Pauly (1980) length equation K=0.78, Linf=17.71, Temp=11
#> 16 Then et al. (2015) one-parameter K equation                      K=0.78
#> 17 Then et al. (2015) two-parameter K equation                      K=0.78
#> 18      Jensen (1996) one parameter K equation                      K=0.78
#> 19      Jensen (2001) two parameter K equation                      K=0.78
#> 20           Alverson & Carney (1975) equation      tmax=3.3861538, K=0.78
#> 21         Richter & Evanov (1976) equation #1               t50=1.3818805
# select just the Hoenig methods
metaM(Mmethods("Hoenig"),K=K,Linf=Linf,Temp=Temp,tmax=tmax,t50=t50)
#>            M        cm       method                                     name
#> 1  1.6028619 0.7986805    HoenigNLS Then et al. (2015) Hoenig (NLS) equation
#> 2  1.2741252 0.7203245      HoenigO    Hoenig (1983) combined equation (OLS)
#> 3  1.2562214 0.7152721     HoenigOF        Hoenig (1983) fish equation (OLS)
#> 4  1.2401272 0.7106526     HoenigOM     Hoenig (1983) mollusk equation (OLS)
#> 5  0.8835623 0.5866920     HoenigOC    Hoenig (1983) cetacean equation (OLS)
#> 6  1.4786176 0.7720474     HoenigO2     Hoenig (1983) combined equation (GM)
#> 7  1.5784652 0.7937085    HoenigO2F         Hoenig (1983) fish equation (GM)
#> 8  1.4266652 0.7598917    HoenigO2M      Hoenig (1983) mollusk equation (GM)
#> 9  1.4625421 0.7683534    HoenigO2C     Hoenig (1983) cetacean equation (GM)
#> 10 1.6243510 0.8029605     HoenigLM  Then et al. (2015) Hoenig (LM) equation
#> 11 1.2462517 0.7124193 HewittHoenig     Hewitt & Hoenig (2005) tmax equation
#>            givens
#> 1  tmax=3.3861538
#> 2  tmax=3.3861538
#> 3  tmax=3.3861538
#> 4  tmax=3.3861538
#> 5  tmax=3.3861538
#> 6  tmax=3.3861538
#> 7  tmax=3.3861538
#> 8  tmax=3.3861538
#> 9  tmax=3.3861538
#> 10 tmax=3.3861538
#> 11 tmax=3.3861538
 
## Example of computing an average M
# select multiple models used in FAMS (example only, these are not best models)
( res <- metaM(Mmethods("FAMS"),tmax=tmax,K=K,Linf=Linf,t0=t0,
               Temp=Temp,PS=0.01,Winf=30) )
#>           M        cm             method                                   name
#> 1 1.3600003 0.7433393        QuinnDeriso        Quinn & Deriso (1999) from FAMS
#> 2 1.2562214 0.7152721           HoenigOF      Hoenig (1983) fish equation (OLS)
#> 3 1.1700000 0.6896331           JensenK1 Jensen (1996) one parameter K equation
#> 4 0.8203911 0.5597405 PetersonWroblewski   Peterson & Watanabe (1984) from FAMS
#> 5 1.1405804 0.6803665             PaulyL           Pauly (1980) length equation
#> 6 0.1403480 0.1309443       ChenWatanabe       Chen & Watanabe (1989) from FAMS
#>                             givens
#> 1          PS=0.01, tmax=3.3861538
#> 2                   tmax=3.3861538
#> 3                           K=0.78
#> 4                          Winf=30
#> 5      K=0.78, Linf=17.71, Temp=11
#> 6 tmax=3.3861538, K=0.78, t0=-0.46
( meanM <- mean(res$M) )
#> [1] 0.9812569
( meancm <- mean(res$cm) )
#> [1] 0.5865493
```
