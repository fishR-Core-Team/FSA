# Growth Function Parameterizations in FSA

## Introduction

The most common growth models used in fisheries, such as von
Bertalanffy, Gompertz, logistic, Richards, Schnute, and
Schnute-Richards, are non-linear models. Most of these functions can be
expressed in different forms, called *parameterizations*, with different
parameters. Parameters in common between the different parameterizations
and all predicted values will be the same across parameterizations.
However, the different parameters may provide insights into different
characteristics of growth (e.g., mean length at a specific age, or mean
age at a specific length) or provide some benefits to fitting the
non-linear function to real data.

The main growth-related analysis functions in `FSA` –
[`makeGrowthFun()`](https://fishr-core-team.github.io/FSA/reference/makeGrowthFun.md),
[`showGrowthFun()`](https://fishr-core-team.github.io/FSA/reference/showGrowthFun.md),
[`findGrowthStarts()`](https://fishr-core-team.github.io/FSA/reference/findGrowthStarts.md)
– may be used for a variety of parameterizations of the common growth
functions. This article shows those different parameterizations and
defines the various parameters.

  

## von Bertalanffy

### Annual Length-at-Age Functions

The von Bertalanffy parameterizations for length and annual age data are
in [Table 1](#tbl-VBparams1). In these equations, the response variable,
$`L`$, is length and the explanatory variable, $`t`$ is age, and
$`E(L_t)`$ is the “expected length at age $`t`$” or the mean length at
age $`t`$.

  

| param | pname | Equation |
|:--:|:--:|:---|
| 1 | Traditional | $`E(L_t)=L_\infty\left(1-e^{-K(t-t_0)}\right)`$ |
| 2 | Original | $`E(L_t)=L_\infty - (L_\infty-L_0)~e^{-Kt}`$ |
| 3 | Gallucci-Quinn | $`E(L_t)=\frac{\omega}{K}\left(1-e^{-K(t-t_0)}\right)`$ |
| 4 | Mooij | $`E(L_t)=L_\infty - (L_\infty-L_0)~e^{-\frac{\omega}{L_\infty}t}`$ |
| 5 | Weisberg | $`E(L_t)=L_\infty\left(1-e^{-log(2)\frac{t-t_0}{t_{50}-t_0}}\right)`$ |
| 6 | Ogle-Isermann | $`E(L_t)=L_r + (L_\infty-L_r)~e^{-e^{-K(t-t_r)}}`$ |
| 7 | Schnute | $`E(L_t)=L_1+(L_3-L_1)\frac{1-e^{-K(t-t_1)}}{1-e^{-K(t_3-t_1)}}`$ |
| 8 | Francis | $`E(L_t)=L_1+(L_3-L_1)\frac{1-r^{2\frac{t-t_1}{t_3-t_1}}}{1-r^2}`$ where $`r=\frac{L_3-L_2}{L_2-L_1}`$ |
| 9 | Double | $`E(L_t)=L_\infty\frac{\left(1-e^{-K_2(t-t_0)}\right)\left(1+e^{-b(t-t_0-a)}\right)}{\left(1+e^{ab}\right)^{-\frac{K_2K_1}{b}}}`$ |

Table 1: Parameterizations of the von Bertalanffy growth equation for
length-at-age (annual) data available in `FSA`. Synonyms are
“Beverton-Holt” for “Traditional”, “von Bertalanffy” for “Original”,
“Ogle” for “Ogle-Isermann”, and “Laslett” or “Polacheck” for “Double”.

  

Parameters in these models are:

- $`L_\infty`$ = asymptotic mean length
- $`K`$ = exponential rate of approach to $`L_\infty`$
- $`t_0`$ = nuisance parameter that is the hypothetical time/age when
  mean length is 0
- $`L_0`$ = mean length at age-0 (i.e., hatching or birth)
- $`\omega`$ = growth rate near $`t_0`$
- $`t_{50}`$ = age when half of $`L_\infty`$ is reached
- $`t_r`$ = mean age at $`L_r`$ (*sometimes this is a constant*)
- $`L_r`$ = mean length at $`t_r`$ (*sometimes this is a constant*)
- $`L_1`$ = mean length at $`t_1`$ (generally a younger age)
- $`L_2`$ = mean length at $`t_2`$ (generally an intermediate age)
- $`L_3`$ = mean length at $`t_3`$ (generally a older age)

Constant values (i.e., set by the user) are:

- $`t_r`$ = mean age at $`L_r`$ (*sometimes this is a parameter*)
- $`L_r`$ = mean length at $`t_r`$ (*sometimes this is a parameter*)
- $`t_1`$ = a younger (generally) age
- $`t_2`$ = an age halfway between $`t_1`$ and $`t_2`$
- $`t_3`$ = an older (generally) age

  

### Seasonal Length-at-Age Functions

The von Bertalanffy parameterizations for length and seasonal age data
are in [Table 2](#tbl-VBparams2).

  

| param | pname | Equation |
|:--:|:--:|:---|
| 10 | Somers | $`E(L_t)=L_\infty\left(1-e^{-K(t-t_0)-S(t)+S(t_0)}\right)`$ where $`S(t)=\frac{CK}{2\pi \text{sin}(2\pi(t-t_s))}`$ |
| 11 | Somers2 | $`E(L_t)=L_\infty\left(1-e^{-K(t-t_0)-R(t)+R(t_0)}\right)`$ where $`R(t)=\frac{CK}{2\pi \text{sin}(2\pi(t-WP+0.5))}`$ |
| 12 | Pauly | $`E(L_t)=L_\infty\left(1-e^{-K'(t'-t_0)-V(t')+V(t_0)}\right)`$ where $`V(t)=\frac{K'(1-NGT)}{2\pi}\text{sin}\left(\frac{2\pi}{(1-NGT)(t-t_s)}\right)`$ |

Table 2: Parameterizations of the von Bertalanffy growth equation for
length-at-age (seasonal) data available in `FSA`. Synonyms are “Somers1”
for “Somers”.

  

New parameters in these growth functions are:[^1]

- $`C`$ = proportional growth depression at “winter peak”
- $`t_s`$ = time from $`t=0`$ until first growth oscillation begins
- $`WP`$ = “winter peak” (point of slowest growth)
- $`K'`$ = exponential rate of approach to $`L_\infty`$ during the
  growth period
- $`NGT`$ = length of “no-growth period”

  

### Tag-Recapture Functions

The von Bertalanffy parameterizations for use with tag-recapture data
are in [Table 3](#tbl-VBparams3). Note that the response variable is
generally the change in length (i.e., growth increment) from time of
marking (i.e., tagging) to time of recapture, $`L_r-L_m`$. Some models
are parameterized to have $`L_m`$ on the right-hand-side though. The
explanatory variable is the change in time between the time of marking
and recapture, $`\delta t`$.

  

| param | pname | Equation |
|:--:|:--:|:---|
| 13 | Fabens | $`E(L_r-L_m)=(L_\infty-L_m)\left(1-e^{-K\delta t}\right)`$ |
| 14 | Fabens2 | $`E(L_r)=L_m + (L_\infty-L_m)\left(1-e^{-K\delta t}\right)`$ |
| 15 | Wang | $`E(L_r-L_m)=(L_\infty+\beta(\bar{L}_m-L_m)-L_m)\left(1-e^{-K\delta t}\right)`$ |
| 16 | Wang2 | $`E(L_r-L_m)=(\alpha+\beta L_m\left(1-e^{-K\delta t}\right)`$ |
| 17 | Wang3 | $`E(L_r)=L_m+(\alpha+\beta L_m\left(1-e^{-K\delta t}\right)`$ |
| 18 | Francis2 | $`E(L_r-L_m)=\left[\frac{L_2g_1-L_1g_2}{g_1-g_2}-L_m\right]\left[1-\left(1+\frac{g_1-g_2}{L_1+L_2}\right)^{dt}\right]`$ |

Table 3: Parameterizations of the von Bertalanffy growth equation for
tag-recaputre data available in `FSA`. Synonyms are “Fabens1” for
“Fabens” and “Wang1” for “Wang”.

  

New parameters in these growth functions are:

- $`\beta`$ = a measure of individual fish variability
- $`\alpha`$ = a nuisance parameter related to $`L_\infty`$ and an
  individual’s $`L_m`$
- $`g_1`$ = mean annual growth rate at the (relatively small) reference
  length $`L_1`$
- $`g_2`$ = mean annual growth rate at the (relatively large) reference
  length $`L_2`$

  

### Seasonal Tag-Recapture Functions

One von Bertalanffy parameterization for seasonal tag recapture data is
in [Table 4](#tbl-VBparams4).

  

| param | pname | Equation |
|:--:|:--:|:---|
| 19 | Francis3 | $`E(L_r-L_m)=\left[\frac{L_2g_1-L_1g_2}{g_1-g_2}-L_m\right]\left[1-\left(1+\frac{g_1-g_2}{L_1+L_2}\right)^{t_2-t_1+S(t_2)-S(t_1)}\right]`$ where $`S(t)=u\text{sin}\left(\frac{2\pi(t-w)}{2\pi}\right)`$ |

Table 4: Parameterizations of the von Bertalanffy growth equation for
seasonal tag-recapture data available in `FSA`.

  

New parameters in this growth function are:

- $`u`$ = “the extent of seasonality” ($`u`$=0 is no seasonality)
- $`w`$ = time of year for maximum growth rate

  

## Gompertz

### Annual Length-at-Age Functions

Gompertz parameterizations for length and annual age data are in
[Table 5](#tbl-Gompparams1).

  

| param |     pname     | Equation                                              |
|:-----:|:-------------:|:------------------------------------------------------|
|   1   |   Original    | $`E(L_t)=L_\infty e^{-e^{a_1-g_it}}`$                 |
|   2   |    Ricker1    | $`E(L_t)=L_\infty e^{-e^{-g_i(t-t_i)}}`$              |
|   3   |    Ricker2    | $`E(L_t)=L_0 e^{a_2(1-e^{-g_it})}`$                   |
|   4   |    Ricker3    | $`E(L_t)=L_\infty e^{-a_2e^{-g_it}}`$                 |
|   5   | Quinn-Deriso3 | $`E(L_t)=L_\infty e^{-\frac{1}{g_i}e^{-g_i(t-t_0)}}`$ |

Table 5: Parameterizations of the Gompertz growth equation for
length-at-age (annual) data available in `FSA`. Synonyms are “Gompertz”
for “Original”, “Quinn-Deriso1” for “Ricker2”, and “Quinn-Deriso2” for
“Ricker3”.

  

Within `FSA` …

- $`L_0`$ = mean length at age 0
- $`L_\infty`$ = mean asymptotic length
- $`t_i`$ = age at the inflection point
- $`g_i`$ = instantaneous growth rate at the inflection point
- $`a_1`$ = nuisance parameter with no real-world interpretation
- $`a_2`$ = nuisance parameter with no real-world interpretation
- $`t_0`$ = nuisance parameter with no real-world interpretation. *The
  use of $`t_0`$ here implies the same meaning at in the von Bertalanffy
  functions. However, the Gompertz function has a horizontal asymptote
  at $`L=0`$ such that there is no “x-intercept.” Thus, $`t_0`$ here
  does not have the same interpretation as for the von Bertalanffy
  functions.*

The parameterizations and parameters for the Gompertz function are
varied and confusing in the literature. To address this confusion the
uniform set of parameters described above are used in `FSA`. However,
this provides some challenges when comparing the equations used in `FSA`
to those used in common literature sources. Thus, some comments to aid
comparisons to the literature are below.

- In the Ricker (1979)\[p. 705\] functions (parameterizations 2-4),
  $`a`$ here is $`k`$ there and $`g_i`$ here is $`g`$ there. Also note
  that their $`w`$ is $`L`$ here.
- In the Ricker (1979) functions (parameterizations 2-4), as presented
  in Campana and Jones (1992), $`a`$ here is $`k`$ there and $`g_i`$
  here is $`G`$ there. Also note that their $`X`$ is $`L`$ here.
- The function in Ricker (1975)\[p. 232\] is the same as the third
  parameterization here where $`a_2`$ here is $`G`$ there and $`g_i`$
  here is $`g`$ there. Also their $`w`$ is $`L`$ here.
- In the Quinn and Deriso (1999) functions (parameterizations 3-5),
  $`a`$ here is $`\frac{\lambda}{K}`$ there and $`g_i`$ here is $`K`$
  there. Also note that their $`Y`$ is $`L`$ here.
- The function in Quist *et al.* (2012)\[p. 714\] is the same as
  parameterization 2 where $`g_i`$ here is $`G`$ there and $`t_i`$ here
  is $`t_0`$ there.
- The function in Katsanevakis and Maravelias (2008) is the same as
  parameterization 2 where $`g_i`$ here is $`k_2`$ there and $`t_i`$
  here is $`t_2`$ there.

  

### Tag-Recapture Functions

The Gompertz parameterizations for tag-recapture data are in
[Table 6](#tbl-Gompparams2).

  

| param | pname | Equation |
|:--:|:--:|:---|
| 6 | Troynikov | $`E(L_r-L_m)=L_{\infty}\left[\frac{L_m}{L_{\infty}}\right]^{e^{-g_i\Delta t}}-L_m`$ |
| 7 | Troynikov2 | $`E(L_r)=L_{\infty}\left[\frac{L_m}{L_{\infty}}\right]^{e^{-g_i\Delta t}}`$ |

Table 6: Parameterizations of the Gompertz growth equation for
tag-recapture data available in `FSA`. Synonyms are “Troynikov1” for
“Troynikov”.

  

## Logistic

### Annual Length-at-Age Functions

The logistic parameterizations for length and annual age data are in
[Table 7](#tbl-Logisticparams1).

  

| param | pname | Equation |
|:--:|:--:|:---|
| 1 | Campana-Jones1 | $`E(L_t)=\frac{L_\infty}{1+e^{-g_{-\infty}(t-t_i)}}`$ |
| 2 | Campana-Jones2 | $`E(L_t)=\frac{L_\infty}{1+ae^{-g_{-\infty}t}}`$ |
| 3 | Karkach | $`E(L_t)=\frac{L_0L_\infty}{L_0+(L_\infty - L_0)e^{-g_{-\infty}t}}`$ |

Table 7: Parameterizations of the Logistic growth equation for
length-at-age (annual) data available in `FSA`.

  

New parameters in these growth functions are:

- $`g_{-\infty}`$ = instantaneous growth rate at $`t=-\infty`$
- $`a`$ = nuisance parameter with no real-world interpretation

  

### Tag-Recapture Functions

The logistic parameterizations for tag-recapture data are in
[Table 8](#tbl-Logisticparams2).

  

| param | pname | Equation |
|:--:|:--:|:---|
| 4 | Haddon | $`E(L_r-L_m)=\frac{\Delta L_{max}}{1+e^{log(19)\frac{L_m-L_{50}}{L_{95}-L_{50}}}}`$ |

Table 8: Parameterizations of the Logistic growth equation for
tag-recapture data available in `FSA`.

  

New parameters in these growth functions are:

- $`\Delta L_{max}`$ = maximum growth increment over the duration of
  observation
- $`L_{50}`$ = length-at-marking that produce a growth increment of 50%
  of $`\Delta L_{max}`$
- $`L_{95}`$ = length-at-marking that produce a growth increment of 95%
  of $`\Delta L_{max}`$

  

## Richards (Annual Length-at-Age)

The Richards parameterizations for length and annual age data are in
[Table 9](#tbl-Richparams1).

  

| param | pname | Equation |
|:--:|:--:|:---|
| 1 | Tjorve4 | $`E(L_t)=L_\infty\left[1-\frac{1}{b}e^{-k(t-t_i)}\right]^b`$ |
| 2 | Tjorve3 | $`E(L_t)=L_\infty\left(1+e^{-k(t-t_0)}\right)^b`$ |
| 3 | Tjorve7 | $`E(L_t)=L_\infty\left[1+\left(\left(\frac{L_0}{L_\infty}\right)^{\frac{1}{b}}-1\right)e^{-kt}\right]^b`$ |

Table 9: Parameterizations of the Richards growth equation for
length-at-age (annual) data available in `FSA`.

  

New parameters in these growth functions are:

- $`k`$ = slope at the inflection point; i.e., maximum relative growth
  rate
- $`b`$ = a nuisance parameter that controls the vertical position of
  the inflection point

  

Only 4-parameter parameterizations from Tjorve and Tjorve (2010) that
seemed useful for modeling fish growth are provided here. In Tjorve and
Tjorve (2010) their $`A`$, $`k`$, $`W_0`$, $`T_i`$,and $`d`$ are
$`L_\infty`$, $`k`$, $`L_0`$, $`t_i`$, and $`b`$, respectively, in
`FSA`. The number at the end of respective `pname` corresponds to the
equation number in Tjorve and Tjorve (2010). However, note that I
modified $`b`$ in parameterizations 2 and 3 so that each equation
appeared as $`L_\infty`$ times a quantity raised to a simple (i.e.,
non-negative and not a fraction) power. Further note that previous
versions of `FSA` had two other parameterizations of the Richards
function that differed only from parameterization 1 by simple additions
or multiplications of $`b`$. As $`b`$ has no biological meaning, these
parameterizations were removed from `FSA`.

  

## Schnute (Annual Length-at-Age)

The four cases for the Schnute model for simple length and annual age
data are in [Table 10](#tbl-Schnute1).

  

| param | case | Equation |
|:--:|:--:|:---|
| 1 | $`a\neq 0`$, $`b\neq 0`$ | $`E(L_t)=\left[L^b_1+(L^b_3-L^b_1)\frac{1-e^{-a(t-t_1)}}{1-e^{-a(t_3-t_1)}}\right]^{\frac{1}{b}}`$ |
| 2 | $`a\neq 0`$, $`b=0`$ | $`E(L_t)=L_1e^{log\left(\frac{L_3}{L_1}\right)\frac{1-e^{-a(t-t_1)}}{1-e^{-a(t_3-t_1)}}}`$ |
| 3 | $`a=0`$, $`b\neq 0`$ | $`E(L_t)=\left[L^b_1+(L^b_3-L^b_1)\frac{t-t_1}{t_3-t_1}\right]^{\frac{1}{b}}`$ |
| 4 | $`a=0`$, $`b=0`$ | $`E(L_t)=L_1e^{log\left(\frac{L_3}{L_1}\right)\frac{t-t_1}{t_3-t_1}}`$ |

Table 10: Cases of the Schnute growth equation for length-at-age
(annual) data available in `FSA`.

  

## Schnute-Richards (Annual Length-at-Age)

The Schnute-Richards model for simple length and annual age data is
$`E(L_t)=L_\infty\left(1-ae^{-kt^c}\right)^{1/b}`$. Note that this
function is slightly modified (a $`+`$ was changed to a $`-`$ so that
$`a`$ is positive) from the original in Schnute and Richards (1990).

[^1]: One parameterization uses a modified time scale, symbolized with
    $`t'`$.
