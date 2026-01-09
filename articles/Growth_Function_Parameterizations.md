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
$L$, is length and the explanatory variable, $t$ is age, and
$E\left( L_{t} \right)$ is the “expected length at age $t$” or the mean
length at age $t$.

  

| param |     pname      | Equation                                                                                                                                                                           |
|:-----:|:--------------:|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|   1   |  Traditional   | $E\left( L_{t} \right) = L_{\infty}\left( 1 - e^{-K{(t - t_{0})}} \right)$                                                                                                         |
|   2   |    Original    | $E\left( L_{t} \right) = L_{\infty} - \left( L_{\infty} - L_{0} \right)\ e^{-Kt}$                                                                                                  |
|   3   | Gallucci-Quinn | $E\left( L_{t} \right) = \frac{\omega}{K}\left( 1 - e^{-K{(t - t_{0})}} \right)$                                                                                                   |
|   4   |     Mooij      | $E\left( L_{t} \right) = L_{\infty} - \left( L_{\infty} - L_{0} \right)\ e^{-\frac{\omega}{L_{\infty}}t}$                                                                          |
|   5   |    Weisberg    | $E\left( L_{t} \right) = L_{\infty}\left( 1 - e^{-log{(2)}\frac{t - t_{0}}{t_{50} - t_{0}}} \right)$                                                                               |
|   6   | Ogle-Isermann  | $E\left( L_{t} \right) = L_{r} + \left( L_{\infty} - L_{r} \right)\ e^{-e^{-K{(t - t_{r})}}}$                                                                                      |
|   7   |    Schnute     | $E\left( L_{t} \right) = L_{1} + \left( L_{3} - L_{1} \right)\frac{1 - e^{-K{(t - t_{1})}}}{1 - e^{-K{(t_{3} - t_{1})}}}$                                                          |
|   8   |    Francis     | $E\left( L_{t} \right) = L_{1} + \left( L_{3} - L_{1} \right)\frac{1 - r^{2\frac{t - t_{1}}{t_{3} - t_{1}}}}{1 - r^{2}}$ where $r = \frac{L_{3} - L_{2}}{L_{2} - L_{1}}$           |
|   9   |     Double     | $E\left( L_{t} \right) = L_{\infty}\frac{\left( 1 - e^{-K_{2}{(t - t_{0})}} \right)\left( 1 + e^{-b{(t - t_{0} - a)}} \right)}{\left( 1 + e^{ab} \right)^{-\frac{K_{2}K_{1}}{b}}}$ |

Table 1: Parameterizations of the von Bertalanffy growth equation for
length-at-age (annual) data available in `FSA`. Synonyms are
“Beverton-Holt” for “Traditional”, “von Bertalanffy” for “Original”,
“Ogle” for “Ogle-Isermann”, and “Laslett” or “Polacheck” for “Double”.

  

Parameters in these models are:

- $L_{\infty}$ = asymptotic mean length
- $K$ = exponential rate of approach to $L_{\infty}$
- $t_{0}$ = nuisance parameter that is the hypothetical time/age when
  mean length is 0
- $L_{0}$ = mean length at age-0 (i.e., hatching or birth)
- $\omega$ = growth rate near $t_{0}$
- $t_{50}$ = age when half of $L_{\infty}$ is reached
- $t_{r}$ = mean age at $L_{r}$ (*sometimes this is a constant*)
- $L_{r}$ = mean length at $t_{r}$ (*sometimes this is a constant*)
- $L_{1}$ = mean length at $t_{1}$ (generally a younger age)
- $L_{2}$ = mean length at $t_{2}$ (generally an intermediate age)
- $L_{3}$ = mean length at $t_{3}$ (generally a older age)

Constant values (i.e., set by the user) are:

- $t_{r}$ = mean age at $L_{r}$ (*sometimes this is a parameter*)
- $L_{r}$ = mean length at $t_{r}$ (*sometimes this is a parameter*)
- $t_{1}$ = a younger (generally) age
- $t_{2}$ = an age halfway between $t_{1}$ and $t_{2}$
- $t_{3}$ = an older (generally) age

  

### Seasonal Length-at-Age Functions

The von Bertalanffy parameterizations for length and seasonal age data
are in [Table 2](#tbl-VBparams2).

  

| param |  pname  | Equation                                                                                                                                                                                                                               |
|:-----:|:-------:|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|  10   | Somers  | $E\left( L_{t} \right) = L_{\infty}\left( 1 - e^{-K{(t - t_{0})} - S{(t)} + S{(t_{0})}} \right)$ where $S(t) = \frac{CK}{2\pi\text{sin}\left( 2\pi\left( t - t_{s} \right) \right)}$                                                   |
|  11   | Somers2 | $E\left( L_{t} \right) = L_{\infty}\left( 1 - e^{-K{(t - t_{0})} - R{(t)} + R{(t_{0})}} \right)$ where $R(t) = \frac{CK}{2\pi\text{sin}\left( 2\pi(t - WP + 0.5) \right)}$                                                             |
|  12   |  Pauly  | $E\left( L_{t} \right) = L_{\infty}\left( 1 - e^{-K\prime{(t\prime - t_{0})} - V{(t\prime)} + V{(t_{0})}} \right)$ where $V(t) = \frac{K\prime(1 - NGT)}{2\pi}\text{sin}\left( \frac{2\pi}{(1 - NGT)\left( t - t_{s} \right)} \right)$ |

Table 2: Parameterizations of the von Bertalanffy growth equation for
length-at-age (seasonal) data available in `FSA`. Synonyms are “Somers1”
for “Somers”.

  

New parameters in these growth functions are:[¹](#fn1)

- $C$ = proportional growth depression at “winter peak”
- $t_{s}$ = time from $t = 0$ until first growth oscillation begins
- $WP$ = “winter peak” (point of slowest growth)
- $K\prime$ = exponential rate of approach to $L_{\infty}$ during the
  growth period
- $NGT$ = length of “no-growth period”

  

### Tag-Recapture Functions

The von Bertalanffy parameterizations for use with tag-recapture data
are in [Table 3](#tbl-VBparams3). Note that the response variable is
generally the change in length (i.e., growth increment) from time of
marking (i.e., tagging) to time of recapture, $L_{r} - L_{m}$. Some
models are parameterized to have $L_{m}$ on the right-hand-side though.
The explanatory variable is the change in time between the time of
marking and recapture, $\delta t$.

  

| param |  pname   | Equation                                                                                                                                                                                                     |
|:-----:|:--------:|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|  13   |  Fabens  | $E\left( L_{r} - L_{m} \right) = \left( L_{\infty} - L_{m} \right)\left( 1 - e^{-K\delta t} \right)$                                                                                                         |
|  14   | Fabens2  | $E\left( L_{r} \right) = L_{m} + \left( L_{\infty} - L_{m} \right)\left( 1 - e^{-K\delta t} \right)$                                                                                                         |
|  15   |   Wang   | $E\left( L_{r} - L_{m} \right) = \left( L_{\infty} + \beta\left( {\bar{L}}_{m} - L_{m} \right) - L_{m} \right)\left( 1 - e^{-K\delta t} \right)$                                                             |
|  16   |  Wang2   | $E\left( L_{r} - L_{m} \right) = (\alpha + \beta L_{m}\left( 1 - e^{-K\delta t} \right)$                                                                                                                     |
|  17   |  Wang3   | $E\left( L_{r} \right) = L_{m} + (\alpha + \beta L_{m}\left( 1 - e^{-K\delta t} \right)$                                                                                                                     |
|  18   | Francis2 | $E\left( L_{r} - L_{m} \right) = \left\lbrack \frac{L_{2}g_{1} - L_{1}g_{2}}{g_{1} - g_{2}} - L_{m} \right\rbrack\left\lbrack 1 - \left( 1 + \frac{g_{1} - g_{2}}{L_{1} + L_{2}} \right)^{dt} \right\rbrack$ |

Table 3: Parameterizations of the von Bertalanffy growth equation for
tag-recaputre data available in `FSA`. Synonyms are “Fabens1” for
“Fabens” and “Wang1” for “Wang”.

  

New parameters in these growth functions are:

- $\beta$ = a measure of individual fish variability
- $\alpha$ = a nuisance parameter related to $L_{\infty}$ and an
  individual’s $L_{m}$
- $g_{1}$ = mean annual growth rate at the (relatively small) reference
  length $L_{1}$
- $g_{2}$ = mean annual growth rate at the (relatively large) reference
  length $L_{2}$

  

### Seasonal Tag-Recapture Functions

One von Bertalanffy parameterization for seasonal tag recapture data is
in [Table 4](#tbl-VBparams4).

  

| param |  pname   | Equation                                                                                                                                                                                                                                                                                                            |
|:-----:|:--------:|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|  19   | Francis3 | $E\left( L_{r} - L_{m} \right) = \left\lbrack \frac{L_{2}g_{1} - L_{1}g_{2}}{g_{1} - g_{2}} - L_{m} \right\rbrack\left\lbrack 1 - \left( 1 + \frac{g_{1} - g_{2}}{L_{1} + L_{2}} \right)^{t_{2} - t_{1} + S{(t_{2})} - S{(t_{1})}} \right\rbrack$ where $S(t) = u\text{sin}\left( \frac{2\pi(t - w)}{2\pi} \right)$ |

Table 4: Parameterizations of the von Bertalanffy growth equation for
seasonal tag-recapture data available in `FSA`.

  

New parameters in this growth function are:

- $u$ = “the extent of seasonality” ($u$=0 is no seasonality)
- $w$ = time of year for maximum growth rate

  

## Gompertz

### Annual Length-at-Age Functions

Gompertz parameterizations for length and annual age data are in
[Table 5](#tbl-Gompparams1).

  

| param |     pname     | Equation                                                                        |
|:-----:|:-------------:|:--------------------------------------------------------------------------------|
|   1   |   Original    | $E\left( L_{t} \right) = L_{\infty}e^{-e^{a_{1} - g_{i}t}}$                     |
|   2   |    Ricker1    | $E\left( L_{t} \right) = L_{\infty}e^{-e^{-g_{i}{(t - t_{i})}}}$                |
|   3   |    Ricker2    | $E\left( L_{t} \right) = L_{0}e^{a_{2}{(1 - e^{-g_{i}t})}}$                     |
|   4   |    Ricker3    | $E\left( L_{t} \right) = L_{\infty}e^{-a_{2}e^{-g_{i}t}}$                       |
|   5   | Quinn-Deriso3 | $E\left( L_{t} \right) = L_{\infty}e^{-\frac{1}{g_{i}}e^{-g_{i}{(t - t_{0})}}}$ |

Table 5: Parameterizations of the Gompertz growth equation for
length-at-age (annual) data available in `FSA`. Synonyms are “Gompertz”
for “Original”, “Quinn-Deriso1” for “Ricker2”, and “Quinn-Deriso2” for
“Ricker3”.

  

Within `FSA` …

- $L_{0}$ = mean length at age 0
- $L_{\infty}$ = mean asymptotic length
- $t_{i}$ = age at the inflection point
- $g_{i}$ = instantaneous growth rate at the inflection point
- $a_{1}$ = nuisance parameter with no real-world interpretation
- $a_{2}$ = nuisance parameter with no real-world interpretation
- $t_{0}$ = nuisance parameter with no real-world interpretation. *The
  use of $t_{0}$ here implies the same meaning at in the von Bertalanffy
  functions. However, the Gompertz function has a horizontal asymptote
  at $L = 0$ such that there is no “x-intercept.” Thus, $t_{0}$ here
  does not have the same interpretation as for the von Bertalanffy
  functions.*

The parameterizations and parameters for the Gompertz function are
varied and confusing in the literature. To address this confusion the
uniform set of parameters described above are used in `FSA`. However,
this provides some challenges when comparing the equations used in `FSA`
to those used in common literature sources. Thus, some comments to aid
comparisons to the literature are below.

- In the Ricker (1979)\[p. 705\] functions (parameterizations 2-4), $a$
  here is $k$ there and $g_{i}$ here is $g$ there. Also note that their
  $w$ is $L$ here.
- In the Ricker (1979) functions (parameterizations 2-4), as presented
  in Campana and Jones (1992), $a$ here is $k$ there and $g_{i}$ here is
  $G$ there. Also note that their $X$ is $L$ here.
- The function in Ricker (1975)\[p. 232\] is the same as the third
  parameterization here where $a_{2}$ here is $G$ there and $g_{i}$ here
  is $g$ there. Also their $w$ is $L$ here.
- In the Quinn and Deriso (1999) functions (parameterizations 3-5), $a$
  here is $\frac{\lambda}{K}$ there and $g_{i}$ here is $K$ there. Also
  note that their $Y$ is $L$ here.
- The function in Quist *et al.* (2012)\[p. 714\] is the same as
  parameterization 2 where $g_{i}$ here is $G$ there and $t_{i}$ here is
  $t_{0}$ there.
- The function in Katsanevakis and Maravelias (2008) is the same as
  parameterization 2 where $g_{i}$ here is $k_{2}$ there and $t_{i}$
  here is $t_{2}$ there.

  

### Tag-Recapture Functions

The Gompertz parameterizations for tag-recapture data are in
[Table 6](#tbl-Gompparams2).

  

| param |   pname    | Equation                                                                                                                     |
|:-----:|:----------:|:-----------------------------------------------------------------------------------------------------------------------------|
|   6   | Troynikov  | $E\left( L_{r} - L_{m} \right) = L_{\infty}\left\lbrack \frac{L_{m}}{L_{\infty}} \right\rbrack^{e^{-g_{i}\Delta t}} - L_{m}$ |
|   7   | Troynikov2 | $E\left( L_{r} \right) = L_{\infty}\left\lbrack \frac{L_{m}}{L_{\infty}} \right\rbrack^{e^{-g_{i}\Delta t}}$                 |

Table 6: Parameterizations of the Gompertz growth equation for
tag-recapture data available in `FSA`. Synonyms are “Troynikov1” for
“Troynikov”.

  

## Logistic

### Annual Length-at-Age Functions

The logistic parameterizations for length and annual age data are in
[Table 7](#tbl-Logisticparams1).

  

| param |     pname      | Equation                                                                                                     |
|:-----:|:--------------:|:-------------------------------------------------------------------------------------------------------------|
|   1   | Campana-Jones1 | $E\left( L_{t} \right) = \frac{L_{\infty}}{1 + e^{-g_{-\infty}{(t - t_{i})}}}$                               |
|   2   | Campana-Jones2 | $E\left( L_{t} \right) = \frac{L_{\infty}}{1 + ae^{-g_{-\infty}t}}$                                          |
|   3   |    Karkach     | $E\left( L_{t} \right) = \frac{L_{0}L_{\infty}}{L_{0} + \left( L_{\infty} - L_{0} \right)e^{-g_{-\infty}t}}$ |

Table 7: Parameterizations of the Logistic growth equation for
length-at-age (annual) data available in `FSA`.

  

New parameters in these growth functions are:

- $g_{-\infty}$ = instantaneous growth rate at $t = -\infty$
- $a$ = nuisance parameter with no real-world interpretation

  

### Tag-Recapture Functions

The logistic parameterizations for tag-recapture data are in
[Table 8](#tbl-Logisticparams2).

  

| param | pname  | Equation                                                                                                         |
|:-----:|:------:|:-----------------------------------------------------------------------------------------------------------------|
|   4   | Haddon | $E\left( L_{r} - L_{m} \right) = \frac{\Delta L_{max}}{1 + e^{log{(19)}\frac{L_{m} - L_{50}}{L_{95} - L_{50}}}}$ |

Table 8: Parameterizations of the Logistic growth equation for
tag-recapture data available in `FSA`.

  

New parameters in these growth functions are:

- $\Delta L_{max}$ = maximum growth increment over the duration of
  observation
- $L_{50}$ = length-at-marking that produce a growth increment of 50% of
  $\Delta L_{max}$
- $L_{95}$ = length-at-marking that produce a growth increment of 95% of
  $\Delta L_{max}$

  

## Richards (Annual Length-at-Age)

The Richards parameterizations for length and annual age data are in
[Table 9](#tbl-Richparams1).

  

| param |  pname  | Equation                                                                                                                                               |
|:-----:|:-------:|:-------------------------------------------------------------------------------------------------------------------------------------------------------|
|   1   | Tjorve4 | $E\left( L_{t} \right) = L_{\infty}\left\lbrack 1 - \frac{1}{b}e^{-k{(t - t_{i})}} \right\rbrack^{b}$                                                  |
|   2   | Tjorve3 | $E\left( L_{t} \right) = L_{\infty}\left( 1 + e^{-k{(t - t_{0})}} \right)^{b}$                                                                         |
|   3   | Tjorve7 | $E\left( L_{t} \right) = L_{\infty}\left\lbrack 1 + \left( \left( \frac{L_{0}}{L_{\infty}} \right)^{\frac{1}{b}} - 1 \right)e^{-kt} \right\rbrack^{b}$ |

Table 9: Parameterizations of the Richards growth equation for
length-at-age (annual) data available in `FSA`.

  

New parameters in these growth functions are:

- $k$ = slope at the inflection point; i.e., maximum relative growth
  rate
- $b$ = a nuisance parameter that controls the vertical position of the
  inflection point

  

Only 4-parameter parameterizations from Tjorve and Tjorve (2010) that
seemed useful for modeling fish growth are provided here. In Tjorve and
Tjorve (2010) their $A$, $k$, $W_{0}$, $T_{i}$,and $d$ are $L_{\infty}$,
$k$, $L_{0}$, $t_{i}$, and $b$, respectively, in `FSA`. The number at
the end of respective `pname` corresponds to the equation number in
Tjorve and Tjorve (2010). However, note that I modified $b$ in
parameterizations 2 and 3 so that each equation appeared as $L_{\infty}$
times a quantity raised to a simple (i.e., non-negative and not a
fraction) power. Further note that previous versions of `FSA` had two
other parameterizations of the Richards function that differed only from
parameterization 1 by simple additions or multiplications of $b$. As $b$
has no biological meaning, these parameterizations were removed from
`FSA`.

  

## Schnute (Annual Length-at-Age)

The four cases for the Schnute model for simple length and annual age
data are in [Table 10](#tbl-Schnute1).

  

| param |          case          | Equation                                                                                                                                                                       |
|:-----:|:----------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|   1   | $a \neq 0$, $b \neq 0$ | $E\left( L_{t} \right) = \left\lbrack L_{1}^{b} + \left( L_{3}^{b} - L_{1}^{b} \right)\frac{1 - e^{-a{(t - t_{1})}}}{1 - e^{-a{(t_{3} - t_{1})}}} \right\rbrack^{\frac{1}{b}}$ |
|   2   |  $a \neq 0$, $b = 0$   | $E\left( L_{t} \right) = L_{1}e^{log{(\frac{L_{3}}{L_{1}})}\frac{1 - e^{-a{(t - t_{1})}}}{1 - e^{-a{(t_{3} - t_{1})}}}}$                                                       |
|   3   |  $a = 0$, $b \neq 0$   | $E\left( L_{t} \right) = \left\lbrack L_{1}^{b} + \left( L_{3}^{b} - L_{1}^{b} \right)\frac{t - t_{1}}{t_{3} - t_{1}} \right\rbrack^{\frac{1}{b}}$                             |
|   4   |    $a = 0$, $b = 0$    | $E\left( L_{t} \right) = L_{1}e^{log{(\frac{L_{3}}{L_{1}})}\frac{t - t_{1}}{t_{3} - t_{1}}}$                                                                                   |

Table 10: Cases of the Schnute growth equation for length-at-age
(annual) data available in `FSA`.

  

## Schnute-Richards (Annual Length-at-Age)

The Schnute-Richards model for simple length and annual age data is
$E\left( L_{t} \right) = L_{\infty}\left( 1 - ae^{-kt^{c}} \right)^{1/b}$.
Note that this function is slightly modified (a $+$ was changed to a $-$
so that $a$ is positive) from the original in Schnute and Richards
(1990).

------------------------------------------------------------------------

1.  One parameterization uses a modified time scale, symbolized with
    $t\prime$.
