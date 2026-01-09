# Computing PSD and Relative Weight Metrics in FSA

## Introduction

Other vignettes introduced using `FSA` to compute [proportional size
distribution
(PSD)](https://fishr-core-team.github.io/FSA/articles/Computing_PSDs.html)
and [relative weight (i.e.,
condition)](https://fishr-core-team.github.io/FSA/articles/Computing_Relative_Weights.html)
metrics. Both of those vignettes mentioned some peculiarities in their
methodology related to when PSDs and relative weights were computed
concurrently on the same data that contained some “issues.” These
peculiarities and issues are explained further and how to handle them
are illustrated here. I urge you to explore those other vignettes before
continuing here.

The following packages are used herein. Note that the `FSA` functions
described here were modified after version 0.9.6 and are thus **specific
to FSA \>v0.9.6**.

``` r
library(FSA)
library(dplyr)  # mutate, select, filter, case_when
```

## Data

The hypothetical `PSDWRtest` data.frame distributed with `FSA` was
created to purposely provide data for a variety of species that were
realistic but created some issues for the
[`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md)
and
[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)
functions described in the other vignettes.

``` r
peek(PSDWRtest,n=20)
#>               species     location  len      wt  sex
#> 1    Bluegill Sunfish    Bass Lake  107    25.8 <NA>
#> 53   Bluegill Sunfish    Bass Lake  116    34.8 <NA>
#> 107  Bluegill Sunfish    Bass Lake  191   138.3 <NA>
#> 160       Brook Trout   Trout Lake  291      NA <NA>
#> 214       Brown Trout   Trout Lake  151    45.4 <NA>
#> 267       Brown Trout   Trout Lake  190    86.3 <NA>
#> 321       Brown Trout Brushy Creek  318   198.4    M
#> 374       Brown Trout Brushy Creek  446   533.4    F
#> 428   Largemouth Bass    Bass Lake  199    70.1 <NA>
#> 481   Largemouth Bass    Bass Lake  306   311.9 <NA>
#> 535   Lean Lake Trout   Trout Lake  529  1480.0    F
#> 588   Lean Lake Trout   Trout Lake  809  5448.1    F
#> 642       Muskellunge    Long Lake 1097 11376.4    U
#> 695           Walleye    Bass Lake   72     3.5 <NA>
#> 749           Walleye    Bass Lake  307   273.6    M
#> 802           Walleye    Bass Lake  345   429.8    F
#> 856      Yellow Perch    Bass Lake  165    59.4    F
#> 909      Yellow Perch    Bass Lake  150    40.2    F
#> 963      Yellow Perch    Bass Lake  241   187.9    F
#> 1016     Yellow Perch    Bass Lake  322   520.0    F
```

Ultimately it is best that you have a full understanding of the issues
that may arise with your data by carefully examining your data and
understanding the Gabelhouse (GH) length categories and relative weight
equations for the species in your data. There are several “issues” that
need to be addressed with the `PSDWRtest` data.

First, Bluegill and Lake Trout exist in `PSDlit` and `WSlit` but appear
as “Bluegill Sunfish” and “Lean Lake Trout” in `PSDWRtest`.

Second, Brook Trout were sampled from a lotic (“Trout Lake”) system and
Brown Trout were sampled from a lotic (“Trout Lake”) and a lentic
(“Brush Creek”) system. There are separate GH length categories and
standard weight equations for these sub-groups for each species.

Third, Muskellunge and Walleye provide a challenge because standard
weight equations differ among sub-groups for each of these species, but
the GH length categories do not differ among sub-groups. Walleye use
separate standard weight equations depending on whether the individual’s
total length is less than 150 mm or not. Muskellunge have a sex-specific
standard weight equation for when sex is known, but an overall equation
for when sex is unknown. As will be seen, a new species variable will be
created that incorporates the subgroup name into the “species name” for
calculating standard weights (as was illustrated in [this
vignette](https://fishr-core-team.github.io/FSA/articles/Computing_Relative_Weights.html)).
Because the GH length categories don’t differ by sub-group for either of
these species, these new names are not needed when finding the GH length
categories. However, for simplicity (as illustrated further below),
these GH length categories have been duplicated for the sub-groups of
each species and labelled with the combined species and sub-group name.
This is illustrated below for muskellunge.

``` r
wsVal("Muskellunge (overall)")  # different from next two
#>                   species measure  units ref method min.TL    int slope
#> 138 Muskellunge (overall)      TL metric  75    RLP    380 -6.066 3.325
#>                        source
#> 138 Neumann and Willis (1994)
wsVal("Muskellunge (female)")
#>                  species measure  units ref method min.TL    int slope
#> 134 Muskellunge (female)      TL metric  75    RLP    380 -6.105  3.34
#>                        source
#> 134 Neumann and Willis (1994)
wsVal("Muskellunge (male)")
#>                species measure  units ref method min.TL    int slope
#> 136 Muskellunge (male)      TL metric  75    RLP    380 -5.823 3.245
#>                        source
#> 136 Neumann and Willis (1994)
psdVal("Muskellunge (overall)") # exact same as next two
#>  substock     stock   quality preferred memorable    trophy 
#>         0       510       760       970      1070      1270
psdVal("Muskellunge (female)")
#>  substock     stock   quality preferred memorable    trophy 
#>         0       510       760       970      1070      1270
psdVal("Muskellunge (male)")
#>  substock     stock   quality preferred memorable    trophy 
#>         0       510       760       970      1070      1270
```

Fourth, Ruffe for which the reference quantile of the standard weight
equation must be specified.

## Adding GH Length Category and Wr Variables

The easiest way to deal with all of these “issues” except for the one
related to Ruffe is to create a new “species” variable (i.e., `species2`
below) that appends the specific sub-groups in parentheses to the
species name. There are a variety of ways to do this and which way (is
best or works) may depend on the specifics of the situation. Here, we
use
[`case_when()`](https://dplyr.tidyverse.org/reference/case_when.html)
from `dplyr` with a series of statements that begin with a “condition”
to the left of the `~` and new species “name” for that condition to the
right of the `~`. The `.default=species` at the end puts the name from
`species` into `species2` for all situations where none of the
conditions above it are met (e.g., if `species` is “Yellow Perch” then
`species2` will be “Yellow Perch”).

``` r
PSDWRtest <- PSDWRtest |>
  mutate(species2=case_when(
           species=="Bluegill Sunfish" ~ "Bluegill",
           species=="Lean Lake Trout" ~ "Lake Trout",
           species=="Brown Trout" & location=="Trout Lake" ~  "Brown Trout (lotic)",
           species=="Brown Trout" & location=="Brushy Creek" ~  "Brown Trout (lentic)",
           species=="Brook Trout" & location=="Trout Lake" ~  "Brook Trout (lotic)",
           species=="Muskellunge" & sex=="M" ~ "Muskellunge (male)",
           species=="Muskellunge" & sex=="F" ~ "Muskellunge (female)",
           species=="Muskellunge" & sex=="U" ~ "Muskellunge (overall)",
           species=="Muskellunge" & is.na(sex) ~ "Muskellunge (overall)",
           species=="Walleye" & len>=150 ~ "Walleye (overall)",
           species=="Walleye" & len<150 ~ "Walleye (30-149 mm)",
           .default=species
         ))
peek(PSDWRtest,n=20)
#>               species     location  len      wt  sex              species2
#> 1    Bluegill Sunfish    Bass Lake  107    25.8 <NA>              Bluegill
#> 53   Bluegill Sunfish    Bass Lake  116    34.8 <NA>              Bluegill
#> 107  Bluegill Sunfish    Bass Lake  191   138.3 <NA>              Bluegill
#> 160       Brook Trout   Trout Lake  291      NA <NA>   Brook Trout (lotic)
#> 214       Brown Trout   Trout Lake  151    45.4 <NA>   Brown Trout (lotic)
#> 267       Brown Trout   Trout Lake  190    86.3 <NA>   Brown Trout (lotic)
#> 321       Brown Trout Brushy Creek  318   198.4    M  Brown Trout (lentic)
#> 374       Brown Trout Brushy Creek  446   533.4    F  Brown Trout (lentic)
#> 428   Largemouth Bass    Bass Lake  199    70.1 <NA>       Largemouth Bass
#> 481   Largemouth Bass    Bass Lake  306   311.9 <NA>       Largemouth Bass
#> 535   Lean Lake Trout   Trout Lake  529  1480.0    F            Lake Trout
#> 588   Lean Lake Trout   Trout Lake  809  5448.1    F            Lake Trout
#> 642       Muskellunge    Long Lake 1097 11376.4    U Muskellunge (overall)
#> 695           Walleye    Bass Lake   72     3.5 <NA>   Walleye (30-149 mm)
#> 749           Walleye    Bass Lake  307   273.6    M     Walleye (overall)
#> 802           Walleye    Bass Lake  345   429.8    F     Walleye (overall)
#> 856      Yellow Perch    Bass Lake  165    59.4    F          Yellow Perch
#> 909      Yellow Perch    Bass Lake  150    40.2    F          Yellow Perch
#> 963      Yellow Perch    Bass Lake  241   187.9    F          Yellow Perch
#> 1016     Yellow Perch    Bass Lake  322   520.0    F          Yellow Perch
```

The GH length categories and relative weights for each fish (for species
with standard weight equations) is added to this data.drame with
[`psdAdd()`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md)
and
[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md) as
described in the other vignettes, specifically noting the use of the
“new” species variable `species2`. Further note the use of `wsOpts=` to
define which reference quantile to use for the Ruffe standard weight
equation.

``` r
PSDWRtest$psd <- psdAdd(len~species2,data=PSDWRtest)
#> Species in the data with no Gabelhouse (PSD) lengths in `PSDlit`: "Iowa
#>   Darter".
PSDWRtest$wr <- wrAdd(wt~len+species2,data=PSDWRtest,
                      WsOpts=list(Ruffe=list(ref=75)))
peek(PSDWRtest,n=20)
#>               species     location  len      wt  sex              species2
#> 1    Bluegill Sunfish    Bass Lake  107    25.8 <NA>              Bluegill
#> 53   Bluegill Sunfish    Bass Lake  116    34.8 <NA>              Bluegill
#> 107  Bluegill Sunfish    Bass Lake  191   138.3 <NA>              Bluegill
#> 160       Brook Trout   Trout Lake  291      NA <NA>   Brook Trout (lotic)
#> 214       Brown Trout   Trout Lake  151    45.4 <NA>   Brown Trout (lotic)
#> 267       Brown Trout   Trout Lake  190    86.3 <NA>   Brown Trout (lotic)
#> 321       Brown Trout Brushy Creek  318   198.4    M  Brown Trout (lentic)
#> 374       Brown Trout Brushy Creek  446   533.4    F  Brown Trout (lentic)
#> 428   Largemouth Bass    Bass Lake  199    70.1 <NA>       Largemouth Bass
#> 481   Largemouth Bass    Bass Lake  306   311.9 <NA>       Largemouth Bass
#> 535   Lean Lake Trout   Trout Lake  529  1480.0    F            Lake Trout
#> 588   Lean Lake Trout   Trout Lake  809  5448.1    F            Lake Trout
#> 642       Muskellunge    Long Lake 1097 11376.4    U Muskellunge (overall)
#> 695           Walleye    Bass Lake   72     3.5 <NA>   Walleye (30-149 mm)
#> 749           Walleye    Bass Lake  307   273.6    M     Walleye (overall)
#> 802           Walleye    Bass Lake  345   429.8    F     Walleye (overall)
#> 856      Yellow Perch    Bass Lake  165    59.4    F          Yellow Perch
#> 909      Yellow Perch    Bass Lake  150    40.2    F          Yellow Perch
#> 963      Yellow Perch    Bass Lake  241   187.9    F          Yellow Perch
#> 1016     Yellow Perch    Bass Lake  322   520.0    F          Yellow Perch
#>            psd        wr
#> 1        stock 113.81070
#> 53       stock 117.44555
#> 107    quality  89.31318
#> 160    quality        NA
#> 214      stock 118.65447
#> 267      stock 114.26156
#> 321    quality  53.30764
#> 374  preferred  48.64940
#> 428   substock  70.72483
#> 481    quality  76.95718
#> 535    quality 102.54739
#> 588  memorable  95.07015
#> 642  memorable 103.11404
#> 695   substock 104.56374
#> 749      stock  95.72295
#> 802      stock 103.75325
#> 856      stock  99.38328
#> 909      stock  91.50636
#> 963    quality  92.47232
#> 1016 memorable 100.37577
```
