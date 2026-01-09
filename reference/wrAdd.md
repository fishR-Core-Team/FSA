# Computes a vector of relative weights specific to a species in an entire data frame.

Returns a vector that contains the relative weight specific to each
species for all individuals in an entire data frame.

## Usage

``` r
wrAdd(wt, ...)

# Default S3 method
wrAdd(
  wt,
  len,
  spec,
  thesaurus = NULL,
  units = c("metric", "English"),
  WsOpts = NULL,
  ...
)

# S3 method for class 'formula'
wrAdd(wt, data, thesaurus = NULL, units = c("metric", "English"), ...)
```

## Arguments

- wt:

  A numeric vector that contains weight measurements or a formula of the
  form `wt~len+spec` where “wt” generically represents the weight
  variable, “len” generically represents the length variable, and “spec”
  generically represents the species variable. Note that this formula
  can only contain three variables and they must be in the order of
  weight first, length second, species third.

- ...:

  Not used.

- len:

  A numeric vector that contains length measurements. Not used if `wt`
  is a formula.

- spec:

  A character or factor vector that contains the species names. Not used
  if `wt` is a formula.

- thesaurus:

  A named list for providing alternative species names (the values in
  the list) that correspond to specific names in `PSDlit` (the names in
  the list). See details and examples.

- units:

  A string that indicates whether the weight and length data in
  `formula` are in `"metric"` (DEFAULT; mm and g) or `"English"` (in and
  lbs) units.

- WsOpts:

  A named list that provides specific choices for `group`, `ref`, or
  `method` for species for which more than one standard weight equation
  exists in
  [`WSlit`](https://fishr-core-team.github.io/FSA/reference/WSlit.md).

- data:

  A data.frame that minimally contains variables of the the observed
  lengths, observed weights, and the species names given in the
  `formula=`.

## Value

A numeric vector that contains the computed relative weights, in the
same order as in `data=`.

## Details

This computes a vector that contains the relative weight specific to
each species for all individuals in an entire data frame. The vector can
be appended to an existing data.frame to create a variable that contains
the relative weights for each individual. The relative weight value will
be `NA` for each individual for which a standard weight equation does
not exist in
[`WSlit`](https://fishr-core-team.github.io/FSA/reference/WSlit.md), a
standard weight equation for the units given in `units=` does not exist
in [`WSlit`](https://fishr-core-team.github.io/FSA/reference/WSlit.md),
or if the individual is shorter or longer than the lengths for which the
standard weight equation should be applied. Either the linear or
quadratic equation has been listed as preferred for each species, so
only that equation will be used.

The species names in `species` must match the spelling and
capitalization of `species` in
[`WSlit`](https://fishr-core-team.github.io/FSA/reference/WSlit.md). Use
[`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md) to
see a list of all species for which standard weight equations exist in
[`WSlit`](https://fishr-core-team.github.io/FSA/reference/WSlit.md) and,
more importantly, how the species names are spelled and capitalized.

The `thesaurus` argument may be used to relate alternate species names
to the species names used in `WSlit`. For example, you (or your data)
may use “Bluegill Sunfish”, but “Bluegill” is used in `WSlit`. The
alternate species name can be used here if it is defined in a named
vector (or list) given to `thesarus=`. The alternate species name is the
value and the species name in `PSDlit` is the name in this vector/list -
e.g., `c("Bluegill"="Bluegill Sunfish")`. See the examples for a
demonstration.

Some species have length categories separated by sub-group. For example,
length categories exist for both lentic and lotic populations of Brown
Trout. The length values for a sub-group may be obtained by either
including the species name in `species` and the sub-group name in
`group` in `WsOpts` or by using the combined species and sub-group name,
with the sub-group name in parentheses, in `species`. Both methods are
demonstrated in the examples. Note that an error is returned if a
species has sub-groups but neither method is used to define the
sub-group.

Some (few) species have more than one equation listed in
[`WSlit`](https://fishr-core-team.github.io/FSA/reference/WSlit.md) (for
the specified units). In these instances the user must select one of the
equations to use with `WsOpts`. `WsOpts` is a list of lists where the
inside list contains one or more of `group`, `ref`, or `method` (see
[`WSlit`](https://fishr-core-team.github.io/FSA/reference/WSlit.md))
required to specify a single equation for a particular species, which is
the name of the inner list. See the examples for an illustration of how
to use `WsOpts`.

See examples and [this
article](https://fishr-core-team.github.io/FSA/articles/Computing_Relative_Weights.html)
for a demonstration.

## IFAR Chapter

8-Condition.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

## See also

See [`wsVal`](https://fishr-core-team.github.io/FSA/reference/wsVal.md),
[`WSlit`](https://fishr-core-team.github.io/FSA/reference/WSlit.md), and
[`psdAdd`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md)
for related functionality. See `mapvalues` for help in changing species
names to match those in
[`WSlit`](https://fishr-core-team.github.io/FSA/reference/WSlit.md).

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
#===== Simple example with 3 species, 2 in WSlit ... nothing unusual
tmp <- subset(PSDWRtest,
              species %in% c("Yellow Perch","Iowa Darter","Largemouth Bass"),
              select=c("species","len","wt"))
peek(tmp,n=10)
#>              species len    wt
#> 380      Iowa Darter  53   1.1
#> 411      Iowa Darter  61   0.5
#> 444  Largemouth Bass 272 191.7
#> 476  Largemouth Bass 285 218.3
#> 854     Yellow Perch 104  12.5
#> 886     Yellow Perch 144  42.7
#> 919     Yellow Perch 126  22.0
#> 951     Yellow Perch 260 269.5
#> 984     Yellow Perch 236    NA
#> 1016    Yellow Perch 322 520.0

#----- Add Wr variable ... using formula interface
tmp$wr1 <- wrAdd(wt~len+species,data=tmp)
#----- same but with non-formula interface
tmp$wr2 <- wrAdd(tmp$wt,tmp$len,tmp$species)
#----- same but using dplyr
if (require(dplyr)) {
  tmp <- tmp %>%
    mutate(wr3=wrAdd(wt,len,species))
}
#----- examine results
peek(tmp,n=10)
#>              species len    wt       wr1       wr2       wr3
#> 380      Iowa Darter  53   1.1        NA        NA        NA
#> 411      Iowa Darter  61   0.5        NA        NA        NA
#> 444  Largemouth Bass 272 191.7  69.54695  69.54695  69.54695
#> 476  Largemouth Bass 285 218.3  67.97458  67.97458  67.97458
#> 854     Yellow Perch 104  12.5  92.87361  92.87361  92.87361
#> 886     Yellow Perch 144  42.7 110.89629 110.89629 110.89629
#> 919     Yellow Perch 126  22.0  87.94797  87.94797  87.94797
#> 951     Yellow Perch 260 269.5 103.79940 103.79940 103.79940
#> 984     Yellow Perch 236    NA        NA        NA        NA
#> 1016    Yellow Perch 322 520.0 100.37577 100.37577 100.37577

#===== Simple example with only one species in the data.frame
tmp <- subset(PSDWRtest,species %in% c("Yellow Perch"),
              select=c("species","len","wt"))
tmp$wr <- wrAdd(wt~len+species,data=tmp)
peek(tmp,n=6)
#>           species len    wt        wr
#> 847  Yellow Perch 140  37.5 106.66887
#> 880  Yellow Perch 146  42.8 106.31243
#> 914  Yellow Perch 186 101.2 114.98831
#> 948  Yellow Perch 203 122.5 104.93585
#> 982  Yellow Perch 215 125.6  89.37446
#> 1016 Yellow Perch 322 520.0 100.37577

#===== Example of species with sub-groups but only 1 sub-group in data.frame
#-----   Group not in species name so must specify group with WsOpts
tmp <- subset(PSDWRtest,species=="Brown Trout" & location=="Trout Lake",
              select=c("species","len","wt"))
tmp$wr1 <- wrAdd(wt~len+species,data=tmp,
                 WsOpts=list("Brown Trout"=list("group"="lotic")))

#-----   Group in species name so don't specify group with WsOpts
tmp$species2 <- "Brown Trout (lotic)"
tmp$wr2 <- wrAdd(wt~len+species2,data=tmp)  # note use of species2

peek(tmp,n=6)
#>         species len    wt       wr1            species2       wr2
#> 201 Brown Trout 128  25.0        NA Brown Trout (lotic)        NA
#> 216 Brown Trout 164  44.1  90.26114 Brown Trout (lotic)  90.26114
#> 232 Brown Trout 205  91.1  96.32232 Brown Trout (lotic)  96.32232
#> 248 Brown Trout 261 196.3 101.54637 Brown Trout (lotic) 101.54637
#> 264 Brown Trout 208 106.5 107.86539 Brown Trout (lotic) 107.86539
#> 280 Brown Trout 350 345.2  74.92545 Brown Trout (lotic)  74.92545

#===== Example of species with sub-groups and 2 sub-groups in data.frame
tmp <- subset(PSDWRtest,species=="Brown Trout",
              select=c("species","location","len","wt"))
#-----   Must create "species" with sub-groups in name
#-----     Many ways to do this, this is just one example for this case
tmp$species2 <- ifelse(tmp$location=="Trout Lake",
                       "Brown Trout (lotic)","Brown Trout (lentic)")
tmp$wr <- wrAdd(wt~len+species2,data=tmp)  # note use of species2
peek(tmp,n=6)
#>         species     location len    wt             species2        wr
#> 201 Brown Trout   Trout Lake 128  25.0  Brown Trout (lotic)        NA
#> 236 Brown Trout   Trout Lake 218 119.4  Brown Trout (lotic) 105.23813
#> 272 Brown Trout   Trout Lake 290 277.4  Brown Trout (lotic) 105.05298
#> 307 Brown Trout Brushy Creek 298 175.9 Brown Trout (lentic)  58.15929
#> 343 Brown Trout Brushy Creek 311    NA Brown Trout (lentic)        NA
#> 379 Brown Trout Brushy Creek 420 390.1 Brown Trout (lentic)  43.10401

#===== Example of a species name that needs the thesaurus
tmp <- subset(PSDWRtest,species %in% c("Yellow Perch","Bluegill Sunfish"),
              select=c("species","len","wt"))
#-----  Below will not add wr for "Bluegill Sunfish" as not in WsLit ("Bluegill" is)
tmp$wr1 <- wrAdd(wt~len+species,data=tmp)
#-----  Use thesaurus to identify "Bluegill Sunfish" as "Blueill
tmp$wr2 <- wrAdd(wt~len+species,data=tmp,thesaurus=c("Bluegill"="Bluegill Sunfish"))
peek(tmp,n=10)
#>               species len    wt       wr1       wr2
#> 1    Bluegill Sunfish 107  25.8        NA 113.81070
#> 32   Bluegill Sunfish 135  35.1        NA  71.63419
#> 64   Bluegill Sunfish 156  81.7        NA 103.23356
#> 97   Bluegill Sunfish 233 332.7        NA 111.14772
#> 855      Yellow Perch 127  23.7  92.35541  92.35541
#> 887      Yellow Perch 153  46.0  98.22104  98.22104
#> 919      Yellow Perch 126  22.0  87.94797  87.94797
#> 952      Yellow Perch 247 260.4 118.36678 118.36678
#> 984      Yellow Perch 236    NA        NA        NA
#> 1016     Yellow Perch 322 520.0 100.37577 100.37577

#===== Example of species that has Ws eqns for multiple reference values
tmp <- subset(PSDWRtest,species=="Ruffe",select=c("species","len","wt"))
#-----  Below will err as Ruffe has Ws eqns for multiple reference values
# tmp$wr <- wrAdd(wt~len+species,data=tmp)
#-----  Must choose which eqn to use with WsOpts
tmp$wr <- wrAdd(wt~len+species,data=tmp,
                WsOpts=list(Ruffe=list(ref=75)))
peek(tmp,n=6)
#>     species len   wt       wr
#> 647   Ruffe  95  9.4 89.03770
#> 654   Ruffe  26  0.1       NA
#> 662   Ruffe  96  9.3 85.34309
#> 670   Ruffe 105 11.7 81.67358
#> 678   Ruffe 173 50.3 70.77283
#> 686   Ruffe 152 36.1 77.90516

#===== Example with two uses of WsOpts (and one species without)
tmp <- subset(PSDWRtest,species %in% c("Ruffe","Muskellunge","Iowa Darter"),
              select=c("species","len","wt"))
tmp$wr <- wrAdd(wt~len+species,data=tmp,
                WsOpts=list(Muskellunge=list(group="overall"),
                            Ruffe=list(ref=75)))
peek(tmp,n=10)
#>         species len     wt        wr
#> 380 Iowa Darter  53    1.1        NA
#> 392 Iowa Darter  41    0.5        NA
#> 405 Iowa Darter  51    0.6        NA
#> 608 Muskellunge 589 1615.3 115.77635
#> 621 Muskellunge 758 4211.3 130.47163
#> 634 Muskellunge 922 6860.5 110.82187
#> 647       Ruffe  95    9.4  89.03770
#> 660       Ruffe 136   30.4  94.08566
#> 673       Ruffe 122   20.0  87.47541
#> 686       Ruffe 152   36.1  77.90516
```
