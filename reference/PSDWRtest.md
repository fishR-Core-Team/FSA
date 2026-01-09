# Hypothetical weight-length data for testing PSD and relative weight functions

Hypothetical weight-length and associated data. These data are useful
for testing PSD and relative weight functions (e.g.,
[`psdAdd`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md)
and
[`wrAdd`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)).

## Format

A data frame of many observations on the following 5 variables:

- species:

  Species name

- location:

  Broad location of capture

- len:

  Length in mm

- wt:

  Weight in g

- sex:

  Sex as `F` for female, `M` for male, or `U` or `NA` for unknown or
  unrecorded

## Topic(s)

- Size structure

- Proportional size structure

- Relative stock density

- Proportional stock density

- Relative weight

- Standard weight

- Condition

## See also

[`psdAdd`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md),
[`psdCalc`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md),
and [`wrAdd`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)

## Examples

``` r
str(PSDWRtest)
#> 'data.frame':    1016 obs. of  5 variables:
#>  $ species : chr  "Bluegill Sunfish" "Bluegill Sunfish" "Bluegill Sunfish" "Bluegill Sunfish" ...
#>  $ location: chr  "Bass Lake" "Bass Lake" "Bass Lake" "Bass Lake" ...
#>  $ len     : num  107 88 102 94 104 100 91 97 101 115 ...
#>  $ wt      : num  25.8 13.1 18.3 15.6 21.9 20.5 13.4 16.2 15.9 23.2 ...
#>  $ sex     : chr  NA NA NA NA ...
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
unique(PSDWRtest$species)
#>  [1] "Bluegill Sunfish" "Brook Trout"      "Brown Trout"      "Iowa Darter"     
#>  [5] "Largemouth Bass"  "Lean Lake Trout"  "Muskellunge"      "Ruffe"           
#>  [9] "Walleye"          "Yellow Perch"    
```
