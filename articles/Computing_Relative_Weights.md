# Computing Relative Weights in FSA

## Introduction

Condition is a measure of the physical condition of a population of fish
based on the relative heaviness or plumpness of the fish as compared to
a standard. One measure is called **relative weight** and is simply 100
times the observed weight of a fish divided by a standard weight derived
from an accepted model for that species and the individual’s observed
length; i.e.,

$$Wr_{i} = 100\frac{W_{i}}{Ws_{i}}\qquad(1)$$

where $Wr_{i}$ is the relative weight, $W_{i}$ is the observed weight,
and $Ws_{i}$ is the standard weight for the $i$th fish. Individual
relative weights are summarized to provide an overall measure of
condition for the population. Relative weight and condition are
explained in a variety of sources, including [Ogle
(2016)](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
The goal of this vignette is to demonstrate how to calculate relative
weights using functions in`FSA`.

The following packages are used herein. Note that the `FSA` functions
described here were modified after version 0.9.6 and are thus **specific
to FSA \>v0.9.6**.

``` r
library(FSA)
library(dplyr)  # mutate, select, group_by, summarize, case_when
```

## Looking Up Standard Weight Equations

Equations for computing the standard weight (i.e., $Ws_{i}$) from an
individual fish’s observed length (i.e., $L_{i}$) have been derived for
a number of freshwater game fish in the United States, as well as
several non-game fish in the United States and some other fish from
outside of the United States. The specifics of these equations have been
collated into the `WSlit` data.frame[¹](#fn1) distributed with `FSA` and
are most easily accessed with
[`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md).
For example, the specifics of the standard weight equation for Bluegill
are retrieved below.

``` r
wsVal("Bluegill")
#>     species measure  units ref method min.TL    int slope         source
#> 34 Bluegill      TL metric  75  Other     80 -5.374 3.316 Hillman (1982)
```

The results returned from
[`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md)
are:

- `species`: species of fish asked for.
- `group`: the sub-group for the species. Some species have separate
  standard weight equations for sub-groups (e.g,. `male` or `female`, or
  `lentic` and `lotic`). If `group` does not appear in the output then
  there is no sub-group for that species (as illustrated here). The
  sub-group can be chosen with `group=` in
  [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md)
  as demonstrated later.[²](#fn2)
- `measure`: the length measure used (will generally be `TL` for total
  length, but depending on the species could be `FL` for fork length,
  `BL` for body length, or `CL` for caudal length).
- `units`: the units for which the equation was developed. The default
  is `metric` which has length in mm and weight in grams. However,
  `English` can also be used which has lengths in inches and weight in
  pounds.[³](#fn3) These are the only two “units” for which the equation
  can be returned; thus, if you recorded lengths and weights in
  different units you will need to adjust your data accordingly (or
  adjust the standard weights computed from the equations (see below)
  accordingly).
- `ref`: the quantile used when developing the standard weight equation.
  The 75th percentile is used for most species, but some species have
  alternatives (50th or 25th percentile).[⁴](#fn4)
- `method`: the method used to develop the standard weight equation.
  This will usually be `RLP` (regression-line-percentile) or `EmP`
  (empirical percentile) but is `Other` only for Bluegill, as shown
  here. Some (very few) species have equations for both
  methods.[⁵](#fn5)
- `min.TL`: the minimum length for which the standard weight equation is
  appropriate.[⁶](#fn6)
- `max.TL`: the maximum length for which the standard weight equation is
  appropriate. Maximum lengths have not been specified for all species
  and, thus, may not appear in all outputs (e.g., for Bluegill here).
- `int`: the intercept, **on the common logarithm (i.e., log10) scale**,
  for the standard weight equation.
- `slope`: the slope, **on the log10 scale**, for the standard weight
  equation.
- `quad`: the coefficient for the quadratic term, **on the log10
  scale**, of the standard weight equation. Most species will not have a
  quadratic term and, thus, this may not appear in many outputs.
- `source`: the literature source for the standard weight equation.
- `comments`: comments about the standard weight equation. Not all
  species have comments, thus, this may not appear in many outputs.

A simpler output of only the equation coefficients and the lengths for
which it applies may be returned by including `simplify=TRUE` in
[`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md).[⁷](#fn7)

``` r
wsVal("Bluegill",simplify=TRUE)
#>     species min.TL    int slope
#> 34 Bluegill     80 -5.374 3.316
```

Blue Sucker is an example where some of the “optional” values are
returned (e.g., `max.TL` and `quad`). The example below further
illustrates that the results can be returned in “English” units for many
species.

``` r
wsVal("Blue Sucker",units="English")
#>        species measure   units ref method min.TL max.TL    int slope  quad
#> 31 Blue Sucker      TL English  75    EmP    9.5  31.75 -3.354 2.273 0.461
#>                 source
#> 31 Neely et al. (2008)
```

Use of
[`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md)
requires spelling (and capitalizing) the species name as it appears in
`WSlit`. One can see all species names available in `WSlit` with
[`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md)
without any arguments.

``` r
wsVal()
#> 
#> Species name must be one of following. Be careful of spelling and capitalization.
#>   [1] "Aegean Chub"                    "African Sharptooth Catfish"    
#>   [3] "Alabama Bass"                   "Alabama Bass (original)"       
#>   [5] "Ankara Nase"                    "Arctic Grayling"               
#>   [7] "Bighead Carp"                   "Bigmouth Buffalo"              
#>   [9] "Bigmouth Sleepers"              "Bigmouth Sleepers (lotic)"     
#>  [11] "Bigmouth Sleepers (overall)"    "Black Bullhead"                
#>  [13] "Black Crappie"                  "Blacktail Redhorse"            
#>  [15] "Blue Catfish"                   "Blue Sucker"                   
#>  [17] "Bluegill"                       "Bridgelip Sucker"              
#>  [19] "Brook Chub"                     "Brook Trout"                   
#>  [21] "Brook Trout (Appalachia)"       "Brook Trout (lentic)"          
#>  [23] "Brook Trout (lotic)"            "Brook Trout (overall)"         
#>  [25] "Brown Bullhead"                 "Brown Trout"                   
#>  [27] "Brown Trout (lentic)"           "Brown Trout (lotic)"           
#>  [29] "Bull Trout"                     "Burbot"                        
#>  [31] "Cavedano Chub"                  "Chain Pickerel"                
#>  [33] "Channel Catfish"                "Chinook Salmon"                
#>  [35] "Chinook Salmon (landlocked)"    "Cisco"                         
#>  [37] "Common Carp"                    "Cutthroat Trout"               
#>  [39] "Cutthroat Trout (lentic)"       "Cutthroat Trout (lotic)"       
#>  [41] "European Chub"                  "European Perch"                
#>  [43] "Flannelmouth Sucker"            "Flathead Catfish"              
#>  [45] "Flier"                          "Fourbarbel Scraper"            
#>  [47] "Freshwater Drum"                "Gizzard Shad"                  
#>  [49] "Golden Shiner"                  "Golden Trout"                  
#>  [51] "Goldeye"                        "Green Sunfish"                 
#>  [53] "Horse Barbel"                   "Humpback Chub"                 
#>  [55] "Kokanee"                        "Lake Chubsucker"               
#>  [57] "Lake Herring"                   "Lake Trout"                    
#>  [59] "Largemouth Bass"                "Largescale Sucker"             
#>  [61] "Longear Sunfish"                "Longnose Gar"                  
#>  [63] "Marble Trout"                   "Mountain Mullet"               
#>  [65] "Mountain Whitefish"             "Muskellunge"                   
#>  [67] "Muskellunge (female)"           "Muskellunge (male)"            
#>  [69] "Muskellunge (overall)"          "Nile Tilapia"                  
#>  [71] "Nipple-Lip Scraper"             "Northern Pike"                 
#>  [73] "Northern Pikeminnow"            "Northern Pikeminnow (original)"
#>  [75] "Northern Snakehead"             "Paddlefish"                    
#>  [77] "Paddlefish (female)"            "Paddlefish (male)"             
#>  [79] "Paddlefish (overall)"           "Palmetto Bass"                 
#>  [81] "Pejerrey"                       "Pumpkinseed"                   
#>  [83] "Pursak Chub"                    "Rainbow Trout"                 
#>  [85] "Rainbow Trout (lentic)"         "Rainbow Trout (lotic)"         
#>  [87] "Razorback Sucker"               "Redbreast Sunfish"             
#>  [89] "Redear Sunfish"                 "Riffle Dace"                   
#>  [91] "River Carpsucker"               "River Goby"                    
#>  [93] "Rock Bass"                      "Roundtail Chub"                
#>  [95] "Ruffe"                          "Sardine"                       
#>  [97] "Sauger"                         "Saugeye"                       
#>  [99] "Shoal Bass"                     "Shorthead Redhorse"            
#> [101] "Shovelnose Sturgeon"            "Silver Carp"                   
#> [103] "Smallmouth Bass"                "Smallmouth Buffalo"            
#> [105] "South European Roach"           "Spotted Bass"                  
#> [107] "Spotted Bass (original)"        "Spotted Gar"                   
#> [109] "Spotted Sunfish"                "Striped Bass"                  
#> [111] "Striped Bass (landlocked)"      "Striped Bass X White Bass"     
#> [113] "Suwannee Bass"                  "Tiger Muskellunge"             
#> [115] "Utah Chub"                      "Walleye"                       
#> [117] "Walleye (30-149 mm)"            "Walleye (overall)"             
#> [119] "Warmouth"                       "White Bass"                    
#> [121] "White Catfish"                  "White Crappie"                 
#> [123] "White Perch"                    "White Sturgeon"                
#> [125] "White Sucker"                   "Yellow Bass"                   
#> [127] "Yellow Bullhead"                "Yellow Perch"
```

All parts of the species names in `WSlit` are capitalized (e.g., “Blue
Sucker” and not “blue sucker” or “Blue sucker”).
[`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md)
will return an informative error message if your capitalization is not
correct but the message will be less informative if your spelling is
off.

``` r
wsVal("Blue sucker")
#> Error:
#> ! There is no Ws equation in 'WSlit' for "Blue sucker". However, there is
#>   an entry for "Blue Sucker" (note spelling, including capitalization).
wsVal("Blue suckr")
#> Error:
#> ! There is no Ws equation in 'WSlit' for "Blue suckr". Type 'wsVal()' to
#>   see a list of available species.
```

It is also not always obvious whether a species has equations for
sub-groups or not. One way to deal with this is to just ask for the
equation for your species of interest without using `group=`. If
sub-groups exist then you will get an error message asking you to choose
which sub-group to use.

``` r
wsVal("Brown Trout")
#> Error:
#> ! "Brown Trout" has Ws equations for these sub-groups: "lentic" and
#>   "lotic". Please use 'group=' to select the equation for one of these
#>   groups.
```

Then try again by choosing the sub-group with `group=`.

``` r
wsVal("Brown Trout",group="lentic")
#>        species  group measure  units ref method min.TL    int slope
#> 51 Brown Trout lentic      TL metric  75    RLP    140 -5.422 3.194
#>                      source
#> 51 Hyatt and Hubert (2001b)
```

These same species and sub-group combinations can be also accessed by
combining the species name and sub-group name (in parenthesis) into the
first argument (and then not using `group=`).

``` r
wsVal("Brown Trout (lotic)")
#>                species measure  units ref method min.TL    int slope
#> 57 Brown Trout (lotic)      TL metric  75    RLP    140 -4.867  2.96
#>                       source
#> 57 Milewski and Brown (1994)
```

The same general process can be used for species that have equations
developed from multiple methods; though, only the sub-group identifiers
can be combined (with parentheses) in the species name and used in the
first argument.

``` r
wsVal("Arctic Grayling")
#> Error:
#> ! Ws equations exist for both the RLP and EmP 'method's for "Arctic
#>   Grayling". Please select one or the other with 'method='.
wsVal("Arctic Grayling",method="EmP")
#>           species measure  units ref method min.TL    int slope
#> 8 Arctic Grayling      TL metric  75    EmP    150 -5.279 3.096
#>                 source                                                comment
#> 8 Gilham et al. (2021) authors note that either RLP or EmP method may be used
```

Or for multiple reference groups.

``` r
wsVal("Ruffe")
#> Error:
#> ! Ws equations exist for more than one 'ref'erence value for "Ruffe".
#>   Please select one of the following values with 'ref=': 50 or 75.
wsVal("Ruffe",ref=50)
#>     species measure  units ref method min.TL max.TL     int  slope   quad
#> 190   Ruffe      TL metric  50    EmP     55    205 -3.3524 1.3969 0.4054
#>                       source
#> 190 Ogle and Winfield (2009)
```

There are two species (Chinook Salmon and Striped Bass) that appear to
have sub-groups (i.e., “landlocked”) but only that one specific
sub-group appears in `WSlit`. The standard weight literature for these
species did not identify the equation as only being for “landlocked”
populations. However, these entries were added to facilitate use when
calculating proportional size distribution (PSD) metrics,[⁸](#fn8) which
were defined just for “landlocked” populations, and relative weight
metrics with the same data.frame.[⁹](#fn9) The standard weight equations
for these species can be accessed by either just the species name or the
species name with the group in parentheses. However, note that they are
the exact same information.

``` r
wsVal("Striped Bass")
#>          species measure  units ref method min.TL    int slope
#> 220 Striped Bass      TL metric  75    RLP    150 -4.924 3.007
#>                       source
#> 220 Brown and Murphy (1991b)
wsVal("Striped Bass (landlocked)")
#>                       species measure  units ref method min.TL    int slope
#> 222 Striped Bass (landlocked)      TL metric  75    RLP    150 -4.924 3.007
#>                       source
#> 222 Brown and Murphy (1991b)
#>                                                          comment
#> 222 Not clear just landlocked; added to simplify use with PSDlit
```

There are also a few species where an original standard weight equation
has been revised in the literature. The original and revised equations
are available in `WSlit` with the revised equations accessed by just
using the species name and the original equations accessed by appending
“(original)” to the species name.

``` r
wsVal("Spotted Bass")             # revised definitions
#>          species measure  units ref method min.TL max.TL     int slope
#> 211 Spotted Bass      TL metric  75    EmP    150    490 -5.3467 3.197
#>                    source                                               comment
#> 211 Sammons et al. (2025) also used RLP and EmP-quadratic but did not recommend
wsVal("Spotted Bass (original)")
#>                     species measure  units ref method min.TL    int slope
#> 214 Spotted Bass (original)      TL metric  75    RLP    100 -5.392 3.215
#>                  source          comment
#> 214 Wiens et al. (1996) HAS BEEN REVISED
```

We strongly urge you to have a good understanding of the standard weight
literature for your species’ of interest and make sure that
[`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md) is
returning the values that you expect.

## Calculate Individual Relative Weight

### For Typical Linear Equation

The specifics of the standard weight equation returned by
[`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md)
can be used to compute the standard weight for a fish given its observed
length. As an example, suppose that the relative weight of a Largemouth
Bass with an observed length of 350 mm and weight of 650 g is desired.

Begin by assigning the specifics of the standard weight equation for
Largemouth Bass returned by
[`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md) to
an object (e.g., `wsLMB` here).

``` r
( wsLMB <- wsVal("Largemouth Bass") )
#>             species measure  units ref method min.TL    int slope        source
#> 114 Largemouth Bass      TL metric  75    RLP    150 -5.528 3.273 Henson (1991)
```

The intercept and slope for the standard weight equation on the
**log10-log10** scale can be extracted from this object.[¹⁰](#fn10)

``` r
wsLMB[["int"]]
#> [1] -5.528
wsLMB[["slope"]]
#> [1] 3.273
```

The standard weight is then computed from these results and the
**log10-transformed** observed length as follows (with the result saved
to an object, called `ex1` here). Make sure to note the use of `10^` to
back-transform the **log10-transformed** standard weight to a standard
weight on the raw scale.

``` r
( ex1 <- 10^(wsLMB[["int"]]+wsLMB[["slope"]]*log10(350)) )
#> [1] 629.1218
```

This calculation suggests that the standard weight for a 350 mm
Largemouth Bass is 629.1 g. With this and [Equation 1](#eq-Wri), the
relative weight for this individual is computed (recalling that the
observed weight was 650 g).

``` r
100*650/ex1
#> [1] 103.3186
```

This indicates that the individual is (slightly) heavier than a
“standard fish” of the same length as this values is greater than 100.

### For Quadratic Equation

A similar process can be followed for a species where the standard
weight equation includes a quadratic term. The only “trick” here is to
include the quadratic term multiplied by the **square** of the
log10-transformed observed length when computing the standard weight.
This calculation is illustrated below with a 500 mm and 1010 g Blue
Sucker.

``` r
( wsBS <- wsVal("Blue Sucker",simplify=TRUE) )
#>        species min.TL max.TL   int slope  quad
#> 32 Blue Sucker    240    809 -2.98 0.977 0.461
( ex2 <- 10^(wsBS[["int"]]+wsBS[["slope"]]*log10(500)+wsBS[["quad"]]*log10(500)^2) )
#> [1] 1035.19
100*1010/ex2
#> [1] 97.56662
```

### Cautions

A few things to consider when calculating relative weights for
individuals.

- Make sure the individual is longer than the minimum and shorter than
  the maximum (if given) length returned by
  [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md).
- Make sure to log10-transform the observed length when computing the
  standard weight. Note that all standard weight equations use log10 so
  never use the natural log with standard weight calculations.
- Make sure to back-transform (i.e., raise to the power of 10) the value
  so that the standard weight is on the raw rather than
  log10-transformed scale.
- Relative weights have traditionally been reported on a scale where the
  fraction is multiplied by 100 (i.e., so 100 means that the observed
  weight equals the standard weight). Thus, don’t forget to multiply by
  100 when computing the relative weight.

## Calculate Relative Weights for All of One Species

It is not common to compute the standard and relative weights for a
single individual as was done in the previous section. Rather, it is
more useful to calculate these values for all individuals in a sample,
and then summarize those values for an overall assessment of the
condition of those individuals.

Consider the `CiscoTL` data.frame distributed with the `FSAdata` package
that contains the lengths (mm) and weights (g) of Cisco (*Coregonus
artedii*) sampled from Trout Lake, WI, USA over a 25 year period. Note
that there are many missing weights in this data.frame.

``` r
data("CiscoTL",package="FSAdata")  # retrieve the data.frame
peek(CiscoTL,n=10)
#>      lakeid year4 sampledate gearid spname length weight  sex
#> 1        TR  1981  8/11/1981 VGN032  CISCO    140   21.4    F
#> 955      TR  1983  7/29/1983 VGN032  CISCO    196     NA <NA>
#> 1910     TR  1984  7/24/1984 VGN038  CISCO    213     NA <NA>
#> 2865     TR  1987  7/28/1987 VGN038  CISCO    208     NA <NA>
#> 3820     TR  1991  7/30/1991 VGN025  CISCO    139     NA <NA>
#> 4774     TR  1993  7/27/1993 VGN038  CISCO    213     NA <NA>
#> 5729     TR  1997  7/28/1997 VGN032  CISCO    174     NA <NA>
#> 6684     TR  1999  7/26/1999 VGN025  CISCO    137     NA <NA>
#> 7639     TR  2002  7/23/2002 VGN025  CISCO    144     NA <NA>
#> 8594     TR  2006  7/20/2006 VGN038  CISCO    216     NA <NA>
```

### “Manual” Calculations

The relative weight calculation begins by finding the specifics of the
standard weight equation for Cisco and assigning them to an object
(here, `wsC`).

``` r
( wsC <- wsVal("Cisco") )
#>    species measure  units ref method min.TL    int slope
#> 72   Cisco      TL metric  75    RLP    100 -5.517 3.224
#>                       source                  comment
#> 72 Fisher and Fielder (1998) same as for Lake Herring
```

Two new variables – `Ws` for standard weight and `Wr` for relative
weight – are added to the data.frame in three steps below. First, `Ws`
is calculated using the coefficients from the standard weight equation
and the log-transformed length variable as shown above (but within
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) from
`dplyr`). Second, if the observed length is less than the minimum length
for which the standard weight equation should be applied, then the
previously calculated `Ws` is replaced with an `NA` (for missing
value).[¹¹](#fn11) Finally, the relative weight is computed from the
observed weight and standard weight variables using
[Equation 1](#eq-Wri).

``` r
CiscoTL <- CiscoTL |>
  mutate(Ws=10^(wsC[["int"]]+wsC[["slope"]]*log10(length)),
         Ws=ifelse(length<wsC$min.TL,NA,Ws),
         Wr=100*weight/Ws)
peek(CiscoTL,n=10)
#>      lakeid year4 sampledate gearid spname length weight  sex        Ws      Wr
#> 1        TR  1981  8/11/1981 VGN032  CISCO    140   21.4    F  25.24159 84.7807
#> 955      TR  1983  7/29/1983 VGN032  CISCO    196     NA <NA>  74.68503      NA
#> 1910     TR  1984  7/24/1984 VGN038  CISCO    213     NA <NA>  97.65531      NA
#> 2865     TR  1987  7/28/1987 VGN038  CISCO    208     NA <NA>  90.45575      NA
#> 3820     TR  1991  7/30/1991 VGN025  CISCO    139     NA <NA>  24.66492      NA
#> 4774     TR  1993  7/27/1993 VGN038  CISCO    213     NA <NA>  97.65531      NA
#> 5729     TR  1997  7/28/1997 VGN032  CISCO    174     NA <NA>  50.87809      NA
#> 6684     TR  1999  7/26/1999 VGN025  CISCO    137     NA <NA>  23.53895      NA
#> 7639     TR  2002  7/23/2002 VGN025  CISCO    144     NA <NA>  27.64144      NA
#> 8594     TR  2006  7/20/2006 VGN038  CISCO    216     NA <NA> 102.15953      NA
```

### Using the `WrAdd()` Convenience Function

[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)
can be used to add a relative weight variable to a data.frame for
**all** species in the data.frame for which a standard weight equation
exists.[¹²](#fn12) The main argument to
[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md) is
a formula of the form `weight~length+species` where `weight` is the name
of the observed weight variable, `length` is the name of the observed
length variable, and `species` is the name of the species variable. One
constraint here is that the species names in the `species` variable must
be spelled (and capitalized) exactly as in `WSlit`. In these data, the
species name is in `spname` but is in all capital letters as “CISCO”,
rather than the required “Cisco”.
[`capFirst()`](https://fishr-core-team.github.io/FSA/reference/capFirst.md)[¹³](#fn13)
may be used to convert a word to a form where just the first letter is
capitalized, and is used below.[¹⁴](#fn14)

``` r
data("CiscoTL",package="FSAdata")
CiscoTL <- CiscoTL |>
  mutate(Wr=wrAdd(weight~length+capFirst(spname)))
peek(CiscoTL,n=10)
#>      lakeid year4 sampledate gearid spname length weight  sex      Wr
#> 1        TR  1981  8/11/1981 VGN032  CISCO    140   21.4    F 84.7807
#> 955      TR  1983  7/29/1983 VGN032  CISCO    196     NA <NA>      NA
#> 1910     TR  1984  7/24/1984 VGN038  CISCO    213     NA <NA>      NA
#> 2865     TR  1987  7/28/1987 VGN038  CISCO    208     NA <NA>      NA
#> 3820     TR  1991  7/30/1991 VGN025  CISCO    139     NA <NA>      NA
#> 4774     TR  1993  7/27/1993 VGN038  CISCO    213     NA <NA>      NA
#> 5729     TR  1997  7/28/1997 VGN032  CISCO    174     NA <NA>      NA
#> 6684     TR  1999  7/26/1999 VGN025  CISCO    137     NA <NA>      NA
#> 7639     TR  2002  7/23/2002 VGN025  CISCO    144     NA <NA>      NA
#> 8594     TR  2006  7/20/2006 VGN038  CISCO    216     NA <NA>      NA
```

A more general approach to deal with species that are spelled
differently than expected in `WSlit` is to provide a named list or
vector that defines how the original species names (i.e., the items in
the vector to the right of the `=`) relate to the species names required
by `WSlit` (i.e., the names in the vector to the left of the `=`) to
`thesaurus=`.
[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)
will match the two names appropriately while creating the relative
weight variable.

``` r
CiscoTL <- CiscoTL |>
  mutate(Wr2=wrAdd(weight~length+spname,thesaurus=c("Cisco"="CISCO")))
peek(CiscoTL,n=10)
#>      lakeid year4 sampledate gearid spname length weight  sex      Wr     Wr2
#> 1        TR  1981  8/11/1981 VGN032  CISCO    140   21.4    F 84.7807 84.7807
#> 955      TR  1983  7/29/1983 VGN032  CISCO    196     NA <NA>      NA      NA
#> 1910     TR  1984  7/24/1984 VGN038  CISCO    213     NA <NA>      NA      NA
#> 2865     TR  1987  7/28/1987 VGN038  CISCO    208     NA <NA>      NA      NA
#> 3820     TR  1991  7/30/1991 VGN025  CISCO    139     NA <NA>      NA      NA
#> 4774     TR  1993  7/27/1993 VGN038  CISCO    213     NA <NA>      NA      NA
#> 5729     TR  1997  7/28/1997 VGN032  CISCO    174     NA <NA>      NA      NA
#> 6684     TR  1999  7/26/1999 VGN025  CISCO    137     NA <NA>      NA      NA
#> 7639     TR  2002  7/23/2002 VGN025  CISCO    144     NA <NA>      NA      NA
#> 8594     TR  2006  7/20/2006 VGN038  CISCO    216     NA <NA>      NA      NA
```

`thesaurus=` can be used even if only some of the species names are
non-“standard.” Additionally, the named list/vector in `thesaurus=` can
contain names that don’t exist in the original data.frame. Thus, a
global thesaurus containing all species that *could* be encountered
could be created, for example as an agency-wide definition, and used
with a variety of specific data.frames.

[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)
handles species that have more than one standard weight
equation[¹⁵](#fn15) with `WsOpts=`. For example, consider adding a
relative weight variable to the `RuffeSLRH92` data.frame distributed
with `FSAdata`. Note that there is no species variable, which is needed
by
[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md),
so one was added with
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).[¹⁶](#fn16)
Additionally, for simplicity of presentation, I removed several other
variables (with
[`select()`](https://dplyr.tidyverse.org/reference/select.html)) that
are not needed for this example.

``` r
data("RuffeSLRH92",package="FSAdata")  # retrieve the data.frame
RuffeSLRH92 <- RuffeSLRH92 |>
  mutate(species="Ruffe") |>
  select(species,length,weight,sex)
peek(RuffeSLRH92,n=10)
#>     species length weight     sex
#> 1     Ruffe     90    9.3    male
#> 82    Ruffe    120   24.0    male
#> 164   Ruffe    142   33.1  female
#> 246   Ruffe     84    7.4  female
#> 328   Ruffe    100   12.6    male
#> 410   Ruffe     25    0.2 unknown
#> 492   Ruffe     94   13.0    male
#> 574   Ruffe     85    7.9    male
#> 656   Ruffe    145   39.1    male
#> 738   Ruffe    160   52.9  female
```

Code similar to that used above for Cisco returns an error because there
are standard weight equations for Ruffe for both 50th and 75th
percentile references (see snippet below with the error message).

``` r
RuffeSLRH92 <- RuffeSLRH92 |>
  mutate(Wr=wrAdd(weight~length+species))
#>     species group ref method
#> 190   Ruffe  <NA>  50    EmP
#> 191   Ruffe  <NA>  75    EmP
#> Error in `mutate()`:
#> ℹ In argument: `Wr = wrAdd(weight ~ length + species)`.
#> Caused by error:
#> ! More than one Ws equation exists for "Ruffe". Please use a named list
#>   in 'WsOpts=' to select one equation for "Ruffe" by specifing 'group',
#>   'ref', or 'method' as appropriate. See details in documentation and
#>   above (for reference).
```

To continue,
[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)
must be instructed as to which of these two equations should be used to
calculate the standard weight and, thus, relative weight. This is
handled with a list in `WsOpts=` which will include the species name set
equal to a list which identifies the specific equation to use (i.e.,
`ref=50` in this example).

``` r
RuffeSLRH92 <- RuffeSLRH92 |>
  mutate(Wr=wrAdd(weight~length+species,WsOpts=list(Ruffe=list(ref=50))))
peek(RuffeSLRH92,n=10)
#>     species length weight     sex        Wr
#> 1     Ruffe     90    9.3    male 110.34512
#> 82    Ruffe    120   24.0    male 119.03123
#> 164   Ruffe    142   33.1  female  97.21733
#> 246   Ruffe     84    7.4  female 107.76315
#> 328   Ruffe    100   12.6    male 108.98596
#> 410   Ruffe     25    0.2 unknown        NA
#> 492   Ruffe     94   13.0    male 135.44538
#> 574   Ruffe     85    7.9    male 111.08527
#> 656   Ruffe    145   39.1    male 107.53033
#> 738   Ruffe    160   52.9  female 106.51935
```

## Calculate Relative Weights for All of Multiple Species

The real value of
[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md) is
that it can be used to efficiently add a relative weight variable for
multiple species in a single data.frame. This is illustrated below for a
variety of scenarios.

### “Good” Names and No Groups

`InchLake2` distributed with `FSAdata` contains lengths and weights for
several species captured from Inch Lake. These data provide a simple
example for using
[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)
because all species names are spelled as required and none of the
species have sub-groups.[¹⁷](#fn17)

``` r
data("InchLake2",package="FSAdata")  # retrieve the data.frame
peek(InchLake2,n=10)
#>     netID fishID          species length weight year
#> 1     206    501         Bluegill    1.5    0.7 2008
#> 57     16    208    Black Crappie   11.6  380.0 2007
#> 115   101    583         Bluegill    5.5   48.0 2008
#> 172   102    642 Bluntnose Minnow    2.1    1.3 2008
#> 229   116    760  Largemouth Bass    2.8    2.0 2008
#> 287   109    843  Largemouth Bass   13.1  460.0 2008
#> 344   130    902  Largemouth Bass   10.1  173.0 2008
#> 401     6    178         Bluegill    6.2   62.0 2007
#> 459    12     45 Bluntnose Minnow    2.7    6.0 2007
#> 516     4    127         Bluegill    6.6   90.0 2007
```

A complication here is that length and weight are in “mixed” units;
i.e., inches for length and grams for weight. One or the other must be
converted to the same unit type. Below length is converted to mm before
[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md) is
used to add a relative weight variable.

``` r
InchLake2 <- InchLake2 |>
  mutate(length=length*25.4,
         Wr=wrAdd(weight~length+species))
peek(InchLake2,n=10)
#>     netID fishID          species length weight year       Wr
#> 1     206    501         Bluegill  38.10    0.7 2008       NA
#> 57     16    208    Black Crappie 294.64  380.0 2007 86.69675
#> 115   101    583         Bluegill 139.70   48.0 2008 87.45204
#> 172   102    642 Bluntnose Minnow  53.34    1.3 2008       NA
#> 229   116    760  Largemouth Bass  71.12    2.0 2008       NA
#> 287   109    843  Largemouth Bass 332.74  460.0 2008 86.27960
#> 344   130    902  Largemouth Bass 256.54  173.0 2008 76.01188
#> 401     6    178         Bluegill 157.48   62.0 2007 75.92630
#> 459    12     45 Bluntnose Minnow  68.58    6.0 2007       NA
#> 516     4    127         Bluegill 167.64   90.0 2007 89.57900
```

Note that species without known standard weight equations in `WSlit`
(e.g., Bluntnose Minnow) will have `NA` for the relative weight
variable, as will individuals with lengths outside the range of lengths
for which the standard weight equation should be applied.

### “Bad” Names and No Groups

Now consider the `BGHRfish` data.frame (distributed with the `FSAdata`
package) that has the lengths (mm) and weights (g) of three species –
Smallmouth Bass, Largemouth Bass, and Bluegill – from Big Hill
Reservoir, KS. These three species do not have sub-groups defined in
`WSlit`, but upon observing the data below[¹⁸](#fn18) it is seen that
the species variable (`specCode`) contains codes for the species names
rather than the names required by `PSDlit`.

``` r
data(BGHRfish,package="FSAdata")  # retrieve the data.frame
peek(BGHRfish,n=10)
#>     UID fishID specCode length weight count
#> 1     1      1      116    100     NA     1
#> 30    2      7      118    317    390     1
#> 59    4      9      122    151     90     1
#> 89    6     NA      122    180     NA     2
#> 118  10      2      116    356    520     1
#> 148  12      3      122    152     80     1
#> 177  15      4      118    311    370     1
#> 207  19      4      118    304    350     1
#> 236  20     NA      118     80     NA     1
#> 266  20     21      122    171    120     1
```

One way to deal with the issue of “bad” species names is to use a named
list or vector that defines how the names from `WSlit` should be matched
to the names in the data.frame. For this purpose, the species names in
`WSlit` are the names in the vector (i.e., before the `=`) and the
species names in the data.frame are the items in the vector (i.e., after
the `=`).[¹⁹](#fn19)

``` r
thes <- c("Smallmouth Bass"="116","Largemouth Bass"="118","Bluegill"="122")
```

This list/vector is then given to `thesaurus=` in
[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)
which will perform the name matching while creating the GH length
categories. However, one complicating factor with these data is that
[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)
expects a string in `species` (and, thus, `thesaurus=`) rather than a
numeric like `specCode`. Thus, `specCode` is converted to a character
below before using
[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md).

``` r
BGHRfish <- BGHRfish |>
  mutate(specCode=as.character(specCode),
         Wr=wrAdd(weight~length+specCode,thesaurus=thes))
peek(BGHRfish,n=10)
#>     UID fishID specCode length weight count        Wr
#> 1     1      1      116    100     NA     1        NA
#> 30    2      7      118    317    390     1  85.72306
#> 59    4      9      122    151     90     1 126.69368
#> 89    6     NA      122    180     NA     2        NA
#> 118  10      2      116    356    520     1  75.92125
#> 148  12      3      122    152     80     1 110.17845
#> 177  15      4      118    311    370     1  86.57589
#> 207  19      4      118    304    350     1  88.23133
#> 236  20     NA      118     80     NA     1        NA
#> 266  20     21      122    171    120     1 111.83201
```

### “Bad” Names, Groups, and Multiple Equations

The use of
[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)
can become complicated for data.frames with species names other than
what `WSlit` expects, with species for which standard weight equations
exist for sub-groups, especially if more than one sub-group is in the
data, and for species with multiple standard weight equations. The
hypothetical data set `PSDWRtest` distributed with `FSA` can be used to
illustrate how to handle these “issues”.

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

[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)
will produce some informative error messages, but it is best that you
have a full understanding of the issues that may arise with your data by
carefully examining your data and understanding the standard weight (and
Gabelhouse length categories) for the species in your data. The “issues”
that need to be addressed with the `PSDWRtest` data are as follows:

- “Bluegill Sunfish” was used rather than “Bluegill”.
- “Lean Lake Trout” was used rather than “Lake Trout”.
- Brook Trout sampled from a lotic (“Trout Lake”) system, for which
  there are standard weight equations for sub-groups.
- Brown Trout sampled from a lotic (“Trout Lake”) and lentic (“Brush
  Creek”) system, for which there are standard weight equations for
  sub-groups.
- Muskellunge for which sex is known for some and not for others, and
  there is a desire to use the sex-specific standard weight equation
  when sex is known (and use the overall equation when it is not).
- Walleye of sizes for which separate standard weight equations may be
  used.
- Ruffe for which the reference quantile of the standard weight equation
  must be specified.

The easiest way to deal with most of these “issues” is to create a new
“species” variable (i.e., `species2` below) that appends the specific
groups in parentheses to the species name. There are a variety of ways
to do this and which way (is best or works) may depend on the specifics
of the situation. Here, we primarily use
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

The relative weights for each fish (for species with standard weight
equations) is added to this data.drame with
[`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md) as
described above, noting the use of the “new” species variable
`species2`. Further note the use of `wsOpts=` to define which reference
quantile to use for the Ruffe standard weight equation.

``` r
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
#>             wr
#> 1    113.81070
#> 53   117.44555
#> 107   89.31318
#> 160         NA
#> 214  118.65447
#> 267  114.26156
#> 321   53.30764
#> 374   48.64940
#> 428   70.72483
#> 481   76.95718
#> 535  102.54739
#> 588   95.07015
#> 642  103.11404
#> 695  104.56374
#> 749   95.72295
#> 802  103.75325
#> 856   99.38328
#> 909   91.50636
#> 963   92.47232
#> 1016 100.37577
```

## Summarizing Relative Weights

### Single Species

As an illustration, the mean and standard deviation of relative weights
for Cisco (in `CiscoTL` from above) are computed below (along with
`validn` which is the number of non-`NA` (i.e., non-missing) relative
weights).[²⁰](#fn20) These results suggest that this population of Cisco
is substantially “skinnier” (i.e., under-weight) than the standard for
the species (because the mean relative weight is substantially less than
100).

``` r
CiscoTL |>
  summarize(validn=sum(!is.na(Wr)),
            mnWr=mean(Wr,na.rm=TRUE),
            sdWr=sd(Wr,na.rm=TRUE))
#>   validn    mnWr     sdWr
#> 1   1190 76.6967 7.784748
```

### Multiple Species

The mean and sd of relative weight for all species in `PSDWRtest` from
above is shown below. It is important to carefully examine these results
because a species could have no mean relative weight calculated because
there is no standard weight equation for that species (in `WSlit`) or
because the species name is not spelled as expected (in `WSlit`). The
only species below with no mean relative weight (i.e., Iowa Darter)
does, indeed, not have a known standard weight equation.

``` r
PSDWRtest |>
  group_by(species,location) |>
  summarize(validn=sum(!is.na(wr)),
            mnWr=mean(wr,na.rm=TRUE),
            sdWr=sd(wr,na.rm=TRUE)) |>
  as.data.frame()
#> `summarise()` has grouped output by 'species'. You can override using the
#> `.groups` argument.
#>             species     location validn      mnWr      sdWr
#> 1  Bluegill Sunfish    Bass Lake    111 101.77768  9.689499
#> 2       Brook Trout   Trout Lake     73 109.45569  1.191287
#> 3       Brown Trout Brushy Creek     91  56.91065  5.724542
#> 4       Brown Trout   Trout Lake     72 102.97181 10.372980
#> 5       Iowa Darter    Bass Lake      0       NaN        NA
#> 6   Largemouth Bass    Bass Lake     87  70.00372  6.558304
#> 7   Lean Lake Trout   Trout Lake     92  97.51835  9.643417
#> 8       Muskellunge    Long Lake     44 110.93352 11.695635
#> 9             Ruffe   Round Lake     36  84.17434  7.423376
#> 10          Walleye    Bass Lake    154  99.46820  9.000269
#> 11     Yellow Perch    Bass Lake    160 102.25132  9.927675
```

------------------------------------------------------------------------

1.  Specifics
    [here](https://fishr-core-team.github.io/FSA/reference/WSlit.html).

2.  Leave `group=` at the default `NULL` for species without sub-group
    equations.

3.  A few species have an equation for only one set of units.

4.  Alternatives for a species can be chosen with `ref=` in
    [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md).
    For species with only one possible `ref`, then `ref=` should be left
    at the default `NULL`.

5.  For species with multiple `method`s, you will need to choose which
    to use with `method=` in
    [`wsVal()`](https://fishr-core-team.github.io/FSA/reference/wsVal.md).
    For species where only one method is available then leave `method=`
    at the default `NULL`.

6.  The postfix after the period will be replaced with the `measure`
    type (here `TL` but for some species `FL`, `BL`, or `CL`).

7.  I don’t often use this, but it is available for a “cleaner” look.

8.  See [this
    vignette](https://fishr-core-team.github.io/FSA/articles/Computing_PSDs.html).

9.  As illustrated in [this companion
    vignette](https://fishr-core-team.github.io/FSA/articles/Computing_PSDs_and_RelativeWeights.html).

10. The double-brackets (i.e., `[[]]`) are used to remove the name
    (e.g., `int`) from the returned value.

11. These two steps could have been combined by replacing `Ws` with the
    calculation in [`ifelse()`](https://rdrr.io/r/base/ifelse.html).

12. Though its use is illustrated here on a data.frame with only one
    species. See the next sections for an example with more species.

13. From `FSA`.

14. Note that the `CiscoTL` data.frame was re-read so that the additions
    in the previous section were removed.

15. As described previously.

16. Note that
    [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)
    is more convenient for data.frames with multiple species as will be
    illustrated in the next section.

17. See the next section for how to deal with these issues.

18. And [the
    documentation](https://fishr-core-team.github.io/FSAdata/reference/Herman.html).

19. The numeric species codes are in parentheses as `specCode` will be
    converted to a string before using
    [`wrAdd()`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md).

20. More interesting summaries and plots may be constructed with these
    data. This is just an example of summarizing relative weights for a
    sample.
