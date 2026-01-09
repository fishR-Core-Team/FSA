# Finds standard weight equation coefficients for a particular species.

Returns a vector that contains information about the standard weight
equation for a given species, including type of measurement units,
reference percentile, method used to derive the equation, and literature
source.

## Usage

``` r
wsVal(
  species = "List",
  group = NULL,
  units = c("metric", "English"),
  ref = NULL,
  method = NULL,
  simplify = FALSE,
  dat = NULL
)
```

## Arguments

- species:

  A string that contains the species name for which to find Ws
  coefficients. See details.

- group:

  A string that contains the sub-group of `species` for which to find
  the Ws coefficients; e.g., things like “lotic”, “lentic”, “female”,
  “male”.

- units:

  A string that indicates whether the coefficients for the standard
  weight equation to be returned are in `"metric"` (DEFAULT; mm and g)
  or `"English"` (in and lbs) units.

- ref:

  A numeric that indicates which percentile the equation should be
  returned for. Note that the vast majority of equations only exist for
  the `75`th percentile (DEFAULT).

- method:

  A string that indicates which equation-derivation method should be
  used (one of `"RLP"`, `"EmP"`, or `"Other"`). Defaults to `NULL` which
  will result in the only method available being returned or an error
  asking the user to choose a method for equations for which more than
  one method is available (which is the case for very few species).

- simplify:

  A logical that indicates whether the ‘units’, ‘ref’, ‘measure’,
  ‘method’, ‘comments’, and ‘source’ fields should be included
  (`=FALSE`) or not (`=TRUE`; DEFAULT). See details.

- dat:

  Data.frame of Gabelhouse length categories for all species. Defaults
  to \`WSlit\` and is generally not used by the user (this simplifies
  use of this function in `wrAdd`).

## Value

A one row data frame from
[`WSlit`](https://fishr-core-team.github.io/FSA/reference/WSlit.md) that
contains all known information about the standard weight equation for a
given species, type of measurement units, and reference percentile if
`simplify=FALSE`. If `simplify=TRUE` then only the species; minimum and
maximum length for which the standard equation should be applied; and
intercept, slope, and quadratic coefficients for the standard weight
equation. Note that the maximum length and the quadratic coefficient
will not be returned if they do not exist in
[`WSlit`](https://fishr-core-team.github.io/FSA/reference/WSlit.md) for
the species.

If no arguments are given to this function then a list of available
species names in
[`WSlit`](https://fishr-core-team.github.io/FSA/reference/WSlit.md) will
be printed. If the species name is mis-spelled (or mis-capitalized),
multiple standard weight equations exist for the species (such that
`group`, `ref`, or `method` should be used), or if a standard weight
equation does not exist for the species in
[`WSlit`](https://fishr-core-team.github.io/FSA/reference/WSlit.md),
then an error will be issued.

## Details

This function extracts all known information from
[`WSlit`](https://fishr-core-team.github.io/FSA/reference/WSlit.md)
about the following standard weight equation,

\$\$log\_{10}(Ws) = log\_{10}(a) + blog\_{10}(L) + blog\_{10}(L)^{2}\$\$

See [`WSlit`](https://fishr-core-team.github.io/FSA/reference/WSlit.md)
for more information about the meaning of each value returned.

Note from above that the coefficients are returned for the TRANSFORMED
model. Thus, to obtain the standard weight (Ws), the returned
coefficients are used to compute the common log of Ws which must then be
raised to the power of 10 to compute the Ws.

Some species have length categories separated by sub-group. For example,
length categories exist for both lentic and lotic populations of Brown
Trout. The length values for a sub-group may be obtained by either
including the species name in `species` and the sub-group name in
`group` or by using the combined species and sub-group name, with the
sub-group name in parentheses, in `species`. Both methods are
demonstrated in the examples. Note that an error is returned if a
species has sub-groups but neither method is used to define the
sub-group.

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

See [`wrAdd`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)
and [`WSlit`](https://fishr-core-team.github.io/FSA/reference/WSlit.md)
for related functionality.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
#===== List all available Ws equations
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

#===== Find equations for Yellow Perch, in different formats
wsVal("Yellow Perch")
#>          species measure  units ref method min.TL    int slope
#> 257 Yellow Perch      TL metric  75    RLP    100 -5.386  3.23
#>                   source
#> 257 Willis et al. (1991)
wsVal("Yellow Perch",units="metric")   # same as default
#>          species measure  units ref method min.TL    int slope
#> 257 Yellow Perch      TL metric  75    RLP    100 -5.386  3.23
#>                   source
#> 257 Willis et al. (1991)
wsVal("Yellow Perch",units="English")
#>          species measure   units ref method min.TL    int slope
#> 256 Yellow Perch      TL English  75    RLP      4 -3.506  3.23
#>                   source
#> 256 Willis et al. (1991)
wsVal("Yellow Perch",units="English",simplify=TRUE)
#>          species min.TL    int slope
#> 256 Yellow Perch      4 -3.506  3.23

#===== Find equation for Ruffe, demonstrating quadratic formula
wsVal("Ruffe",units="metric",ref=75,simplify=TRUE)
#>     species min.TL max.TL   int slope   quad
#> 191   Ruffe     55    205 -2.58 0.621 0.6073
wsVal("Ruffe",units="metric",ref=50,simplify=TRUE)
#>     species min.TL max.TL     int  slope   quad
#> 190   Ruffe     55    205 -3.3524 1.3969 0.4054

#===== Find equation for Brown Trout, which has equations for sub-groups
#-----   demonstrating use of group= argument
wsVal("Brown Trout",group="lotic")
#>        species group measure  units ref method min.TL    int slope
#> 53 Brown Trout lotic      TL metric  75    RLP    140 -4.867  2.96
#>                       source
#> 53 Milewski and Brown (1994)
wsVal("Brown Trout",group="lentic")
#>        species  group measure  units ref method min.TL    int slope
#> 51 Brown Trout lentic      TL metric  75    RLP    140 -5.422 3.194
#>                      source
#> 51 Hyatt and Hubert (2001b)
#-----   demonstrating group combined in species name, so no group= arg
wsVal("Brown Trout (lotic)")
#>                species measure  units ref method min.TL    int slope
#> 57 Brown Trout (lotic)      TL metric  75    RLP    140 -4.867  2.96
#>                       source
#> 57 Milewski and Brown (1994)
wsVal("Brown Trout (lentic)")
#>                 species measure  units ref method min.TL    int slope
#> 55 Brown Trout (lentic)      TL metric  75    RLP    140 -5.422 3.194
#>                      source
#> 55 Hyatt and Hubert (2001b)

#===== Add Ws & Wr values to a data frame (for one species) ... also see wrAdd()
#----- Example data from PSDWRtest, simplify variables for this example
yepdf <- subset(PSDWRtest,species=="Yellow Perch",select=c("species","len","wt"))
str(yepdf)
#> 'data.frame':    170 obs. of  3 variables:
#>  $ species: chr  "Yellow Perch" "Yellow Perch" "Yellow Perch" "Yellow Perch" ...
#>  $ len    : num  140 128 133 123 158 146 125 104 127 165 ...
#>  $ wt     : num  37.5 22.6 26.9 26.8 NA 37.2 28.2 12.5 23.7 59.4 ...

#----- Get Ws equation info
( wsYEP <- wsVal("Yellow Perch",units="metric") )
#>          species measure  units ref method min.TL    int slope
#> 257 Yellow Perch      TL metric  75    RLP    100 -5.386  3.23
#>                   source
#> 257 Willis et al. (1991)

#----- Add Ws (eqn is on log10-log10 scale ... so log10 length, 10^ result)
yepdf$ws <- 10^(wsYEP[["int"]]+wsYEP[["slope"]]*log10(yepdf$len))

#----- Change Ws for fish less than min.TL to NA
yepdf$ws[yepdf$len<wsYEP[["min.TL"]]] <- NA

#----- Add Wr
yepdf$wr <- yepdf$wt/yepdf$ws*100

#----- Examine results
peek(yepdf,n=6)
#>           species len    wt        ws        wr
#> 847  Yellow Perch 140  37.5  35.15552 106.66887
#> 880  Yellow Perch 146  42.8  40.25870 106.31243
#> 914  Yellow Perch 186 101.2  88.00894 114.98831
#> 948  Yellow Perch 203 122.5 116.73799 104.93585
#> 982  Yellow Perch 215 125.6 140.53232  89.37446
#> 1016 Yellow Perch 322 520.0 518.05333 100.37577

#----- Same as above but using dplyr
if (require(dplyr)) {
  yepdf <- PSDWRtest %>% filter(species=="Yellow Perch") %>% select(species,len,wt) %>%
    mutate(ws=10^(wsYEP[["int"]]+wsYEP[["slope"]]*log10(len)),
           ws=ifelse(len<wsYEP[["min.TL"]],NA,ws),
           wr=wt/ws*100)
  peek(yepdf,n=6)
}
#>          species len    wt        ws        wr
#> 1   Yellow Perch 140  37.5  35.15552 106.66887
#> 34  Yellow Perch 146  42.8  40.25870 106.31243
#> 68  Yellow Perch 186 101.2  88.00894 114.98831
#> 102 Yellow Perch 203 122.5 116.73799 104.93585
#> 136 Yellow Perch 215 125.6 140.53232  89.37446
#> 170 Yellow Perch 322 520.0 518.05333 100.37577
```
