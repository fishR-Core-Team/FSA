# Finds Gabelhouse lengths (for PSD calculations) for a species.

Returns a vector with the five Gabelhouse lengths for a chosen species.

## Usage

``` r
psdVal(
  species = "List",
  group = NULL,
  units = c("mm", "cm", "in"),
  addLens = NULL,
  addNames = NULL,
  incl.zero = TRUE,
  showJustSource = FALSE,
  dat = NULL
)
```

## Arguments

- species:

  A string that contains the species name for which to find Gabelhouse
  lengths. See details.

- group:

  A string that contains the sub-group of `species` for which to find
  the Gabelhouse lengths; e.g., things like “landlocked”, “lentic”.

- units:

  A string that indicates the units for the returned lengths. Choices
  are `mm` for millimeters (DEFAULT), `cm` for centimeters, and `in` for
  inches.

- addLens:

  A numeric vector that contains minimum length definitions for
  additional categories. See details.

- addNames:

  A string vector that contains names for the additional length
  categories added with `addLens`. See details.

- incl.zero:

  A logical that indicates if a zero is included in the first position
  of the returned vector (DEFAULT) or not. This position will be named
  “substock”. See details.

- showJustSource:

  A logical that indicates whether just the literature source
  information should be returned (`TRUE`) or not. If `TRUE` this will
  NOT return any of the Gabelhouse length information.

- dat:

  Data.frame of Gabelhouse length categories for all species. Defaults
  to \`PSDlit\` and is generally not used by the user (this simplifies
  use of this function in `psdAdd`).

## Value

A vector of minimum values for length categories for the chosen species.

## Details

Finds the Gabelhouse lengths from `data(PSDlit)` for the species given
in `species`. The species name must be spelled exactly (including
capitalization) as it appears in `data(PSDlit)`. Type `psdVal()` to see
the list of species and how they are spelled.

Some species have length categories separated by sub-group. For example,
length categories exist for both lentic and lotic populations of Brown
Trout. The length values for a sub-group may be obtained by either
including the species name in `species` and the sub-group name in
`group` or by using the combined species and sub-group name, with the
sub-group name in parentheses, in `species`. Both methods are
demonstrated in the examples. Note that an error is returned if a
species has sub-groups but neither method is used to define the
sub-group.#'

A zero is included in the first position of the returned vector if
`incl.zero=TRUE`. This is useful when computing PSD values with a
data.frame that contains fish smaller than the stock length.

Additional lengths may be added to the returned vector with `addLens`.
Names for these lengths can be included as names in `addLens` or
separately in `addNames`. If `addNames` is NULL and `addLens` is not
named then the default category names will be the lengths from
`addLens`. The `addLens` argument is useful for calculating PSD values
that are different from the Gabelhouse lengths.

See examples and [this
article](https://fishr-core-team.github.io/FSA/articles/Computing_PSDs.html)
for a demonstration.

## IFAR Chapter

6-Size Structure.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

Guy, C.S., R.M. Neumann, and D.W. Willis. 2006. New terminology for
proportional stock density (PSD) and relative stock density (RSD):
proportional size structure (PSS). Fisheries 31:86-87. \[Was (is?) from
http://pubstorage.sdstate.edu/wfs/415-F.pdf.\]

Guy, C.S., R.M. Neumann, D.W. Willis, and R.O. Anderson. 2006.
Proportional size distribution (PSD): A further refinement of population
size structure index terminology. Fisheries 32:348. \[Was (is?) from
http://pubstorage.sdstate.edu/wfs/450-F.pdf.\]

Willis, D.W., B.R. Murphy, and C.S. Guy. 1993. Stock density indices:
development, use, and limitations. Reviews in Fisheries Science
1:203-222. \[Was (is?) from
http://web1.cnre.vt.edu/murphybr/web/Readings/Willis%20et%20al.pdf.\]

## See also

See
[`psdCalc`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md),
[`psdPlot`](https://fishr-core-team.github.io/FSA/reference/psdPlot.md),
[`psdAdd`](https://fishr-core-team.github.io/FSA/reference/psdAdd.md),
[`PSDlit`](https://fishr-core-team.github.io/FSA/reference/PSDlit.md),
[`tictactoe`](https://fishr-core-team.github.io/FSA/reference/tictactoe.md),
[`lencat`](https://fishr-core-team.github.io/FSA/reference/lencat.md),
and
[`rcumsum`](https://fishr-core-team.github.io/FSA/reference/rcumsum.md)
for related functionality.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
#===== List all available species in PSDlit
psdVal()
#> 
#> Species name must be one of following. Be careful of spelling and capitalization.
#>  [1] "Alabama Bass"                "Arctic Grayling"            
#>  [3] "Bighead Carp"                "Bigmouth Buffalo"           
#>  [5] "Black Bullhead"              "Black Carp"                 
#>  [7] "Black Crappie"               "Blue Catfish"               
#>  [9] "Bluegill"                    "Brook Trout"                
#> [11] "Brook Trout (lentic)"        "Brook Trout (lotic)"        
#> [13] "Brook Trout (overall)"       "Brown Bullhead"             
#> [15] "Brown Trout"                 "Brown Trout (lentic)"       
#> [17] "Brown Trout (lotic)"         "Bull Trout"                 
#> [19] "Burbot"                      "Chain Pickerel"             
#> [21] "Channel Catfish"             "Chinook Salmon"             
#> [23] "Chinook Salmon (landlocked)" "Common Carp"                
#> [25] "Cutthroat Trout"             "Cutthroat Trout (lentic)"   
#> [27] "Cutthroat Trout (lotic)"     "Flathead Catfish"           
#> [29] "Flier"                       "Freshwater Drum"            
#> [31] "Gizzard Shad"                "Golden Trout"               
#> [33] "Goldeye"                     "Grass Carp"                 
#> [35] "Green Sunfish"               "Kokanee"                    
#> [37] "Lake Chubsucker"             "Lake Trout"                 
#> [39] "Largemouth Bass"             "Longear Sunfish"            
#> [41] "Longnose Gar"                "Muskellunge"                
#> [43] "Muskellunge (female)"        "Muskellunge (male)"         
#> [45] "Muskellunge (overall)"       "Northern Pike"              
#> [47] "Northern Pikeminnow"         "Northern Snakehead"         
#> [49] "Paddlefish"                  "Paddlefish (female)"        
#> [51] "Paddlefish (male)"           "Paddlefish (overall)"       
#> [53] "Pallid Sturgeon"             "Palmetto Bass"              
#> [55] "Palmetto Bass (original)"    "Pumpkinseed"                
#> [57] "Rainbow Trout"               "Rainbow Trout (lentic)"     
#> [59] "Rainbow Trout (lotic)"       "Redbreast Sunfish"          
#> [61] "Redear Sunfish"              "River Carpsucker"           
#> [63] "Rock Bass"                   "Ruffe"                      
#> [65] "Sauger"                      "Saugeye"                    
#> [67] "Shoal Bass"                  "Shorthead Redhorse"         
#> [69] "Silver Carp"                 "Smallmouth Bass"            
#> [71] "Smallmouth Buffalo "         "Splake"                     
#> [73] "Spotted Bass"                "Spotted Bass (original)"    
#> [75] "Spotted Gar"                 "Spotted Sunfish"            
#> [77] "Striped Bass"                "Striped Bass (landlocked)"  
#> [79] "Striped Bass X White Bass"   "Suwannee Bass"              
#> [81] "Utah Chub"                   "Walleye"                    
#> [83] "Walleye (30-149 mm)"         "Walleye (overall)"          
#> [85] "Warmouth"                    "White Bass"                 
#> [87] "White Catfish"               "White Crappie"              
#> [89] "White Perch"                 "White Sucker"               
#> [91] "Yellow Bass"                 "Yellow Bullhead"            
#> [93] "Yellow Perch"               

#===== Typical usages
psdVal("Bluegill")
#>  substock     stock   quality preferred memorable    trophy 
#>         0        80       150       200       250       300 
psdVal("Bluegill",units="in")
#>  substock     stock   quality preferred memorable    trophy 
#>         0         3         6         8        10        12 
psdVal("Bluegill",units="in",incl.zero=FALSE)
#>     stock   quality preferred memorable    trophy 
#>         3         6         8        10        12 
psdVal("Bluegill",showJustSource=TRUE)
#>    species             source
#> 9 Bluegill Gabelhouse (1984a)

#===== For species that have sub-groups
#-----   using group= argument
psdVal("Brown Trout",group="lentic")
#>  substock     stock   quality preferred memorable    trophy 
#>         0       200       300       400       500       600 
psdVal("Brown Trout",group="lotic")
#>  substock     stock   quality preferred memorable    trophy 
#>         0       150       230       300       380       460 
#-----   group combined in species name, so no group= use
psdVal("Brown Trout (lentic)")
#>  substock     stock   quality preferred memorable    trophy 
#>         0       200       300       400       500       600 

#===== For species with revised values
psdVal("Palmetto Bass")
#>  substock     stock   quality preferred memorable    trophy 
#>         0       250       410       510       610       710 
psdVal("Palmetto Bass (original)")
#>  substock     stock   quality preferred memorable    trophy 
#>         0       200       300       380       510       630 

#===== Adding user-defined categories
#-----   with lengths and names separately in addLens= and addNames=
psdVal("Bluegill",units="in",addLens=7)
#>  substock     stock   quality         7 preferred memorable    trophy 
#>         0         3         6         7         8        10        12 
psdVal("Bluegill",units="in",addLens=7,addNames="MinLen")
#>  substock     stock   quality    MinLen preferred memorable    trophy 
#>         0         3         6         7         8        10        12 
psdVal("Bluegill",units="in",addLens=c(7,9),addNames=c("MinSlot","MaxSlot"))
#>  substock     stock   quality   MinSlot preferred   MaxSlot memorable    trophy 
#>         0         3         6         7         8         9        10        12 
#-----   with a named vector in addLens=
psdVal("Bluegill",units="in",addLens=c("MinLen"=7))
#>  substock     stock   quality    MinLen preferred memorable    trophy 
#>         0         3         6         7         8        10        12 
psdVal("Bluegill",units="in",addLens=c("MinSlot"=7,"MaxSlot"=9))
#>  substock     stock   quality   MinSlot preferred   MaxSlot memorable    trophy 
#>         0         3         6         7         8         9        10        12 
```
