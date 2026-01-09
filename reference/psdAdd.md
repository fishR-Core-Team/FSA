# Creates a vector of Gabelhouse lengths for each species in an entire data frame.

Creates a vector of the Gabelhouse lengths specific to a species for all
individuals in an entire data frame.

## Usage

``` r
psdAdd(len, ...)

# Default S3 method
psdAdd(
  len,
  species,
  thesaurus = NULL,
  group = NULL,
  units = c("mm", "cm", "in"),
  use.names = TRUE,
  as.fact = ifelse(is.null(addLens), use.names, FALSE),
  addLens = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'formula'
psdAdd(
  len,
  data = NULL,
  thesaurus = NULL,
  group = NULL,
  units = c("mm", "cm", "in"),
  use.names = TRUE,
  as.fact = ifelse(is.null(addLens), use.names, FALSE),
  addLens = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- len:

  A numeric vector that contains lengths measurements or a formula of
  the form `len~spec` where “len” generically represents the length
  variable and “spec” generically represents the species variable. Note
  that this formula can only contain two variables and must have the
  length variable on the left-hand-side and the species variable on the
  right-hand-side.

- ...:

  Not used.

- species:

  A character or factor vector that contains the species names. Ignored
  if `len` is a formula.

- thesaurus:

  A named list for providing alternative species names (the values in
  the list) that correspond to specific names in `PSDlit` (the names in
  the list). See details and examples.

- group:

  A named list that provides specific choices for `group` for species
  for which more than one set of Gabelhouse lengths exists in
  [`PSDlit`](https://fishr-core-team.github.io/FSA/reference/PSDlit.md).

- units:

  A string that indicates the type of units used for the lengths.
  Choices are `mm` for millimeters (DEFAULT), `cm` for centimeters, and
  `in` for inches.

- use.names:

  A logical that indicates whether the vector returned is numeric
  (`=FALSE`) or string (`=TRUE`; default) representations of the
  Gabelhouse lengths. See details.

- as.fact:

  A logical that indicates that the new variable should be returned as a
  factor (`=TRUE`) or not (`=FALSE`). Defaults to same as `use.names`
  unless `addLens` is not `NULL`, in which case it will default to
  `FALSE`. See details.

- addLens:

  A named list with (possibly named) numeric vectors of lengths that
  should be used in addition to the Gabelhouse lengths for the species
  that form the names in the list. See examples.

- verbose:

  A logical that indicates whether detailed messages about species
  without Gabelhouse lengths or with no recorded values should be
  printed or not.

- data:

  A data.frame that minimally contains the length measurements and
  species names if `len` is a formula.

## Value

A numeric or factor vector that contains the Gabelhouse length
categories.

## Details

This computes a vector that contains the Gabelhouse lengths specific to
each species for all individuals in an entire data frame. The vector can
be appended to an existing data.frame to create a variable that contains
the Gabelhouse lengths for each individual. The Gabelhouse length value
will be `NA` for each individual for which Gabelhouse length definitions
do not exist in
[`PSDlit`](https://fishr-core-team.github.io/FSA/reference/PSDlit.md).
Species names in the data.frame must be the same as those used in
[`PSDlit`](https://fishr-core-team.github.io/FSA/reference/PSDlit.md)
(i.e., same spelling and capitalization; use
[`psdVal()`](https://fishr-core-team.github.io/FSA/reference/psdVal.md)
to see the list of species).

The `thesaurus` argument may be used to relate alternate species names
to the species names used in `PSDlit`. For example, you (or your data)
may use “Bluegill Sunfish”, but “Bluegill” is used in `PSDlit`. The
alternate species name can be used here if it is defined in a named
vector (or list) given to `thesarus=`. The alternate species name is the
value and the species name in `PSDlit` is the name in this vector/list -
e.g., `c("Bluegill"="Bluegill Sunfish")`. See the examples for a
demonstration.

Some species have length categories separated by sub-group. For example,
length categories exist for both lentic and lotic populations of Brown
Trout. The length values for a sub-group may be obtained by either
including the species name in `species` and the sub-group name in
`group` or by using the combined species and sub-group name, with the
sub-group name in parentheses, in `species`. Both methods are
demonstrated in the examples. Note that an error is returned if a
species has sub-groups but neither method is used to define the
sub-group.#'

Individuals shorter than “stock” length will be listed as `substock` if
`use.names=TRUE` or `0` if `use.names=FALSE`.

Additional lengths to be used for a species may be included by giving a
named list with vectors of additional lengths in `addLens`. Note,
however, that `as.fact` will be reset to `FALSE` if `addLens` are
specified, as there is no way to order the names (i.e., factor levels)
for all species when additional lengths are used.

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

[`psdVal`](https://fishr-core-team.github.io/FSA/reference/psdVal.md),
[`psdCalc`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md),
[`psdPlot`](https://fishr-core-team.github.io/FSA/reference/psdPlot.md),
[`PSDlit`](https://fishr-core-team.github.io/FSA/reference/PSDlit.md),
and [`wrAdd`](https://fishr-core-team.github.io/FSA/reference/wrAdd.md)
for related functions. See `mapvalues` for help in changing species
names to match those in
[`PSDlit`](https://fishr-core-team.github.io/FSA/reference/PSDlit.md).

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
#===== Simple examples -- 2 species, no groups, names as in PSDlit
#----- Isolate simple data from PSDWRtest
tmp <- subset(PSDWRtest,
              species %in% c("Yellow Perch","Largemouth Bass"),
              select=c("species","len"))
peek(tmp,n=6)
#>              species len
#> 412  Largemouth Bass 183
#> 463  Largemouth Bass 319
#> 860     Yellow Perch 180
#> 912     Yellow Perch 170
#> 964     Yellow Perch 266
#> 1016    Yellow Perch 322

#----- Add variable using category names -- non-formula notation
tmp$PSD <- psdAdd(tmp$len,tmp$species)
peek(tmp,n=6)
#>              species len       PSD
#> 412  Largemouth Bass 183  substock
#> 463  Largemouth Bass 319   quality
#> 860     Yellow Perch 180     stock
#> 912     Yellow Perch 170     stock
#> 964     Yellow Perch 266 preferred
#> 1016    Yellow Perch 322 memorable

#----- Add variable using category names -- formula notation
tmp$PSD1 <- psdAdd(len~species,data=tmp)
peek(tmp,n=6)
#>              species len       PSD      PSD1
#> 412  Largemouth Bass 183  substock  substock
#> 463  Largemouth Bass 319   quality   quality
#> 860     Yellow Perch 180     stock     stock
#> 912     Yellow Perch 170     stock     stock
#> 964     Yellow Perch 266 preferred preferred
#> 1016    Yellow Perch 322 memorable memorable

#----- Add variable using length values as names
tmp$PSD2 <- psdAdd(len~species,data=tmp,use.names=FALSE)
peek(tmp,n=6)
#>              species len       PSD      PSD1 PSD2
#> 412  Largemouth Bass 183  substock  substock    0
#> 463  Largemouth Bass 319   quality   quality  300
#> 860     Yellow Perch 180     stock     stock  130
#> 912     Yellow Perch 170     stock     stock  130
#> 964     Yellow Perch 266 preferred preferred  250
#> 1016    Yellow Perch 322 memorable memorable  300

#----- Same as above but using dplyr
if (require(dplyr)) {
  tmp <- tmp %>%
    mutate(PSD1A=psdAdd(len,species),
           PSD2A=psdAdd(len,species,use.names=FALSE))
  peek(tmp,n=6)
}
#>              species len       PSD      PSD1 PSD2     PSD1A PSD2A
#> 412  Largemouth Bass 183  substock  substock    0  substock     0
#> 463  Largemouth Bass 319   quality   quality  300   quality   300
#> 860     Yellow Perch 180     stock     stock  130     stock   130
#> 912     Yellow Perch 170     stock     stock  130     stock   130
#> 964     Yellow Perch 266 preferred preferred  250 preferred   250
#> 1016    Yellow Perch 322 memorable memorable  300 memorable   300

#===== Add lengths besides Gabelhouse lengths (start over with same simple data)
tmp <- subset(PSDWRtest,
              species %in% c("Yellow Perch","Largemouth Bass"),
              select=c("species","len"))

#----- Add a "minimum length" for one species
tmp$PSD3 <- psdAdd(len~species,data=tmp,
                   addLens=list("Yellow Perch"=c("minLen"=225)))
tmp$PSD3A <- psdAdd(len~species,data=tmp,
                    addLens=list("Yellow Perch"=225))
tmp$PSD3B <- psdAdd(len~species,data=tmp,
                    addLens=list("Yellow Perch"=c("minLen"=225)),use.names=FALSE)
head(tmp,n=6)
#>             species len     PSD3    PSD3A PSD3B
#> 412 Largemouth Bass 183 substock substock     0
#> 413 Largemouth Bass 208    stock    stock   200
#> 414 Largemouth Bass 200    stock    stock   200
#> 415 Largemouth Bass 200    stock    stock   200
#> 416 Largemouth Bass 191 substock substock     0
#> 417 Largemouth Bass 207    stock    stock   200

#----- Add add'l lengths and names for multiple species
tmp$psd4 <- psdAdd(len~species,data=tmp,
                   addLens=list("Yellow Perch"=175,
                                "Largemouth Bass"=c(254,306)))
peek(tmp,n=20)
#>              species len      PSD3     PSD3A PSD3B      psd4
#> 412  Largemouth Bass 183  substock  substock     0  substock
#> 425  Largemouth Bass 202     stock     stock   200     stock
#> 438  Largemouth Bass 213     stock     stock   200     stock
#> 452  Largemouth Bass 296     stock     stock   200       254
#> 466  Largemouth Bass 295     stock     stock   200       254
#> 479  Largemouth Bass 316   quality   quality   300       306
#> 493  Largemouth Bass 335   quality   quality   300       306
#> 852     Yellow Perch 146     stock     stock   130     stock
#> 865     Yellow Perch 170     stock     stock   130     stock
#> 879     Yellow Perch 182     stock     stock   130       175
#> 893     Yellow Perch 172     stock     stock   130     stock
#> 907     Yellow Perch 148     stock     stock   130     stock
#> 920     Yellow Perch 135     stock     stock   130     stock
#> 934     Yellow Perch 141     stock     stock   130     stock
#> 948     Yellow Perch 203   quality   quality   200   quality
#> 961     Yellow Perch 244    minLen       225   225   quality
#> 975     Yellow Perch 249    minLen       225   225   quality
#> 989     Yellow Perch 228    minLen       225   225   quality
#> 1002    Yellow Perch 306 memorable memorable   300 memorable
#> 1016    Yellow Perch 322 memorable memorable   300 memorable

#===== Handle additional species in PSDlit but named differently
#----- Isolate different species data from PSDWRtest
tmp <- subset(PSDWRtest,
              species %in% c("Bluegill Sunfish","Lean Lake Trout"),
              select=c("species","len"))

#----- No "Bluegill Sunfish" in PSDlit, use thesaurus to note this is "Bluegill"
#        Note: "Lean Lake Trout" not processed as not in PSDlit
tmp$psd5 <- psdAdd(len~species,data=tmp,
                   thesaurus=c("Bluegill"="Bluegill Sunfish"))
#> Species in the data with no Gabelhouse (PSD) lengths in `PSDlit`: "Lean
#>   Lake Trout".
peek(tmp,n=6)
#>              species len    psd5
#> 1   Bluegill Sunfish 107   stock
#> 44  Bluegill Sunfish 154 quality
#> 88  Bluegill Sunfish 172 quality
#> 513  Lean Lake Trout 500    <NA>
#> 557  Lean Lake Trout 478    <NA>
#> 601  Lean Lake Trout 829    <NA>

#----- Process multiple species in PSDlit with different names
#        Note: Can still use addLens=, but with original name
thes <- c("Bluegill"="Bluegill Sunfish","Lake Trout"="Lean Lake Trout")
tmp$psd6 <- psdAdd(len~species,data=tmp,thesaurus=thes)
tmp$psd7 <- psdAdd(len~species,data=tmp,thesaurus=thes,
                   addLens=list("Bluegill Sunfish"=c("minLen"=175)))
peek(tmp,n=20)
#>              species len      psd5      psd6      psd7
#> 1   Bluegill Sunfish 107     stock     stock     stock
#> 12  Bluegill Sunfish 108     stock     stock     stock
#> 23  Bluegill Sunfish  85     stock     stock     stock
#> 35  Bluegill Sunfish 136     stock     stock     stock
#> 46  Bluegill Sunfish 167   quality   quality   quality
#> 58  Bluegill Sunfish 165   quality   quality   quality
#> 69  Bluegill Sunfish 164   quality   quality   quality
#> 81  Bluegill Sunfish 144     stock     stock     stock
#> 93  Bluegill Sunfish 103     stock     stock     stock
#> 104 Bluegill Sunfish 156   quality   quality   quality
#> 116 Bluegill Sunfish 215 preferred preferred preferred
#> 508  Lean Lake Trout 274      <NA>  substock  substock
#> 520  Lean Lake Trout 448      <NA>     stock     stock
#> 532  Lean Lake Trout 487      <NA>     stock     stock
#> 543  Lean Lake Trout 512      <NA>   quality   quality
#> 555  Lean Lake Trout 521      <NA>   quality   quality
#> 566  Lean Lake Trout 503      <NA>   quality   quality
#> 578  Lean Lake Trout 781      <NA> preferred preferred
#> 589  Lean Lake Trout 770      <NA> preferred preferred
#> 601  Lean Lake Trout 829      <NA> memorable memorable

#===== Example for a species with sub-groups but only one sub-group in data
#----- Isolate species data from PSDWRtest ... only Brook Trout has sub-group
tmp <- subset(PSDWRtest,
              species %in% c("Yellow Perch","Brook Trout"),
              select=c("species","len"))

#----- This will err as Brook Trout has sub-groups in PSDlit (as message notes)
# tmp$psd8 <- psdAdd(len~species,data=tmp)

#----- Can choose "overall" sub-group with group=
tmp$psd8 <- psdAdd(len~species,data=tmp,
                   group=list("Brook Trout"="overall"))
peek(tmp,n=10)
#>           species len      psd8
#> 121   Brook Trout 147  substock
#> 148   Brook Trout 241     stock
#> 176   Brook Trout 250     stock
#> 849  Yellow Perch 133     stock
#> 877  Yellow Perch 101  substock
#> 905  Yellow Perch 123  substock
#> 933  Yellow Perch 141     stock
#> 960  Yellow Perch 277 preferred
#> 988  Yellow Perch 257 preferred
#> 1016 Yellow Perch 322 memorable

#----- Or can create species name with sub-group name in parentheses
#        Note: this is more useful in next examples
tmp$species2 <- ifelse(tmp$species=="Brook Trout","Brook Trout (overall)",
                       tmp$species)
tmp$psd8A <- psdAdd(len~species2,data=tmp) # note use of species2
peek(tmp,n=10)
#>           species len      psd8              species2     psd8A
#> 121   Brook Trout 147  substock Brook Trout (overall)  substock
#> 148   Brook Trout 241     stock Brook Trout (overall)     stock
#> 176   Brook Trout 250     stock Brook Trout (overall)     stock
#> 849  Yellow Perch 133     stock          Yellow Perch     stock
#> 877  Yellow Perch 101  substock          Yellow Perch  substock
#> 905  Yellow Perch 123  substock          Yellow Perch  substock
#> 933  Yellow Perch 141     stock          Yellow Perch     stock
#> 960  Yellow Perch 277 preferred          Yellow Perch preferred
#> 988  Yellow Perch 257 preferred          Yellow Perch preferred
#> 1016 Yellow Perch 322 memorable          Yellow Perch memorable

#===== Example for species with more than one sub-group in data
#----- Isolate species data from PSDWRtest ... Brown Trout has two sub-groups
tmp <- subset(PSDWRtest,
              species %in% c("Yellow Perch","Largemouth Bass","Brown Trout"),
              select=c("species","len","location"))
peek(tmp,n=10)
#>              species len     location
#> 201      Brown Trout 128   Trout Lake
#> 249      Brown Trout 269   Trout Lake
#> 298      Brown Trout 207 Brushy Creek
#> 346      Brown Trout 298 Brushy Creek
#> 427  Largemouth Bass 198    Bass Lake
#> 476  Largemouth Bass 285    Bass Lake
#> 870     Yellow Perch 190    Bass Lake
#> 918     Yellow Perch 197    Bass Lake
#> 967     Yellow Perch 239    Bass Lake
#> 1016    Yellow Perch 322    Bass Lake

#----- Must create a species name variable with sub-groups in parentheses
#        Note: there are likely many ways to do this specific to each use-case
tmp$species2 <- tmp$species
tmp$species2[tmp$species=="Brown Trout" & 
             tmp$location=="Trout Lake"] <- "Brown Trout (lotic)"
tmp$species2[tmp$species=="Brown Trout" & 
             tmp$location=="Brushy Creek"] <- "Brown Trout (lentic)"
peek(tmp,n=10)
#>              species len     location             species2
#> 201      Brown Trout 128   Trout Lake  Brown Trout (lotic)
#> 249      Brown Trout 269   Trout Lake  Brown Trout (lotic)
#> 298      Brown Trout 207 Brushy Creek Brown Trout (lentic)
#> 346      Brown Trout 298 Brushy Creek Brown Trout (lentic)
#> 427  Largemouth Bass 198    Bass Lake      Largemouth Bass
#> 476  Largemouth Bass 285    Bass Lake      Largemouth Bass
#> 870     Yellow Perch 190    Bass Lake         Yellow Perch
#> 918     Yellow Perch 197    Bass Lake         Yellow Perch
#> 967     Yellow Perch 239    Bass Lake         Yellow Perch
#> 1016    Yellow Perch 322    Bass Lake         Yellow Perch

tmp$psd9 <- psdAdd(len~species2,data=tmp)
peek(tmp,n=10)
#>              species len     location             species2      psd9
#> 201      Brown Trout 128   Trout Lake  Brown Trout (lotic)  substock
#> 249      Brown Trout 269   Trout Lake  Brown Trout (lotic)   quality
#> 298      Brown Trout 207 Brushy Creek Brown Trout (lentic)     stock
#> 346      Brown Trout 298 Brushy Creek Brown Trout (lentic)     stock
#> 427  Largemouth Bass 198    Bass Lake      Largemouth Bass  substock
#> 476  Largemouth Bass 285    Bass Lake      Largemouth Bass     stock
#> 870     Yellow Perch 190    Bass Lake         Yellow Perch     stock
#> 918     Yellow Perch 197    Bass Lake         Yellow Perch     stock
#> 967     Yellow Perch 239    Bass Lake         Yellow Perch   quality
#> 1016    Yellow Perch 322    Bass Lake         Yellow Perch memorable
```
