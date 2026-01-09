# Convenience function for calculating PSD-X and PSD X-Y values.

Convenience function for calculating (traditional) PSD-X and
(incremental) PSD X-Y values for all Gabelhouse lengths and increments
thereof.

## Usage

``` r
psdCalc(
  formula,
  data,
  species,
  group = NULL,
  units = c("mm", "cm", "in"),
  method = c("multinomial", "binomial"),
  conf.level = 0.95,
  addLens = NULL,
  addNames = NULL,
  justAdds = FALSE,
  what = c("all", "traditional", "incremental", "none"),
  drop0Est = TRUE,
  showIntermediate = FALSE,
  digits = 0
)
```

## Arguments

- formula:

  A formula of the form `~length` where “length” generically represents
  a variable in `data` that contains the observed lengths. Note that
  this formula may only contain one variable and it must be numeric.

- data:

  A data.frame that minimally contains the observed lengths given in the
  variable in `formula`.

- species:

  A string that contains the species name for which Gabelhouse lengths
  exist. See
  [`psdVal`](https://fishr-core-team.github.io/FSA/reference/psdVal.md)
  for details. See details for how to use this function for species for
  which Gabelhouse lengths are not defined.

- group:

  A string that contains the sub-group of `species` for which to find
  the Gabelhouse lengths; e.g., things like “landlocked”, “lentic”.

- units:

  A string that indicates the type of units used for the lengths.
  Choices are `mm` for millimeters (DEFAULT), `cm` for centimeters, and
  `in` for inches.

- method:

  A character that identifies the confidence interval method to use. See
  details in
  [`psdCI`](https://fishr-core-team.github.io/FSA/reference/psdCI.md).

- conf.level:

  A number that indicates the level of confidence to use for
  constructing confidence intervals (default is `0.95`).

- addLens:

  A numeric vector that contains minimum lengths for additional
  categories. See
  [`psdVal`](https://fishr-core-team.github.io/FSA/reference/psdVal.md)
  for details.

- addNames:

  A string vector that contains names for the additional lengths added
  with `addLens`. See
  [`psdVal`](https://fishr-core-team.github.io/FSA/reference/psdVal.md)
  for details.

- justAdds:

  A logical that indicates whether just the values related to the
  lengths in `addLens` should be returned.

- what:

  A string that indicates the type of PSD values that will be printed.
  See details.

- drop0Est:

  A logical that indicates whether the PSD values that are zero should
  be dropped from the output.

- showIntermediate:

  A logical that indicates whether the number of fish in the category
  and the number of stock fish (i.e., “intermediate” values) should be
  included in the returned matrix. Default is to not include these
  values.

- digits:

  A numeric that indicates the number of decimals to round the result
  to. Default is zero digits following the recommendation of Neumann and
  Allen (2007).

## Value

A matrix with columns that contain the computed PSD-X or PSD X-Y values
and associated confidence intervals. If `showIntermediate=TRUE` then the
number of fish in the category and the number of stock fish will also be
shown.

## Details

Computes the (traditional) PSD-X and (incremental) PSD X-Y values, with
associated confidence intervals, for each Gabelhouse length. All PSD-X
and PSD X-Y values are printed if `what="all"` (DEFAULT), only PSD-X
values are printed if `what="traditional"`, only PSD X-Y values are
printed if `what="incremental"`, and nothing is printed (but the matrix
is still returned) if `what="none"`.

Confidence intervals can be computed with either the multinomial
(DEFAULT) or binomial distribution as set in `method`See details in
[`psdCI`](https://fishr-core-team.github.io/FSA/reference/psdCI.md) for
more information.

This function may be used for species for which Gabelhouse length
categories are not defined. In this case do not include a name in
`species`, but define at least two lengths in `addLens` where the first
category MUST be called “stock”.

See examples and [this
article](https://fishr-core-team.github.io/FSA/articles/Computing_PSDs.html)
for a demonstration.

## Testing

Point estimate calculations match those constructed "by hand."

## IFAR Chapter

6-Size Structure.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r)Chapman
& Hall/CRC, Boca Raton, FL.

Guy, C.S., R.M. Neumann, and D.W. Willis. 2006. New terminology for
proportional stock density (PSD) and relative stock density (RSD):
proportional size structure (PSS). Fisheries 31:86-87. \[Was (is?) from
http://pubstorage.sdstate.edu/wfs/415-F.pdf.\]

Guy, C.S., R.M. Neumann, D.W. Willis, and R.O. Anderson2006Proportional
size distribution (PSD): A further refinement of population size
structure index terminology. Fisheries. 32:348. \[Was (is?) from
http://pubstorage.sdstate.edu/wfs/450-F.pdf.\]

Neumann, R.M. and Allen, M.S. 2007. Size structure. In Guy, C.S. and
Brown, M.L., editors, Analysis and Interpretation of Freshwater
Fisheries Data, Chapter 9, pages 375-421. American Fisheries Society,
Bethesda, MD.

Willis, D.W., B.R. Murphy, and C.S. Guy. 1993. Stock density indices:
development, use, and limitations. Reviews in Fisheries Science
1:203-222. \[Was (is?) from
http://web1.cnre.vt.edu/murphybr/web/Readings/Willis%20et%20al.pdf.\]

## See also

See
[`psdVal`](https://fishr-core-team.github.io/FSA/reference/psdVal.md),
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
#===== Simple (typical) uses with just Gabelhouse lengths
tmp <- subset(PSDWRtest,species=="Yellow Perch",select=c("species","len"))

#----- All results
psdCalc(~len,data=tmp,species="Yellow Perch")
#> Warning: Some category sample size <20, some CI coverage may be lower than 95%.
#>         Estimate 95% LCI 95% UCI
#> PSD-Q         48      36      59
#> PSD-P         31      21      42
#> PSD-M          5       0      11
#> PSD S-Q       52      41      64
#> PSD Q-P       16       8      25
#> PSD P-M       26      16      36
#> PSD M-T        5       0      11

#----- Just the traditional indices
psdCalc(~len,data=tmp,species="Yellow Perch",what="traditional")
#> Warning: Some category sample size <20, some CI coverage may be lower than 95%.
#>       Estimate 95% LCI 95% UCI
#> PSD-Q       48      36      59
#> PSD-P       31      21      42
#> PSD-M        5       0      11

#----- Just the incremental indices
psdCalc(~len,data=tmp,species="Yellow Perch",what="incremental")
#> Warning: Some category sample size <20, some CI coverage may be lower than 95%.
#>         Estimate 95% LCI 95% UCI
#> PSD S-Q       52      41      64
#> PSD Q-P       16       8      25
#> PSD P-M       26      16      36
#> PSD M-T        5       0      11

#===== Add a custom length of interest (to the Gabelhouse lengths)
psdCalc(~len,data=tmp,species="Yellow Perch",addLens=150)
#> Warning: Some category sample size <20, some CI coverage may be lower than 95%.
#>           Estimate 95% LCI 95% UCI
#> PSD-150         84      74      93
#> PSD-Q           48      35      60
#> PSD-P           31      20      43
#> PSD-M            5       0      11
#> PSD S-150       16       7      26
#> PSD 150-Q       36      24      48
#> PSD Q-P         16       7      26
#> PSD P-M         26      15      37
#> PSD M-T          5       0      11

#----- Additional lengths can be named
psdCalc(~len,data=tmp,species="Yellow Perch",addLens=c("minLen"=150))
#> Warning: Some category sample size <20, some CI coverage may be lower than 95%.
#>              Estimate 95% LCI 95% UCI
#> PSD-minLen         84      74      93
#> PSD-Q              48      35      60
#> PSD-P              31      20      43
#> PSD-M               5       0      11
#> PSD S-minLen       16       7      26
#> PSD minLen-Q       36      24      48
#> PSD Q-P            16       7      26
#> PSD P-M            26      15      37
#> PSD M-T             5       0      11
psdCalc(~len,data=tmp,species="Yellow Perch",
        addLens=c("minLen"=150,"maxslot"=275))
#> Warning: Some category sample size <20, some CI coverage may be lower than 95%.
#>               Estimate 95% LCI 95% UCI
#> PSD-minLen          84      74      94
#> PSD-Q               48      34      61
#> PSD-P               31      19      44
#> PSD-maxslot         14       5      24
#> PSD-M                5       0      12
#> PSD S-minLen        16       6      26
#> PSD minLen-Q        36      23      49
#> PSD Q-P             16       6      26
#> PSD P-maxslot       17       7      27
#> PSD maxslot-M        9       1      17
#> PSD M-T              5       0      12

#----- Can return just those results that include the additional lengths
psdCalc(~len,data=tmp,species="Yellow Perch",
        addLens=c("minSlot"=150,"maxSlot"=275),justAdds=TRUE)
#> Warning: Some category sample size <20, some CI coverage may be lower than 95%.
#>               Estimate 95% LCI 95% UCI
#> PSD-minSlot         84      74      94
#> PSD-maxSlot         14       5      24
#> PSD S-minSlot       16       6      26
#> PSD minSlot-Q       36      23      49
#> PSD P-maxSlot       17       7      27
#> PSD maxSlot-M        9       1      17
psdCalc(~len,data=tmp,species="Yellow Perch",
        addLens=c("minSlot"=150,"maxSlot"=275),justAdds=TRUE,what="traditional")
#> Warning: Some category sample size <20, some CI coverage may be lower than 95%.
#>             Estimate 95% LCI 95% UCI
#> PSD-minSlot       84      74      94
#> PSD-maxSlot       14       5      24

#===== Can show intermediate values (num in category and in stock)
psdCalc(~len,data=tmp,species="Yellow Perch",showInterm=TRUE)
#> Warning: Some category sample size <20, some CI coverage may be lower than 95%.
#>         num stock Estimate 95% LCI 95% UCI
#> PSD-Q    71   147       48      36      59
#> PSD-P    46   147       31      21      42
#> PSD-M     7   147        5       0      11
#> PSD S-Q  76   147       52      41      64
#> PSD Q-P  24   147       16       8      25
#> PSD P-M  38   147       26      16      36
#> PSD M-T   7   147        5       0      11

#===== Some species require use of group
tmp <- subset(PSDWRtest,species=="Brown Trout" & location=="Trout Lake",
              select=c("species","location","len"))
peek(tmp,n=6)
#>         species   location len
#> 201 Brown Trout Trout Lake 128
#> 216 Brown Trout Trout Lake 164
#> 232 Brown Trout Trout Lake 205
#> 248 Brown Trout Trout Lake 261
#> 264 Brown Trout Trout Lake 208
#> 280 Brown Trout Trout Lake 350

# will err because Brown Trout has sub-groups in PSDlit
# psdCalc(~len,data=tmp,species="Brown Trout")
psdCalc(~len,data=tmp,species="Brown Trout",group="lotic")
#> Warning: Some category sample size <20, some CI coverage may be lower than 95%.
#>         Estimate 95% LCI 95% UCI
#> PSD-Q         58      43      72
#> PSD-P         10       1      19
#> PSD S-Q       42      28      57
#> PSD Q-P       48      33      62
#> PSD P-M       10       1      19
psdCalc(~len,data=tmp,species="Brown Trout (lotic)")
#> Warning: Some category sample size <20, some CI coverage may be lower than 95%.
#>         Estimate 95% LCI 95% UCI
#> PSD-Q         58      43      72
#> PSD-P         10       1      19
#> PSD S-Q       42      28      57
#> PSD Q-P       48      33      62
#> PSD P-M       10       1      19

#===== For species not in PSDlit ... don't include species and use addLens
#      Note these are same data as above, but treated as species not in PSDlit
psdCalc(~len,data=tmp,addLens=c("stock"=130,"quality"=200,"preferred"=250,
                                "memorable"=300,"trophy"=380))
#> Warning: Some category sample size <20, some CI coverage may be lower than 95%.
#>         Estimate 95% LCI 95% UCI
#> PSD-Q         71      56      85
#> PSD-P         32      17      47
#> PSD-M          9       0      18
#> PSD S-Q       29      15      44
#> PSD Q-P       38      23      54
#> PSD P-M       23      10      36
#> PSD M-T        9       0      18
                                    
```
