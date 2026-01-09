# Adds zeros for catches of species not collected in some sampling events.

Adds zeros for catches of species that were not captured in a sampling
event but were captured in at least one other sampling event (i.e., adds
zeros to the data.frame for capture events where a species was not
observed).

## Usage

``` r
addZeroCatch(df, eventvar, specvar, zerovar, na.rm = TRUE)
```

## Arguments

- df:

  A data.frame that contains the capture summary data as described in
  the details.

- eventvar:

  A string for the variable that identifies unique capture events.

- specvar:

  A string or vector of strings for the variable(s) that identify the
  “species” (if multiple variables, could be species, sex, and life
  stage, for example) captured. See examples.

- zerovar:

  A string or vector of strings for the variable(s) that should be set
  equal to zero. See details and examples.

- na.rm:

  A logical that indicates if rows where `specvar` that are `NA` should
  be removed after adding the zeros. See details.

## Value

A data.frame with the same structure as `df` but with rows of zero
observation data appended.

## Details

The data.frame in `df` must contain a column that identifies a unique
capture event (given in `eventvar`), a column with the name for the
species captured (given in `specvar`), and a column that contains the
number of that species captured (potentially given to `zerovar`; see
details). All sampling event and species combinations where catch
information does not exist is identified and a new data.frame that
contains a zero for the catch for all of these combinations is created.
This new data.frame is appended to the original data.frame to construct
a data.frame that contains complete catch information – i.e., including
zeros for species in events where that species was not captured.

The data.frame may contain other information related to the catch, such
as number of recaptured fish, number of fish released, etc. These
additional variables can be included in `zerovar` so that zeros will be
added to these variables as well (e.g., if the catch of the species is
zero, then the number of recaptures must also be zero). All variables
not given in `eventvar`, `specvar`, or `zerovar` will be assumed to be
related to `eventvar` and `specvar` (e.g., date, gear type, and habitat)
and, thus, will be repeated with these variables.

In situations where no fish were captured in some events, the `df` may
contain rows that have a value for `eventvar` but not for `specvar`.
These rows are important because zeros need to be added for each
observed species for these events. However, in these situations, a
`<NA>` species will appear in the resulting data.frame. It is unlikely
that these “missing” species are needed so they will be removed if
`na.rm=TRUE` (DEFAULT) is used.

One should test the results of this function by creating a frequency
table of the `eventvar` or `specvar`. In either case, the table should
contain the same value in each cell of the table. See the examples.

## Note

An error will be returned if either `specvar` or `eventvar` are factors
with any `NA` levels. This usually arises if the data.frame was
subsetted/filtered prior to using `addZeroCatch`. See
[`droplevels`](https://rdrr.io/r/base/droplevels.html) for descriptions
of how to drop unused levels.

## IFAR Chapter

2-Basic Data Manipulations

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

## See also

`complete` in tidyr package.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## Example Data #1 (some nets missing some fish, ancillary net data)
df1 <- data.frame(net=c(1,1,1,2,2,3),
                  eff=c(1,1,1,1,1,1),
                  species=c("BKT","LKT","RBT","BKT","LKT","RBT"),
                  catch=c(3,4,5,5,4,3))
df1
#>   net eff species catch
#> 1   1   1     BKT     3
#> 2   1   1     LKT     4
#> 3   1   1     RBT     5
#> 4   2   1     BKT     5
#> 5   2   1     LKT     4
#> 6   3   1     RBT     3
# not all 1s
xtabs(~net+species,data=df1)
#>    species
#> net BKT LKT RBT
#>   1   1   1   1
#>   2   1   1   0
#>   3   0   0   1

df1mod1 <- addZeroCatch(df1,"net","species",zerovar="catch")
df1mod1
#>   net eff species catch
#> 1   1   1     BKT     3
#> 2   1   1     LKT     4
#> 3   1   1     RBT     5
#> 4   2   1     BKT     5
#> 5   2   1     LKT     4
#> 6   3   1     RBT     3
#> 7   3   1     BKT     0
#> 8   3   1     LKT     0
#> 9   2   1     RBT     0
# check, should all be 3
xtabs(~net,data=df1mod1)
#> net
#> 1 2 3 
#> 3 3 3 
# check, should all be 1
xtabs(~net+species,data=df1mod1)
#>    species
#> net BKT LKT RBT
#>   1   1   1   1
#>   2   1   1   1
#>   3   1   1   1
# correct mean/sd of catches
Summarize(catch~species,data=df1mod1)
#>   species n     mean       sd min  Q1 median Q3 max percZero
#> 1     BKT 3 2.666667 2.516611   0 1.5      3  4   5 33.33333
#> 2     LKT 3 2.666667 2.309401   0 2.0      4  4   4 33.33333
#> 3     RBT 3 2.666667 2.516611   0 1.5      3  4   5 33.33333
# incorrect mean/sd of catches (no zeros)
Summarize(catch~species,data=df1)
#>   species n mean       sd min  Q1 median  Q3 max
#> 1     BKT 2    4 1.414214   3 3.5      4 4.5   5
#> 2     LKT 2    4 0.000000   4 4.0      4 4.0   4
#> 3     RBT 2    4 1.414214   3 3.5      4 4.5   5

# Same as example 1 but with no ancillary data specific to the net number
df2 <- df1[,-2]
df2
#>   net species catch
#> 1   1     BKT     3
#> 2   1     LKT     4
#> 3   1     RBT     5
#> 4   2     BKT     5
#> 5   2     LKT     4
#> 6   3     RBT     3
df1mod2 <- addZeroCatch(df2,"net","species",zerovar="catch")
df1mod2
#>   net species catch
#> 1   1     BKT     3
#> 2   1     LKT     4
#> 3   1     RBT     5
#> 4   2     BKT     5
#> 5   2     LKT     4
#> 6   3     RBT     3
#> 7   3     BKT     0
#> 8   3     LKT     0
#> 9   2     RBT     0
# check, should all be 1
xtabs(~net+species,data=df1mod2)
#>    species
#> net BKT LKT RBT
#>   1   1   1   1
#>   2   1   1   1
#>   3   1   1   1

## Example Data #3 (All nets have same species ... no zeros needed)
df3 <- data.frame(net=c(1,1,1,2,2,2,3,3,3),
                  eff=c(1,1,1,1,1,1,1,1,1),
                  species=c("BKT","LKT","RBT","BKT","LKT",
                            "RBT","BKT","LKT","RBT"),
                  catch=c(3,4,5,5,4,3,3,2,7))
df3
#>   net eff species catch
#> 1   1   1     BKT     3
#> 2   1   1     LKT     4
#> 3   1   1     RBT     5
#> 4   2   1     BKT     5
#> 5   2   1     LKT     4
#> 6   2   1     RBT     3
#> 7   3   1     BKT     3
#> 8   3   1     LKT     2
#> 9   3   1     RBT     7
# should all be 1 for this example
xtabs(~net+species,data=df3)
#>    species
#> net BKT LKT RBT
#>   1   1   1   1
#>   2   1   1   1
#>   3   1   1   1

# should receive a warning and table should still all be 1
df3mod1 <- addZeroCatch(df3,"net","species",zerovar="catch")
#> Warning: All 'eventvar' have all species in 'specvar'; thus, there are no zeros
#>   to add. The original data.frame was returned.
xtabs(~net+species,data=df3mod1)
#>    species
#> net BKT LKT RBT
#>   1   1   1   1
#>   2   1   1   1
#>   3   1   1   1

## Example Data #4 (another variable that needs zeros)
df4 <- df1
df4$recaps <- c(0,0,0,1,2,1)
df4
#>   net eff species catch recaps
#> 1   1   1     BKT     3      0
#> 2   1   1     LKT     4      0
#> 3   1   1     RBT     5      0
#> 4   2   1     BKT     5      1
#> 5   2   1     LKT     4      2
#> 6   3   1     RBT     3      1
# not all 1s
xtabs(~net+species,data=df4)
#>    species
#> net BKT LKT RBT
#>   1   1   1   1
#>   2   1   1   0
#>   3   0   0   1

df4mod1 <- addZeroCatch(df4,"net","species",zerovar=c("catch","recaps"))
# note zeros in both variables
df4mod1
#>   net eff species catch recaps
#> 1   1   1     BKT     3      0
#> 2   1   1     LKT     4      0
#> 3   1   1     RBT     5      0
#> 4   2   1     BKT     5      1
#> 5   2   1     LKT     4      2
#> 6   3   1     RBT     3      1
#> 7   3   1     BKT     0      0
#> 8   3   1     LKT     0      0
#> 9   2   1     RBT     0      0
# check, should all be 1
xtabs(~net+species,data=df4mod1)
#>    species
#> net BKT LKT RBT
#>   1   1   1   1
#>   2   1   1   1
#>   3   1   1   1
# observe difference from next
Summarize(catch~species,data=df4)
#>   species n mean       sd min  Q1 median  Q3 max
#> 1     BKT 2    4 1.414214   3 3.5      4 4.5   5
#> 2     LKT 2    4 0.000000   4 4.0      4 4.0   4
#> 3     RBT 2    4 1.414214   3 3.5      4 4.5   5
Summarize(catch~species,data=df4mod1)
#>   species n     mean       sd min  Q1 median Q3 max percZero
#> 1     BKT 3 2.666667 2.516611   0 1.5      3  4   5 33.33333
#> 2     LKT 3 2.666667 2.309401   0 2.0      4  4   4 33.33333
#> 3     RBT 3 2.666667 2.516611   0 1.5      3  4   5 33.33333
# observe difference from next
Summarize(recaps~species,data=df4)
#>   species n mean        sd min   Q1 median   Q3 max percZero
#> 1     BKT 2  0.5 0.7071068   0 0.25    0.5 0.75   1       50
#> 2     LKT 2  1.0 1.4142136   0 0.50    1.0 1.50   2       50
#> 3     RBT 2  0.5 0.7071068   0 0.25    0.5 0.75   1       50
Summarize(recaps~species,data=df4mod1)
#>   species n      mean        sd min Q1 median  Q3 max percZero
#> 1     BKT 3 0.3333333 0.5773503   0  0      0 0.5   1 66.66667
#> 2     LKT 3 0.6666667 1.1547005   0  0      0 1.0   2 66.66667
#> 3     RBT 3 0.3333333 0.5773503   0  0      0 0.5   1 66.66667

## Example Data #5 (two "specvar"s)
df5 <- df1
df5$sex <- c("m","m","f","m","f","f")
df5
#>   net eff species catch sex
#> 1   1   1     BKT     3   m
#> 2   1   1     LKT     4   m
#> 3   1   1     RBT     5   f
#> 4   2   1     BKT     5   m
#> 5   2   1     LKT     4   f
#> 6   3   1     RBT     3   f
# not all 1s
xtabs(~sex+species+net,data=df5)
#> , , net = 1
#> 
#>    species
#> sex BKT LKT RBT
#>   f   0   0   1
#>   m   1   1   0
#> 
#> , , net = 2
#> 
#>    species
#> sex BKT LKT RBT
#>   f   0   1   0
#>   m   1   0   0
#> 
#> , , net = 3
#> 
#>    species
#> sex BKT LKT RBT
#>   f   0   0   1
#>   m   0   0   0
#> 

df5mod1 <- addZeroCatch(df5,"net",c("species","sex"),zerovar="catch")
df5mod1
#>    net eff species catch sex
#> 1    1   1     BKT     3   m
#> 2    1   1     LKT     4   m
#> 3    1   1     RBT     5   f
#> 4    2   1     BKT     5   m
#> 5    2   1     LKT     4   f
#> 6    3   1     RBT     3   f
#> 7    1   1     BKT     0   f
#> 8    2   1     BKT     0   f
#> 9    3   1     BKT     0   f
#> 10   1   1     LKT     0   f
#> 11   3   1     LKT     0   f
#> 12   2   1     RBT     0   f
#> 13   3   1     BKT     0   m
#> 14   2   1     LKT     0   m
#> 15   3   1     LKT     0   m
#> 16   1   1     RBT     0   m
#> 17   2   1     RBT     0   m
#> 18   3   1     RBT     0   m
# all 1s
xtabs(~sex+species+net,data=df5mod1)
#> , , net = 1
#> 
#>    species
#> sex BKT LKT RBT
#>   f   1   1   1
#>   m   1   1   1
#> 
#> , , net = 2
#> 
#>    species
#> sex BKT LKT RBT
#>   f   1   1   1
#>   m   1   1   1
#> 
#> , , net = 3
#> 
#>    species
#> sex BKT LKT RBT
#>   f   1   1   1
#>   m   1   1   1
#> 
str(df5mod1) 
#> 'data.frame':    18 obs. of  5 variables:
#>  $ net    : num  1 1 1 2 2 3 1 2 3 1 ...
#>  $ eff    : num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ species: chr  "BKT" "LKT" "RBT" "BKT" ...
#>  $ catch  : num  3 4 5 5 4 3 0 0 0 0 ...
#>  $ sex    : chr  "m" "m" "f" "m" ...

## Example Data #6 (three "specvar"s)
df6 <- df5
df6$size <- c("lrg","lrg","lrg","sm","lrg","sm")
df6
#>   net eff species catch sex size
#> 1   1   1     BKT     3   m  lrg
#> 2   1   1     LKT     4   m  lrg
#> 3   1   1     RBT     5   f  lrg
#> 4   2   1     BKT     5   m   sm
#> 5   2   1     LKT     4   f  lrg
#> 6   3   1     RBT     3   f   sm

df6mod1 <- addZeroCatch(df6,"net",c("species","sex","size"),zerovar="catch")
df6mod1
#>    net eff species catch sex size
#> 1    1   1     BKT     3   m  lrg
#> 2    1   1     LKT     4   m  lrg
#> 3    1   1     RBT     5   f  lrg
#> 4    2   1     BKT     5   m   sm
#> 5    2   1     LKT     4   f  lrg
#> 6    3   1     RBT     3   f   sm
#> 7    1   1     BKT     0   f  lrg
#> 8    2   1     BKT     0   f  lrg
#> 9    3   1     BKT     0   f  lrg
#> 10   1   1     LKT     0   f  lrg
#> 11   3   1     LKT     0   f  lrg
#> 12   2   1     RBT     0   f  lrg
#> 13   3   1     RBT     0   f  lrg
#> 14   2   1     BKT     0   m  lrg
#> 15   3   1     BKT     0   m  lrg
#> 16   2   1     LKT     0   m  lrg
#> 17   3   1     LKT     0   m  lrg
#> 18   1   1     RBT     0   m  lrg
#> 19   2   1     RBT     0   m  lrg
#> 20   3   1     RBT     0   m  lrg
#> 21   1   1     BKT     0   f   sm
#> 22   2   1     BKT     0   f   sm
#> 23   3   1     BKT     0   f   sm
#> 24   1   1     LKT     0   f   sm
#> 25   2   1     LKT     0   f   sm
#> 26   3   1     LKT     0   f   sm
#> 27   1   1     RBT     0   f   sm
#> 28   2   1     RBT     0   f   sm
#> 29   1   1     BKT     0   m   sm
#> 30   3   1     BKT     0   m   sm
#> 31   1   1     LKT     0   m   sm
#> 32   2   1     LKT     0   m   sm
#> 33   3   1     LKT     0   m   sm
#> 34   1   1     RBT     0   m   sm
#> 35   2   1     RBT     0   m   sm
#> 36   3   1     RBT     0   m   sm
 
```
