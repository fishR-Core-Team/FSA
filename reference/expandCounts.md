# Repeat individual fish data (including lengths) from tallied counts.

Repeat individual fish data, including lengths, from tallied counts and,
optionally, add a random digit to length measurements to simulate actual
length of fish in the bin. This is useful as a precursor to summaries
that require information, e.g., lengths, of individual fish (e.g.,
length frequency histograms, means lengths).

## Usage

``` r
expandCounts(
  data,
  cform,
  lform = NULL,
  removeCount = TRUE,
  lprec = 0.1,
  new.name = "newlen",
  cwid = 0,
  verbose = TRUE,
  ...
)
```

## Arguments

- data:

  A data.frame that contains variables in `cform` and `lform`.

- cform:

  A formula of the form `~countvar` where `countvar` generically
  represents the variable in `data` that contains the counts of
  individuals. See details.

- lform:

  An optional formula of the form `~lowerbin+upperbin` where `lowerbin`
  and `upperbin` generically represent the variables in `data` that
  identify the lower- and upper-values of the length bins. See details.

- removeCount:

  A single logical that indicates if the variable that contains the
  counts of individuals (as given in `cform`) should be removed form the
  returned data.frame. The default is `TRUE` such that the variable will
  be removed as the returned data.frame contains individuals and the
  counts of individuals in tallied bins is not relevant to an
  individual.

- lprec:

  A single numeric that controls the precision to which the random
  lengths are recorded. See details.

- new.name:

  A single string that contains a name for the new length variable if
  random lengths are to be created.

- cwid:

  A single positive numeric that will be added to the lower length bin
  value in instances where the count exceeds one but only a lower (and
  not an upper) length were recorded. See details.

- verbose:

  A logical indicating whether progress message should be printed or
  not.

- ...:

  Not yet implemented.

## Value

A data.frame of the same structure as `data` except that the variable in
`cform` may be deleted and the variable in `new.name` may be added. The
returned data.frame will have more rows than `data` because of the
potential addition of new individuals expanded from the counts in
`cform`.

## Details

Fisheries data may be recorded as tallied counts in the field. For
example, field biologists may have simply recorded that there were 10
fish in one group, 15 in another, etc. More specifically, the biologist
may have recorded that there were 10 male Bluegill from the first
sampling event between 100 and 124 mm, 15 male Bluegill from the first
sampling event between 125 and 149 mm, and so on. At times, it may be
necessary to expand these counts such that the repeated information
appears in individual rows in a new data.frame. In this specific
example, the tallied counts would be repeated such that the male,
Bluegill, first sampling event, 100-124 mm information would be repeated
10 times; the male, Bluegill, first sampling event, 125-149 mm
information would be repeated 15 times, and so on. This function
facilitates this type of expansion.

Length data has often been collected in a “binned-and-tallied” format
(e.g., 10 fish in the 100-124 mm group, 15 in the 125-149 mm group,
etc.). This type of data collection does not facilitate easy or precise
calculations of summary statistics of length (i.e., mean and standard
deviations of length). Expanding the data as described above does not
solve this problem because the length data are still essentially
categorical (i.e., which group the fish belongs to rather than what it's
actual length is). To facilitate computation of summary statistics, the
data can be expanded as described above and then a length can be
randomly selected from within the recorded length bin to serve as a
“measured” length for that fish. This function performs this type of
expansion by randomly selecting the length from a uniform distribution
within the length bin (e.g., each value between 100 and 124 mm has the
same probability of being selected).

This function makes some assumptions for some coding situations. First,
it assumes that all `lowerbin` values are actually lower than all
`upperbin` values. The function will throw an error if this is not true.
Second, it assumes that if a `lowerbin` but no `upperbin` value is given
then the `lowerbin` value is the exact measurement for those fish.
Third, it assumes that if an `upperbin` but no `lowerbin` value is given
that this is a data entry error and that the `upperbin` value should be
the `lowerbin` value. Fourth, it assumes that it is a data entry error
if `varcount` is zero or `NA` and `lowerbin` or `upperbin` contains
values (i.e., why would there be lengths if no fish were captured?).

## See also

See
[`expandLenFreq`](https://fishr-core-team.github.io/FSA/reference/expandLenFreq.md)
for expanding length frequencies where individual fish measurements were
made on individual fish in a subsample and the remaining fish were
simply counted.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
# all need expansion
( d1 <- data.frame(name=c("Johnson","Johnson","Jones","Frank","Frank","Max"),
                   lwr.bin=c(15,15.5,16,16,17,17),
                   upr.bin=c(15.5,16,16.5,16.5,17.5,17.5),
                   freq=c(6,4,2,3,1,1)) )
#>      name lwr.bin upr.bin freq
#> 1 Johnson    15.0    15.5    6
#> 2 Johnson    15.5    16.0    4
#> 3   Jones    16.0    16.5    2
#> 4   Frank    16.0    16.5    3
#> 5   Frank    17.0    17.5    1
#> 6     Max    17.0    17.5    1
expandCounts(d1,~freq)
#> Results messages from expandCounts():
#>   2 rows had an individual measurement.
#>   4 rows with multiple measurements were expanded to 15 rows of individual measurements.
#>       name lwr.bin upr.bin
#> 1    Frank    17.0    17.5
#> 2      Max    17.0    17.5
#> 3  Johnson    15.0    15.5
#> 4  Johnson    15.0    15.5
#> 5  Johnson    15.0    15.5
#> 6  Johnson    15.0    15.5
#> 7  Johnson    15.0    15.5
#> 8  Johnson    15.0    15.5
#> 9  Johnson    15.5    16.0
#> 10 Johnson    15.5    16.0
#> 11 Johnson    15.5    16.0
#> 12 Johnson    15.5    16.0
#> 13   Jones    16.0    16.5
#> 14   Jones    16.0    16.5
#> 15   Frank    16.0    16.5
#> 16   Frank    16.0    16.5
#> 17   Frank    16.0    16.5
expandCounts(d1,~freq,~lwr.bin+upr.bin)
#> Results messages from expandCounts():
#>   2 rows had an individual measurement.
#>   4 rows with multiple measurements were expanded to 15 rows of individual measurements.
#>       name lwr.bin upr.bin newlen         lennote
#> 1    Frank    17.0    17.5   17.5 Expanded length
#> 2      Max    17.0    17.5   17.3 Expanded length
#> 3  Johnson    15.0    15.5   15.2 Expanded length
#> 4  Johnson    15.0    15.5   15.3 Expanded length
#> 5  Johnson    15.0    15.5   15.4 Expanded length
#> 6  Johnson    15.0    15.5   15.1 Expanded length
#> 7  Johnson    15.0    15.5   15.5 Expanded length
#> 8  Johnson    15.0    15.5   15.1 Expanded length
#> 9  Johnson    15.5    16.0   15.8 Expanded length
#> 10 Johnson    15.5    16.0   15.9 Expanded length
#> 11 Johnson    15.5    16.0   15.6 Expanded length
#> 12 Johnson    15.5    16.0   15.7 Expanded length
#> 13   Jones    16.0    16.5   16.1 Expanded length
#> 14   Jones    16.0    16.5   16.3 Expanded length
#> 15   Frank    16.0    16.5   16.2 Expanded length
#> 16   Frank    16.0    16.5   16.1 Expanded length
#> 17   Frank    16.0    16.5   16.1 Expanded length

# some need expansion
( d2 <- data.frame(name=c("Johnson","Johnson","Jones","Frank","Frank","Max"),
                   lwr.bin=c(15,15.5,16,16,17.1,17.3),
                   upr.bin=c(15.5,16,16.5,16.5,17.1,17.3),
                   freq=c(6,4,2,3,1,1)) )
#>      name lwr.bin upr.bin freq
#> 1 Johnson    15.0    15.5    6
#> 2 Johnson    15.5    16.0    4
#> 3   Jones    16.0    16.5    2
#> 4   Frank    16.0    16.5    3
#> 5   Frank    17.1    17.1    1
#> 6     Max    17.3    17.3    1
expandCounts(d2,~freq)
#> Results messages from expandCounts():
#>   2 rows had an individual measurement.
#>   4 rows with multiple measurements were expanded to 15 rows of individual measurements.
#>       name lwr.bin upr.bin
#> 1    Frank    17.1    17.1
#> 2      Max    17.3    17.3
#> 3  Johnson    15.0    15.5
#> 4  Johnson    15.0    15.5
#> 5  Johnson    15.0    15.5
#> 6  Johnson    15.0    15.5
#> 7  Johnson    15.0    15.5
#> 8  Johnson    15.0    15.5
#> 9  Johnson    15.5    16.0
#> 10 Johnson    15.5    16.0
#> 11 Johnson    15.5    16.0
#> 12 Johnson    15.5    16.0
#> 13   Jones    16.0    16.5
#> 14   Jones    16.0    16.5
#> 15   Frank    16.0    16.5
#> 16   Frank    16.0    16.5
#> 17   Frank    16.0    16.5
expandCounts(d2,~freq,~lwr.bin+upr.bin)
#> Results messages from expandCounts():
#>   2 rows had an individual measurement.
#>   4 rows with multiple measurements were expanded to 15 rows of individual measurements.
#>       name lwr.bin upr.bin newlen         lennote
#> 1    Frank    17.1    17.1   17.1 Observed length
#> 2      Max    17.3    17.3   17.3 Observed length
#> 3  Johnson    15.0    15.5   15.5 Expanded length
#> 4  Johnson    15.0    15.5   15.3 Expanded length
#> 5  Johnson    15.0    15.5   15.5 Expanded length
#> 6  Johnson    15.0    15.5   15.5 Expanded length
#> 7  Johnson    15.0    15.5   15.3 Expanded length
#> 8  Johnson    15.0    15.5   15.5 Expanded length
#> 9  Johnson    15.5    16.0   15.9 Expanded length
#> 10 Johnson    15.5    16.0   16.0 Expanded length
#> 11 Johnson    15.5    16.0   15.6 Expanded length
#> 12 Johnson    15.5    16.0   15.8 Expanded length
#> 13   Jones    16.0    16.5   16.2 Expanded length
#> 14   Jones    16.0    16.5   16.4 Expanded length
#> 15   Frank    16.0    16.5   16.2 Expanded length
#> 16   Frank    16.0    16.5   16.4 Expanded length
#> 17   Frank    16.0    16.5   16.4 Expanded length

# none need expansion
( d3 <- data.frame(name=c("Johnson","Johnson","Jones","Frank","Frank","Max"),
                   lwr.bin=c(15,15.5,16,16,17.1,17.3),
                   upr.bin=c(15,15.5,16,16,17.1,17.3),
                   freq=c(6,4,2,3,1,1)) )
#>      name lwr.bin upr.bin freq
#> 1 Johnson    15.0    15.0    6
#> 2 Johnson    15.5    15.5    4
#> 3   Jones    16.0    16.0    2
#> 4   Frank    16.0    16.0    3
#> 5   Frank    17.1    17.1    1
#> 6     Max    17.3    17.3    1
expandCounts(d3,~freq)
#> Results messages from expandCounts():
#>   2 rows had an individual measurement.
#>   4 rows with multiple measurements were expanded to 15 rows of individual measurements.
#>       name lwr.bin upr.bin
#> 1    Frank    17.1    17.1
#> 2      Max    17.3    17.3
#> 3  Johnson    15.0    15.0
#> 4  Johnson    15.0    15.0
#> 5  Johnson    15.0    15.0
#> 6  Johnson    15.0    15.0
#> 7  Johnson    15.0    15.0
#> 8  Johnson    15.0    15.0
#> 9  Johnson    15.5    15.5
#> 10 Johnson    15.5    15.5
#> 11 Johnson    15.5    15.5
#> 12 Johnson    15.5    15.5
#> 13   Jones    16.0    16.0
#> 14   Jones    16.0    16.0
#> 15   Frank    16.0    16.0
#> 16   Frank    16.0    16.0
#> 17   Frank    16.0    16.0
expandCounts(d3,~freq,~lwr.bin+upr.bin)
#> Results messages from expandCounts():
#>   2 rows had an individual measurement.
#>   4 rows with multiple measurements were expanded to 15 rows of individual measurements.
#>       name lwr.bin upr.bin newlen         lennote
#> 1    Frank    17.1    17.1   17.1 Observed length
#> 2      Max    17.3    17.3   17.3 Observed length
#> 3  Johnson    15.0    15.0   15.0 Observed length
#> 4  Johnson    15.0    15.0   15.0 Observed length
#> 5  Johnson    15.0    15.0   15.0 Observed length
#> 6  Johnson    15.0    15.0   15.0 Observed length
#> 7  Johnson    15.0    15.0   15.0 Observed length
#> 8  Johnson    15.0    15.0   15.0 Observed length
#> 9  Johnson    15.5    15.5   15.5 Observed length
#> 10 Johnson    15.5    15.5   15.5 Observed length
#> 11 Johnson    15.5    15.5   15.5 Observed length
#> 12 Johnson    15.5    15.5   15.5 Observed length
#> 13   Jones    16.0    16.0   16.0 Observed length
#> 14   Jones    16.0    16.0   16.0 Observed length
#> 15   Frank    16.0    16.0   16.0 Observed length
#> 16   Frank    16.0    16.0   16.0 Observed length
#> 17   Frank    16.0    16.0   16.0 Observed length

# some need expansion, but different bin widths
( d4 <- data.frame(name=c("Johnson","Johnson","Jones","Frank","Frank","Max"),
                   lwr.bin=c(15,  15,  16,  16,  17.1,17.3),
                   upr.bin=c(15.5,15.9,16.5,16.9,17.1,17.3),
                   freq=c(6,4,2,3,1,1)) )
#>      name lwr.bin upr.bin freq
#> 1 Johnson    15.0    15.5    6
#> 2 Johnson    15.0    15.9    4
#> 3   Jones    16.0    16.5    2
#> 4   Frank    16.0    16.9    3
#> 5   Frank    17.1    17.1    1
#> 6     Max    17.3    17.3    1
expandCounts(d4,~freq)
#> Results messages from expandCounts():
#>   2 rows had an individual measurement.
#>   4 rows with multiple measurements were expanded to 15 rows of individual measurements.
#>       name lwr.bin upr.bin
#> 1    Frank    17.1    17.1
#> 2      Max    17.3    17.3
#> 3  Johnson    15.0    15.5
#> 4  Johnson    15.0    15.5
#> 5  Johnson    15.0    15.5
#> 6  Johnson    15.0    15.5
#> 7  Johnson    15.0    15.5
#> 8  Johnson    15.0    15.5
#> 9  Johnson    15.0    15.9
#> 10 Johnson    15.0    15.9
#> 11 Johnson    15.0    15.9
#> 12 Johnson    15.0    15.9
#> 13   Jones    16.0    16.5
#> 14   Jones    16.0    16.5
#> 15   Frank    16.0    16.9
#> 16   Frank    16.0    16.9
#> 17   Frank    16.0    16.9
expandCounts(d4,~freq,~lwr.bin+upr.bin)
#> Results messages from expandCounts():
#>   2 rows had an individual measurement.
#>   4 rows with multiple measurements were expanded to 15 rows of individual measurements.
#>       name lwr.bin upr.bin newlen         lennote
#> 1    Frank    17.1    17.1   17.1 Observed length
#> 2      Max    17.3    17.3   17.3 Observed length
#> 3  Johnson    15.0    15.5   15.0 Expanded length
#> 4  Johnson    15.0    15.5   15.2 Expanded length
#> 5  Johnson    15.0    15.5   15.1 Expanded length
#> 6  Johnson    15.0    15.5   15.0 Expanded length
#> 7  Johnson    15.0    15.5   15.1 Expanded length
#> 8  Johnson    15.0    15.5   15.1 Expanded length
#> 9  Johnson    15.0    15.9   15.7 Expanded length
#> 10 Johnson    15.0    15.9   15.6 Expanded length
#> 11 Johnson    15.0    15.9   15.6 Expanded length
#> 12 Johnson    15.0    15.9   15.0 Expanded length
#> 13   Jones    16.0    16.5   16.0 Expanded length
#> 14   Jones    16.0    16.5   16.2 Expanded length
#> 15   Frank    16.0    16.9   16.1 Expanded length
#> 16   Frank    16.0    16.9   16.6 Expanded length
#> 17   Frank    16.0    16.9   16.3 Expanded length

# some need expansion but include zeros and NAs for counts
( d2a <- data.frame(name=c("Johnson","Johnson","Jones","Frank","Frank","Max","Max","Max","Max"),
                    lwr.bin=c(15,  15.5,16  ,16  ,17.1,17.3,NA,NA,NA),
                    upr.bin=c(15.5,16  ,16.5,16.5,17.1,17.3,NA,NA,NA),
                    freq=c(6,4,2,3,1,1,NA,0,NA)) )
#>      name lwr.bin upr.bin freq
#> 1 Johnson    15.0    15.5    6
#> 2 Johnson    15.5    16.0    4
#> 3   Jones    16.0    16.5    2
#> 4   Frank    16.0    16.5    3
#> 5   Frank    17.1    17.1    1
#> 6     Max    17.3    17.3    1
#> 7     Max      NA      NA   NA
#> 8     Max      NA      NA    0
#> 9     Max      NA      NA   NA
expandCounts(d2a,~freq,~lwr.bin+upr.bin)
#> Results messages from expandCounts():
#>   Rows "7", "8", and "9" had zero or no counts in freq.
#>   2 rows had an individual measurement.
#>   4 rows with multiple measurements were expanded to 15 rows of individual measurements.
#>       name lwr.bin upr.bin newlen         lennote
#> 1      Max      NA      NA     NA Observed length
#> 2      Max      NA      NA     NA Observed length
#> 3      Max      NA      NA     NA Observed length
#> 4    Frank    17.1    17.1   17.1 Observed length
#> 5      Max    17.3    17.3   17.3 Observed length
#> 6  Johnson    15.0    15.5   15.0 Expanded length
#> 7  Johnson    15.0    15.5   15.1 Expanded length
#> 8  Johnson    15.0    15.5   15.5 Expanded length
#> 9  Johnson    15.0    15.5   15.3 Expanded length
#> 10 Johnson    15.0    15.5   15.0 Expanded length
#> 11 Johnson    15.0    15.5   15.1 Expanded length
#> 12 Johnson    15.5    16.0   15.7 Expanded length
#> 13 Johnson    15.5    16.0   15.5 Expanded length
#> 14 Johnson    15.5    16.0   15.8 Expanded length
#> 15 Johnson    15.5    16.0   15.9 Expanded length
#> 16   Jones    16.0    16.5   16.2 Expanded length
#> 17   Jones    16.0    16.5   16.1 Expanded length
#> 18   Frank    16.0    16.5   16.3 Expanded length
#> 19   Frank    16.0    16.5   16.3 Expanded length
#> 20   Frank    16.0    16.5   16.3 Expanded length
 
# some need expansion but include NAs for upper values
( d2b <- data.frame(name=c("Johnson","Johnson","Jones","Frank","Frank","Max"),
                    lwr.bin=c(15,  15.5,16  ,16  ,17.1,17.3),
                    upr.bin=c(NA  ,NA  ,16.5,16.5,17.1,17.3),
                    freq=c(6,4,2,3,1,1)) )
#>      name lwr.bin upr.bin freq
#> 1 Johnson    15.0      NA    6
#> 2 Johnson    15.5      NA    4
#> 3   Jones    16.0    16.5    2
#> 4   Frank    16.0    16.5    3
#> 5   Frank    17.1    17.1    1
#> 6     Max    17.3    17.3    1
expandCounts(d2b,~freq,~lwr.bin+upr.bin)
#> Results messages from expandCounts():
#>   2 rows had an individual measurement.
#>   4 rows with multiple measurements were expanded to 15 rows of individual measurements.
#>       name lwr.bin upr.bin newlen         lennote
#> 1    Frank    17.1    17.1   17.1 Observed length
#> 2      Max    17.3    17.3   17.3 Observed length
#> 3  Johnson    15.0    15.0   15.0 Observed length
#> 4  Johnson    15.0    15.0   15.0 Observed length
#> 5  Johnson    15.0    15.0   15.0 Observed length
#> 6  Johnson    15.0    15.0   15.0 Observed length
#> 7  Johnson    15.0    15.0   15.0 Observed length
#> 8  Johnson    15.0    15.0   15.0 Observed length
#> 9  Johnson    15.5    15.5   15.5 Observed length
#> 10 Johnson    15.5    15.5   15.5 Observed length
#> 11 Johnson    15.5    15.5   15.5 Observed length
#> 12 Johnson    15.5    15.5   15.5 Observed length
#> 13   Jones    16.0    16.5   16.1 Expanded length
#> 14   Jones    16.0    16.5   16.4 Expanded length
#> 15   Frank    16.0    16.5   16.0 Expanded length
#> 16   Frank    16.0    16.5   16.4 Expanded length
#> 17   Frank    16.0    16.5   16.5 Expanded length
 
# some need expansion but include NAs for upper values
( d2c <- data.frame(name=c("Johnson","Johnson","Jones","Frank","Frank","Max"),
                    lwr.bin=c(NA,NA,  16  ,16  ,17.1,17.3),
                    upr.bin=c(15,15.5,16.5,16.5,17.1,17.3),
                    freq=c(6,4,2,3,1,1)) )
#>      name lwr.bin upr.bin freq
#> 1 Johnson      NA    15.0    6
#> 2 Johnson      NA    15.5    4
#> 3   Jones    16.0    16.5    2
#> 4   Frank    16.0    16.5    3
#> 5   Frank    17.1    17.1    1
#> 6     Max    17.3    17.3    1
expandCounts(d2c,~freq,~lwr.bin+upr.bin)
#> Results messages from expandCounts():
#>   2 rows had an individual measurement.
#>   4 rows with multiple measurements were expanded to 15 rows of individual measurements.
#>       name lwr.bin upr.bin newlen         lennote
#> 1    Frank    17.1    17.1   17.1 Observed length
#> 2      Max    17.3    17.3   17.3 Observed length
#> 3  Johnson    15.0    15.0   15.0 Observed length
#> 4  Johnson    15.0    15.0   15.0 Observed length
#> 5  Johnson    15.0    15.0   15.0 Observed length
#> 6  Johnson    15.0    15.0   15.0 Observed length
#> 7  Johnson    15.0    15.0   15.0 Observed length
#> 8  Johnson    15.0    15.0   15.0 Observed length
#> 9  Johnson    15.5    15.5   15.5 Observed length
#> 10 Johnson    15.5    15.5   15.5 Observed length
#> 11 Johnson    15.5    15.5   15.5 Observed length
#> 12 Johnson    15.5    15.5   15.5 Observed length
#> 13   Jones    16.0    16.5   16.2 Expanded length
#> 14   Jones    16.0    16.5   16.3 Expanded length
#> 15   Frank    16.0    16.5   16.0 Expanded length
#> 16   Frank    16.0    16.5   16.4 Expanded length
#> 17   Frank    16.0    16.5   16.0 Expanded length

if (FALSE) { # \dontrun{
##!!##!!## Change path to where example file is and then run to demo

## Read in datafile (note periods in names)
df <- read.csv("c:/aaawork/consulting/R_WiDNR/Statewide/Surveysummaries2010.csv")
str(df) 
## narrow variables for simplicity
df1 <- df[,c("County","Waterbody.Name","Survey.Year","Gear","Species",
             "Number.of.Fish","Length.or.Lower.Length.IN","Length.Upper.IN",
             "Weight.Pounds","Gender")]
## Sum the count to see how many fish there should be after expansion
sum(df1$Number.of.Fish)

## Simple expansion
df2 <- expandCounts(df1,~Number.of.Fish)

## Same expansion but include random component to lengths (thus new variable)
##   also note default lprec=0.1
df3 <- expandCounts(df1,~Number.of.Fish,~Length.or.Lower.Length.IN+Length.Upper.IN)

} # }
```
