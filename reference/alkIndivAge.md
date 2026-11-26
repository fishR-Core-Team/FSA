# Use an age-length key to assign age to individuals in the unaged sample.

Use either the semi- or completely-random methods from Isermann and
Knight (2005) to assign ages to individual fish in the unaged sample
according to the information in an age-length key supplied by the user.

## Usage

``` r
alkIndivAge(
  key,
  formula,
  data,
  type = c("SR", "CR"),
  breaks = NULL,
  seed = NULL
)
```

## Arguments

- key:

  A numeric matrix that contains the age-length key. The format of this
  matrix is important. See details.

- formula:

  A formula of the form `age~length` where `age` generically represents
  the variable that will contain the estimated ages once the key is
  applied (i.e., should currently contain no values) and `length`
  generically represents the variable that contains the known length
  measurements. If only `~length` is used, then a new variable called
  “age” will be created in the resulting data frame.

- data:

  A data.frame that minimally contains the length measurements and
  possibly contains a variable that will receive the age assignments as
  given in `formula`.

- type:

  A string that indicates whether to use the semi-random (`type="SR"`,
  default) or completely-random (`type="CR"`) methods for assigning ages
  to individual fish. See the [IFAR
  chapter](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r)
  for more details.

- breaks:

  A numeric vector of lower values that define the length intervals. See
  details.

- seed:

  A single numeric that is given to `set.seed` to set the random seed.
  This allows repeatability of results.

## Value

The original data.frame in `data` with assigned ages added to the column
supplied in `formula` or in an additional column labeled as `age`. See
details.

## Details

The age-length key in `key` must have length intervals as rows and ages
as columns. The row names of `key` (i.e., `rownames(key)`) must contain
the minimum values of each length interval (e.g., if an interval is
100-109, then the corresponding row name must be 100). The column names
of `key` (i.e., `colnames(key)`) must contain the age values (e.g., the
columns can NOT be named with “age.1”, for example).

The length intervals in the rows of `key` must contain all of the length
intervals present in the unaged sample to which the age-length key is to
be applied (i.e., sent in the `length` portion of the `formula`). If
this constraint is not met, then the function will stop with an error
message.

If `breaks=NULL`, then the length intervals for the unaged sample will
be determined with a starting interval at the minimum value of the row
names in `key` and a width of the length intervals as determined by the
minimum difference in adjacent row names of `key`. If length intervals
of differing widths were used when constructing `key`, then those breaks
should be supplied to `breaks=`. Use of `breaks=` may be useful when
“uneven” length interval widths were used because the lengths in the
unaged sample are not fully represented in the aged sample. See the
examples.

Assigned ages will be stored in the column identified on the
left-hand-side of `formula` (if the formula has both a left- and
right-hand-side). If this variable is missing in `formula`, then the new
column will be labeled with `age`.

## Testing

The `type="SR"` method worked perfectly on a small example. The
`type="SR"` method provides results that reasonably approximate the
results from
[`alkAgeDist`](https://fishr-core-team.github.io/FSA/reference/alkAgeDist.md)
and
[`alkMeanVar`](https://fishr-core-team.github.io/FSA/reference/alkMeanVar.md),
which suggests that the age assessments are reasonable.

## IFAR Chapter

5-Age-Length Key.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

Isermann, D.A. and C.T. Knight. 2005. A computer program for age-length
keys incorporating age assignment to individual fish. North American
Journal of Fisheries Management, 25:1153-1160. \[Was (is?) from
http://www.tandfonline.com/doi/abs/10.1577/M04-130.1.\]

## See also

See
[`alkAgeDist`](https://fishr-core-team.github.io/FSA/reference/alkAgeDist.md)
and
[`alkMeanVar`](https://fishr-core-team.github.io/FSA/reference/alkMeanVar.md)
for alternative methods to derived age distributions and mean (and SD)
values for each age. See
[`alkPlot`](https://fishr-core-team.github.io/FSA/reference/alkPlot.md)
for methods to visualize age-length keys.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>. This is largely an R version of
the SAS code provided by Isermann and Knight (2005).

## Examples

``` r
## First Example -- Even breaks for length categories
WR1 <- WR79
# add length categories (width=5)
WR1$LCat <- lencat(WR1$len,w=5)
# isolate aged and unaged samples
WR1.age <- subset(WR1, !is.na(age))
WR1.len <- subset(WR1, is.na(age))
# note no ages in unaged sample
head(WR1.len)
#>   ID len age LCat
#> 1  1  37  NA   35
#> 2  2  37  NA   35
#> 3  3  39  NA   35
#> 4  4  37  NA   35
#> 7  7  42  NA   40
#> 8  8  42  NA   40
# create age-length key
raw <- xtabs(~LCat+age,data=WR1.age)
( WR1.key <- prop.table(raw, margin=1) )
#>      age
#> LCat           4          5          6          7          8          9
#>   35  1.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   40  1.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   45  1.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   50  1.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   55  1.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   60  0.60000000 0.40000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   65  0.00000000 1.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   70  0.00000000 1.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   75  0.00000000 0.88888889 0.11111111 0.00000000 0.00000000 0.00000000
#>   80  0.00000000 0.25000000 0.75000000 0.00000000 0.00000000 0.00000000
#>   85  0.00000000 0.00000000 0.90909091 0.09090909 0.00000000 0.00000000
#>   90  0.00000000 0.00000000 0.26315789 0.63157895 0.10526316 0.00000000
#>   95  0.00000000 0.00000000 0.05882353 0.70588235 0.17647059 0.00000000
#>   100 0.00000000 0.00000000 0.00000000 0.55555556 0.16666667 0.27777778
#>   105 0.00000000 0.00000000 0.00000000 0.28571429 0.42857143 0.14285714
#>   110 0.00000000 0.00000000 0.00000000 0.20000000 0.20000000 0.20000000
#>   115 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>      age
#> LCat          10         11
#>   35  0.00000000 0.00000000
#>   40  0.00000000 0.00000000
#>   45  0.00000000 0.00000000
#>   50  0.00000000 0.00000000
#>   55  0.00000000 0.00000000
#>   60  0.00000000 0.00000000
#>   65  0.00000000 0.00000000
#>   70  0.00000000 0.00000000
#>   75  0.00000000 0.00000000
#>   80  0.00000000 0.00000000
#>   85  0.00000000 0.00000000
#>   90  0.00000000 0.00000000
#>   95  0.05882353 0.00000000
#>   100 0.00000000 0.00000000
#>   105 0.14285714 0.00000000
#>   110 0.20000000 0.20000000
#>   115 1.00000000 0.00000000
# apply the age-length key
WR1.len <- alkIndivAge(WR1.key,age~len,data=WR1.len)
# now there are ages
head(WR1.len)
#>   ID len age LCat
#> 1  1  37   4   35
#> 2  2  37   4   35
#> 3  3  39   4   35
#> 4  4  37   4   35
#> 7  7  42   4   40
#> 8  8  42   4   40
# combine orig age & new ages
WR1.comb <- rbind(WR1.age, WR1.len)
# mean length-at-age
Summarize(len~age,data=WR1.comb,digits=2)
#>   age   n   mean   sd min     Q1 median     Q3 max
#> 1   4 987  51.86 5.13  35  48.00   52.0  56.00  64
#> 2   5 395  71.78 5.26  60  68.00   72.0  76.00  84
#> 3   6 272  86.79 4.76  75  83.00   87.0  89.00  99
#> 4   7 447  97.56 5.17  85  93.00   97.0 102.00 113
#> 5   8 146 101.19 5.81  90  96.00  101.0 106.75 113
#> 6   9  78 103.83 3.15 100 102.00  103.0 105.50 113
#> 7  10  38 105.03 7.30  95  97.25  106.0 109.75 119
#> 8  11   6 112.17 0.98 111 111.25  112.5 113.00 113
# age frequency distribution
( af <- xtabs(~age,data=WR1.comb) )
#> age
#>   4   5   6   7   8   9  10  11 
#> 987 395 272 447 146  78  38   6 
# proportional age distribution
( ap <- prop.table(af) )
#> age
#>           4           5           6           7           8           9 
#> 0.416631490 0.166737020 0.114816378 0.188687210 0.061629379 0.032925285 
#>          10          11 
#> 0.016040523 0.002532714 

## Second Example -- length sample does not have an age variable
WR2 <- WR79
# isolate age and unaged samples
WR2.age <- subset(WR2, !is.na(age))
WR2.len <- subset(WR2, is.na(age))
# remove age variable (for demo only)
WR2.len <- WR2.len[,-3]
# add length categories to aged sample
WR2.age$LCat <- lencat(WR2.age$len,w=5)
# create age-length key
raw <- xtabs(~LCat+age,data=WR2.age)
( WR2.key <- prop.table(raw, margin=1) )
#>      age
#> LCat           4          5          6          7          8          9
#>   35  1.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   40  1.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   45  1.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   50  1.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   55  1.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   60  0.60000000 0.40000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   65  0.00000000 1.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   70  0.00000000 1.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   75  0.00000000 0.88888889 0.11111111 0.00000000 0.00000000 0.00000000
#>   80  0.00000000 0.25000000 0.75000000 0.00000000 0.00000000 0.00000000
#>   85  0.00000000 0.00000000 0.90909091 0.09090909 0.00000000 0.00000000
#>   90  0.00000000 0.00000000 0.26315789 0.63157895 0.10526316 0.00000000
#>   95  0.00000000 0.00000000 0.05882353 0.70588235 0.17647059 0.00000000
#>   100 0.00000000 0.00000000 0.00000000 0.55555556 0.16666667 0.27777778
#>   105 0.00000000 0.00000000 0.00000000 0.28571429 0.42857143 0.14285714
#>   110 0.00000000 0.00000000 0.00000000 0.20000000 0.20000000 0.20000000
#>   115 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>      age
#> LCat          10         11
#>   35  0.00000000 0.00000000
#>   40  0.00000000 0.00000000
#>   45  0.00000000 0.00000000
#>   50  0.00000000 0.00000000
#>   55  0.00000000 0.00000000
#>   60  0.00000000 0.00000000
#>   65  0.00000000 0.00000000
#>   70  0.00000000 0.00000000
#>   75  0.00000000 0.00000000
#>   80  0.00000000 0.00000000
#>   85  0.00000000 0.00000000
#>   90  0.00000000 0.00000000
#>   95  0.05882353 0.00000000
#>   100 0.00000000 0.00000000
#>   105 0.14285714 0.00000000
#>   110 0.20000000 0.20000000
#>   115 1.00000000 0.00000000
# apply the age-length key
WR2.len <- alkIndivAge(WR2.key,~len,data=WR2.len)
# add length cat to length sample
WR2.len$LCat <- lencat(WR2.len$len,w=5)
head(WR2.len)
#>   ID len age LCat
#> 1  1  37   4   35
#> 2  2  37   4   35
#> 3  3  39   4   35
#> 4  4  37   4   35
#> 7  7  42   4   40
#> 8  8  42   4   40
# combine orig age & new ages
WR2.comb <- rbind(WR2.age, WR2.len)
Summarize(len~age,data=WR2.comb,digits=2)
#>   age   n   mean   sd min  Q1 median     Q3 max
#> 1   4 986  51.85 5.12  35  48   52.0  56.00  64
#> 2   5 397  71.79 5.33  60  68   72.0  76.00  84
#> 3   6 269  86.74 4.70  75  83   87.0  89.00  99
#> 4   7 448  97.53 5.24  85  93   97.0 102.00 113
#> 5   8 146 101.19 5.68  90  97  102.0 106.00 113
#> 6   9  78 103.77 3.25 100 101  103.0 105.75 114
#> 7  10  39 105.13 6.81  96  98  106.0 109.00 119
#> 8  11   6 112.50 0.55 112 112  112.5 113.00 113

## Third Example -- Uneven breaks for length categories
WR3 <- WR79
# set up uneven breaks
brks <- c(seq(35,100,5),110,130)
WR3$LCat <- lencat(WR3$len,breaks=brks)
WR3.age <- subset(WR3, !is.na(age))
WR3.len <- subset(WR3, is.na(age))
head(WR3.len)
#>   ID len age LCat
#> 1  1  37  NA   35
#> 2  2  37  NA   35
#> 3  3  39  NA   35
#> 4  4  37  NA   35
#> 7  7  42  NA   40
#> 8  8  42  NA   40
raw <- xtabs(~LCat+age,data=WR3.age)
( WR3.key <- prop.table(raw, margin=1) )
#>      age
#> LCat           4          5          6          7          8          9
#>   35  1.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   40  1.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   45  1.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   50  1.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   55  1.00000000 0.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   60  0.60000000 0.40000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   65  0.00000000 1.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   70  0.00000000 1.00000000 0.00000000 0.00000000 0.00000000 0.00000000
#>   75  0.00000000 0.88888889 0.11111111 0.00000000 0.00000000 0.00000000
#>   80  0.00000000 0.25000000 0.75000000 0.00000000 0.00000000 0.00000000
#>   85  0.00000000 0.00000000 0.90909091 0.09090909 0.00000000 0.00000000
#>   90  0.00000000 0.00000000 0.26315789 0.63157895 0.10526316 0.00000000
#>   95  0.00000000 0.00000000 0.05882353 0.70588235 0.17647059 0.00000000
#>   100 0.00000000 0.00000000 0.00000000 0.48000000 0.24000000 0.24000000
#>   110 0.00000000 0.00000000 0.00000000 0.14285714 0.14285714 0.14285714
#>      age
#> LCat          10         11
#>   35  0.00000000 0.00000000
#>   40  0.00000000 0.00000000
#>   45  0.00000000 0.00000000
#>   50  0.00000000 0.00000000
#>   55  0.00000000 0.00000000
#>   60  0.00000000 0.00000000
#>   65  0.00000000 0.00000000
#>   70  0.00000000 0.00000000
#>   75  0.00000000 0.00000000
#>   80  0.00000000 0.00000000
#>   85  0.00000000 0.00000000
#>   90  0.00000000 0.00000000
#>   95  0.05882353 0.00000000
#>   100 0.04000000 0.00000000
#>   110 0.42857143 0.14285714
WR3.len <- alkIndivAge(WR3.key,age~len,data=WR3.len,breaks=brks)
#> Warning: The maximum observed length in the length sample (117) is greater than
#>   the largest length category in the age-length key (110). The last
#>   length category will be treated as all-inclusive.
head(WR3.len)
#>   ID len age LCat
#> 1  1  37   4   35
#> 2  2  37   4   35
#> 3  3  39   4   35
#> 4  4  37   4   35
#> 7  7  42   4   40
#> 8  8  42   4   40
WR3.comb <- rbind(WR3.age, WR3.len)
Summarize(len~age,data=WR3.comb,digits=2)
#>   age   n   mean   sd min  Q1 median     Q3 max
#> 1   4 987  51.87 5.15  35  48     52  56.00  64
#> 2   5 395  71.78 5.35  60  68     72  76.00  84
#> 3   6 270  86.71 4.79  75  83     87  89.00  99
#> 4   7 453  97.78 5.46  85  93     97 102.00 117
#> 5   8 140 100.39 5.39  90  96    101 104.00 113
#> 6   9  78 104.06 3.06 100 102    103 106.75 111
#> 7  10  41 104.93 7.12  95  98    104 112.00 119
#> 8  11   5 112.60 1.14 111 112    113 113.00 114
```
