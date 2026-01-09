# Proportions-at-age from an age-length key

Computes the proportions-at-age (with standard errors) in a larger
sample based on an age-length-key created from a subsample of ages
through a two-stage random sampling design. Follows the methods in Quinn
and Deriso (1999).

## Usage

``` r
alkAgeDist(key, lenA.n, len.n)
```

## Arguments

- key:

  A numeric matrix that contains the age-length key. See details.

- lenA.n:

  A numeric vector of sample sizes for each length interval in the *aged
  sample*.

- len.n:

  A numeric vector of sample sizes for each length interval in the
  *complete sample* (i.e., all fish regardless of whether they were aged
  or not).

## Value

A data.frame with as many rows as ages (columns) present in `key` and
the following three variables:

- age The ages.

- prop The proportion of fish at each age.

- se The SE for the proportion of fish at each age.

## Details

The age-length key in `key` must have length intervals as rows and ages
as columns. The row names of `key` (i.e., `rownames(key)`) must contain
the minimum values of each length interval (e.g., if an interval is
100-109 then the corresponding row name must be 100). The column names
of `key` (i.e., `colnames(key)`) must contain the age values (e.g., the
columns can NOT be named with “age.1”, for example).

The length intervals in the rows of `key` must contain all of the length
intervals present in the larger sample. Thus, the length of `len.n`
must, at least, equal the number of rows in `key`. If this constraint is
not met, then the function will stop with an error message.

The values in `lenA.n` are equal to what the row sums of `key` would
have been before `key` was converted to a row proportions table. Thus,
the length of `lenA.n` must also be equal to the number of rows in
`key`. If this constraint is not met, then the function will stop with
an error message.

## Testing

The results from this function perfectly match the results in Table 8.4
(left) of Quinn and Deriso (1999) using
[`SnapperHG2`](https://fishr-core-team.github.io/FSAdata/reference/SnapperHG2.html)
from FSAdata. The results also perfectly match the results from using
[`alkprop`](https://rdrr.io/pkg/fishmethods/man/alkprop.html) in
fishmethods.

## IFAR Chapter

5-Age-Length Key.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

Lai, H.-L. 1987. Optimum allocation for estimating age composition using
age-length key. Fishery Bulletin, 85:179-185.

Lai, H.-L. 1993. Optimum sampling design for using the age-length key to
estimate age composition of a fish population. Fishery Bulletin,
92:382-388.

Quinn, T. J. and R. B. Deriso. 1999. Quantitative Fish Dynamics. Oxford
University Press, New York, New York. 542 pages.

## See also

See
[`alkIndivAge`](https://fishr-core-team.github.io/FSA/reference/alkIndivAge.md)
and related functions for a completely different methodology. See
[`alkprop`](https://rdrr.io/pkg/fishmethods/man/alkprop.html) from
fishmethods for the exact same methodology but with a different format
for the inputs.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## Example -- Even breaks for length categories
WR1 <- WR79
# add length intervals (width=5)
WR1$LCat <- lencat(WR1$len,w=5)
# get number of fish in each length interval in the entire sample
len.n <- xtabs(~LCat,data=WR1)
# isolate aged sample and get number in each length interval
WR1.age <- subset(WR1, !is.na(age))
lenA.n <- xtabs(~LCat,data=WR1.age)
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

# use age-length key to estimate age distribution of all fish
alkAgeDist(WR1.key,lenA.n,len.n)
#>   age        prop          se
#> 1   4 0.416378219 0.013206893
#> 2   5 0.167201351 0.013907259
#> 3   6 0.113882857 0.014938965
#> 4   7 0.189170701 0.021247383
#> 5   8 0.061572157 0.016537338
#> 6   9 0.032955436 0.011718417
#> 7  10 0.016306565 0.008988814
#> 8  11 0.002532714 0.002574054
```
