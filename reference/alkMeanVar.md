# Mean Values-at-age from an age-length key

Computes the mean value-at-age in a larger sample based on an
age-length-key created from a subsample of ages through a two-stage
random sampling design. The mean values could be mean length-, weight-,
or fecundity-at-age, for example. The methods of Bettoli and Miranda
(2001) or Quinn and Deriso (1999) are used. A standard deviation is
computed for the Bettoli and Miranda (2001) method and standard error
for the Quinn and Deriso (1999) method. See the testing section notes.

## Usage

``` r
alkMeanVar(
  key,
  formula,
  data,
  len.n,
  method = c("BettoliMiranda", "QuinnDeriso")
)
```

## Arguments

- key:

  A numeric matrix that contains the age-length key. See details.

- formula:

  A formula of the form `var~lencat+age` where `var` generically
  represents the variable to be summarized (e.g., length, weight,
  fecundity), `lencat` generically represents the variable that contains
  the length intervals, and `age` generically represents the variable
  that contains the assigned ages.

- data:

  A data.frame that minimally contains the length intervals, assessed
  ages, and the variable to be summarized (i.e., this should be the aged
  sample) as given in `formula`.

- len.n:

  A vector of sample sizes for each length interval in the *complete
  sample* (i.e., all fish regardless of whether they were aged or not).

- method:

  A string that indicates which method of calculation should be used.
  See details.

## Value

A data.frame with as many rows as ages (columns) present in `key` and
the following three variables:

- age The ages.

- mean The mean value at each age.

- sd,se The SD if `method="BettoliMiranda"` or SE of the mean if
  `method="QuinnDeriso"` for the value at each age.

## Details

The age-length key `key` must have length intervals as rows and ages as
columns. The row names of `key` (i.e., `rownames(key)`) must contain the
minimum values of each length interval (e.g., if an interval is 100-109,
then the corresponding row name must be 100). The column names of `key`
(i.e., `colnames(key)`) must contain the age values (e.g., the columns
can NOT be named with “age.1”, for example).

The length intervals in the rows of `key` must contain all of the length
intervals present in the larger sample. Thus, the length of `len.n`
must, at least, equal the number of rows in `key`. If this constraint is
not met, then the function will stop with an error message.

Note that the function will stop with an error if the formula in
`formula` does not meet the specific criteria outlined in the parameter
list above.

## Testing

The results of these functions have not yet been rigorously tested. The
Bettoli and Miranda (2001) results appear, at least, approximately
correct when compared to the results from
[`alkIndivAge`](https://fishr-core-team.github.io/FSA/reference/alkIndivAge.md).
The Quinn and Deriso (1999) results appear at least approximately
correct for the mean values, but do not appear to be correct for the SE
values. Thus, a note is returned with the Quinn and Deriso (1999)
results that the SE should not be trusted.

## IFAR Chapter

5-Age-Length Key.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

Bettoli, P. W. and Miranda, L. E. 2001. A cautionary note about
estimating mean length at age with subsampled data. North American
Journal of Fisheries Management, 21:425-428.

Quinn, T. J. and R. B. Deriso. 1999. Quantitative Fish Dynamics. Oxford
University Press, New York, New York. 542 pages

## See also

See
[`alkIndivAge`](https://fishr-core-team.github.io/FSA/reference/alkIndivAge.md)
and related functions for a completely different methodology. See
[`alkAgeDist`](https://fishr-core-team.github.io/FSA/reference/alkAgeDist.md)
for a related method of determining the proportion of fish at each age.
See the ALKr package.

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
# isolate aged sample
WR1.age <- subset(WR1, !is.na(age))
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

## use age-length key to estimate mean length-at-age of all fish
# Bettoli-Miranda method
alkMeanVar(WR1.key,len~LCat+age,WR1.age,len.n)
#>   age      mean       sd
#> 1   4  51.95492 5.050430
#> 2   5  72.01342 5.519135
#> 3   6  86.59311 4.714664
#> 4   7  97.45567 5.247117
#> 5   8 101.38084 5.576238
#> 6   9 103.65599 2.312767
#> 7  10 104.86056 7.497515
#> 8  11 113.00000 0.000000

# Quinn-Deriso method
alkMeanVar(WR1.key,len~LCat+age,WR1.age,len.n,method="QuinnDeriso")
#> The 'se' values should not be trusted!
#>   age      mean        se
#> 1   4  51.95492 0.3031374
#> 2   5  72.01342 0.5018225
#> 3   6  86.59311 0.5977608
#> 4   7  97.45567 0.4823013
#> 5   8 101.38084 1.4384950
#> 6   9 103.65599 0.9653842
#> 7  10 104.86056 0.6696853
#> 8  11 113.00000 0.0000000
```
