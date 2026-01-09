# Calculates the geometric mean or geometric standard deviation.

Calculates the geometric mean or standard deviation of a vector of
numeric values.

## Usage

``` r
geomean(x, na.rm = FALSE, zneg.rm = FALSE)

geosd(x, na.rm = FALSE, zneg.rm = FALSE)
```

## Arguments

- x:

  Vector of numeric values.

- na.rm:

  Logical indicating whether to remove missing values or not.

- zneg.rm:

  Logical indicating whether to ignore or remove zero or negative values
  found in `x`.

## Value

A numeric value that is the geometric mean or geometric standard
deviation of the numeric values in `x`.

## Details

The geometric mean is computed by log transforming the raw data in `x`,
computing the arithmetic mean of the transformed data, and
back-transforming this mean to the geometric mean by exponentiating.

The geometric standard deviation is computed by log transforming the raw
data in `x`, computing the arithmetic standard deviation of the
transformed data, and back-transforming this standard deviation to the
geometric standard deviation by exponentiating.

## Note

This function is largely an implementation of the code suggested by
Russell Senior on R-help in November, 1999.

## See also

See `geometric.mean` in psych and `Gmean` for geometric mean
calculators. See `Gsd` (documented with `Gmean`) from DescTools for
geometric standard deviation calculators.

## Examples

``` r
## generate random lognormal data
d <- rlnorm(500,meanlog=0,sdlog=1)
# d has a mean on log scale of 0; thus, gm should be exp(0)~=1
# d has a sd on log scale of 1; thus, gsd should be exp(1)~=2.7
geomean(d)
#> [1] 0.9476784
geosd(d)
#> [1] 2.670075

if (FALSE) { # \dontrun{
## Demonstrate handling of zeros and negative values
x <- seq(-1,5)
# this will given an error
geomean(x)
# this will only give a warning, but might not be what you want
geomean(x,zneg.rm=TRUE)
} # }
```
