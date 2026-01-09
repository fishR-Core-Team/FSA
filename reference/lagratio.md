# Ratio of lagged observations.

Computes the ratio of lagged observations in a vector.

## Usage

``` r
lagratio(
  x,
  lag = 1L,
  recursion = 1L,
  differences = recursion,
  direction = c("backward", "forward"),
  ...
)
```

## Arguments

- x:

  A numeric vector or matrix.

- lag:

  An integer representing the lag ‘distance’.

- recursion:

  An integer that indicates the level of recursion for the calculations.
  A `1` will simply compute the ratios. A `2`, for example, will compute
  the ratios, save the result, and then compute the ratios of the
  results using the same `lag`. See examples.

- differences:

  Same as `recursion`. Used for symmetry with
  [`diff`](https://rdrr.io/r/base/diff.html).

- direction:

  A string that indicates the direction of calculation. A `"backward"`
  indicates that ‘latter’ values are divided by ‘former’ values. A
  `"forward"` indicates that ‘former’ values are divided by ‘latter’
  values. See examples.

- ...:

  Additional arguments to [`diff()`](https://rdrr.io/r/base/diff.html).

## Value

A vector or matrix of lagged ratios.

## Details

This function behaves similarly to
[`diff()`](https://rdrr.io/r/base/diff.html) except that it returns a
vector or matrix of ratios rather than differences.

## See also

`diff`

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## Backward lagged ratios
# no recursion
lagratio(1:10,1)
#> [1] 2.000000 1.500000 1.333333 1.250000 1.200000 1.166667 1.142857 1.125000
#> [9] 1.111111
lagratio(1:10,2)
#> [1] 3.000000 2.000000 1.666667 1.500000 1.400000 1.333333 1.285714 1.250000
# with recursion
lagratio(1:10,1,2)
#> [1] 0.7500000 0.8888889 0.9375000 0.9600000 0.9722222 0.9795918 0.9843750
#> [8] 0.9876543
lagratio(1:10,2,2)
#> [1] 0.5555556 0.7500000 0.8400000 0.8888889 0.9183673 0.9375000

## Forward lagged ratios
# no recursion
lagratio(10:1,1,direction="forward")
#> [1] 1.111111 1.125000 1.142857 1.166667 1.200000 1.250000 1.333333 1.500000
#> [9] 2.000000
lagratio(10:1,2,direction="forward")
#> [1] 1.250000 1.285714 1.333333 1.400000 1.500000 1.666667 2.000000 3.000000
# with recursion
lagratio(10:1,1,2,direction="forward")
#> [1] 0.9876543 0.9843750 0.9795918 0.9722222 0.9600000 0.9375000 0.8888889
#> [8] 0.7500000
lagratio(10:1,2,2,direction="forward")
#> [1] 0.9375000 0.9183673 0.8888889 0.8400000 0.7500000 0.5555556
```
