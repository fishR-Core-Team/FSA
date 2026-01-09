# Computes the prior to or reverse cumulative sum of a vector.

Computes the prior-to (i.e., the cumulative sum prior to but not
including the current value) or the reverse (i.e., the number that large
or larger) cumulative sum of a vector. Also works for 1-dimensional
tables, matrices, and data.frames, though it is best used with vectors.

## Usage

``` r
rcumsum(x)

pcumsum(x)
```

## Arguments

- x:

  a numeric object.

## Value

A numeric vector that contains the prior-to or reverse cumulative sums.

## Note

An `NA` in the vector causes all returned values at and after the first
`NA` for `pcumsum` and at and before the last `NA` for `rcumsum` to be
`NA`. See the examples.

## See also

[`cumsum`](https://rdrr.io/r/base/cumsum.html).

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## Simple example
cbind(vals=1:10,
      cum=cumsum(1:10),
      pcum=pcumsum(1:10),
      rcum=rcumsum(1:10))
#>       vals cum pcum rcum
#>  [1,]    1   1    0   55
#>  [2,]    2   3    1   54
#>  [3,]    3   6    3   52
#>  [4,]    4  10    6   49
#>  [5,]    5  15   10   45
#>  [6,]    6  21   15   40
#>  [7,]    7  28   21   34
#>  [8,]    8  36   28   27
#>  [9,]    9  45   36   19
#> [10,]   10  55   45   10

## Example with NA
vals <- c(1,2,NA,3)
cbind(vals,
      cum=cumsum(vals),
      pcum=pcumsum(vals),
      rcum=rcumsum(vals))
#>      vals cum pcum rcum
#> [1,]    1   1    0   NA
#> [2,]    2   3    1   NA
#> [3,]   NA  NA   NA   NA
#> [4,]    3  NA   NA    3

## Example with NA
vals <- c(1,2,NA,3,NA,4)
cbind(vals,
      cum=cumsum(vals),
      pcum=pcumsum(vals),
      rcum=rcumsum(vals))
#>      vals cum pcum rcum
#> [1,]    1   1    0   NA
#> [2,]    2   3    1   NA
#> [3,]   NA  NA   NA   NA
#> [4,]    3  NA   NA   NA
#> [5,]   NA  NA   NA   NA
#> [6,]    4  NA   NA    4
      
## Example with a matrix
mat <- matrix(c(1,2,3,4,5),nrow=1)
cumsum(mat)
#> [1]  1  3  6 10 15
pcumsum(mat)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    0    1    3    6   10
rcumsum(mat)
#> [1] 15 14 12  9  5

## Example with a table (must be 1-d)
df <- sample(1:10,100,replace=TRUE)
tbl <- table(df)
cumsum(tbl)
#>   1   2   3   4   5   6   7   8   9  10 
#>  10  22  30  42  54  67  76  91  94 100 
pcumsum(tbl)
#> df
#>  1  2  3  4  5  6  7  8  9 10 
#>  0 10 22 30 42 54 67 76 91 94 
rcumsum(tbl)
#>   1   2   3   4   5   6   7   8   9  10 
#> 100  90  78  70  58  46  33  24   9   6 

## Example with a data.frame (must be 1-d)
df <- sample(1:10,100,replace=TRUE)
tbl <- as.data.frame(table(df))[,-1]
cumsum(tbl)
#>  [1]  11  23  41  50  58  71  78  84  95 100
pcumsum(tbl)
#>  [1]  0 11 23 41 50 58 71 78 84 95
rcumsum(tbl)
#>  [1] 100  89  77  59  50  42  29  22  16   5
```
