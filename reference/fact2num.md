# Converts "numeric" factor levels to numeric values.

Converts “numeric” factor levels to numeric values.

## Usage

``` r
fact2num(object)
```

## Arguments

- object:

  A vector with “numeric” factor levels to be converted to numeric
  values.

## Value

A numeric vector.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
junk <- factor(c(1,7,2,4,3,10))
str(junk)
#>  Factor w/ 6 levels "1","2","3","4",..: 1 5 2 4 3 6
junk2 <- fact2num(junk)
str(junk2)
#>  num [1:6] 1 7 2 4 3 10

## ONLY RUN IN INTERACTIVE MODE
if (interactive()) {

bad <- factor(c("A","B","C"))
# This will result in an error -- levels are not 'numeric'
bad2 <- fact2num(bad)

}  ## END IF INTERACTIVE MODE
```
