# Finds the number of valid (non-NA) values in a vector.

Finds the number of valid (non-NA) values in a vector.

## Usage

``` r
validn(object)
```

## Arguments

- object:

  A vector.

## Value

A single numeric value that is the number of non-`NA` values in a
vector.

## IFAR Chapter

2-Basic Data Manipulations.

## See also

See
[`valid.n`](https://plotrix.github.io/plotrix/reference/valid.n.html) in
plotrix and `nobs` in gdata for similar functionality. See
[`is.na`](https://rdrr.io/r/base/NA.html) for finding the missing
values.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
junk1 <- c(1,7,2,4,3,10,NA)
junk2 <- c("Derek","Hugh","Ogle","Santa","Claus","Nick",NA,NA)
junk3 <- factor(junk2)
junk4 <- c(TRUE,TRUE,FALSE,FALSE,FALSE,TRUE,NA,NA)
junk5 <- data.frame(junk1)
junk6 <- data.frame(junk3)

validn(junk1)
#> [1] 6
validn(junk2)
#> [1] 6
validn(junk3)
#> [1] 6
validn(junk4)
#> [1] 6
validn(junk5)
#> [1] 6
validn(junk6)
#> [1] 6
 
```
