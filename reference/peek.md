# Peek into (show a subset of) a data frame or matrix.

Shows the first, last, and approximately evenly spaced rows from a data
frame or matrix.

## Usage

``` r
peek(x, n = 20L, which = NULL, addrownums = TRUE)
```

## Arguments

- x:

  A data frame or matrix.

- n:

  A single numeric that indicates the number of rows to display.

- which:

  A numeric or string vector that contains the column numbers or names
  to display. Defaults to showing all columns.

- addrownums:

  If there are no row names for the MATRIX, then create them from the
  row numbers.

## Value

A matrix or data.frame with n rows.

## Note

If `n` is larger than the number of rows in `x` then all of `x` is
displayed.

## See also

`headtail`

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

A. Powell Wheeler, <powell.wheeler@gmail.com>

## Examples

``` r
peek(CutthroatAL)
#>        id y1998 y1999 y2000 y2001 y2002 y2003 y2004 y2005 y2006
#> 1       1     0     0     0     0     0     0     0     0     1
#> 89     89     0     0     0     0     0     0     0     0     1
#> 177   177     0     0     0     0     0     0     0     1     0
#> 266   266     0     0     0     0     0     0     1     0     0
#> 355   355     0     0     0     0     0     0     1     0     0
#> 443   443     0     0     0     0     0     1     0     0     0
#> 532   532     0     0     0     0     0     1     0     0     0
#> 620   620     0     0     0     0     0     1     1     0     0
#> 709   709     0     0     0     0     1     0     0     0     0
#> 798   798     0     0     0     0     1     1     0     0     0
#> 886   886     0     0     0     1     0     0     0     0     0
#> 975   975     0     0     0     1     0     0     0     0     0
#> 1064 1064     0     0     0     1     1     1     0     0     0
#> 1152 1152     0     0     1     0     0     0     0     0     0
#> 1241 1241     0     0     1     1     0     0     0     0     0
#> 1329 1329     0     1     0     0     0     0     0     0     0
#> 1418 1418     0     1     0     0     0     0     0     0     0
#> 1507 1507     0     1     0     1     0     0     0     0     0
#> 1595 1595     0     1     1     1     1     0     0     0     0
#> 1684 1684     1     1     1     0     0     0     0     0     0
peek(CutthroatAL,n=6)
#>        id y1998 y1999 y2000 y2001 y2002 y2003 y2004 y2005 y2006
#> 1       1     0     0     0     0     0     0     0     0     1
#> 337   337     0     0     0     0     0     0     1     0     0
#> 674   674     0     0     0     0     0     1     1     1     1
#> 1010 1010     0     0     0     1     0     0     0     0     0
#> 1347 1347     0     1     0     0     0     0     0     0     0
#> 1684 1684     1     1     1     0     0     0     0     0     0
peek(CutthroatAL,n=6,which=c("id","y1998","y1999","y2000"))
#>        id y1998 y1999 y2000
#> 1       1     0     0     0
#> 337   337     0     0     0
#> 674   674     0     0     0
#> 1010 1010     0     0     0
#> 1347 1347     0     1     0
#> 1684 1684     1     1     1

## Make a matrix for demonstration purposes only
mCutthroatAL <- as.matrix(CutthroatAL)
peek(mCutthroatAL)
#>        id y1998 y1999 y2000 y2001 y2002 y2003 y2004 y2005 y2006
#> 1       1     0     0     0     0     0     0     0     0     1
#> 89     89     0     0     0     0     0     0     0     0     1
#> 177   177     0     0     0     0     0     0     0     1     0
#> 266   266     0     0     0     0     0     0     1     0     0
#> 355   355     0     0     0     0     0     0     1     0     0
#> 443   443     0     0     0     0     0     1     0     0     0
#> 532   532     0     0     0     0     0     1     0     0     0
#> 620   620     0     0     0     0     0     1     1     0     0
#> 709   709     0     0     0     0     1     0     0     0     0
#> 798   798     0     0     0     0     1     1     0     0     0
#> 886   886     0     0     0     1     0     0     0     0     0
#> 975   975     0     0     0     1     0     0     0     0     0
#> 1064 1064     0     0     0     1     1     1     0     0     0
#> 1152 1152     0     0     1     0     0     0     0     0     0
#> 1241 1241     0     0     1     1     0     0     0     0     0
#> 1329 1329     0     1     0     0     0     0     0     0     0
#> 1418 1418     0     1     0     0     0     0     0     0     0
#> 1507 1507     0     1     0     1     0     0     0     0     0
#> 1595 1595     0     1     1     1     1     0     0     0     0
#> 1684 1684     1     1     1     0     0     0     0     0     0
peek(mCutthroatAL,n=6)
#>        id y1998 y1999 y2000 y2001 y2002 y2003 y2004 y2005 y2006
#> 1       1     0     0     0     0     0     0     0     0     1
#> 337   337     0     0     0     0     0     0     1     0     0
#> 674   674     0     0     0     0     0     1     1     1     1
#> 1010 1010     0     0     0     1     0     0     0     0     0
#> 1347 1347     0     1     0     0     0     0     0     0     0
#> 1684 1684     1     1     1     0     0     0     0     0     0
peek(mCutthroatAL,n=6,addrownums=FALSE)
#>        id y1998 y1999 y2000 y2001 y2002 y2003 y2004 y2005 y2006
#> [1,]    1     0     0     0     0     0     0     0     0     1
#> [2,]  337     0     0     0     0     0     0     1     0     0
#> [3,]  674     0     0     0     0     0     1     1     1     1
#> [4,] 1010     0     0     0     1     0     0     0     0     0
#> [5,] 1347     0     1     0     0     0     0     0     0     0
#> [6,] 1684     1     1     1     0     0     0     0     0     0
peek(mCutthroatAL,n=6,which=2:4)
#>      y1998 y1999 y2000
#> 1        0     0     0
#> 337      0     0     0
#> 674      0     0     0
#> 1010     0     0     0
#> 1347     0     1     0
#> 1684     1     1     1

## Make a tibble type from dplyr ... note how peek() is not limited by
## the tibble restriction on number of rows to show (but head() is).
if (FALSE) { # \dontrun{
  if (require(dplyr)) {
    CutthroatAL2 <- as_tibble(CutthroatAL)
    class(CutthroatAL2)
    peek(CutthroatAL2,n=6)
    head(CutthroatAL2,n=15)
  }
} # }
```
