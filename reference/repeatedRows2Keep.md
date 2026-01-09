# Find non-repeated consecutive rows in a data.frame.

Finds the rows in a data.frame that are not repeats of the row
immediately above or below it.

## Usage

``` r
repeatedRows2Keep(
  df,
  cols2use = NULL,
  cols2ignore = NULL,
  keep = c("first", "last")
)
```

## Arguments

- df:

  A data.frame.

- cols2use:

  A string or numeric vector that indicates columns in `df` to use.
  Negative numeric values will not use those columns. Cannot use both
  `cols2use` and `col2ignore`.

- cols2ignore:

  A string or numeric vector that indicates columns in `df` to ignore.
  Cannot use both `cols2use` and `col2ignore`.

- keep:

  A string that indicates whether the `first` (DEFAULT) or `last` row of
  consecutive repeated rows should be kept.

## Value

A single logical that indicates which rows of `df` to keep such that no
consecutive rows (for the columns used) will be repeated.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
test1 <- data.frame(ID=1:10,
                    KEEP=c("First","Last","Both","Both","Both",
                           "Both","First","Neither","Last","Both"),
                    V1=c("a","a","a","B","b","B","A","A","A","a"),
                    V2=c("a","a","A","B","B","b","A","A","A","a"))
keepFirst <- repeatedRows2Keep(test1,cols2ignore=1:2)
keepLast <- repeatedRows2Keep(test1,cols2use=3:4,keep="last")
data.frame(test1,keepFirst,keepLast)
#>    ID    KEEP V1 V2 keepFirst keepLast
#> 1   1   First  a  a      TRUE    FALSE
#> 2   2    Last  a  a     FALSE     TRUE
#> 3   3    Both  a  A      TRUE     TRUE
#> 4   4    Both  B  B      TRUE     TRUE
#> 5   5    Both  b  B      TRUE     TRUE
#> 6   6    Both  B  b      TRUE     TRUE
#> 7   7   First  A  A      TRUE    FALSE
#> 8   8 Neither  A  A     FALSE    FALSE
#> 9   9    Last  A  A     FALSE     TRUE
#> 10 10    Both  a  a      TRUE     TRUE

droplevels(subset(test1,keepFirst))  # should be all "First" or "Both" (7 items)
#>    ID  KEEP V1 V2
#> 1   1 First  a  a
#> 3   3  Both  a  A
#> 4   4  Both  B  B
#> 5   5  Both  b  B
#> 6   6  Both  B  b
#> 7   7 First  A  A
#> 10 10  Both  a  a
droplevels(subset(test1,keepLast))   # should be all "Last" or "Both" (7 items)
#>    ID KEEP V1 V2
#> 2   2 Last  a  a
#> 3   3 Both  a  A
#> 4   4 Both  B  B
#> 5   5 Both  b  B
#> 6   6 Both  B  b
#> 9   9 Last  A  A
#> 10 10 Both  a  a
```
