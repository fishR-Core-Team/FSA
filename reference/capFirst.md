# Capitalizes the first letter of first or all words in a string.

Capitalizes the first letter of first or all words in a string.

## Usage

``` r
capFirst(x, which = c("all", "first"))
```

## Arguments

- x:

  A single string.

- which:

  A single string that indicates whether all (the default) or only the
  first words should be capitalized.

## Value

A single string with the first letter of the first or all words
capitalized.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## Capitalize first letter of all words (the default)
capFirst("Derek Ogle")
#> [1] "Derek Ogle"
capFirst("derek ogle")
#> [1] "Derek Ogle"
capFirst("derek")
#> [1] "Derek"

## Capitalize first letter of only the first words
capFirst("Derek Ogle",which="first")
#> [1] "Derek ogle"
capFirst("derek ogle",which="first")
#> [1] "Derek ogle"
capFirst("derek",which="first")
#> [1] "Derek"
## apply to all elements in a vector
vec <- c("Derek Ogle","derek ogle","Derek ogle","derek Ogle","DEREK OGLE")
capFirst(vec)
#> [1] "Derek Ogle" "Derek Ogle" "Derek Ogle" "Derek Ogle" "Derek Ogle"
capFirst(vec,which="first")
#> [1] "Derek ogle" "Derek ogle" "Derek ogle" "Derek ogle" "Derek ogle"

## check class types
class(vec)
#> [1] "character"
vec1 <- capFirst(vec)
class(vec1)
#> [1] "character"
fvec <- factor(vec)
fvec1 <- capFirst(fvec)
class(fvec1)
#> [1] "factor"
```
