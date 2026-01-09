# Converts an R color to RGB (red/green/blue) including a transparency (alpha channel).

Converts an R color to RGB (red/green/blue) including a transparency
(alpha channel). Similar to
[`col2rgb`](https://rdrr.io/r/grDevices/col2rgb.html) except that a
transparency (alpha channel) can be included.

## Usage

``` r
col2rgbt(col, transp = 1)
```

## Arguments

- col:

  A vector of any of the three kinds of R color specifications (i.e.,
  either a color name (as listed by
  [`colors`](https://rdrr.io/r/grDevices/colors.html)()), a hexadecimal
  string of the form "#rrggbb" or "#rrggbbaa" (see
  [`rgb`](https://rdrr.io/r/grDevices/rgb.html)), or a positive integer
  i meaning
  [`palette`](https://rdrr.io/r/grDevices/palette.html)()\[i\].

- transp:

  A numeric vector that indicates the transparency level for the color.
  The transparency values must be greater than 0. Transparency values
  greater than 1 are interpreted as the number of points plotted on top
  of each other before the transparency is lost and is, thus,
  transformed to the inverse of the transparency value provided.

## Value

A vector of hexadecimal strings of the form "#rrggbbaa" as would be
returned by [`rgb`](https://rdrr.io/r/grDevices/rgb.html).

## See also

See [`col2rgb`](https://rdrr.io/r/grDevices/col2rgb.html) for similar
functionality.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
col2rgbt("black")
#> [1] "#000000FF"
col2rgbt("black",1/4)
#> [1] "#00000040"
clrs <- c("black","blue","red","green")
col2rgbt(clrs)
#> [1] "#000000FF" "#0000FFFF" "#FF0000FF" "#00FF00FF"
col2rgbt(clrs,1/4)
#> [1] "#00000040" "#0000FF40" "#FF000040" "#00FF0040"
trans <- (1:4)/5
col2rgbt(clrs,trans)
#> [1] "#00000033" "#0000FF66" "#FF000099" "#00FF00CC"
```
