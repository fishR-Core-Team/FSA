# Specific utilities for use in a knitr document.

Specific utilities for pretty printing various items in a knitr
document.

## Usage

``` r
kCounts(value, capitalize = FALSE)

kPvalue(value, digits = 4, include.p = TRUE, latex = TRUE)

purl2(
  file,
  out.dir = NULL,
  newname = NULL,
  topnotes = NULL,
  moreItems = NULL,
  blanks = c("extra", "all", "none"),
  delHeader = NULL,
  timestamp = TRUE,
  ...
)

reproInfo(
  out = c("r", "markdown", "latex"),
  rqrdPkgs = NULL,
  elapsed = NULL,
  width = 0.95 * getOption("width"),
  addTOC = TRUE,
  newpage = FALSE,
  links = NULL,
  closeGraphics = TRUE,
  ind = 1
)
```

## Arguments

- value:

  A single numeric count or p-value.

- capitalize:

  A logical that indicates if the returned words should be capitalized
  or not (the default).

- digits:

  Number of decimal places to round the values to.

- include.p:

  A logical that indicates whether the result should be a character
  string with “p=” appended to the numerical result.

- latex:

  A logical that indicates whether the resultant p-value string should
  be contained within dollar signs to form a latex formula.

- file:

  A string that contains the root name of the .RNW file. This will also
  be the name of the resultant purled file with .R appended.

- out.dir:

  A string that indicates the directory structure in which the purled
  file should be located. This should not have a forward slash at the
  end.

- newname:

  A string for the output filename (without the extension) from `purl2`.

- topnotes:

  A character vector of lines to be added to the top of the output file.
  Each value in the vector will be placed on a single line at the top of
  the output file.

- moreItems:

  A string that contains additional words that when found in the purled
  file will result in the entire line with those words to be deleted.

- blanks:

  A string that indicates if blank lines should be removed. If
  `blanks="all"` then all blank lines will be removed. If
  `blanks="extra"` then only “extra” blanks lines will be removed (i.e.,
  one blank line will be left where there was originally more than one
  blank line).

- delHeader:

  A single character that denotes the top and bottom of a block of lines
  that should be deleted from the script created by `purl2`.

- timestamp:

  A logical that indicates whether a timestamp comment should be
  appended to the bottom of the script created by `purl2`.

- ...:

  Additional arguments for the original `purl`.

- out:

  A string that indicates the type of output from `reproInfo` –
  Markdown, LaTeX, or simple R code.

- rqrdPkgs:

  A string vector that contains packages that are required for the
  vignette and for which all dependencies should be found.

- elapsed:

  A numeric, usually from `proc.time`, that is the time required to run
  the vignette. If `NULL` then this output will not be used. See the
  note below.

- width:

  A numeric that indicates the width to use for wrapping the
  reproducibility information when `out="r"`.

- addTOC:

  A logical that indicates whether or not a table of contents entry for
  the reproducibility section should be added to the LaTeX output. Used
  only if `out="latex"`

- newpage:

  A logical that indicates whether or not the reproducibility
  information should begin on a new page. Used only if `out="latex"`

- links:

  A named character vector that will add a links bullet to the
  reproducibility information. The names will be shown and the values
  are the links. Used only if `out="markdown"`.

- closeGraphics:

  A logical that indicates whether the graphics device should be closed
  or not.

- ind:

  An integer that indicates the CRAN mirror to use. Defaults to 1.

## Value

- `kCounts` returns a numeric value if the count is less than zero or
  greater than ten and returns a character string of the number ‘name’.
  See the examples.

- `kPvalue` returns a character string of the supplied p-value rounded
  to the requested number of digits or a character string that indicates
  what the p-value is less than the value with a ‘5’ in the `digits`+1
  place. See the examples.

- `purl2` is a modification of `purl` from knitr that creates a file
  with the same name as `file` but with lines removed that contain
  certain words (those found in `ItemsToRemove` and `moreItems`).

- `reproInfo` returns Markdown, LaTeX, or R code that prints
  “reproducibility information” at the bottom of the knitted document.

## Details

- `kCounts` is used to convert numeric numbers to ‘word’ numbers in a
  sentence.

- `kPvalue` is used to print ‘pretty’ p-values.

- `purl2` is used to create a modified (see below) Stangled or purled
  script.

- `reproInfo` is used to print ‘reproducibility information’ for the
  document.

## Note

In `reproInfo`, `elapsed` can be used to print the time it took to
process the document by sending the elapsed time for processing to this
argument. The simplest way to get an approximate elapsed time is to put
`st <- proc.time()` very early (first line?) in your knitr code, put
`et <- proc.time()-st` very late in your knitr code (i.e., just prior to
`reproInfo`), and then used `elapsed=et["user.self"]+et["sys.self"]` in
`reproInfo`.

## See also

See [`formatC`](https://rdrr.io/r/base/formatc.html) for functionality
similar to `kPvalue`. See `purl` and
[`knit`](https://rdrr.io/pkg/knitr/man/knit.html) in knitr for
functionality similar to `purl2`.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
kCounts(7)
#> [1] "seven"
kCounts(17)
#> [1] 17
kCounts(0)
#> [1] "zero"
kCounts(-6)
#> [1] -6
kCounts(3,capitalize=TRUE)
#> [1] "Three"

kPvalue(0.123456789)
#> [1] "$p=0.1235$"
kPvalue(0.000123456)
#> [1] "$p=0.0001$"
kPvalue(0.000012345)
#> [1] "$p<0.00005$"
kPvalue(0.000012345,include.p=FALSE)
#> [1] "$<0.00005$"
kPvalue(0.000012345,include.p=FALSE,latex=FALSE)
#> [1] "<0.00005"
```
