# Opens web pages associated with the fishR website.

Opens web pages associated with the [fishR
website](https://fishr-core-team.github.io/fishR/) in a browser. The
user can open the main page or choose a specific page to open.

## Usage

``` r
fishR(
  where = c("home", "posts", "books", "IFAR", "AIFFD", "packages", "data", "teaching"),
  open = TRUE
)
```

## Arguments

- where:

  A string that indicates a particular page on the fishR website to
  open.

- open:

  A logical that indicates whether the webpage should be opened in the
  default browser. Defaults to `TRUE`; `FALSE` is used for unit testing.

## Value

None, but a webpage will be opened in the default browser.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{
## Opens an external webpage ... only run interactively
fishR()            # home page
fishR("posts")     # blog posts (some examples) page
fishR("books")     # examples page
fishR("IFAR")      # Introduction to Fisheries Analysis with R page
fishR("AIFFD")     # Analysis & Interpretation of Freshw. Fisher. Data page
fishR("packages")  # list of r-related fisheries packages
fishR("data")      # list of fisheries data sets
fishR("teaching")  # teaching resources
} # }
```
