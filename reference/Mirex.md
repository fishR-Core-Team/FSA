# Mirex concentration, weight, capture year, and species of Lake Ontario salmon.

Mirex concentration, weight, capture year, and species of Lake Ontario
Coho and Chinook salmon.

## Format

A data frame with 122 observations on the following 4 variables.

- year:

  a numeric vector of capture years

- weight:

  a numeric vector of salmon weights (kg)

- mirex:

  a numeric vector of mirex concentration in the salmon tissue (mg/kg)

- species:

  a factor with levels `chinook` and `coho`

## Source

From (actual data) Makarewicz, J.C., E.Damaske, T.W. Lewis, and M.
Merner. 2003. Trend analysis reveals a recent reduction in mirex
concentrations in Coho (*Oncorhynchus kisutch*) and Chinook (*O.
tshawytscha*) Salmon from Lake Ontario. Environmental Science and
Technology, 37:1521-1527. [CSV
file](https://raw.githubusercontent.com/fishR-Core-Team/FSA/master/data-raw/Mirex.csv)

## Details

The `year` variable should be converted to a factor as shown in the
example.

## Topic(s)

- Linear models

- Other

## Examples

``` r
Mirex$year <- factor(Mirex$year)
lm1 <- lm(mirex~weight*year*species,data=Mirex)
anova(lm1)
#> Analysis of Variance Table
#> 
#> Response: mirex
#>                     Df  Sum Sq  Mean Sq F value    Pr(>F)    
#> weight               1 0.22298 0.222980 60.0785 8.586e-12 ***
#> year                 5 0.50667 0.101333 27.3028 < 2.2e-16 ***
#> species              1 0.00000 0.000001  0.0004 0.9840448    
#> weight:year          5 0.09491 0.018982  5.1143 0.0003287 ***
#> weight:species       1 0.00240 0.002397  0.6457 0.4235926    
#> year:species         5 0.02338 0.004677  1.2601 0.2873654    
#> weight:year:species  5 0.01650 0.003299  0.8890 0.4916170    
#> Residuals           98 0.36372 0.003711                      
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
