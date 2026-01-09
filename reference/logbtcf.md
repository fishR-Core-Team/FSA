# Constructs the correction-factor used when back-transforming log-transformed values.

Constructs the correction-factor used when back-transforming
log-transformed values according to Sprugel (1983). Sprugel's main
formula – exp((syx^2)/2) – is used when syx is estimated for natural log
transformed data. A correction for any base is obtained by multiplying
the syx term by log_e(base) to give exp(((log_e(base)\*syx)^2)/2). This
more general formula is implemented here (if, of course, the base is
exp(1) then the general formula reduces to the original specific
formula).

## Usage

``` r
logbtcf(obj, base = exp(1))
```

## Arguments

- obj:

  An object from `lm`.

- base:

  A single numeric that indicates the base of the logarithm used.

## Value

A numeric value that is the correction factor according to Sprugel
(1983).

## References

Sprugel, D.G. 1983. Correcting for bias in log-transformed allometric
equations. Ecology 64:209-210.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
# toy data
df <- data.frame(y=rlnorm(10),x=rlnorm(10))
df$logey <- log(df$y)
df$log10y <- log10(df$y)
df$logex <- log(df$x)
df$log10x <- log10(df$x)

# model and predictions on loge scale
lme <- lm(logey~logex,data=df)
( ploge <- predict(lme,data.frame(logex=log(10))) )
#>        1 
#> 1.399936 
( pe <- exp(ploge) )
#>       1 
#> 4.05494 
( cfe <- logbtcf(lme) )
#> [1] 1.440596
( cpe <- cfe*pe )
#>        1 
#> 5.841532 

# model and predictions on log10 scale
lm10 <- lm(log10y~log10x,data=df)
plog10 <- predict(lm10,data.frame(log10x=log10(10)))
p10 <- 10^(plog10)
( cf10 <- logbtcf(lm10,10) )
#> [1] 1.440596
( cp10 <- cf10*p10 )
#>        1 
#> 5.841532 

# cfe and cf10, cpe and cp10 should be equal
all.equal(cfe,cf10)
#> [1] TRUE
all.equal(cpe,cp10)
#> [1] TRUE
```
