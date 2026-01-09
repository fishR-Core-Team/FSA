# Likelihood ratio and extra sum-of-squares tests.

Likelihood ratio and extra sum-of-squares tests with multiple `lm` or
`nls` models nested within one common model. This function is most
useful when the nested functions are all at the same level; otherwise
use [`anova()`](https://rdrr.io/r/stats/anova.html) or `lrtest()` which
are more flexible.

## Usage

``` r
lrt(sim, ..., com, sim.names = sim.name, sim.name = NULL, com.name = NULL)

extraSS(sim, ..., com, sim.names = sim.name, sim.name = NULL, com.name = NULL)

# S3 method for class 'extraTest'
print(x, ...)
```

## Arguments

- sim:

  The results of one `lm` or `nls` model, for example, that is a nested
  subset of the model in `com=`.

- ...:

  More model results that are nested subsets of the model in `com=`.

- com:

  The results of one `lm` or `nls` model, for example, that the models
  in `sim=` and `...` are a subset of.

- sim.name, sim.names:

  A string vector of “names” for simple model in `sim=` and `...`.
  `sim.names` is preferred but `sim.name` is allowed to allow for a
  common typing mistake.

- com.name:

  A single “name” string for the complex model in `com=`.

- x:

  An object from `lrt()` or `extraSS()`.

## Value

The main function returns a matrix with as many rows as model
comparisons and columns of the following types:

- `DfO` The error degrees-of-freedom from the subset (more simple)
  model.

- `RSSO`, `logLikO` The residual sum-of-squares (from `extraSS`) or
  log-likelihood (from `lrt`) from the subset (more simple) model.

- `DfA` The error degrees-of-freedom from the `com=` model.

- `RSSA`, `logLikA` The residual sum-of-squares (from `extraSS`) or
  log-likelihood (from `lrt`) from the `com=` model.

- `Df` The difference in error degrees-of-freedom between the two
  models.

- `SS`, `logLik` The difference in residual sum-of-squares (from
  `extraSS`) or log-likelihood (from `lrt`) between the two models.

- `F`, `Chisq` The corresponding F- (from `extraSS`) or Chi-square (from
  `lrt`) test statistic.

- `Pr(>F)`, `Pr(>Chisq)` The corresponding p-value.

## Details

[`anova`](https://rdrr.io/r/stats/anova.html) and
[`lrtest`](https://rdrr.io/pkg/lmtest/man/lrtest.html) (from lmtest)
provide simple methods for conducting extra sum-of-squares or likelihood
ratio tests when one model is nested within another model or when there
are several layers of simple models all sequentially nested within each
other. However, to compare several models that are nested at the same
level with one common more complex model, then
[`anova()`](https://rdrr.io/r/stats/anova.html) and `lrtest()` must be
repeated for each comparison. This repetition can be eliminated with
[`lapply()`](https://rdrr.io/r/base/lapply.html) but then the output is
voluminous. This function is designed to remove the repetitiveness and
to provide output that is compact and easy to read.

## Note

This function is experimental at this point. It seems to work fine for
`lm` and `nls` models. An error will be thrown by `extraSS` for other
model classes, but `lrt` will not (but it has not been thoroughly tests
for other models).

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## Example data
df <- data.frame(x=c(1,2,3,4,5,6,7,8,9,10),
                 y=c(4,6,5,7,9,8,7,12,16,22),
                 z=as.factor(rep(c("A","B"),each=5)),
                 w=as.factor(rep(c("A","B"),times=5)))
df$x2 <- df$x^2

## Linear (lm()) models
#  ... regression
fit.0 <- lm(y~1,data=df)
fit.1 <- lm(y~x,data=df)
fit.2 <- lm(y~x2+x,data=df)
extraSS(fit.0,fit.1,com=fit.2)
#> Model 1: y ~ 1
#> Model 2: y ~ x
#> Model A: y ~ x2 + x 
#> 
#>     DfO    RSSO DfA    RSSA Df      SS      F    Pr(>F)    
#> 1vA   9 282.400   7  27.617  2 254.783 32.290 0.0002925 ***
#> 2vA   8  67.988   7  27.617  1  40.371 10.233 0.0150891 *  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
lrt(fit.0,fit.1,com=fit.2)
#> Loading required namespace: lmtest
#> Model 1: y ~ 1
#> Model 2: y ~ x
#> Model A: y ~ x2 + x 
#> 
#>     DfO  logLikO DfA  logLikA Df   logLik   Chisq Pr(>Chisq)    
#> 1vA   9 -30.8931   7 -19.2686  2 -11.6245 23.2491  8.944e-06 ***
#> 2vA   8 -23.7731   7 -19.2686  1  -4.5045  9.0091   0.002686 ** 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# ... show labels for models
extraSS(fit.0,fit.1,com=fit.2,
    sim.names=c("Null Model","Linear"),com.name="Quadratic")
#> Model 1: Null Model
#> Model 2: Linear
#> Model A: Quadratic 
#> 
#>     DfO    RSSO DfA    RSSA Df      SS      F    Pr(>F)    
#> 1vA   9 282.400   7  27.617  2 254.783 32.290 0.0002925 ***
#> 2vA   8  67.988   7  27.617  1  40.371 10.233 0.0150891 *  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
lrt(fit.0,fit.1,com=fit.2,
    sim.names=c("Null Model","Linear"),com.name="Quadratic")
#> Model 1: Null Model
#> Model 2: Linear
#> Model A: Quadratic 
#> 
#>     DfO  logLikO DfA  logLikA Df   logLik   Chisq Pr(>Chisq)    
#> 1vA   9 -30.8931   7 -19.2686  2 -11.6245 23.2491  8.944e-06 ***
#> 2vA   8 -23.7731   7 -19.2686  1  -4.5045  9.0091   0.002686 ** 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#  ... dummy variable regression
fit.2b <- lm(y~x*z,data=df)
extraSS(fit.0,fit.1,com=fit.2b)
#> Model 1: y ~ 1
#> Model 2: y ~ x
#> Model A: y ~ x * z 
#> 
#>     DfO    RSSO DfA    RSSA Df      SS       F    Pr(>F)    
#> 1vA   9 282.400   6  17.800  3 264.600 29.7303 0.0005347 ***
#> 2vA   8  67.988   6  17.800  2  50.188  8.4586 0.0179459 *  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
lrt(fit.0,fit.1,com=fit.2b)
#> Model 1: y ~ 1
#> Model 2: y ~ x
#> Model A: y ~ x * z 
#> 
#>     DfO  logLikO DfA  logLikA Df   logLik  Chisq Pr(>Chisq)    
#> 1vA   9 -30.8931   6 -17.0725  3 -13.8206 27.641  4.319e-06 ***
#> 2vA   8 -23.7731   6 -17.0725  2  -6.7007 13.401    0.00123 ** 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#  ... ANOVAs
fit.1 <- lm(y~w,data=df)
fit.2 <- lm(y~w*z,data=df)
extraSS(fit.0,fit.1,com=fit.2)
#> Model 1: y ~ 1
#> Model 2: y ~ w
#> Model A: y ~ w * z 
#> 
#>     DfO  RSSO DfA  RSSA Df    SS      F Pr(>F)
#> 1vA   9 282.4   6 159.0  3 123.4 1.5522 0.2955
#> 2vA   8 262.8   6 159.0  2 103.8 1.9585 0.2215
lrt(fit.0,fit.1,com=fit.2)
#> Model 1: y ~ 1
#> Model 2: y ~ w
#> Model A: y ~ w * z 
#> 
#>     DfO  logLikO DfA  logLikA Df   logLik  Chisq Pr(>Chisq)  
#> 1vA   9 -30.8931   6 -28.0210  3  -2.8721 5.7442    0.12474  
#> 2vA   8 -30.5334   6 -28.0210  2  -2.5124 5.0249    0.08107 .
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


## Non-linear (nls()) models
fit.0 = nls(y~c,data=df,start=list(c=10))
fit.1 = nls(y~a*x+c,data=df,start=list(a=1,c=1))
fit.2 = nls(y~b*x2+a*x+c,data=df,start=list(a=-1,b=0.3,c=10))
extraSS(fit.0,fit.1,com=fit.2)
#> Model 1: y ~ c
#> Model 2: y ~ a * x + c
#> Model A: y ~ b * x2 + a * x + c 
#> 
#>     DfO    RSSO DfA    RSSA Df      SS      F    Pr(>F)    
#> 1vA   9 282.400   7  27.617  2 254.783 32.290 0.0002925 ***
#> 2vA   8  67.988   7  27.617  1  40.371 10.233 0.0150891 *  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
lrt(fit.0,fit.1,com=fit.2)
#> Model 1: y ~ c
#> Model 2: y ~ a * x + c
#> Model A: y ~ b * x2 + a * x + c 
#> 
#>     DfO  logLikO DfA  logLikA Df   logLik   Chisq Pr(>Chisq)    
#> 1vA   9 -30.8931   7 -19.2686  2 -11.6245 23.2491  8.944e-06 ***
#> 2vA   8 -23.7731   7 -19.2686  1  -4.5045  9.0091   0.002686 ** 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## General least-squares (gls()) models
if (FALSE) { # \dontrun{
  require(nlme)
  fit.0 <- gls(y~1,data=df,method="ML")
  fit.1 <- gls(y~x,data=df,method="ML")
  fit.2 <- gls(y~x2+x,data=df,method="ML")
  lrt(fit.0,fit.1, com=fit.2)
  ## will return an error ... does not work with gls() models
  # extraSS(fit.0,fit.1, com=fit.2)
} # }
```
