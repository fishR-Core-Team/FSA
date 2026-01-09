# Population estimates for k-, 3-, or 2-pass removal data.

Computes estimates, with confidence intervals, of the population size
and probability of capture from the number of fish removed in k-, 3-, or
2-passes in a closed population.

## Usage

``` r
removal(catch, ...)

# S3 method for class 'formula'
removal(
  catch,
  data,
  method = c("CarleStrub", "Zippin", "Seber3", "Seber2", "RobsonRegier2", "Moran",
    "Schnute", "Burnham"),
  alpha = 1,
  beta = 1,
  CS.se = c("Zippin", "alternative"),
  conf.level = 0.95,
  Tmult = 3,
  CIMicroFish = FALSE,
  ...
)

# Default S3 method
removal(
  catch,
  method = c("CarleStrub", "Zippin", "Seber3", "Seber2", "RobsonRegier2", "Moran",
    "Schnute", "Burnham"),
  alpha = 1,
  beta = 1,
  CS.se = c("Zippin", "alternative"),
  conf.level = 0.95,
  Tmult = 3,
  CIMicroFish = FALSE,
  just.ests = FALSE,
  ...
)

# S3 method for class 'removal'
coef(object, parm = c("all", "No", "p", "p1"), as.df = FALSE, ...)

# S3 method for class 'removal'
confint(
  object,
  parm = c("all", "No", "p", "p1"),
  level = conf.level,
  conf.level = NULL,
  digits = getOption("digits"),
  verbose = FALSE,
  incl.est = FALSE,
  as.df = FALSE,
  ...
)

# S3 method for class 'removal'
summary(
  object,
  parm = c("all", "No", "p", "p1"),
  digits = getOption("digits"),
  verbose = FALSE,
  as.df = FALSE,
  ...
)
```

## Arguments

- catch:

  A numerical vector of catch at each pass, or a formula of the form
  `~catch`.

- ...:

  Additional arguments for methods.

- data:

  A data.frame from which the variables in the `catch` formula can be
  found. Not used if `catch` is not a formula.

- method:

  A single string that identifies the removal method to use. See
  details.

- alpha:

  A single numeric value for the alpha parameter in the CarleStrub
  method (default is `1`).

- beta:

  A single numeric value for the beta parameter in the CarleStrub method
  (default is `1`).

- CS.se:

  A single string that identifies whether the SE in the CarleStrub
  method should be computed according to Seber or Zippin.

- conf.level:

  A single number representing the level of confidence to use for
  constructing confidence intervals. This is sent in the main `removal`
  function rather than `confint`.

- Tmult:

  A single numeric that will be multiplied by the total catch in all
  samples to set the upper value for the range of population sizes when
  minimizing the log-likelihood and creating confidence intervals for
  the Moran and Schnute methods. Large values are much slower to
  compute, but values that are too low may result in missing the best
  estimate. A warning is issued if too low of a value is suspected.

- CIMicroFish:

  A logical that indicates whether the t value used to calculate
  confidence intervals when `method="Burnham"` should be rounded to two
  or three decimals and whether the confidence intervals for No should
  be rounded to whole numbers as done in MicroFish 3.0. The default
  (`=FALSE`) is to NOT round the t values or No confidence interval.
  This option is provided only so that results will exactly match
  MicroFish results (see testing).

- just.ests:

  Deprecated as of v0.9.6. This was primarily used when using `removal`
  with a split-and-apply approach to estimate N for multiple groups. See
  examples and use of `incl.ests=` in `confint` for similar
  functionality.

- object:

  An object saved from `removal()`.

- parm:

  A specification of which parameters are to be given confidence
  intervals, either a vector of numbers or a vector of names. If
  missing, all parameters are considered.

- as.df:

  A logical that indicates whether the results of `coef`, `confint`, or
  `summary` should be returned as a data.frame. Defaults to `FALSE`.

- level:

  Not used, but here for compatibility with generic `confint` function.

- digits:

  A single numeric that controls the number of decimals in the output
  from `summary` and `confint`.

- verbose:

  A logical that indicates whether descriptive labels should be printed
  from `summary` and if certain warnings are shown with `confint`.

- incl.est:

  A logical that indicated whether the parameter point estimate should
  be included in the results from `confint`. Defaults to `FALSE`.

## Value

A list with at least the following items:

- catch The original vector of observed catches.

- method The method used (provided by the user).

- lbl A descriptive label for the method used.

- est A matrix that contains the estimates and standard errors for No
  and p.

In addition, if the Moran or Schnute methods are used the list will also
contain

- min.nlogLH The minimum value of the negative log-likelihood function.

- Tmult The Tmult value sent by the user.

## Details

The main function computes the estimates and associated standard errors,
if possible, for the initial population size, No, and probability of
capture, p, for eight methods chosen with `method=`. The possible
methods are:

- `method="CarleStrub"`: The general weighted k-pass estimator proposed
  by Carle and Strub (1978). This function iteratively solves for No in
  equation 7 of Carle and Strub (1978).

- `method="Zippin"`: The general k-pass estimator generally attributed
  to Zippin. This function iteratively solves for No in bias corrected
  version of equation 3 (page 622) of Carle and Strub (1978). These
  results are not yet trustworthy (see Testing section below).

- `method="Seber3"`: The special case for k=3 estimator shown in
  equation 7.24 of Seber(2002).

- `method="Seber2"`: The special case for k=2 estimator shown on page
  312 of Seber(2002).

- `method="RobsonRegier2"`: The special case for k=2 estimator shown by
  Robson and Regier (1968).

- `method="Moran"`: The likelihood method of Moran (1951) as implemented
  by Schnute (1983).

- `method="Schnute"`: The likelihood method of Schnute (1983) for the
  model that has a different probability of capture for the first sample
  but a constant probability of capture for all ensuing samples.

- `method="Burnham"`: The general k-pass estimator likelihood method
  created by Ken Burnham and presented by Van Deventer and Platts
  (1983). This method is used in the Microfish software (Van Deventer
  1989).

Confidence intervals for the first five methods are computed using
standard large-sample normal distribution theory. Note that the
confidence intervals for the 2- and 3-pass special cases are only
approximately correct if the estimated population size is greater than
200. If the estimated population size is between 50 and 200 then a 95%
CI behaves more like a 90% CI.

Confidence intervals for the next two methods use likelihood ratio
theory as described in Schnute (1983) and are only produced for the No
parameter. Standard errors are not produced with the Moran or Schnute
methods.

Confidence intervals for the last method are computed as per Ken
Burnham's instructions for the Burnham Method (Jack Van Deventer,
personal communication). Specifically, they are calculated with the
t-statistic and No-1 degrees of freedom. Please note that the MicroFish
software rounds the t-statistic before it calculates the confidence
intervals about No and p. If you need the confidence interals produced
by FSA::removal to duplicate MicroFish, please use CIMicroFish=TRUE.

## testing

The Carle-Strub method matches the examples in Carle and Strub (1978)
for No, p, and the variance of No. The Carle-Strub estimates of No and p
match the examples in Cowx (1983) but the SE of No does not. The
Carle-Strub estimates of No match the results (for estimates that they
did not reject) from Jones and Stockwell (1995) to within 1 individual
in most instances and within 1% for all other instances (e.g., off by 3
individuals when the estimate was 930 individuals).

The Seber3 results for No match the results in Cowx (1983).

The Seber2 results for No, p, and the SE of No match the results in
example 7.4 of Seber (2002) and in Cowx (1983).

The RobsonRegier2 results for No and the SE of NO match the results in
Cowx (1983)

The Zippin method results do not match the examples in Seber (2002) or
Cowx (1983) because `removal` uses the bias-corrected version from Carle
and Strub (1978) and does not use the tables in Zippin (1958). The
Zippin method is not yet trustworthy.

The Moran and Schnute methods match the examples in Schnute (1983)
perfectly for all point estimates and within 0.1 units for all
confidence intervals.

The Burnham method was tested against the free (gratis) Demo Version of
MicroFish 3.0. Powell Wheeler used R to simulate 100, three-pass removal
samples with capture probabilities between 0 and 1 and population sizes
\<= 1000. The Burnham method implemented here exactly matched MicroFish
in all 100 trials for No and p. In addition, the CIs for No exactly
matched all 100 trials when CIMicroFish=TRUE. Powell was not able to
check the CIs for p because the MicroFish 'Quick Population Estimate'
does not report them.

## IFAR Chapter

10-Abundance from Depletion Data.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

Carle, F.L. and M.R. Strub. 1978. A new method for estimating population
size from removal data. Biometrics, 34:621-630.

Cowx, I.G. 1983. Review of the methods for estimating fish population
size from survey removal data. Fisheries Management, 14:67-82.

Moran, P.A.P. 1951. A mathematical theory of animal trapping. Biometrika
38:307-311.

Robson, D.S., and H.A. Regier. 1968. Estimation of population number and
mortality rates. pp. 124-158 in Ricker, W.E. (editor) Methods for
Assessment of Fish Production in Fresh Waters. IBP Handbook NO. 3
Blackwell Scientific Publications, Oxford.

Schnute, J. 1983. A new approach to estimating populations by the
removal method. Canadian Journal of Fisheries and Aquatic Sciences,
40:2153-2169.

Seber, G.A.F. 2002. The Estimation of Animal Abundance. Edward Arnold,
second edition (Reprint).

van Dishoeck, P. 2009. Effects of catchability variation on performance
of depletion estimators: Application to an adaptive management
experiment. Masters Thesis, Simon Fraser University. \[Was (is?) from
http://rem-main.rem.sfu.ca/theses/vanDishoeckPier_2009_MRM483.pdf.\]

Van Deventer, J.S. 1989. Microcomputer Software System for Generating
Population Statistics from Electrofishing Data–User's Guide for
MicroFish 3.0. USDA Forest Service, General Technical Report INT-254. 29
p. \[Was (is?) from
https://relicensing.pcwa.net/documents/Library/PCWA-L

Van Deventer, J.S., and W.S. Platts. 1983. Sampling and estimating fish
populations from streams. Transactions of the 48th North American
Wildlife and Natural Resource Conference. pp. 349-354.

## See also

See
[`depletion`](https://fishr-core-team.github.io/FSA/reference/depletion.md)
for related functionality.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

A. Powell Wheeler, <powell.wheeler@gmail.com>

## Examples

``` r
## First example -- 3 passes
ct3 <- c(77,50,37)

# Carle Strub (default) method
p1 <- removal(ct3)
summary(p1)
#>       Estimate Std. Error
#> No 233.0000000 31.3578504
#> p    0.3313131  0.0666816
summary(p1,verbose=TRUE)
#> The Carle & Strub (1978) K-Pass Removal Method was used.
#>       Estimate Std. Error
#> No 233.0000000 31.3578504
#> p    0.3313131  0.0666816
summary(p1,parm="No")
#>    Estimate Std. Error
#> No      233   31.35785
summary(p1,parm="p")
#>    Estimate Std. Error
#> p 0.3313131  0.0666816
confint(p1)
#>        95% LCI     95% UCI
#> No 171.5397426 294.4602574
#> p    0.2006195   0.4620067
confint(p1,parm="No")
#>     95% LCI  95% UCI
#> No 171.5397 294.4603
confint(p1,parm="p")
#>     95% LCI   95% UCI
#> p 0.2006195 0.4620067

# Moran method
p2 <- removal(ct3,method="Moran")
summary(p2,verbose=TRUE)
#> The Moran (1951) K-Pass Removal Method was used (SEs not computed).
#>       Estimate Std. Error
#> No 237.5965440         NA
#> p    0.3223336         NA
confint(p2,verbose=TRUE)
#> Confidence intervals for 'p' can not be computed for Moran method.
#>    95% LCI 95% UCI
#> No   194.7   370.9
#> p       NA      NA
#'
# Schnute method
p3 <- removal(ct3,method="Schnute")
summary(p3,verbose=TRUE)
#> The Schnute (1983) K-Pass Removal Method w/ Non-constant Initial Catchability was used (SEs not computed).
#>       Estimate Std. Error
#> No 245.0955993         NA
#> p    0.3039926         NA
#> p1   0.3141631         NA
confint(p3,verbose=TRUE)
#> An upper confidence value for 'No' cannot be determined.
#> Confidence intervals for 'p' can not be computed for Schnute method.
#> Confidence intervals for 'p1' can not be computed for Schnute method.
#>    95% LCI 95% UCI
#> No   183.9     Inf
#> p       NA      NA
#> p1      NA      NA

# Burnham method
p4 <- removal(ct3,method="Burnham")
summary(p4)
#>       Estimate Std. Error
#> No 238.0000000 33.8404319
#> p    0.3215686  0.0673948
summary(p4,verbose=TRUE)
#> The Burnham K-Pass Removal Method (Van Deventer and Platts 1983) was used.
#>       Estimate Std. Error
#> No 238.0000000 33.8404319
#> p    0.3215686  0.0673948
summary(p4,parm="No")
#>    Estimate Std. Error
#> No      238   33.84043
summary(p4,parm="p")
#>    Estimate Std. Error
#> p 0.3215686  0.0673948
confint(p4)
#>        95% LCI     95% UCI
#> No 171.3335366 304.6664634
#> p    0.1887992   0.4543381
confint(p4,parm="No")
#>     95% LCI  95% UCI
#> No 171.3335 304.6665
confint(p4,parm="p")
#>     95% LCI   95% UCI
#> p 0.1887992 0.4543381
## Second example -- 2 passes
ct2 <- c(77,37)

# Seber method
p4 <- removal(ct2,method="Seber2")
summary(p4,verbose=TRUE)
#> The Seber (2002) 2-Pass Removal Method was used.
#>       Estimate Std. Error
#> No 148.2250000 19.0118725
#> p    0.5194805  0.0961208
confint(p4)
#>        95% LCI     95% UCI
#> No 110.9624147 185.4875853
#> p    0.3310873   0.7078737

## Use formula with a data.frame
d <- data.frame(ct=ct3)
p1a <- removal(~ct,data=d)
summary(p1a,verbose=TRUE)
#> The Carle & Strub (1978) K-Pass Removal Method was used.
#>       Estimate Std. Error
#> No 233.0000000 31.3578504
#> p    0.3313131  0.0666816
confint(p1a,incl.est=TRUE)
#>            Est     95% LCI     95% UCI
#> No 233.0000000 171.5397426 294.4602574
#> p    0.3313131   0.2006195   0.4620067

### Test if catchability differs between first sample and the other samples
# chi-square test statistic from  negative log-likelihoods
#   from Moran and Schnute fits (from above)
chi2.val <- 2*(p2$min.nlogLH-p3$min.nlogLH)
# p-value ... no significant difference
pchisq(chi2.val,df=1,lower.tail=FALSE)
#> [1] 0.8882765

# Another LRT example ... sample 1 from Schnute (1983)
ct4 <- c(45,11,18,8)
p2a <- removal(ct4,method="Moran")
p3a <- removal(ct4,method="Schnute")
chi2.val <- 2*(p2a$min.nlogLH-p3a$min.nlogLH)  # 4.74 in Schnute(1983)
pchisq(chi2.val,df=1,lower.tail=FALSE)         # sig diff (catchability differs)
#> [1] 0.02955309
summary(p3a)
#>       Estimate Std. Error
#> No 123.5879686         NA
#> p    0.1890032         NA
#> p1   0.3641131         NA

# Demonstrate multiple groups ... data in long format
## create a dummy data frame
d <- data.frame(lake=factor(rep(c("Ash Tree","Bark","Clay"),each=5)),
                year=factor(rep(c("2010","2011","2010","2011","2010","2011"),
                                times=c(2,3,3,2,2,3))),
                pass=factor(c(1,2,1,2,3,1,2,3,1,2,1,2,1,2,3)),
                catch=c(57,34,65,34,12,54,26,9,54,27,67,34,68,35,12))
d
#>        lake year pass catch
#> 1  Ash Tree 2010    1    57
#> 2  Ash Tree 2010    2    34
#> 3  Ash Tree 2011    1    65
#> 4  Ash Tree 2011    2    34
#> 5  Ash Tree 2011    3    12
#> 6      Bark 2010    1    54
#> 7      Bark 2010    2    26
#> 8      Bark 2010    3     9
#> 9      Bark 2011    1    54
#> 10     Bark 2011    2    27
#> 11     Clay 2010    1    67
#> 12     Clay 2010    2    34
#> 13     Clay 2011    1    68
#> 14     Clay 2011    2    35
#> 15     Clay 2011    3    12

## note use of confint with incl.est= and as.df=
if (require(dplyr) & require(tidyr)) {
  res <- d %>%
    dplyr::group_by(interaction(lake,year)) %>%
    dplyr::group_modify(~confint(removal(~catch,data=.x),
                                 incl.est=TRUE,as.df=TRUE)) %>%
    tidyr::separate_wider_delim(1,names=c("lake","year"),delim=".") %>%
    as.data.frame() # removes tibble and grouping structure
  res
}
#> Loading required package: tidyr
#>       lake year  No    No.LCI   No.UCI         p     p.LCI     p.UCI
#> 1 Ash Tree 2010 130  78.82817 181.1718 0.4482759 0.2107166 0.6858351
#> 2     Bark 2010  95  86.67469 103.3253 0.5894040 0.4636055 0.7152024
#> 3     Clay 2010 130  95.80615 164.1938 0.5233161 0.3239490 0.7226831
#> 4 Ash Tree 2011 121 109.68805 132.3120 0.5577889 0.4398676 0.6757103
#> 5     Bark 2011 103  74.06990 131.9301 0.5328947 0.3138934 0.7518961
#> 6     Clay 2011 125 113.89427 136.1057 0.5637255 0.4489247 0.6785262

# Demonstrate multiple groups ... data in wide format
## create a dummy data frame ... same data as previous ... note that this is
##   not an efficient way to enter data, used here just for simple example
d2w <- rbind(data.frame(lake="Ash Tree",year=2011,pass1=65,pass2=34,pass3=12),
             data.frame(lake="Bark",year=2010,pass1=54,pass2=26,pass3=9),
             data.frame(lake="Bark",year=2011,pass1=54,pass2=27,pass3=NA),
             data.frame(lake="Clay",year=2010,pass1=67,pass2=34,pass3=NA),
             data.frame(lake="Clay",year=2011,pass1=68,pass2=35,pass3=12))
d2w
#>       lake year pass1 pass2 pass3
#> 1 Ash Tree 2011    65    34    12
#> 2     Bark 2010    54    26     9
#> 3     Bark 2011    54    27    NA
#> 4     Clay 2010    67    34    NA
#> 5     Clay 2011    68    35    12

## convert to long format first
d2l <- tidyr::pivot_longer(d2w,cols=c("pass1","pass2","pass3"),
                           names_to="pass",values_to="catch")
d2l
#> # A tibble: 15 × 4
#>    lake      year pass  catch
#>    <chr>    <dbl> <chr> <dbl>
#>  1 Ash Tree  2011 pass1    65
#>  2 Ash Tree  2011 pass2    34
#>  3 Ash Tree  2011 pass3    12
#>  4 Bark      2010 pass1    54
#>  5 Bark      2010 pass2    26
#>  6 Bark      2010 pass3     9
#>  7 Bark      2011 pass1    54
#>  8 Bark      2011 pass2    27
#>  9 Bark      2011 pass3    NA
#> 10 Clay      2010 pass1    67
#> 11 Clay      2010 pass2    34
#> 12 Clay      2010 pass3    NA
#> 13 Clay      2011 pass1    68
#> 14 Clay      2011 pass2    35
#> 15 Clay      2011 pass3    12

## then same process as previous example
if (require(dplyr)) {
  res2 <- d2l %>%
    dplyr::group_by(interaction(lake,year)) %>%
    dplyr::group_modify(~confint(removal(~catch,data=.x),
                                 incl.est=TRUE,as.df=TRUE)) %>%
    tidyr::separate_wider_delim(1,names=c("lake","year"),delim=".") %>%
    as.data.frame() # removes tibble and grouping structure
  res2
}
#> Warning: 'NA's removed from 'catch' to continue.
#> Warning: 'NA's removed from 'catch' to continue.
#>       lake year  No    No.LCI   No.UCI         p     p.LCI     p.UCI
#> 1     Bark 2010  95  86.67469 103.3253 0.5894040 0.4636055 0.7152024
#> 2     Clay 2010 130  95.80615 164.1938 0.5233161 0.3239490 0.7226831
#> 3 Ash Tree 2011 121 109.68805 132.3120 0.5577889 0.4398676 0.6757103
#> 4     Bark 2011 103  74.06990 131.9301 0.5328947 0.3138934 0.7518961
#> 5     Clay 2011 125 113.89427 136.1057 0.5637255 0.4489247 0.6785262
```
