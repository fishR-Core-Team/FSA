# Constructs length class/category variable.

Constructs a vector that contains the length class or category to which
an individual belongs. Optionally, that vector can be appended to the
original data frame.

## Usage

``` r
lencat(x, ...)

# Default S3 method
lencat(
  x,
  w = 1,
  startcat = NULL,
  breaks = NULL,
  right = FALSE,
  use.names = FALSE,
  as.fact = use.names,
  droplevels = drop.levels,
  drop.levels = FALSE,
  ...
)

# S3 method for class 'formula'
lencat(
  x,
  data,
  w = 1,
  startcat = NULL,
  breaks = NULL,
  right = FALSE,
  use.names = FALSE,
  as.fact = use.names,
  droplevels = drop.levels,
  drop.levels = FALSE,
  vname = NULL,
  ...
)
```

## Arguments

- x:

  A numeric vector that contains the length measurements or a formula of
  the form `~x` where “x” generically represents a variable in `data`
  that contains length measurements. This formula can only contain one
  variable.

- ...:

  Not implemented.

- w:

  A single numeric that indicates the width of length categories to
  create. Ignored if `breaks` is not `NULL`.

- startcat:

  A single numeric that indicates the beginning of the first length
  category. Only used with `w`. See details for how this is handled when
  `NULL`.

- breaks:

  A numeric vector of lower values for the break points of the length
  categories.

- right:

  A logical that indicates if the intervals should be closed on the
  right (and open on the left) or vice versa.

- use.names:

  A logical that indicates whether the names for the values in `breaks`
  should be used for the levels in the new variable. Will throw a
  warning and then use default levels if `TRUE` but `names(breaks)` is
  `NULL`.

- as.fact:

  A logical that indicates that the new variable should be returned as a
  factor (`=TRUE`) or not (`=FALSE`; default).

- droplevels, drop.levels:

  A logical that indicates that the new variable should retain all
  levels indicated in `breaks` (`=FALSE`; default) or not. Ignored if
  `as.fact=FALSE`.

- data:

  A data.frame that minimally contains the length measurements given in
  the variable in the `formula`.

- vname:

  A string that contains the name for the new length class variable.

## Value

If the formula version of the function is used, then a data.frame is
returned with the a new variable, named as in `vname` (defaults to
`LCat`), appended to the original data.frame. If the default version of
the function is used, then a single vector is returned. The returned
values will be numeric unless `breaks` is named and `use.names=TRUE` or
if `as.fact=TRUE`.

## Details

If `breaks` is non-NULL, then `w` and `startcat` will be ignored. The
vector of values in `breaks` should begin with a value smaller than the
minimum observed value and end with a value larger than the maximum
observed value. If the lowest break value is larger than the minimum
observed value, then an error will occur. If the largest break value is
smaller than the maximum observed value, then an additional break value
larger than the maximum observed value will be added to `breaks` (and a
warning will be sent). The values in `breaks` do not have to be equally
spaced.

If `breaks=NULL` (the default), then the value in `w` is used to create
equally spaced categories. If `startcat=NULL` (the default), then the
length categories will begin with the first value less than the minimum
observed value “rounded” by `w`. For example, if the minimum observed
value is 67, then the first length category will be 65 if `w=5`, 60 if
`w=10`, 50 if `w=25`, and 50 if `w=50`. The length categories will
continue from this starting value by values of `w` until a value greater
than the largest observed value in `x`. The length categories are
left-inclusive and right-exclusive by default (i.e., `right=FALSE`).

The start of the length categories may also be set with `startcat`. The
number in the `startcat` argument should be less than the smallest value
in `x`. Additionally, the number of decimals in `startcat` should not be
more than the number of decimals in `w` (e.g., `startcat=0.4` and `w=1`
will result in an error).

One may want to convert apparent numeric values to factor values if some
of the length categories are missing (e.g., if factor values are used,
for example, then tables of the length category values will have values
for all length categories; i.e., it will have zeros for the length
categories that are missing). The numeric values can be converted to
factors by including `as.fact`. See the “real data” example.

The observed values in `x` should be rounded to the appropriate number
of decimals to avoid misplacement of individuals into incorrect length
categories due to issues with machine-precision (see discussion in
`all.equal`.)

## IFAR Chapter

2-Data Manipulation.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
# Create random lengths measured to nearest 0.1 unit
df1 <- data.frame(len=round(runif(50,0.1,9.9),1))

# Create length categories by 0.1 unit
df1$LCat1 <- lencat(df1$len,w=0.1)
xtabs(~LCat1,data=df1)
#> LCat1
#> 0.2 0.9   1 1.3 1.6 1.7 2.1 2.8 2.9   3 3.3 3.8 4.1 4.2 4.5 4.9 5.1 5.3 5.7 6.2 
#>   1   1   1   1   1   1   1   1   2   4   1   2   1   1   1   1   1   1   1   1 
#> 6.3 6.4 7.1 7.3 7.6 7.7 7.9 8.3 8.6 8.7 9.2 9.3 9.7 
#>   2   1   2   1   2   3   2   4   2   3   1   1   1 

# length categories by 0.2 units
df1$LCat2 <- lencat(df1$len,w=0.2)
xtabs(~LCat2,data=df1)
#> LCat2
#> 0.2 0.8   1 1.2 1.6   2 2.8   3 3.2 3.8   4 4.2 4.4 4.8   5 5.2 5.6 6.2 6.4   7 
#>   1   1   1   1   2   1   3   4   1   2   1   1   1   1   1   1   1   3   1   2 
#> 7.2 7.6 7.8 8.2 8.6 9.2 9.6 
#>   1   5   2   4   5   2   1 

# length categories by 0.2 units starting at 0.1
df1$LCat3 <- lencat(df1$len,w=0.2,startcat=0.1)
xtabs(~LCat3,data=df1)
#> LCat3
#> 0.1 0.9 1.3 1.5 1.7 2.1 2.7 2.9 3.3 3.7 4.1 4.5 4.9 5.1 5.3 5.7 6.1 6.3 7.1 7.3 
#>   1   2   1   1   1   1   1   6   1   2   2   1   1   1   1   1   1   3   2   1 
#> 7.5 7.7 7.9 8.3 8.5 8.7 9.1 9.3 9.7 
#>   2   3   2   4   2   3   1   1   1 

# length categories as set by breaks
df1$LCat4 <- lencat(df1$len,breaks=c(0,2,4,7,10))
xtabs(~LCat4,data=df1)
#> LCat4
#>  0  2  4  7 
#>  6 11 11 22 

## A Second example
# random lengths measured to nearest unit
df2 <- data.frame(len=round(runif(50,10,117),0))    

# length categories by 5 units
df2$LCat1 <- lencat(df2$len,w=5)
xtabs(~LCat1,data=df2)
#> LCat1
#>  10  15  20  25  30  35  45  50  65  70  75  80  85  95 100 105 110 
#>   4   5   2   2   1   2   4   4   3   3   1   2   3   5   2   3   4 

# length categories by 5 units starting at 7
df2$LCat2 <- lencat(df2$len,w=5,startcat=7)
xtabs(~LCat2,data=df2)
#> LCat2
#>   7  12  17  22  27  32  37  42  47  52  62  67  72  77  82  87  92  97 102 107 
#>   3   2   5   1   2   1   2   1   4   3   1   2   3   1   3   2   1   5   2   5 
#> 112 
#>   1 

# length categories by 10 units
df2$LCat3 <- lencat(df2$len,w=10)
xtabs(~LCat3,data=df2)
#> LCat3
#>  10  20  30  40  50  60  70  80  90 100 110 
#>   9   4   3   4   4   3   4   5   5   5   4 

# length categories by 10 units starting at 5
df2$LCat4 <- lencat(df2$len,w=10,startcat=5)
xtabs(~LCat4,data=df2)
#> LCat4
#>   5  15  25  35  45  65  75  85  95 105 
#>   4   7   3   2   8   6   3   3   7   7 

# length categories as set by breaks
df2$LCat5 <- lencat(df2$len,breaks=c(5,50,75,150))
xtabs(~LCat5,data=df2)
#> LCat5
#>  5 50 75 
#> 20 10 20 

## A Third example
# random lengths measured to nearest 0.1 unit
df3 <- data.frame(len=round(runif(50,10,117),1))

# length categories by 5 units
df3$LCat1 <- lencat(df3$len,w=5)
xtabs(~LCat1,data=df3)
#> LCat1
#>  20  25  30  35  40  45  50  55  60  65  70  75  80  85  90  95 105 
#>   2   1   1   2   7   1   3   3   8   2   2   3   3   4   1   4   3 

## A Fourth example
# random lengths measured to nearest 0.01 unit
df4 <- data.frame(len=round(runif(50,0.1,9.9),2))

# length categories by 0.1 unit
df4$LCat1 <- lencat(df4$len,w=0.1)
xtabs(~LCat1,data=df4)
#> LCat1
#> 0.1 0.7 0.8 1.2 1.8 1.9   2 2.1 2.2 2.3 2.5 2.7 2.9 3.1 3.2 3.3 3.4 3.6   4 4.3 
#>   1   1   1   1   1   1   1   1   1   1   1   1   1   1   2   1   2   1   1   2 
#> 4.4 4.5 4.7 4.8   5 5.1 5.3 5.6 6.2 6.4 6.7 6.8 7.4 7.7 8.4 8.8 8.9   9 9.6 
#>   1   2   1   1   1   2   1   3   1   1   2   1   1   1   1   4   1   1   1 

# length categories by 0.1 unit, but without missing categories
df4$LCat2 <- lencat(df4$len,w=0.1,as.fact=TRUE)
xtabs(~LCat2,data=df4)
#> LCat2
#> 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9   1 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9   2 
#>   1   0   0   0   0   0   1   1   0   0   0   1   0   0   0   0   0   1   1   1 
#> 2.1 2.2 2.3 2.4 2.5 2.6 2.7 2.8 2.9   3 3.1 3.2 3.3 3.4 3.5 3.6 3.7 3.8 3.9   4 
#>   1   1   1   0   1   0   1   0   1   0   1   2   1   2   0   1   0   0   0   1 
#> 4.1 4.2 4.3 4.4 4.5 4.6 4.7 4.8 4.9   5 5.1 5.2 5.3 5.4 5.5 5.6 5.7 5.8 5.9   6 
#>   0   0   2   1   2   0   1   1   0   1   2   0   1   0   0   3   0   0   0   0 
#> 6.1 6.2 6.3 6.4 6.5 6.6 6.7 6.8 6.9   7 7.1 7.2 7.3 7.4 7.5 7.6 7.7 7.8 7.9   8 
#>   0   1   0   1   0   0   2   1   0   0   0   0   0   1   0   0   1   0   0   0 
#> 8.1 8.2 8.3 8.4 8.5 8.6 8.7 8.8 8.9   9 9.1 9.2 9.3 9.4 9.5 9.6 
#>   0   0   0   1   0   0   0   4   1   1   0   0   0   0   0   1 

# length categories by 2 unit
df4$LCat3 <- lencat(df4$len,w=2)
xtabs(~LCat3,data=df4)
#> LCat3
#>  0  2  4  6  8 
#>  6 14 15  7  8 

## A Fifth example -- with real data
# remove variables with "anu" and "radcap" just for simplicity
smb1 <- smb2 <- SMBassWB[,-c(8:20)]

# 10 mm length classes - in default LCat variable
smb1$LCat10 <- lencat(smb1$lencap,w=10)
head(smb1)
#>   species lake gear yearcap fish agecap lencap LCat10
#> 1     SMB   WB    E    1988    5      1     71     70
#> 2     SMB   WB    E    1988    3      1     64     60
#> 3     SMB   WB    E    1988    2      1     57     50
#> 4     SMB   WB    E    1988    4      1     68     60
#> 5     SMB   WB    E    1988    6      1     72     70
#> 6     SMB   WB    E    1988    7      1     80     80
xtabs(~LCat10,data=smb1)
#> LCat10
#>  50  60  70  80  90 100 110 120 130 140 150 160 170 180 190 200 210 220 230 240 
#>   3   4   7   5   7  20  21  23  23  23  35  22   7  12  21  21  23  15  19  18 
#> 250 260 270 280 290 300 310 320 330 340 360 440 
#>  19  23  17  22  17   4   4   4   2   1   2   1 

# Same as previous but returned as factor so levels with no fish still seen
smb1$LCat10A <- lencat(smb1$lencap,w=10,as.fact=TRUE)
head(smb1)
#>   species lake gear yearcap fish agecap lencap LCat10 LCat10A
#> 1     SMB   WB    E    1988    5      1     71     70      70
#> 2     SMB   WB    E    1988    3      1     64     60      60
#> 3     SMB   WB    E    1988    2      1     57     50      50
#> 4     SMB   WB    E    1988    4      1     68     60      60
#> 5     SMB   WB    E    1988    6      1     72     70      70
#> 6     SMB   WB    E    1988    7      1     80     80      80
xtabs(~LCat10A,data=smb1)
#> LCat10A
#>  50  60  70  80  90 100 110 120 130 140 150 160 170 180 190 200 210 220 230 240 
#>   3   4   7   5   7  20  21  23  23  23  35  22   7  12  21  21  23  15  19  18 
#> 250 260 270 280 290 300 310 320 330 340 350 360 370 380 390 400 410 420 430 440 
#>  19  23  17  22  17   4   4   4   2   1   0   2   0   0   0   0   0   0   0   1 

# Same as previous but returned as a factor with unused levels dropped
smb1$LCat10B <- lencat(smb1$lencap,w=10,as.fact=TRUE,droplevels=TRUE)
head(smb1)
#>   species lake gear yearcap fish agecap lencap LCat10 LCat10A LCat10B
#> 1     SMB   WB    E    1988    5      1     71     70      70      70
#> 2     SMB   WB    E    1988    3      1     64     60      60      60
#> 3     SMB   WB    E    1988    2      1     57     50      50      50
#> 4     SMB   WB    E    1988    4      1     68     60      60      60
#> 5     SMB   WB    E    1988    6      1     72     70      70      70
#> 6     SMB   WB    E    1988    7      1     80     80      80      80
xtabs(~LCat10B,data=smb1)
#> LCat10B
#>  50  60  70  80  90 100 110 120 130 140 150 160 170 180 190 200 210 220 230 240 
#>   3   4   7   5   7  20  21  23  23  23  35  22   7  12  21  21  23  15  19  18 
#> 250 260 270 280 290 300 310 320 330 340 360 440 
#>  19  23  17  22  17   4   4   4   2   1   2   1 

# 25 mm length classes - in custom variable name
smb1$LCat25 <- lencat(smb1$lencap,w=25)
head(smb1)
#>   species lake gear yearcap fish agecap lencap LCat10 LCat10A LCat10B LCat25
#> 1     SMB   WB    E    1988    5      1     71     70      70      70     50
#> 2     SMB   WB    E    1988    3      1     64     60      60      60     50
#> 3     SMB   WB    E    1988    2      1     57     50      50      50     50
#> 4     SMB   WB    E    1988    4      1     68     60      60      60     50
#> 5     SMB   WB    E    1988    6      1     72     70      70      70     50
#> 6     SMB   WB    E    1988    7      1     80     80      80      80     75
xtabs(~LCat25,data=smb1)
#> LCat25
#>  50  75 100 125 150 175 200 225 250 275 300 325 350 425 
#>  12  14  52  58  60  37  51  45  48  50   9   6   2   1 

# using values from psdVal for Smallmouth Bass
smb1$PSDCat1 <- lencat(smb1$lencap,breaks=psdVal("Smallmouth Bass"))
head(smb1)
#>   species lake gear yearcap fish agecap lencap LCat10 LCat10A LCat10B LCat25
#> 1     SMB   WB    E    1988    5      1     71     70      70      70     50
#> 2     SMB   WB    E    1988    3      1     64     60      60      60     50
#> 3     SMB   WB    E    1988    2      1     57     50      50      50     50
#> 4     SMB   WB    E    1988    4      1     68     60      60      60     50
#> 5     SMB   WB    E    1988    6      1     72     70      70      70     50
#> 6     SMB   WB    E    1988    7      1     80     80      80      80     75
#>   PSDCat1
#> 1       0
#> 2       0
#> 3       0
#> 4       0
#> 5       0
#> 6       0
xtabs(~PSDCat1,data=smb1)
#> PSDCat1
#>   0 180 280 350 430 
#> 200 188  54   2   1 

# add category names
smb1$PSDCat2 <- lencat(smb1$lencap,breaks=psdVal("Smallmouth Bass"),use.names=TRUE)
head(smb1)
#>   species lake gear yearcap fish agecap lencap LCat10 LCat10A LCat10B LCat25
#> 1     SMB   WB    E    1988    5      1     71     70      70      70     50
#> 2     SMB   WB    E    1988    3      1     64     60      60      60     50
#> 3     SMB   WB    E    1988    2      1     57     50      50      50     50
#> 4     SMB   WB    E    1988    4      1     68     60      60      60     50
#> 5     SMB   WB    E    1988    6      1     72     70      70      70     50
#> 6     SMB   WB    E    1988    7      1     80     80      80      80     75
#>   PSDCat1  PSDCat2
#> 1       0 substock
#> 2       0 substock
#> 3       0 substock
#> 4       0 substock
#> 5       0 substock
#> 6       0 substock
xtabs(~PSDCat2,data=smb1)
#> PSDCat2
#>  substock     stock   quality preferred memorable    trophy 
#>       200       188        54         2         1         0 

# same as above but drop the unused levels
smb1$PSDCat2A <- lencat(smb1$lencap,breaks=psdVal("Smallmouth Bass"),
                        use.names=TRUE,droplevels=TRUE)
head(smb1)
#>   species lake gear yearcap fish agecap lencap LCat10 LCat10A LCat10B LCat25
#> 1     SMB   WB    E    1988    5      1     71     70      70      70     50
#> 2     SMB   WB    E    1988    3      1     64     60      60      60     50
#> 3     SMB   WB    E    1988    2      1     57     50      50      50     50
#> 4     SMB   WB    E    1988    4      1     68     60      60      60     50
#> 5     SMB   WB    E    1988    6      1     72     70      70      70     50
#> 6     SMB   WB    E    1988    7      1     80     80      80      80     75
#>   PSDCat1  PSDCat2 PSDCat2A
#> 1       0 substock substock
#> 2       0 substock substock
#> 3       0 substock substock
#> 4       0 substock substock
#> 5       0 substock substock
#> 6       0 substock substock
xtabs(~PSDCat2A,data=smb1)
#> PSDCat2A
#>  substock     stock   quality preferred memorable 
#>       200       188        54         2         1 
str(smb1)
#> 'data.frame':    445 obs. of  14 variables:
#>  $ species : Factor w/ 1 level "SMB": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ lake    : Factor w/ 1 level "WB": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ gear    : Factor w/ 2 levels "E","T": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ yearcap : int  1988 1988 1988 1988 1988 1988 1989 1990 1990 1990 ...
#>  $ fish    : num  5 3 2 4 6 7 50 482 768 428 ...
#>  $ agecap  : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ lencap  : int  71 64 57 68 72 80 55 75 75 71 ...
#>  $ LCat10  : num  70 60 50 60 70 80 50 70 70 70 ...
#>  $ LCat10A : Factor w/ 40 levels "50","60","70",..: 3 2 1 2 3 4 1 3 3 3 ...
#>  $ LCat10B : Factor w/ 32 levels "50","60","70",..: 3 2 1 2 3 4 1 3 3 3 ...
#>  $ LCat25  : num  50 50 50 50 50 75 50 75 75 50 ...
#>  $ PSDCat1 : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ PSDCat2 : Factor w/ 6 levels "substock","stock",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ PSDCat2A: Factor w/ 5 levels "substock","stock",..: 1 1 1 1 1 1 1 1 1 1 ...

# same as above but not returned as a factor (returned as a character)
smb1$PSDcat2B <- lencat(smb1$lencap,breaks=psdVal("Smallmouth Bass"),
                        use.names=TRUE,as.fact=FALSE)
str(smb1)
#> 'data.frame':    445 obs. of  15 variables:
#>  $ species : Factor w/ 1 level "SMB": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ lake    : Factor w/ 1 level "WB": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ gear    : Factor w/ 2 levels "E","T": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ yearcap : int  1988 1988 1988 1988 1988 1988 1989 1990 1990 1990 ...
#>  $ fish    : num  5 3 2 4 6 7 50 482 768 428 ...
#>  $ agecap  : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ lencap  : int  71 64 57 68 72 80 55 75 75 71 ...
#>  $ LCat10  : num  70 60 50 60 70 80 50 70 70 70 ...
#>  $ LCat10A : Factor w/ 40 levels "50","60","70",..: 3 2 1 2 3 4 1 3 3 3 ...
#>  $ LCat10B : Factor w/ 32 levels "50","60","70",..: 3 2 1 2 3 4 1 3 3 3 ...
#>  $ LCat25  : num  50 50 50 50 50 75 50 75 75 50 ...
#>  $ PSDCat1 : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ PSDCat2 : Factor w/ 6 levels "substock","stock",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ PSDCat2A: Factor w/ 5 levels "substock","stock",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ PSDcat2B: chr  "substock" "substock" "substock" "substock" ...

## A Sixth example -- similar to fifth example but using the formula notation
# 10 mm length classes - in default LCat variable
smb2 <- lencat(~lencap,data=smb2,w=10)
head(smb2)
#>   species lake gear yearcap fish agecap lencap LCat
#> 1     SMB   WB    E    1988    5      1     71   70
#> 2     SMB   WB    E    1988    3      1     64   60
#> 3     SMB   WB    E    1988    2      1     57   50
#> 4     SMB   WB    E    1988    4      1     68   60
#> 5     SMB   WB    E    1988    6      1     72   70
#> 6     SMB   WB    E    1988    7      1     80   80

# 25 mm length classes - in custom variable name
smb2 <- lencat(~lencap,data=smb2,w=25,vname="LenCat25")
head(smb2)
#>   species lake gear yearcap fish agecap lencap LCat LenCat25
#> 1     SMB   WB    E    1988    5      1     71   70       50
#> 2     SMB   WB    E    1988    3      1     64   60       50
#> 3     SMB   WB    E    1988    2      1     57   50       50
#> 4     SMB   WB    E    1988    4      1     68   60       50
#> 5     SMB   WB    E    1988    6      1     72   70       50
#> 6     SMB   WB    E    1988    7      1     80   80       75

# using values from psdVal for Smallmouth Bass
smb2 <- lencat(~lencap,data=smb2,breaks=psdVal("Smallmouth Bass"),vname="LenPsd")
head(smb2)
#>   species lake gear yearcap fish agecap lencap LCat LenCat25 LenPsd
#> 1     SMB   WB    E    1988    5      1     71   70       50      0
#> 2     SMB   WB    E    1988    3      1     64   60       50      0
#> 3     SMB   WB    E    1988    2      1     57   50       50      0
#> 4     SMB   WB    E    1988    4      1     68   60       50      0
#> 5     SMB   WB    E    1988    6      1     72   70       50      0
#> 6     SMB   WB    E    1988    7      1     80   80       75      0

# add category names
smb2 <- lencat(~lencap,data=smb2,breaks=psdVal("Smallmouth Bass"),vname="LenPsd2",
               use.names=TRUE,droplevels=TRUE)
head(smb2)
#>   species lake gear yearcap fish agecap lencap LCat LenCat25 LenPsd  LenPsd2
#> 1     SMB   WB    E    1988    5      1     71   70       50      0 substock
#> 2     SMB   WB    E    1988    3      1     64   60       50      0 substock
#> 3     SMB   WB    E    1988    2      1     57   50       50      0 substock
#> 4     SMB   WB    E    1988    4      1     68   60       50      0 substock
#> 5     SMB   WB    E    1988    6      1     72   70       50      0 substock
#> 6     SMB   WB    E    1988    7      1     80   80       75      0 substock
str(smb2)
#> 'data.frame':    445 obs. of  11 variables:
#>  $ species : Factor w/ 1 level "SMB": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ lake    : Factor w/ 1 level "WB": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ gear    : Factor w/ 2 levels "E","T": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ yearcap : int  1988 1988 1988 1988 1988 1988 1989 1990 1990 1990 ...
#>  $ fish    : num  5 3 2 4 6 7 50 482 768 428 ...
#>  $ agecap  : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ lencap  : int  71 64 57 68 72 80 55 75 75 71 ...
#>  $ LCat    : num  70 60 50 60 70 80 50 70 70 70 ...
#>  $ LenCat25: num  50 50 50 50 50 75 50 75 75 50 ...
#>  $ LenPsd  : num  0 0 0 0 0 0 0 0 0 0 ...
#>  $ LenPsd2 : Factor w/ 5 levels "substock","stock",..: 1 1 1 1 1 1 1 1 1 1 ...
```
