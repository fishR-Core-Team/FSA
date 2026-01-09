# Expands a length frequency based on a subsample.

Creates a vector of lengths for the individuals not measured based on
the lengths measured in a subsample of individuals.

## Usage

``` r
expandLenFreq(
  x,
  w,
  additional,
  startcat = NULL,
  total = additional + length(x),
  decimals = decs$wdec,
  show.summary = TRUE,
  ...
)
```

## Arguments

- x:

  A numeric vector of length measurements.

- w:

  A number that indicates the width of length classes to create.

- additional:

  The number of individuals that were not measured in the sample (for
  which measurements should be determined).

- startcat:

  A number that indicates the beginning of the first length-class.

- total:

  The total number of individuals in the sample (including those that
  were measured in the subsample).

- decimals:

  A number that indicates the number of decimals used in the output
  vector of estimated lengths.

- show.summary:

  A logical that indicates whether a summary of the process should be
  shown at the end.

- ...:

  Optional arguments to be passed to
  [`lencat`](https://fishr-core-team.github.io/FSA/reference/lencat.md).

## Value

Returns a vector that consists of measurements for the non-measured
individuals in the entire sample.

## Details

Creates a vector of lengths for the individuals not measured based on
the lengths measured in a subsample of individuals. Length categories
are created first that begin with the value in `startcat` (or the
minimum observed value by default) and continue by values of `w` until a
category value greater than the largest observed length in `x`.
Categories of different widths are not allowed.

The resulting “expanded” lengths are created by allocating individuals
to each length class based on the proportion of measured individuals in
the subsample in that length class. Individuals within a length class
are then assigned a specific length within that length class based on a
uniform distribution. Because the expanded number of individuals in a
length class is rounded down based on the measured number per length
class, not all individuals will initially be assigned a length value.
The remaining individuals are assigned to a length class randomly
according to weights based on the proportion of individuals in the
measured length classes. Finally, these individuals are randomly
assigned a specific length within the respective length class from a
uniform distribution, same as above.

The resulting length assignments are rounded to the number of decimals
shown in `decimal`. If `decimals` is not set by the user then it will
default to the same number of decimals shown in the `w` value. Care is
taken to make sure that the rounded result will not pass out of the
given length category (i.e., will not be allowed to round up to the next
length category). Generally speaking, one will want to use more decimals
then is shown in `w`. For example, one may want to create length
categories with a width of 1 inch (i.e., `w=1`) but have the results
recorded as if measured to within 0.1 inch (i.e., `decimals=1`).

## See also

See
[`expandCounts`](https://fishr-core-team.github.io/FSA/reference/expandCounts.md)
for expanding more than just lengths or expanding lengths when there is
a known number in each length bin. See
[`lencat`](https://fishr-core-team.github.io/FSA/reference/lencat.md)
for creating length bins.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## Set the random seed for reproducibility
set.seed(15343437)

## First example
# random lengths measured to nearest 0.1 unit -- values in a vector
len1 <- round(runif(50,0.1,9.9),1)
# assignment of integer lengths to 110 non-measured individuals
new.len1a <- expandLenFreq(len1,w=1,total=160)
#> Length Frequency Expansion using:
#>  Measured length frequency of 50 individuals:
#>    0    1    2    3    4    5    6    7    8    9 
#> 0.08 0.06 0.04 0.08 0.14 0.10 0.14 0.04 0.16 0.16 
#> 
#> Non-random allocations of 105 individuals by length category.
#>  [1]  8  6  4  8 15 11 15  4 17 17
#> 
#> Random allocations of 5 individuals
#>  With final length frequency table of:
#>  0  1  2  3  4  5  6  7  8  9 
#>  8  6  4  8 17 11 15  4 20 17 
new.len1a
#>   [1] 0 0 0 0 0 0 0 0 1 1 1 1 1 1 2 2 2 2 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4
#>  [38] 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 7 7 7 7 8
#>  [75] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
# assignment of lengths to 0.1 to 110 non-measured individuals
new.len1b <- expandLenFreq(len1,w=1,total=160,decimals=1)
#> Length Frequency Expansion using:
#>  Measured length frequency of 50 individuals:
#>    0    1    2    3    4    5    6    7    8    9 
#> 0.08 0.06 0.04 0.08 0.14 0.10 0.14 0.04 0.16 0.16 
#> 
#> Non-random allocations of 105 individuals by length category.
#>  [1]  8  6  4  8 15 11 15  4 17 17
#> 
#> Random allocations of 5 individuals
#>  With final length frequency table of:
#>  0  1  2  3  4  5  6  7  8  9 
#>  8  6  5  8 16 11 16  4 19 17 
new.len1b
#>   [1] 0.1 0.1 0.2 0.3 0.3 0.3 0.5 0.9 1.3 1.4 1.5 1.5 1.6 1.6 2.1 2.1 2.3 2.6
#>  [19] 2.8 3.0 3.1 3.3 3.5 3.5 3.7 3.8 3.9 4.0 4.1 4.2 4.3 4.3 4.4 4.4 4.5 4.6
#>  [37] 4.7 4.8 4.8 4.8 4.8 4.8 4.9 5.0 5.1 5.3 5.4 5.6 5.6 5.8 5.8 5.8 5.8 5.9
#>  [55] 6.0 6.1 6.2 6.4 6.5 6.6 6.6 6.6 6.6 6.6 6.7 6.8 6.8 6.8 6.9 6.9 7.4 7.5
#>  [73] 7.6 7.6 8.0 8.0 8.0 8.0 8.1 8.2 8.3 8.3 8.3 8.3 8.3 8.4 8.4 8.6 8.7 8.7
#>  [91] 8.8 8.8 8.8 9.0 9.2 9.2 9.2 9.2 9.2 9.3 9.3 9.4 9.5 9.6 9.6 9.6 9.8 9.8
#> [109] 9.8 9.8

## Second example -- if values are in a data.frame
# random lengths measured to nearest 0.1 unit
len2 <- data.frame(len=round(runif(50,10,117),1))
# assignment of lengths to 0.1 for 140 non-measured indivs
new.len2a <- expandLenFreq(len2$len,w=10,total=190,decimals=1)
#> Length Frequency Expansion using:
#>  Measured length frequency of 50 individuals:
#>   10   20   30   40   50   60   70   80   90  100  110 
#> 0.06 0.06 0.06 0.14 0.04 0.10 0.12 0.14 0.12 0.04 0.12 
#> 
#> Non-random allocations of 134 individuals by length category.
#>  [1]  8  8  8 19  5 14 16 19 16  5 16
#> 
#> Random allocations of 6 individuals
#>  With final length frequency table of:
#>  10  20  30  40  50  60  70  80  90 100 110 
#>   8   8   8  20   6  15  18  20  16   5  16 
new.len2a
#>   [1]  10.1  10.7  12.2  15.9  16.2  16.3  17.8  18.2  24.2  25.0  25.4  25.6
#>  [13]  27.0  27.5  29.1  29.9  31.2  31.9  31.9  32.9  33.5  36.0  38.0  38.6
#>  [25]  41.0  41.8  42.5  42.7  42.7  42.8  43.1  43.3  43.4  44.0  44.2  44.2
#>  [37]  44.9  45.1  47.3  47.5  49.1  49.1  49.6  49.6  51.9  52.1  55.4  56.6
#>  [49]  58.1  58.7  60.1  61.3  61.6  62.1  64.5  64.6  66.3  66.8  67.7  67.8
#>  [61]  68.0  68.3  68.5  68.5  69.3  70.8  71.2  71.5  72.4  72.5  72.5  72.8
#>  [73]  73.0  74.0  75.8  76.7  76.8  77.3  77.5  78.2  78.7  79.9  79.9  80.4
#>  [85]  80.6  80.7  81.7  82.8  85.1  85.5  85.7  86.4  86.6  87.0  87.1  87.2
#>  [97]  87.7  88.8  88.8  89.0  89.1  89.5  89.5  90.0  90.6  91.8  92.4  92.5
#> [109]  92.5  93.7  93.8  94.6  94.7  95.2  95.3  95.9  96.2  98.6  99.5 103.6
#> [121] 104.6 104.9 106.0 109.5 110.4 111.1 111.4 111.7 112.5 112.6 113.9 114.1
#> [133] 114.1 114.5 115.2 116.8 117.1 118.6 119.3 119.7

## Third example
# hypothetically measured lengths
len <- c(6.7,6.9,7.3,7.4,7.5,8.2,8.7,8.9)
# find lengths for unmeasured fish assuming a total of 30
newlen1 <- expandLenFreq(len,w=0.5,total=30,decimals=1)
#> Length Frequency Expansion using:
#>  Measured length frequency of 8 individuals:
#>   6.5     7   7.5     8   8.5 
#> 0.250 0.250 0.125 0.125 0.250 
#> 
#> Non-random allocations of 19 individuals by length category.
#> [1] 5 5 2 2 5
#> 
#> Random allocations of 3 individuals
#>  With final length frequency table of:
#> 6.5   7 7.5   8 8.5 
#>   6   5   2   4   5 
newlen1
#>  [1] 6.6 6.7 6.7 6.7 6.8 6.9 7.1 7.1 7.2 7.3 7.3 7.5 7.7 8.0 8.2 8.3 8.3 8.5 8.6
#> [20] 8.7 8.8 8.8
# set a starting length category
newlen2 <- expandLenFreq(len,w=0.5,startcat=6.2,total=30,decimals=1)
#> Length Frequency Expansion using:
#>  Measured length frequency of 8 individuals:
#>   6.7   7.2   8.2   8.7 
#> 0.250 0.375 0.125 0.250 
#> 
#> Non-random allocations of 20 individuals by length category.
#> [1] 5 8 2 5
#> 
#> Random allocations of 2 individuals
#>  With final length frequency table of:
#> 6.7 7.2 8.2 8.7 
#>   6   9   2   5 
newlen2
#>  [1] 6.7 6.7 6.9 6.9 6.9 6.9 7.2 7.3 7.3 7.3 7.4 7.4 7.4 7.6 7.6 8.4 8.5 8.7 8.8
#> [20] 9.0 9.0 9.1
```
