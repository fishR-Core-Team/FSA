# Construct a base tic-tac-toe plot for presenting predator-prey PSD values.

Construct a base tic-tac-toe plot for presenting predator-prey PSD
values. Predator-prey PSD values are added with
[`plotCI`](https://plotrix.github.io/plotrix/reference/plotCI.html) from
plotrix.

## Usage

``` r
tictactoe(
  predobj = c(30, 70),
  preyobj = c(30, 70),
  predlab = "Predator PSD",
  preylab = "Prey PSD",
  obj.col = "black",
  obj.trans = 0.2,
  bnd.col = "black",
  bnd.lwd = 1,
  bnd.lty = 2
)
```

## Arguments

- predobj:

  A vector of length 2 that contains the target objective range for the
  predator.

- preyobj:

  A vector of length 2 that contains the target objective range for the
  prey.

- predlab:

  A string representing a label for the x-axis.

- preylab:

  A string representing a label for the y-axis.

- obj.col:

  A string designating a color to which the target objective regions
  should be shaded.

- obj.trans:

  A numeric (decimal) that indicates the level of transparency for
  marking the target objective regions.

- bnd.col:

  A string that indicates a color for the boundaries of the target
  objective regions.

- bnd.lwd:

  A numeric that indicates the line width for the boundaries of the
  target objective regions.

- bnd.lty:

  A numeric that indicates the line type for the boundaries of the
  target objective regions.

## Value

None. However, a graphic is produced.

## Details

This function simply creates a base tic-tac-toe plot. Observed values,
with confidence intervals, are added to this plot with
[`plotCI`](https://plotrix.github.io/plotrix/reference/plotCI.html) from
plotrix; see examples.

## IFAR Chapter

6-Size Structure.

## References

Ogle, D.H. 2016. [Introductory Fisheries Analyses with
R](https://fishr-core-team.github.io/fishR/pages/books.html#introductory-fisheries-analyses-with-r).
Chapman & Hall/CRC, Boca Raton, FL.

## See also

See
[`psdVal`](https://fishr-core-team.github.io/FSA/reference/psdVal.md)
and
[`psdCalc`](https://fishr-core-team.github.io/FSA/reference/psdCalc.md)
for related functionality.

## Author

Derek H. Ogle, <DerekOgle51@gmail.com>

## Examples

``` r
## Create hypothetical data for plotting one point .. similar to what might come from psdCalc()
prey <- c(45.4,30.2,56.8)
pred <- c(24.5,10.2,36.7)
names(prey) <- names(pred) <- c("Estimate","95% LCI","95% UCI")
prey
#> Estimate  95% LCI  95% UCI 
#>     45.4     30.2     56.8 
pred
#> Estimate  95% LCI  95% UCI 
#>     24.5     10.2     36.7 

tictactoe()
if (require(plotrix)) {
  plotCI(prey[1],pred[1],li=prey[2],ui=prey[3],err="x",pch=16,add=TRUE)
  plotCI(prey[1],pred[1],li=pred[2],ui=pred[3],err="y",pch=16,add=TRUE) 
}
#> Loading required package: plotrix


## Create hypothetical data for plotting three points ... similar to what might come from psdCalc()
prey <- rbind(c(45.4,30.2,56.8),
              c(68.2,56.7,79.4),
              c(17.1, 9.5,26.3))
pred <- rbind(c(24.5,10.2,36.7),
              c(14.2, 7.1,21.3),
              c(16.3, 8.2,24.4))
colnames(prey) <- colnames(pred) <- c("Estimate","95% LCI","95% UCI")
prey
#>      Estimate 95% LCI 95% UCI
#> [1,]     45.4    30.2    56.8
#> [2,]     68.2    56.7    79.4
#> [3,]     17.1     9.5    26.3
pred
#>      Estimate 95% LCI 95% UCI
#> [1,]     24.5    10.2    36.7
#> [2,]     14.2     7.1    21.3
#> [3,]     16.3     8.2    24.4

tictactoe()
if (require(plotrix)) {
  plotCI(prey[,1],pred[,1],li=prey[,2],ui=prey[,3],err="x",pch=16,add=TRUE)
  plotCI(prey[,1],pred[,1],li=pred[,2],ui=pred[,3],err="y",pch=16,add=TRUE)
}
lines(prey[,1],pred[,1])
text(prey[,1],pred[,1],labels=c(2010,2011,2012),adj=c(-0.5,-0.5))

```
