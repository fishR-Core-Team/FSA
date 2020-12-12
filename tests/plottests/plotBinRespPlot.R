## NASA space shuttle o-ring failures -- from graphics package
fail <- factor(c(2,2,2,2,1,1,1,1,1,1,2,1,2,1,1,1,1,2,1,1,1,1,1),
levels = 1:2, labels = c("no","yes"))
temperature <- c(53,57,58,63,66,67,67,67,68,69,70,70,70,70,72,73,75,75,76,76,78,79,81)
d <- data.frame(temperature,fail,fail2=factor(fail,levels=c("yes","no")))

## Default plot (using formula notation)
plotBinResp(fail~temperature,data=d)
plotBinResp(fail2~temperature,data=d)

## Controlling where proportions are computed with a sequence in breaks
plotBinResp(fail~temperature,data=d,breaks=seq(50,85,5))

## Controlling where proportions are computed with an integer in breaks
plotBinResp(fail~temperature,data=d,breaks=10)

## Controlling where proportions are computed at each value of x
plotBinResp(fail~temperature,data=d,breaks=NULL)

## Don't plot points, just plot proportions
plotBinResp(fail~temperature,data=d,plot.pts=FALSE)

## Don't plot proportions, just plot points
plotBinResp(fail~temperature,data=d,plot.p=FALSE)

## Change points colors, and eliminate transparency
plotBinResp(fail~temperature,data=d,col.pt="red",transparency=1)

## Remove the right y-axis
plotBinResp(fail~temperature,data=d,yaxis2.show=FALSE)

## Change left y-axis ticks
plotBinResp(fail~temperature,data=d,yaxis1.ticks=c(0,1),yaxis1.lbls=c(0,1))
