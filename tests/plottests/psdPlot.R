## Random length data
# suppose this is yellow perch to the nearest mm
mm <- c(rnorm(100,mean=125,sd=15),rnorm(50,mean=200,sd=25),
        rnorm(20,mean=300,sd=40))
# same data to the nearest 0.1 cm
cm <- mm/10
# same data to the nearest 0.1 in
inch <- mm/25.4
# put together as data.frame
df <- data.frame(mm,cm,inch)


## Example graphics
op <- par(mar=c(3,3,2,1),mgp=c(1.7,0.5,0))
# mm data using 10-mm increments
psdPlot(~mm,data=df,species="Yellow perch",w=10)
# cm data using 1-cm increments
psdPlot(~cm,data=df,species="Yellow perch",units="cm",w=1)
# inch data using 1-in increments
psdPlot(~inch,data=df,species="Yellow perch",units="in",w=1)
# same as first with some color changes
psdPlot(~mm,data=df,species="Yellow perch",w=10,substock.col="gray90",
        stock.col="gray30")
# ... but without the PSD values
psdPlot(~mm,data=df,species="Yellow perch",w=10,psd.add=FALSE)
# ... demonstrate use of xlim
psdPlot(~mm,data=df,species="Yellow perch",w=10,xlim=c(100,300))

## different subsets of fish
# ... without any sub-stock fish
brks <- psdVal("Yellow Perch")
tmp <- filterD(df,mm>brks["stock"])
psdPlot(~mm,data=tmp,species="Yellow perch",w=10)
# ... without any sub-stock or stock fish
tmp <- filterD(df,mm>brks["quality"])
psdPlot(~mm,data=tmp,species="Yellow perch",w=10)
# ... with only sub-stock, stock, and quality fish ... only PSD-Q
tmp <- filterD(df,mm<brks["preferred"])
psdPlot(~mm,data=tmp,species="Yellow perch",w=10)
# ... with only sub-stock fish (don't run ... setup to give an error)
tmp <- filterD(df,mm<brks["stock"])
psdPlot(~mm,data=tmp,species="Yellow perch",w=10)
par(op)
