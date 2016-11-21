## simulate data set
set.seed(56768)
df <- data.frame(tl=round(c(rnorm(100,mean=125,sd=15),
                            rnorm(50,mean=200,sd=25),
                            rnorm(20,mean=300,sd=40)),0),
                 species=rep("Yellow Perch",170))

## simulate data set for psdAdd() tests
set.seed(345234534)
dbt <- data.frame(species=factor(rep(c("Bluefin Tuna"),30)),
                  tl=round(rnorm(30,1900,300),0))
dbt$wt <- round(4.5e-05*dbt$tl^2.8+rnorm(30,0,6000),1)
dbg <- data.frame(species=factor(rep(c("Bluegill"),30)),
                  tl=round(rnorm(30,130,50),0))
dbg$wt <- round(4.23e-06*dbg$tl^3.316+rnorm(30,0,10),1)
dlb <- data.frame(species=factor(rep(c("Largemouth Bass"),30)),
                  tl=round(rnorm(30,350,60),0))
dlb$wt <- round(2.96e-06*dlb$tl^3.273+rnorm(30,0,60),1)
df <- rbind(dbt,dbg,dlb)

# read in external CSV file
ftmp <- system.file("extdata", "PSDWR_testdata.csv", package="FSA")
df2 <- read.csv(ftmp)
df2bg <- Subset(df2,species=="Bluegill")
df2lmb <- Subset(df2,species=="Largemouth Bass")
