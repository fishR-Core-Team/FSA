## create a "good" small ALK matrix for testing
alk <- matrix(c(0.4,0.3,0.3,0.0,
                0.2,0.4,0.3,0.1,
                0.1,0.2,0.4,0.3,
                0.0,0.1,0.4,0.5,
                0.0,0.0,0.2,0.8),
              nrow=5,byrow=TRUE)
rownames(alk) <- c(10,20,30,40,50)
colnames(alk) <- c(2,3,4,5)

## Get some real data
WR1 <- WR79
WR1$LCat <- lencat(WR1$len,w=5)
len.n <- xtabs(~LCat,data=WR1)
WR1.age <- filterD(WR1,!is.na(age))
lenA.n <- xtabs(~LCat,data=WR1.age)
WR1.len <- filterD(WR1,is.na(age))
WR1.key <- prop.table(xtabs(~LCat+age,data=WR1.age),margin=1)
