## simulate some data sets
# Example 1
df1 <- data.frame(net=c(1,1,1,2,2,3),
                  eff=c(1,1,1,1,1,1),
                  species=c("BKT","LKT","RBT","BKT","LKT","RBT"),
                  catch=c(3,4,5,5,4,3))
# Same as example 1 but with no ancillary data specific to the net number
df2 <- df1[,-2]
# Example Data #3 (All nets have same species ... no zeroes needed)
df3 <- data.frame(net=c(1,1,1,2,2,2,3,3,3),
                  eff=c(1,1,1,1,1,1,1,1,1),
                  species=c("BKT","LKT","RBT","BKT","LKT","RBT","BKT","LKT","RBT"),
                  catch=c(3,4,5,5,4,3,3,2,7))
# Example Data #4 (another variable that needs zeroes)
df4 <- df1
df4$recaps <- c(0,0,0,1,2,1)
# Example Data #5 (two "specvar"s)
df5 <- df1
df5$sex <- c("m","m","f","m","f","f")

