## simulate first data set
df1 <- data.frame(len=round(runif(50,0.1,9.9),1),
                  fish=sample(c("A","B","C"),50,replace=TRUE))

## simulate second data set
vals <- 1:5
freq <- 11:15
tmp <- rep(vals,freq)
tmp <- tmp+round(runif(length(tmp),0,0.9),1)
df2 <- data.frame(len1=tmp,
                  len0.1=tmp/10,
                  len0.01=tmp/100,
                  len10=tmp*10,
                  fish=rep(c("A","B","C"),c(30,20,15)))
