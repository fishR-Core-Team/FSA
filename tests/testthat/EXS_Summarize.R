## Create simple dummy data
d1 <- data.frame(f1=factor(c("A","A","A","B","B","B","C","C","C")),
                 f2=factor(c("a","b","c","a","b","c","a","b","c")),
                 f3=factor(c("A","A","A","B","B","B","C","C",NA)),
                 c1=c("x","x","x","x","x","y","y","y","y"),
                 c2=c("A","A","A","B","B","B","C","C",NA),
                 q1=0:8,q2=11:19,q3=c(1:3,1:3,1,2,2),q4=c(NA,0:7),
                 stringsAsFactors=FALSE)

## Result labels
qnms1 <- c("n","nvalid","mean","sd","min","Q1","median","Q3","max","percZero")
