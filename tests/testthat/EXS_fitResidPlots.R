## Create some dummy data
df <- data.frame(y1=runif(100),y2=runif(100),
                 x1=rnorm(100),x2=rnorm(100),x3=rnorm(100),
                 z1=as.factor(sample(letters[1:3],100,replace=TRUE)),
                 z2=as.factor(sample(LETTERS[1:2],100,replace=TRUE)),
                 z3=as.factor(sample(letters[23:26],100,replace=TRUE)))
