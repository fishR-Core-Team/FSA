## Using the defaults
hist(Sepal.Length~Species,data=iris)

## Add x-labels and use a pre-fix on the main labels
hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",
     pre.main="Species==")

## Use different breaks and different y-axis limits for each graph
hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",
     same.breaks=FALSE,same.ylim=FALSE)

## Use same but user-controlled breaks for each graph
hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",
     breaks=seq(4,8,1))

## Use same but user-controlled maximum value for y-axis
hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",ymax=30)

## Control the organization of the 'grid' of histograms
hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",nrow=1,ncol=3)

## Use right=FALSE & freq=FALSE to demon sending an argument used by base hist()
hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",right=FALSE,
     freq=FALSE,ymax=2)

## Add a junk variable to the iris data set to show two factors on RHS
iris$junk <- factor(sample(c("A","B"),nrow(iris),replace=TRUE))
hist(Sepal.Length~Species*junk,data=iris,xlab="Sepal Length (cm)")

## Single histogram without grouping using formula notation
hist(~Sepal.Length,data=iris,xlab="Sepal Length (cm)")

## Single histogram with "axis correction" turned off (compare to previous)
hist(~Sepal.Length,data=iris,xlab="Sepal Length (cm)",iaxs=FALSE)

## Single histogram with "axis correction", testing xlim and ylim
hist(~Sepal.Length,data=iris,xlab="Sepal Length (cm)",
     xlim=c(3.8,8.2),ylim=c(0,35))
hist(~Sepal.Length,data=iris,xlab="Sepal Length (cm)",
     xlim=c(3.8,8.2),ymax=35)

## Using the bin width argument
hist(~Sepal.Length,data=iris,xlab="Sepal Length (cm)",w=1)
hist(~Sepal.Length,data=iris,xlab="Sepal Length (cm)",w=0.25)
hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",w=1)
hist(Sepal.Length~Species,data=iris,xlab="Sepal Length (cm)",w=0.25)

## Using a vector (and not a data.frame)
vec <- 1:100
hist(~vec)
