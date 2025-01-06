## Prepare for tests ----
# Create a nonlinear function
fnx <- function(days,B1,B2,B3) {
  if (length(B1) > 1) {
     B2 <- B1[2]
     B3 <- B1[3]
     B1 <- B1[1]
  }
  B1/(1+exp(B2+B3*days))
}

# Fit the nonlinear model
nl1 <- nls(cells~fnx(days,B1,B2,B3),data=Ecoli,start=list(B1=6,B2=7.2,B3=-1.45))

# Get the results saved previously because nlsBoot() and bootCase() will not
#   run within a testing environment. This is the code to create the files.
#      nlsBoot1 <- nlstools::nlsBoot(nl1)
#      save(nlsBoot1,file="inst/extdata/nlsBoot1.RData")
#      Boot1 <- car::Boot(nl1)
#      save(Boot1,file="inst/extdata/Boot1.RData")
load(system.file("extdata", "nlsBoot1.RData", package="FSA"))
load(system.file("extdata", "Boot1.RData", package="FSA"))


## Test Messages ----
test_that("nlsBoot() methods messages",{
  # testing confint()
  expect_error(confint(nlsBoot1,"derek"),"does not exist in")
  expect_error(confint(nlsBoot1,c("B1","derek")),"does not exist in")
  expect_error(confint(nlsBoot1,4),"exceeds number of columns")
  expect_error(confint(nlsBoot1,-4),"exceeds number of columns")
  expect_error(confint(nlsBoot1,c(1,4)),"exceeds number of columns")
  expect_error(confint(nlsBoot1,-c(1,4)),"exceeds number of columns")
  expect_error(confint(nlsBoot1,c(-1,2)),"cannot be both positive and negative")
  expect_error(confint(nlsBoot1,conf.level=0),"must be between 0 and 1")
  expect_error(confint(nlsBoot1,conf.level=1),"must be between 0 and 1")
  # testing htest()
  expect_error(htest(nlsBoot1,"derek"),"does not exist in")
  expect_error(htest(nlsBoot1,c("B1","derek")),"must be of length 1")
  expect_error(htest(nlsBoot1,4),"exceeds number of columns")
  expect_error(htest(nlsBoot1,-4),"must be positive")
  expect_error(htest(nlsBoot1,c(1,4)),"must be of length 1")
  expect_error(htest(nlsBoot1),"must select a parameter")
  # testing predict()
  expect_error(predict(nlsBoot1,1:7,days=2),"is not a function")
  expect_error(predict(nlsBoot1,fnx,derek=2),"unused argument")
  expect_error(predict(nlsBoot1,fnx,days=2,conf.level=0),"must be between 0 and 1")
  expect_error(predict(nlsBoot1,fnx,days=2,conf.level=1),"must be between 0 and 1")
  expect_error(predict(nlsBoot1,fnx,days=2,digits=0),"must be positive")
})

test_that("Boot() methods messages",{
  # testing confint()
  expect_error(confint(Boot1,"derek"),
               "does not exist in")
  expect_error(confint(Boot1,c("B1","derek")),
               "does not exist in")
  expect_error(confint(Boot1,4),
               "exceeds number of columns")
  expect_error(confint(Boot1,-4),
               "exceeds number of columns")
  expect_error(confint(Boot1,c(1,4)),
               "exceeds number of columns")
  expect_error(confint(Boot1,-c(1,4)),
               "exceeds number of columns")
  expect_error(confint(Boot1,c(-1,2)),
               "cannot be both positive and negative")
  expect_error(confint(Boot1,conf.level=0),
               "must be between 0 and 1")
  expect_error(confint(Boot1,conf.level=1),
               "must be between 0 and 1")
  expect_error(confint(Boot1,conf.level="R"),
               "must be numeric")
  # testing htest()
  expect_error(htest(Boot1,"derek"),
               "does not exist in")
  expect_error(htest(Boot1,c("B1","derek")),
               "must be of length 1")
  expect_error(htest(Boot1,4),
               "exceeds number of columns")
  expect_error(htest(Boot1,-4),
               "must be positive")
  expect_error(htest(Boot1,c(1,4)),
               "must be of length 1")
  expect_error(htest(Boot1),
               "must select a parameter")
  # testing predict()
  expect_error(predict(Boot1,1:7,days=2),
               "is not a function")
  expect_error(predict(Boot1,fnx,derek=2),
               "unused argument")
  expect_error(predict(Boot1,fnx,days=2,conf.level=0),
               "must be between 0 and 1")
  expect_error(predict(Boot1,fnx,days=2,conf.level=1),
               "must be between 0 and 1")
  expect_error(predict(Boot1,fnx,days=2,conf.level="R"),
               "must be numeric")
  expect_error(predict(Boot1,fnx,days=2,digits=0),
               "must be positive")
})


## Test Output Types ----
test_that("nlsBoot() methods output types",{
  # testing confint()
  tmp <- confint(nlsBoot1)
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
  expect_equal(rownames(tmp),c("B1","B2","B3"))
  expect_equal(nrow(tmp),3)
  expect_equal(ncol(tmp),2)
  tmp <- confint(nlsBoot1,"B1")
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
  expect_equal(rownames(tmp),c("B1"))
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  tmp <- confint(nlsBoot1,c(1,3))
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(colnames(tmp),c("95% LCI","95% UCI"))
  expect_equal(rownames(tmp),c("B1","B3"))
  expect_equal(nrow(tmp),2)
  expect_equal(ncol(tmp),2)
  # testing htest()
  tmp <- htest(nlsBoot1,"B1")
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(colnames(tmp),c("Ho Value","p value"))
  expect_equal(rownames(tmp),c("B1"))
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  tmp <- htest(nlsBoot1,1)
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(colnames(tmp),c("Ho Value","p value"))
  expect_equal(rownames(tmp),c("B1"))
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  # testing predict()
  tmp <- predict(nlsBoot1,fnx,days=3)
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(colnames(tmp),c("days","Median","95% LCI","95% UCI"))
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),4)
  expect_equal(tmp[,"days"],c(days=3))
  tmp <- predict(nlsBoot1,fnx,days=1:5)
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(colnames(tmp),c("days","Median","95% LCI","95% UCI"))
  expect_equal(nrow(tmp),5)
  expect_equal(ncol(tmp),4)
  expect_equal(tmp[,"days"],1:5)
  # get same output when digits are used?
  tmp <- predict(nlsBoot1,fnx,days=1:5,digits=2)
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(colnames(tmp),c("days","Median","95% LCI","95% UCI"))
  expect_equal(nrow(tmp),5)
  expect_equal(ncol(tmp),4)
  expect_equal(tmp[,"days"],1:5)
})

test_that("Boot() methods output types",{
  # testing confint()
  tmp <- confint(Boot1)
  expect_equal(class(tmp),c("confint.boot","matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))
  expect_equal(rownames(tmp),c("B1","B2","B3"))
  expect_equal(nrow(tmp),3)
  expect_equal(ncol(tmp),3)
  tmp <- confint(Boot1,"B1")
  expect_equal(class(tmp),c("confint.boot","matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))
  expect_equal(rownames(tmp),c("B1"))
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),3)
  tmp <- confint(Boot1,c(1,3))
  expect_equal(class(tmp),c("confint.boot","matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(colnames(tmp),c("Estimate","95% LCI","95% UCI"))
  expect_equal(rownames(tmp),c("B1","B3"))
  expect_equal(nrow(tmp),2)
  expect_equal(ncol(tmp),3)
  # testing htest()
  tmp <- htest(Boot1,"B1")
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(colnames(tmp),c("Ho Value","p value"))
  expect_equal(rownames(tmp),c("B1"))
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  tmp <- htest(Boot1,1)
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(colnames(tmp),c("Ho Value","p value"))
  expect_equal(rownames(tmp),c("B1"))
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),2)
  # testing predict()
  tmp <- predict(Boot1,fnx,days=3)
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(colnames(tmp),c("days","Median","95% LCI","95% UCI"))
  expect_equal(nrow(tmp),1)
  expect_equal(ncol(tmp),4)
  expect_equal(tmp[,"days"],c(days=3))
  tmp <- predict(Boot1,fnx,days=1:5)
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(colnames(tmp),c("days","Median","95% LCI","95% UCI"))
  expect_equal(nrow(tmp),5)
  expect_equal(ncol(tmp),4)
  expect_equal(tmp[,"days"],1:5)
  # get same output when digits are used?
  tmp <- predict(Boot1,fnx,days=1:5,digits=2)
  expect_equal(class(tmp),c("matrix","array"))
  expect_equal(mode(tmp),"numeric")
  expect_equal(colnames(tmp),c("days","Median","95% LCI","95% UCI"))
  expect_equal(nrow(tmp),5)
  expect_equal(ncol(tmp),4)
  expect_equal(tmp[,"days"],1:5)
})


## Validate Results ----

