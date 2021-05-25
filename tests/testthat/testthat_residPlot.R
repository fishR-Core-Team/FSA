## Create data for testing ----
df <- data.frame(y1=runif(100),y2=runif(100),
                 x1=rnorm(100),x2=rnorm(100),x3=rnorm(100),
                 z1=as.factor(sample(letters[1:3],100,replace=TRUE)),
                 z2=as.factor(sample(LETTERS[1:2],100,replace=TRUE)),
                 z3=as.factor(sample(letters[23:26],100,replace=TRUE)))


## Test Messages ----
test_that("residPlot() errors and warnings",{
  ## Tried to fit a non-conforming IVR
  expect_error(residPlot(lm(y1~x1*x2*z1*z2,data=df)),
               "covariate in an IVR")
  expect_error(residPlot(lm(y1~x1*x2*x3*z1,data=df)),
               "covariate in an IVR")
  expect_error(residPlot(lm(y1~x1*z1*z2*z3,data=df)),
               "factors in an IVR")
  expect_error(residPlot(lm(y1~x1*x2*z1*z2*z3,data=df)),
               "covariate in an IVR")
  ## Bad colors, points, or line types for one-way IVR
  tmp <- lm(y1~x1*z1,data=df)
  expect_warning(residPlot(tmp,col=c("orange","green")),
                 "changed to default colors")
  expect_warning(residPlot(tmp,col="Dark 2",pch=17:18),
                 "Fewer pchs sent then levels")
  ## Bad colors, points, or line types for two-way IVR
  tmp <- lm(y1~x1*z1*z3,data=df)
  expect_warning(residPlot(tmp,col=c("orange","green")),
                 "changed to default colors")
  expect_warning(residPlot(tmp,col="Dark 2",pch=17:18),
                 "Fewer pchs sent then levels")
  
  ## Wrong residual types for nls and nlme
  tmp <- nls(y1~a*exp(b*x2),data=df,start=c(a=1,b=1))
  expect_error(residPlot(tmp,resid.type="studentized"),
               "cannot be 'studentized'")
})


## Test Output Types ----
test_that("iGetMainTitle() returns",{
  tmp <- FSA:::iTypeoflm(lm(y1~x2,data=df))
  expect_equal(FSA:::iGetMainTitle(tmp,main=""),"")
  expect_equal(FSA:::iGetMainTitle(tmp,main="Derek"),"Derek")
  expect_equal(FSA:::iGetMainTitle(tmp,main="MODEL"),"y1~x2")
  tmp <- FSA:::iTypeoflm(lm(y1~x2*x1,data=df))
  expect_equal(FSA:::iGetMainTitle(tmp,main=""),"")
  expect_equal(FSA:::iGetMainTitle(tmp,main="Derek"),"Derek")
  expect_equal(FSA:::iGetMainTitle(tmp,main="MODEL"),"y1~x2*x1")
})

## Validate Results ----

