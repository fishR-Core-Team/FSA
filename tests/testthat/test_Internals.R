context("FSA internals tests")

test_that("iHndlCols2Use() messages and results",{
  ## simulate data set
  df1 <- data.frame(net=c(1,1,1,2,2,3),
                    eff=c(1,1,1,1,1,1),
                    species=c("BKT","LKT","RBT","BKT","LKT","RBT"),
                    catch=c(3,4,5,5,4,3))
  nms <- names(df1)

  #### Check warnings and errors
  expect_error(FSA:::iHndlCols2use(df1,1,2))
  expect_warning(FSA:::iHndlCols2use(df1,NULL,1:4))
    
  #### Check cols2use
  ## use one column by number
  ind <- 1
  tmp <- FSA:::iHndlCols2use(df1,ind,NULL)
  expect_equivalent(tmp,df1[,ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[ind])
  ## use one column by name
  tmp <- FSA:::iHndlCols2use(df1,"net",NULL)
  expect_equivalent(tmp,df1[,ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[ind])
  ## use two contiguous columns by number
  ind <- 1:2
  tmp <- FSA:::iHndlCols2use(df1,ind,NULL)
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## use two contiguous columns by name
  tmp <- FSA:::iHndlCols2use(df1,c("net","eff"),NULL)
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## use two non-contiguous columns by number
  ind <- c(1,3)
  tmp <- FSA:::iHndlCols2use(df1,ind,NULL)
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## use two non-contiguous columns by name
  tmp <- FSA:::iHndlCols2use(df1,c("net","species"),NULL)
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## use three columns by number
  ind <- c(1,3,4)
  tmp <- FSA:::iHndlCols2use(df1,ind,NULL)
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## use three columns by name
  tmp <- FSA:::iHndlCols2use(df1,c("net","species","catch"),NULL)
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## use three columns by number
  ind <- 1:4
  tmp <- FSA:::iHndlCols2use(df1,ind,NULL)
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## use three columns by name
  tmp <- FSA:::iHndlCols2use(df1,c("net","eff","species","catch"),NULL)
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])

  #### Check cols2isnore
  ## ignore one column by number
  ind <- 1
  tmp <- FSA:::iHndlCols2use(df1,NULL,ind)
  expect_equivalent(tmp,df1[,-ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore one column by name
  tmp <- FSA:::iHndlCols2use(df1,NULL,"net")
  expect_equivalent(tmp,df1[,-ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore two contiguous columns by number
  ind <- 1:2
  tmp <- FSA:::iHndlCols2use(df1,NULL,ind)
  expect_equivalent(tmp,df1[,-ind])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore two contiguous columns by name
  tmp <- FSA:::iHndlCols2use(df1,NULL,c("net","eff"))
  expect_equivalent(tmp,df1[,-ind])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore two non-contiguous columns by number
  ind <- c(1,3)
  tmp <- FSA:::iHndlCols2use(df1,NULL,ind)
  expect_equivalent(tmp,df1[,-ind])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore two non-contiguous columns by name
  tmp <- FSA:::iHndlCols2use(df1,NULL,c("net","species"))
  expect_equivalent(tmp,df1[,-ind])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore three columns by number
  ind <- c(1,3,4)
  tmp <- FSA:::iHndlCols2use(df1,NULL,ind)
  expect_equivalent(tmp,df1[,-ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore three columns by name
  tmp <- FSA:::iHndlCols2use(df1,NULL,c("net","species","catch"))
  expect_equivalent(tmp,df1[,-ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore three columns by number
  ind <- 1:4
  tmp <- FSA:::iHndlCols2use(df1,NULL,ind)
  expect_equivalent(tmp,df1[,-ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore three columns by name
  tmp <- FSA:::iHndlCols2use(df1,NULL,c("net","eff","species","catch"))
  expect_equivalent(tmp,df1[,-ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[-ind])
  
  #### Check cols2ignore with negative indices
  ## ignore one column by number
  ind <- -1
  tmp <- FSA:::iHndlCols2use(df1,NULL,ind)
  expect_equivalent(tmp,df1[,ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[ind])
  ## ignore two contiguous columns by number
  ind <- -c(1:2)
  tmp <- FSA:::iHndlCols2use(df1,NULL,ind)
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## ignore two non-contiguous columns by number
  ind <- -c(1,3)
  tmp <- FSA:::iHndlCols2use(df1,NULL,ind)
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## ignore three columns by number
  ind <- -c(1,3,4)
  tmp <- FSA:::iHndlCols2use(df1,NULL,ind)
  expect_equivalent(tmp,df1[,ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[ind])
  ## ignore three columns by number
  ind <- -c(1:4)
  tmp <- FSA:::iHndlCols2use(df1,NULL,ind)
  expect_equivalent(tmp,df1[,ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[ind])
})


test_that("iHndlMultWhat() messages and results",{
  expect_message(tmp <- FSA:::iHndlMultWhat(letters,"a"))
  expect_equal(length(tmp),25)
  expect_equal(letters[-1],tmp)
  expect_message(tmp <- FSA:::iHndlMultWhat(tmp,"z"))
  expect_equal(length(tmp),24)
  expect_equal(letters[-c(1,26)],tmp)
})


test_that("iHndlMultWhat() messages and results",{
  data(PSDlit)
  expect_message(FSA:::iListSpecies(PSDlit))
})


test_that("iMakeColor() messages and results",{
  expect_error(FSA:::iMakeColor("black",-1),"must be greater than 0")
  expect_error(FSA:::iMakeColor("black",0),"must be greater than 0")
  expect_equal(FSA:::iMakeColor("black",10),rgb(0,0,0,1/10))
  expect_equal(FSA:::iMakeColor("black",1/10),rgb(0,0,0,1/10))
  expect_equal(FSA:::iMakeColor("red",10),rgb(1,0,0,1/10))
  expect_equal(FSA:::iMakeColor("blue",1/10),rgb(0,0,1,1/10))
  expect_equal(FSA:::iMakeColor("black",1),rgb(0,0,0,1))
})


test_that("iTypeoflm() messages and results",{
  ## Get data
  data(Mirex)
  Mirex$year <- factor(Mirex$year)
  ## Check return types
  tmp <- lm(mirex~weight*year*species,data=Mirex)
  expect_is(FSA:::iTypeoflm(tmp),c("IVR","list"))
  tmp <- lm(mirex~weight*year,data=Mirex)
  expect_is(FSA:::iTypeoflm(tmp),c("IVR","list"))
  tmp <- lm(mirex~weight+year,data=Mirex)
  expect_is(FSA:::iTypeoflm(tmp),c("IVR","list"))
  tmp <- lm(mirex~weight,data=Mirex)
  expect_is(FSA:::iTypeoflm(tmp),c("SLR","list"))
  tmp <- lm(mirex~year,data=Mirex)
  expect_is(FSA:::iTypeoflm(tmp),c("ONEWAY","list"))
  tmp <- lm(mirex~year*species,data=Mirex)
  expect_is(FSA:::iTypeoflm(tmp),c("TWOWAY","list"))
  tmp  <- lm(mirex~weight+I(weight^2),data=Mirex)
  expect_is(FSA:::iTypeoflm(tmp),c("POLY","list"))
  tmp <- lm(mirex~weight+rnorm(nrow(Mirex)+rnorm(nrow(Mirex))),data=Mirex)
  expect_is(FSA:::iTypeoflm(tmp),c("MLR","list"))
  ## Check some errors
  glm1 <- glm(year~weight,data=Mirex,family="binomial")
  expect_error(FSA:::iTypeoflm(glm1),"only works with")
  nl1 <- nls(mirex~B1/(1+exp(B2+B3*weight)),start=list(B1=0.4,B2=2,B3=-0.5),data=Mirex)
  expect_error(FSA:::iTypeoflm(nl1),"only works with")
})
