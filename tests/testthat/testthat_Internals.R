test_that("iCheckStartcatW() messages",{
  expect_error(FSA:::iCheckW(-1),"must be positive")
  expect_error(FSA:::iCheckW(1:2),"must be a single value")
  expect_error(FSA:::iCheckW("derek"),"must be numeric")
  tmp <- 1:10
  expect_error(FSA:::iCheckStartcat(-1,1,tmp),"must be non-negative")
  expect_error(FSA:::iCheckStartcat(1:2,1,tmp),"must be a single value")
  expect_error(FSA:::iCheckStartcat("derek",1,tmp),"must be numeric")
  expect_error(FSA:::iCheckStartcatW(3,1,tmp),
               "larger than the minimum observation")
  expect_error(FSA:::iCheckStartcatW(0.5,1,tmp),"should not have more decimals")
})


test_that("iGetDecimals() messages and results",{
  expect_error(FSA:::iGetDecimals(1:2),"must be a single value")
  expect_error(FSA:::iGetDecimals("derek"),"must be numeric")
  expect_equal(FSA:::iGetDecimals(1),0)
  expect_equal(FSA:::iGetDecimals(0.1),1)
  expect_equal(FSA:::iGetDecimals(0.11),2)
  expect_equal(FSA:::iGetDecimals(0.111),3)
  expect_equal(FSA:::iGetDecimals(0.1111),4)
  expect_equal(FSA:::iGetDecimals(0.11111),5)
  expect_equal(FSA:::iGetDecimals(0.111111),6)
  expect_equal(FSA:::iGetDecimals(0.1111111),7)
  expect_equal(FSA:::iGetDecimals(0.11111111),8)
  expect_equal(FSA:::iGetDecimals(0.111111111),9)
  expect_equal(FSA:::iGetDecimals(0.1111111111),10)
  expect_equal(FSA:::iGetDecimals(3.1),1)
  expect_equal(FSA:::iGetDecimals(10.1),1)
  expect_equal(FSA:::iGetDecimals(100.1),1)
  expect_equal(FSA:::iGetDecimals(10),0)
  expect_equal(FSA:::iGetDecimals(100),0)
  expect_equal(FSA:::iGetDecimals(1000),0)
  expect_equal(FSA:::iGetDecimals(10000),0)
  expect_equal(suppressWarnings(FSA:::iGetDecimals(100000)),0)
  expect_equal(suppressWarnings(FSA:::iGetDecimals(1000000)),0)
  expect_equal(suppressWarnings(FSA:::iGetDecimals(10000000)),0)
  expect_equal(FSA:::iGetDecimals(10000000.1),1)
  expect_equal(FSA:::iGetDecimals(10000000.01),2)
  expect_warning(FSA:::iGetDecimals(10000000.001),"in exponential notation")
  expect_equal(suppressWarnings(FSA:::iGetDecimals(10000000.001)),0)
  expect_warning(FSA:::iGetDecimals(0.0000000001),"in exponential notation")
  expect_equal(suppressWarnings(FSA:::iGetDecimals(0.0000000001)),0)
  expect_equal(FSA:::iGetDecimals(pi),9)
  ## Handle integers
  expect_equal(FSA:::iGetDecimals(0L),0)
  expect_equal(FSA:::iGetDecimals(1L),0)
  expect_equal(FSA:::iGetDecimals(10L),0)
  expect_equal(FSA:::iGetDecimals(175L),0)
})


test_that("iHndlCols2Use() messages and results",{
  ## simulate data set
  df1 <- data.frame(net=c(1,1,1,2,2,3),
                    eff=c(1,1,1,1,1,1),
                    species=c("BKT","LKT","RBT","BKT","LKT","RBT"),
                    catch=c(3,4,5,5,4,3))
  nms <- names(df1)

  #### Check warnings and errors
  expect_error(FSA:::iHndlCols2UseIgnore(df1,1,2),"Cannot use both")
  expect_error(FSA:::iHndlCols2UseIgnore(df1,cols2use=FALSE),
               "must be a numeric index or column name")
  expect_error(FSA:::iHndlCols2UseIgnore(df1,cols2use=c(-1,2)),
               "must be all positive or all negative")
  expect_error(FSA:::iHndlCols2UseIgnore(df1,cols2use=1:6),"do not exist in")
  expect_error(FSA:::iHndlCols2UseIgnore(df1,cols2use=c("frank","derek")),
               "None of columns in")
  expect_warning(FSA:::iHndlCols2UseIgnore(df1,cols2ignore=1:4),
                 "contains no columns")
  expect_error(FSA:::iHndlCols2UseIgnore(df1,cols2ignore=FALSE),
               "must be a numeric index or column name")
  expect_error(FSA:::iHndlCols2UseIgnore(df1,cols2ignore=c(-1,2)),
               "must be all positive or all negative")
  expect_error(FSA:::iHndlCols2UseIgnore(df1,cols2ignore=1:5),
               "do not exist in")
  expect_error(FSA:::iHndlCols2UseIgnore(df1,cols2ignore=-(1:5)),
               "do not exist in")
  expect_error(FSA:::iHndlCols2UseIgnore(df1,cols2ignore=0:3),"cannot be zero")
  expect_error(FSA:::iHndlCols2UseIgnore(df1,cols2ignore=c("frank","derek")),
               "None of columns in")
  
  #### Check results using cols2use
  ## use one column by number
  ind <- 1
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2use=ind)
  expect_equivalent(tmp,df1[,ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[ind])
  ## use one column by name
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2use="net")
  expect_equivalent(tmp,df1[,ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[ind])
  ## use two contiguous columns by number
  ind <- 1:2
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2use=ind)
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## use two contiguous columns by name
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2use=c("net","eff"))
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## use two non-contiguous columns by number
  ind <- c(1,3)
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2use=ind)
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## use two non-contiguous columns by name
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2use=c("net","species"))
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## use three columns by number
  ind <- c(1,3,4)
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2use=ind)
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## use three columns by name
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2use=c("net","species","catch"))
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## use four columns by number
  ind <- 1:4
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2use=ind)
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## use four columns by name
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2use=c("net","eff","species","catch"))
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])

  #### Check results using cols2isnore
  ## ignore one column by number
  ind <- 1
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2ignore=ind)
  expect_equivalent(tmp,df1[,-ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore one column by name
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2ignore="net")
  expect_equivalent(tmp,df1[,-ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore two contiguous columns by number
  ind <- 1:2
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2ignore=ind)
  expect_equivalent(tmp,df1[,-ind])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore two contiguous columns by name
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2ignore=c("net","eff"))
  expect_equivalent(tmp,df1[,-ind])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore two non-contiguous columns by number
  ind <- c(1,3)
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2ignore=ind)
  expect_equivalent(tmp,df1[,-ind])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore two non-contiguous columns by name
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2ignore=c("net","species"))
  expect_equivalent(tmp,df1[,-ind])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore three columns by number
  ind <- c(1,3,4)
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2ignore=ind)
  expect_equivalent(tmp,df1[,-ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore three columns by name
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2ignore=c("net","species","catch"))
  expect_equivalent(tmp,df1[,-ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore four columns by number
  ind <- 1:4
  tmp <- suppressWarnings(FSA:::iHndlCols2UseIgnore(df1,cols2ignore=ind))
  expect_equivalent(tmp,df1[,-ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[-ind])
  ## ignore four columns by name
  tmp <- suppressWarnings(
    FSA:::iHndlCols2UseIgnore(df1,cols2ignore=c("net","eff","species","catch")))
  expect_equivalent(tmp,df1[,-ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[-ind])
  
  #### Check results with cols2ignore using negative indices
  ## ignore one column by number
  ind <- -1
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2ignore=ind)
  expect_equivalent(tmp,df1[,ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[ind])
  ## ignore two contiguous columns by number
  ind <- -c(1:2)
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2ignore=ind)
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## ignore two non-contiguous columns by number
  ind <- -c(1,3)
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2ignore=ind)
  expect_equivalent(tmp,df1[,ind])
  expect_equivalent(names(tmp),nms[ind])
  ## ignore three columns by number
  ind <- -c(1,3,4)
  tmp <- FSA:::iHndlCols2UseIgnore(df1,cols2ignore=ind)
  expect_equivalent(tmp,df1[,ind,drop=FALSE])
  expect_equivalent(names(tmp),nms[ind])
  ## ignore three columns by number
  ind <- -c(1:4)
  tmp <- suppressWarnings(FSA:::iHndlCols2UseIgnore(df1,cols2ignore=ind))
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


test_that("iLegendHel() messages and results",{
  expect_error(iLegendHelp("Derek"),"Must use proper keyword")
  tmp <- FSA:::iLegendHelp("topright")
  expect_true(tmp$do.legend)
  expect_equal(tmp$x,"topright")
  expect_null(tmp$y)
  tmp <- FSA:::iLegendHelp(FALSE)
  expect_false(tmp$do.legend)
  expect_null(tmp$x)
  expect_null(tmp$y)
  tmp <- FSA:::iLegendHelp(c(1,1))
  expect_true(tmp$do.legend)
  expect_equal(tmp$x,1)
  expect_equal(tmp$y,1)
  
})

test_that("iListSpecies() messages",{
  expect_message(capture.output(FSA:::iListSpecies(PSDlit)))
  expect_output(suppressMessages(FSA:::iListSpecies(PSDlit)))
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
  nl1 <- nls(mirex~B1/(1+exp(B2+B3*weight)),start=list(B1=0.4,B2=2,B3=-0.5),
             data=Mirex)
  expect_error(FSA:::iTypeoflm(nl1),"only works with")
  Mirex$speciesZ <- as.character(Mirex$species)
  Mirex$yearZ <- as.character(Mirex$year)
  tmp <- lm(mirex~weight*speciesZ,data=Mirex)
  expect_warning(FSA:::iTypeoflm(tmp),"variable is a 'character'")
  tmp <- lm(mirex~yearZ*speciesZ,data=Mirex)
  expect_warning(FSA:::iTypeoflm(tmp),"variable is a 'character'")
})
