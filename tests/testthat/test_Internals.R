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