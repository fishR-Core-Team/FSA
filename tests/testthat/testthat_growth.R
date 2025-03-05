## Simulate some data sets ----
# Setup ages, sample sizes (general reduction in numbers with
#   increasing age), and additive SD to model
set.seed(234234)
t <- 0:15
n <- c(5,10,40,35,25,12,10,10,8,6,5,3,3,3,2,2)
sd <- 15
# Expand ages and put in a data.frame
df <- data.frame(age=rep(t,n))
# Add lengths from 1st parameterization of each model type with
#   random error for individuals
vb1 <- makeGrowthFun(type="von Bertalanffy")
df$tlv <- round(vb1(df$age,Linf=450,K=0.3,t0=-0.5)+rnorm(sum(n),0,sd),0)
g1 <- makeGrowthFun(type="Gompertz")
df$tlg <- round(g1(df$age,Linf=450,a=1,g=0.3)+rnorm(sum(n),0,sd),0)
l1 <- makeGrowthFun(type="logistic")
df$tll <- round(l1(df$age,Linf=450,gninf=0.3,ti=2)+rnorm(sum(n),0,sd),0)
r1 <- makeGrowthFun(type="Richards")
df$tlr <- round(r1(df$age,Linf=450,ti=2,k=0.5,b1=1.5)+rnorm(sum(n),0,sd),0)


## Test Messages ----
test_that("findGrowthStarts() von Bertalanffy messages",{
  expect_error(findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=0),
               "'param' must be greater than 1")
  expect_error(findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=20),
               "'param' must be between 1 and 19")
  for (i in c(9,15:19))
    expect_warning(findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=i),
                   "Starting values not yet implemented in 'FSA'")
  
  expect_error(findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=1,
                                fixed=list("Linf"=500)),
               "'fixed' should be a vector rather than a list")
  expect_error(findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=1,
                                fixed=c("Linf"="A")),
               "'fixed' must be numeric")
  expect_warning(findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=1,
                                  fixed=c("L0"=1)),
                 "Some names in 'fixed'")
  expect_warning(findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=2,
                                  fixed=c("t0"=1)),
                 "Some names in 'fixed'")
  
  for (i in 6:8)
    expect_error(findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=i),
                 "You must use 'constvals' with")
  expect_error(findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=6,
                                constvals=list("tr"=1)),
               "'constvals' should be a vector rather than a list")
  expect_error(findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=6,
                                constvals=c("t1"=1)),
               "Value names in 'constvals' must be 'Lr' or 'tr'")
  expect_error(findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=6,
                                constvals=c("t1"="A")),
               "'constvals' must be numeric")
  expect_error(findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=7,
                                constvals=c("t1"=1)),
               "'constvals' must have exactly two values")
  expect_error(findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=7,
                                constvals=c("t1"=1,"t2"=12)),
               "Value names in 'constvals' must be 't1' and 't3'")
  expect_error(findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=8,
                                constvals=c("t1"=1,"t2"=12)),
               "Value names in 'constvals' must be 't1' and 't3'")

  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=1,
                   fixed=c("Linf"=-1)) %>%
    expect_warning("Starting value for 'Linf' is negative") %>%
    expect_warning("Starting value for 't0' is greater than 2")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=1,
                   fixed=c("Linf"=100)) %>%
    expect_warning("Starting value for 'Linf' is very different from the")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=1,
                   fixed=c("Linf"=1000)) %>%
    expect_warning("Starting value for 'Linf' is very different from the")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=1,
                   fixed=c("K"=-0.1)) %>%
    expect_warning("Starting value for 'K' is negative")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=1,
                   fixed=c("K"=1.6)) %>%
    expect_warning("Starting value for 'K' is greater than 1.5")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=1,
                   fixed=c("t0"=-7)) %>%
    expect_warning("Starting value for 't0' is less than -6")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=1,
                   fixed=c("t0"=2.5)) %>%
    expect_warning("Starting value for 't0' is greater than 2")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=2,
                   fixed=c("L0"=-1)) %>%
    expect_warning("Starting value for 'L0' is negative")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=2,
                   fixed=c("L0"=100)) %>%
    expect_warning("Starting value for 'L0' is more than 25% greater than")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=3,
                   fixed=c("omega"=-1)) %>%
    expect_warning("Starting value for 'omega' is negative")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=5,
                   fixed=c("t50"=-1)) %>%
    expect_warning("Starting value for 't50' is negative")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=6,
                   constvals=c("tr"=3),fixed=c("Lr"=-1)) %>%
    expect_warning("Starting value for 'Lr' is negative")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=6,
                   constvals=c("Lr"=200),fixed=c("tr"=-1)) %>%
    expect_warning("Starting value for 'tr' is negative")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=7,
                   constvals=c("t1"=2,"t3"=12),fixed=c("L1"=-1)) %>%
    expect_warning("Starting value for 'L1' is negative")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=7,
                   constvals=c("t1"=2,"t3"=12),fixed=c("L3"=-1)) %>%
    expect_warning("Starting value for 'L3' is negative")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=8,
                   constvals=c("t1"=2,"t3"=12),fixed=c("L1"=-1)) %>%
    expect_warning("Starting value for 'L1' is negative")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=8,
                   constvals=c("t1"=2,"t3"=12),fixed=c("L3"=-1)) %>%
    expect_warning("Starting value for 'L3' is negative")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=8,
                   constvals=c("t1"=2,"t3"=12),fixed=c("L2"=-1)) %>%
    expect_warning("Starting value for 'L2' is negative")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=10,
                   fixed=c("C"=-1)) %>%
    expect_warning("Starting value for 'C' must be bewteen 0 and 1")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=10,
                   fixed=c("C"=2)) %>%
    expect_warning("Starting value for 'C' must be bewteen 0 and 1")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=10,
                   fixed=c("ts"=-1)) %>%
    expect_warning("Starting value for 'ts' must be bewteen 0 and 1")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=10,
                   fixed=c("ts"=2)) %>%
    expect_warning("Starting value for 'ts' must be bewteen 0 and 1")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=11,
                   fixed=c("WP"=-1)) %>%
    expect_warning("Starting value for 'WP' must be bewteen 0 and 1")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=11,
                   fixed=c("WP"=2)) %>%
    expect_warning("Starting value for 'WP' must be bewteen 0 and 1")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=12,
                   fixed=c("NGT"=-1)) %>%
    expect_warning("Starting value for 'NGT' must be bewteen 0 and 1")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=12,
                   fixed=c("NGT"=2)) %>%
    expect_warning("Starting value for 'NGT' must be bewteen 0 and 1") %>%
    expect_warning("Starting value for 'Kpr' is negative")
  findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=12,
                   fixed=c("Kpr"=-1)) %>%
    expect_warning("Starting value for 'Kpr' is negative")
})

test_that("findGrowthStarts() Gompertz messages",{
  expect_error(findGrowthStarts(tlg~age,data=df,type="Gompertz",param=0),
               "'param' must be greater than 1")
  expect_error(findGrowthStarts(tlg~age,data=df,type="Gompertz",param=8),
               "'param' must be between 1 and 7")
  for (i in 6:7)
    expect_warning(findGrowthStarts(tlg~age,data=df,type="Gompertz",param=i),
                   "Starting values not yet implemented in 'FSA'")
  
  expect_error(findGrowthStarts(tlv~age,data=df,type="Gompertz",param=1,
                                constvals=c("t1"=1)),
               "'constvals' only used with the von Bertalanffy model")
  
  findGrowthStarts(tlg~age,data=df,type="Gompertz",param=1,
                   fixed=c("ti"=0.1)) %>%
    expect_warning("Some names in 'fixed'")
  findGrowthStarts(tlg~age,data=df,type="Gompertz",param=5,
                   fixed=c("a1"=0.1)) %>%
    expect_warning("Some names in 'fixed'")
  
  findGrowthStarts(tlg~age,data=df,type="Gompertz",param=1,
                   fixed=c("Linf"=-1)) %>%
    expect_warning("Starting value for 'Linf' is negative")
  findGrowthStarts(tlg~age,data=df,type="Gompertz",param=1,
                   fixed=c("Linf"=1)) %>%
    expect_warning("Starting value for 'Linf' is very different from")
  findGrowthStarts(tlg~age,data=df,type="Gompertz",param=1,
                   fixed=c("gi"=-0.1)) %>%
    expect_warning("Starting value for 'gi' is negative") %>%
    expect_warning("NaNs produced")
  findGrowthStarts(tlg~age,data=df,type="Gompertz",param=2,
                   fixed=c("gi"=-0.1)) %>%
    expect_warning("Starting value for 'gi' is negative") %>%
    expect_warning("Starting value for 'ti' is negative") %>%
    expect_warning("NaNs produced")
  findGrowthStarts(tlg~age,data=df,type="Gompertz",param=2,
                   fixed=c("ti"=-0.1)) %>%
    expect_warning("Starting value for 'ti' is negative")
})

test_that("findGrowthStarts() logistic  messages",{
  expect_error(findGrowthStarts(tll~age,data=df,type="logistic",param=0),
               "'param' must be greater than 1")
  expect_error(findGrowthStarts(tll~age,data=df,type="logistic",param=5),
               "'param' must be between 1 and 4")
  expect_warning(findGrowthStarts(tll~age,data=df,type="logistic",param=4),
                 "Starting values not yet implemented in 'FSA'")
  
  expect_error(findGrowthStarts(tlv~age,data=df,type="logistic",param=1,
                                constvals=c("t1"=1)),
               "'constvals' only used with the von Bertalanffy model")
  
  findGrowthStarts(tll~age,data=df,type="logistic",param=1,
                   fixed=c("gi"=0.1)) %>%
    expect_warning("Some names in 'fixed'")
  findGrowthStarts(tll~age,data=df,type="logistic",param=3,
                   fixed=c("ti"=0.1)) %>%
    expect_warning("Some names in 'fixed'")

  findGrowthStarts(tll~age,data=df,type="logistic",param=1,
                   fixed=c("Linf"=-1)) %>%
    expect_warning("Starting value for 'Linf' is negative")
  findGrowthStarts(tll~age,data=df,type="logistic",param=1,
                   fixed=c("Linf"=1)) %>%
    expect_warning("Starting value for 'Linf' is very different from")
  findGrowthStarts(tll~age,data=df,type="logistic",param=1,
                   fixed=c("gninf"=-0.1)) %>%
    expect_warning("Starting value for 'gninf' is negative")
  findGrowthStarts(tll~age,data=df,type="logistic",param=3,
                   fixed=c("L0"=-1)) %>%
    expect_warning("Starting value for 'L0' is negative")
  findGrowthStarts(tll~age,data=df,type="logistic",param=3,
                   fixed=c("L0"=1000)) %>%
    expect_warning("Starting value for 'L0' is more than 25% greater")
  
})

test_that("findGrowthStarts() Richards messages",{
  expect_error(findGrowthStarts(tlr~age,data=df,type="Richards",param=0),
               "'param' must be greater than 1")
  expect_error(findGrowthStarts(tlr~age,data=df,type="Richards",param=6),
               "'param' must be between 1 and 5")
  
  expect_error(findGrowthStarts(tlv~age,data=df,type="Richards",param=1,
                                constvals=c("t1"=1)),
               "'constvals' only used with the von Bertalanffy model")
  
  findGrowthStarts(tlr~age,data=df,type="Richards",param=1,
                   fixed=c("K"=0.1)) %>%
    expect_warning("Some names in 'fixed'")
  findGrowthStarts(tlr~age,data=df,type="Richards",param=2,
                   fixed=c("b1"=0.1)) %>%
    expect_warning("Some names in 'fixed'")
  
  findGrowthStarts(tlr~age,data=df,type="Richards",param=1,
                   fixed=c("Linf"=-1)) %>%
    expect_warning("Starting value for 'Linf' is negative")
  findGrowthStarts(tlr~age,data=df,type="Richards",param=1,
                   fixed=c("Linf"=1)) %>%
    expect_warning("Starting value for 'Linf' is very different from")
  findGrowthStarts(tlr~age,data=df,type="Richards",param=1,
                   fixed=c("k"=-1)) %>%
    expect_warning("Starting value for 'k' is negative")
  findGrowthStarts(tlr~age,data=df,type="Richards",param=3,
                   fixed=c("L0"=-1)) %>%
    expect_warning("Starting value for 'L0' is negative")
  
})


## Test Output Types ----


## Validate Results ----
