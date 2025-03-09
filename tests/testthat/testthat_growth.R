## Simulate some data sets ----
# Setup ages, sample sizes (general reduction in numbers with
#   increasing age), and additive SD to model
set.seed(234234)
t <- 0:15
n <- c(5,10,40,35,25,13,10,10,8,6,5,3,3,3,2,2)
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
df$tlr <- round(r1(df$age,Linf=450,ti=2,k=0.5,b=-0.7)+rnorm(sum(n),0,sd),0)
df$cat <- as.factor(rep(c("A","B","C"),each=sum(n)/3))

## Test Messages ----
test_that("findGrowthStarts() general messages",{
  expect_error(findGrowthStarts(tlv+tlg~age,data=df),
               "Function does not work with more than one variable")
  expect_error(findGrowthStarts(tlv~tlg+age,data=df),
               "'formula' must have only one RHS variable")
  expect_error(findGrowthStarts(tlv~cat,data=df),
               "RHS variable must be numeric")
  expect_error(findGrowthStarts(cat~age,data=df),
               "LHS variable must be numeric")
  expect_error(findGrowthStarts(tlv~age,data=df,type="Derek"),
               "'arg' should be one of")
  expect_warning(findGrowthStarts(tlv~age,data=df,type="Schnute"),
                 "Starting values not yet implemented")
  expect_warning(findGrowthStarts(tlv~age,data=df,type="Schnute-Richards"),
                 "Starting values not yet implemented")
})

test_that("findGrowthStarts() von Bertalanffy messages",{
  expect_error(findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=0),
               "'param' must be between 1 and 19")
  expect_error(findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=20),
               "'param' must be between 1 and 19")
  for (i in c(9,13:19))
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
               "'param' must be between 1 and 7")
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
               "'param' must be between 1 and 4")
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
               "'param' must be between 1 and 3")
  expect_error(findGrowthStarts(tlr~age,data=df,type="Richards",param=6),
               "'param' must be between 1 and 3")
  
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


test_that("makeGrowthFun() messages",{
  expect_error(makeGrowthFun(type="Derek",param=1),
               "'arg' should be one of")
  expect_error(makeGrowthFun(type="von Bertalanffy",param=0),
               "'param' must be between 1 and 19")
  expect_error(makeGrowthFun(type="von Bertalanffy",param=20),
               "'param' must be between 1 and 19")
  expect_error(makeGrowthFun(type="Gompertz",param=0),
               "'param' must be between 1 and 7")
  expect_error(makeGrowthFun(type="Gompertz",param=8),
               "'param' must be between 1 and 7")
  expect_error(makeGrowthFun(type="logistic",param=0),
               "'param' must be between 1 and 4")
  expect_error(makeGrowthFun(type="logistic",param=5),
               "'param' must be between 1 and 4")
  expect_error(makeGrowthFun(type="Richards",param=0),
               "'param' must be between 1 and 3")
  expect_error(makeGrowthFun(type="Richards",param=6),
               "'param' must be between 1 and 3")
  expect_error(makeGrowthFun(type="Schnute",param=0),
               "'param' can only be 1")
  expect_error(makeGrowthFun(type="Schnute",param=2),
               "'param' can only be 1")
  expect_error(makeGrowthFun(type="Schnute-Richards",param=0),
               "'param' can only be 1")
  expect_error(makeGrowthFun(type="Schnute-Richards",param=2),
               "'param' can only be 1")
})

test_that("showGrowthFun() messages",{
  expect_error(showGrowthFun(type="Derek",param=1),
               "'arg' should be one of")
  expect_error(showGrowthFun(type="von Bertalanffy",param=0),
               "'param' must be between 1 and 19")
  expect_error(showGrowthFun(type="von Bertalanffy",param=20),
               "'param' must be between 1 and 19")
  expect_error(showGrowthFun(type="Gompertz",param=0),
               "'param' must be between 1 and 7")
  expect_error(showGrowthFun(type="Gompertz",param=8),
               "'param' must be between 1 and 7")
  expect_error(showGrowthFun(type="logistic",param=0),
               "'param' must be between 1 and 4")
  expect_error(showGrowthFun(type="logistic",param=5),
               "'param' must be between 1 and 4")
  expect_error(showGrowthFun(type="Richards",param=0),
               "'param' must be between 1 and 3")
  expect_error(showGrowthFun(type="Richards",param=6),
               "'param' must be between 1 and 3")
  expect_error(showGrowthFun(type="Schnute",case=0),
               "'case' must be between 1 and 4")
  expect_error(showGrowthFun(type="Schnute",case=5),
               "'case' must be between 1 and 4")
  expect_error(showGrowthFun(type="Schnute-Richards",param=0),
               "'param' can only be 1")
  expect_error(showGrowthFun(type="Schnute-Richards",param=2),
               "'param' can only be 1")
  expect_error(showGrowthFun(type="von Bertalanffy",case=1),
               "'case' only used when 'type' is 'Schnute'")
  expect_error(showGrowthFun(type="Gompertz",case=1),
               "'case' only used when 'type' is 'Schnute'")
  expect_error(showGrowthFun(type="logistic",case=1),
               "'case' only used when 'type' is 'Schnute'")
  expect_error(showGrowthFun(type="Richards",case=1),
               "'case' only used when 'type' is 'Schnute'")
  expect_error(showGrowthFun(type="Schnute-Richards",case=1),
               "'case' only used when 'type' is 'Schnute'")
  tmp <- c(9,18,19)
  for (i in tmp) expect_error(showGrowthFun(type="von Bertalanffy",param=i),
                              "Not yet implemented for")
  expect_error(showGrowthFun(type="Schnute-Richards"),"Not yet implemented for")
})


## Test Output Types ----
test_that("param(s) equal pname(s)",{
  ## Test that pname(s) go to the right param(s) ... assume other functionality
  ##   works as tested above and below after that
  ## This param_list comes from iHndlGrowthModelParams
  param_list <- list(
    "von Bertalanffy"=data.frame(pnum=c(1,1,1,2,2,2,3,4,5,6,6,7,8,9,9,9,9,10,10,
                                        11,12,13,13,14,15,15,16,17,18,19),
                                 pnms=c("typical","Typical","Beverton-Holt",
                                        "original","Original","von Bertalanffy",
                                        "Gallucci-Quinn","Mooij","Weisberg",
                                        "Ogle-Isermann","Ogle",
                                        "Schnute","Francis",
                                        "double","Double","Laslett","Polacheck",
                                        "Somers","Somers1","Somers2","Pauly",
                                        "Fabens","Fabens1","Fabens2",
                                        "Wang","Wang1","Wang2","Wang3",
                                        "Francis2","Francis3")),
    "Gompertz"=data.frame(pnum=c(1,1,1,2,3,3,4,4,5,6,6,7),
                          pnms=c("original","Original","Gompertz",
                                 "Ricker1","Ricker2","Quinn-Deriso1",
                                 "Ricker3","Quinn-Deriso2","Quinn-Deriso3",
                                 "Troynikov","Troynikov1","Troynikov2")),
    "logistic"=data.frame(pnum=c(1,2,3,4),
                          pnms=c("Campana-Jones1","Campana-Jones2","Karkach","Haddon")),
    "Richards"=data.frame(pnum=c(1,2,3),
                          pnms=c("Tjorve4","Tjorve3","Tjorve7")))
  
  for (i in c("von Bertalanffy","Gompertz","logistic","Richards")) {
    for (j in 1:nrow(param_list[[i]])) {
      expect_equal(makeGrowthFun(type=i,param=param_list[[i]]$pnum[j]),
                   makeGrowthFun(type=i,pname=param_list[[i]]$pnms[j]))
      ## skip over those where it is not implemented
      if (!(i=="von Bertalanffy" & param_list[[i]]$pnum[j] %in% c(9,18,19))) {
        expect_equal(showGrowthFun(type=i,param=param_list[[i]]$pnum[j]),
                     showGrowthFun(type=i,pname=param_list[[i]]$pnms[j]))
      }
    }
  }
})


test_that("findGrowthStarts() von Bertalanffy outputs",{
  ## Check that vectors are named with proper model parameters
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=1)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","K","t0"))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=2)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","K","L0"))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=3)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("omega","K","t0"))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=4)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","L0","omega"))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=5)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","t0","t50"))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=6,
                          constvals=c(tr=2))
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","K","Lr"))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=6,
                          constvals=c(Lr=237))
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","K","tr"))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=7,
                          constvals=c(t1=1,t3=13))
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("L1","L3","K"))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=8,
                          constvals=c(t1=1,t3=13))
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("L1","L2","L3"))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=10)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","K","t0","C","ts"))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=11)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","K","t0","C","WP"))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=12)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","Kpr","t0","ts","NGT"))
  
  # Check that values are fixed as expected ... did not check all possible
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=1,
                          fixed=c(Linf=500))
  expect_equal(tmp[["Linf"]],500)
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=2,
                          fixed=c(K=0.5))
  expect_equal(tmp[["K"]],0.5)
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=3,
                          fixed=c(omega=50))
  expect_equal(tmp[["omega"]],50)
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=5,
                          fixed=c(t50=5))
  expect_equal(tmp[["t50"]],5)
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=6,
                          constvals=c(tr=2),fixed=c(Lr=300))
  expect_equal(tmp[["Lr"]],300)
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=6,
                          constvals=c(Lr=237),fixed=c(tr=5))
  expect_equal(tmp[["tr"]],5)
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=7,
                          constvals=c(t1=1,t3=13),fixed=c(L1=100))
  expect_equal(tmp[["L1"]],100)
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=8,
                          constvals=c(t1=1,t3=13),fixed=c(L1=100,L2=300))
  expect_equal(tmp[["L1"]],100)
  expect_equal(tmp[["L2"]],300)
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=10,
                          fixed=c(Linf=500,ts=0.5))
  expect_equal(tmp[["Linf"]],500)
  expect_equal(tmp[["ts"]],0.5)
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=11,
                          fixed=c(WP=0.5))
  expect_equal(tmp[["WP"]],0.5)
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=12,
                          fixed=c(NGT=0.5))
  expect_equal(tmp[["NGT"]],0.5)
  
})

test_that("findGrowthStarts() Gompertz outputs",{
  ## Check that vectors are named with proper model parameters
  tmp <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=1)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","gi","a1"))
  tmp <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=2)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","gi","ti"))
  tmp <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=3)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("L0","gi","a2"))
  tmp <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=4)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","gi","a2"))
  tmp <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=5)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","gi","t0"))

  # Check that values are fixed as expected ... did not check all possible
  tmp <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=1,
                          fixed=c(Linf=500))
  expect_equal(tmp[["Linf"]],500)
  tmp <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=1,
                          fixed=c(Linf=500,gi=0.5))
  expect_equal(tmp[["Linf"]],500)
  expect_equal(tmp[["gi"]],0.5)
  tmp <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=2,
                          fixed=c(ti=5))
  expect_equal(tmp[["ti"]],5)
  tmp <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=3,
                          fixed=c(L0=5,a2=1))
  expect_equal(tmp[["L0"]],5)
  expect_equal(tmp[["a2"]],1)
  tmp <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=4,
                          fixed=c(a2=1))
  expect_equal(tmp[["a2"]],1)
  tmp <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=5,
                          fixed=c(t0=-1))
  expect_equal(tmp[["t0"]],-1)
})

test_that("findGrowthStarts() logistic outputs",{
  ## Check that vectors are named with proper model parameters
  tmp <- findGrowthStarts(tll~age,data=df,type="logistic",param=1)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","gninf","ti"))
  tmp <- findGrowthStarts(tll~age,data=df,type="logistic",param=2)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","gninf","a"))
  tmp <- findGrowthStarts(tll~age,data=df,type="logistic",param=3)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","gninf","L0"))
  
  # Check that values are fixed as expected ... did not check all possible
  tmp <- findGrowthStarts(tll~age,data=df,type="logistic",param=1,
                          fixed=c(Linf=500))
  expect_equal(tmp[["Linf"]],500)
  tmp <- findGrowthStarts(tll~age,data=df,type="logistic",param=2,
                          fixed=c(Linf=500,gninf=0.5))
  expect_equal(tmp[["Linf"]],500)
  expect_equal(tmp[["gninf"]],0.5)
  tmp <- findGrowthStarts(tll~age,data=df,type="logistic",param=2,
                          fixed=c(a=0.5))
  expect_equal(tmp[["a"]],0.5)
  tmp <- findGrowthStarts(tll~age,data=df,type="logistic",param=3,
                          fixed=c(L0=5))
  expect_equal(tmp[["L0"]],5)
})


test_that("findGrowthStarts() Richards outputs",{
  ## Check that vectors are named with proper model parameters
  tmp <- findGrowthStarts(tlr~age,data=df,type="Richards",param=1)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","k","ti","b"))
  tmp <- findGrowthStarts(tlr~age,data=df,type="Richards",param=2)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","k","t0","b"))
  tmp <- findGrowthStarts(tlr~age,data=df,type="Richards",param=3)
  expect_equal(class(tmp),"numeric")
  expect_named(tmp,c("Linf","k","L0","b"))
  
  # Check that values are fixed as expected ... did not check all possible
  tmp <- findGrowthStarts(tlr~age,data=df,type="Richards",param=1,
                          fixed=c(Linf=500))
  expect_equal(tmp[["Linf"]],500)
  tmp <- findGrowthStarts(tlr~age,data=df,type="Richards",param=1,
                          fixed=c(Linf=500,b=0.5))
  expect_equal(tmp[["Linf"]],500)
  expect_equal(tmp[["b"]],0.5)
  tmp <- findGrowthStarts(tlr~age,data=df,type="Richards",param=2,
                          fixed=c(t0=-1,b=0.5))
  expect_equal(tmp[["t0"]],-1)
  expect_equal(tmp[["b"]],0.5)
  tmp <- findGrowthStarts(tlr~age,data=df,type="Richards",param=3,
                          fixed=c(L0=5,b=0.5))
  expect_equal(tmp[["L0"]],5)
  expect_equal(tmp[["b"]],0.5)
})

test_that("makeGrowthFun() von Bertalanffy output",{
  ptmp <- list("1"=c("t","Linf","K","t0"),
               "2"=c("t","Linf","K","L0"),
               "3"=c("t","omega","K","t0"),
               "4"=c("t","Linf","L0","omega"),
               "5"=c("t","Linf","t0","t50"),
               "6"=c("t","Linf","K","tr","Lr"),
               "7"=c("t","L1","L3","K","t1","t3"),
               "8"=c("t","L1","L2","L3","t1","t3"),
               "9"=c("t","Linf","K1","K2","t0","a","b"),
               "10"=c("t","Linf","K","t0","C","ts"),
               "11"=c("t","Linf","K","t0","C","WP"),
               "12"=c("t","Linf","Kpr","t0","ts","NGT"),
               "13"=c("Lm","dt","Linf","K"),
               "14"=c("Lm","dt","Linf","K"),
               "15"=c("Lm","dt","Linf","K","b"),
               "16"=c("Lm","dt","K","a","b"),
               "17"=c("Lm","dt","K","a","b"),
               "18"=c("Lm","dt","g1","g2","L1","L2"),
               "19"=c("Lm","t1","t2","g1","g2","w","u","L1","L2"))
  nnull <- list("1"=c(1:2),"2"=c(1:2),"3"=c(1:2),"4"=c(1:2),"5"=c(1:2),"6"=c(1:2),
                "7"=c(1:2,5),"8"=c(1:2,5),
                "9"=c(1:2),"10"=c(1:2),"11"=c(1:2),"12"=c(1:2),
                "13"=c(1:3),"14"=c(1:3),"15"=c(1:3),"16"=c(1:3),"17"=c(1:3),
                "18"=c(1:3,5),
                "19"=c(1:4,8))
  itmp <- names(ptmp)
  for (i in itmp) {
    #print(i) # uncomment if need to find where expectation is not met
    tmp <- makeGrowthFun(type="von Bertalanffy",param=as.numeric(i))
    expect_equal(mode(tmp),"function")
    expect_equal(names(formals(tmp)),ptmp[[i]])
    expect_true(all(sapply(formals(tmp),FUN=is.null)[-nnull[[i]]]))
    tmp2 <- makeGrowthFun(type="von Bertalanffy",param=as.numeric(i),simple=TRUE)
    expect_equal(mode(tmp2),"function")
    expect_equal(names(formals(tmp2)),ptmp[[i]])
    expect_true(all(!sapply(formals(tmp2),FUN=is.null)))       # none NULL
    expect_message(makeGrowthFun(type="von Bertalanffy",param=as.numeric(i),msg=TRUE),
                   paste("You have chosen paramaterization",i))
  }
})

test_that("makeGrowthFun() Gompertz output",{
  ptmp <- list("1"=c("t","Linf","gi","a1"),
               "2"=c("t","Linf","gi","ti"),
               "3"=c("t","L0","gi","a2"),
               "4"=c("t","Linf","gi","a2"),
               "5"=c("t","Linf","gi","t0"),
               "6"=c("Lm","dt","Linf","gi"),
               "7"=c("Lm","dt","Linf","gi"))
  nnull <- list("1"=c(1:2),"2"=c(1:2),"3"=c(1:2),"4"=c(1:2),"5"=c(1:2),
                "6"=c(1:3),"7"=c(1:3))
  itmp <- names(ptmp)
  for (i in itmp) {
    #print(i) # uncomment if need to find where expectation is not met
    tmp <- makeGrowthFun(type="Gompertz",param=as.numeric(i))
    expect_equal(mode(tmp),"function")
    expect_equal(names(formals(tmp)),ptmp[[i]])
    expect_true(all(sapply(formals(tmp),FUN=is.null)[-nnull[[i]]]))
    tmp2 <- makeGrowthFun(type="Gompertz",param=as.numeric(i),simple=TRUE)
    expect_equal(mode(tmp2),"function")
    expect_equal(names(formals(tmp2)),ptmp[[i]])
    expect_true(all(!sapply(formals(tmp2),FUN=is.null)))       # none NULL
    expect_message(makeGrowthFun(type="Gompertz",param=as.numeric(i),msg=TRUE),
                   paste("You have chosen paramaterization",i))
  }
})

test_that("makeGrowthFun() logistic output",{
  ptmp <- list("1"=c("t","Linf","gninf","ti"),
               "2"=c("t","Linf","gninf","a"),
               "3"=c("t","Linf","gninf","L0"),
               "4"=c("Lm","dLmax","L50","L95"))
  nnull <- 1:2
  itmp <- names(ptmp)
  for (i in itmp) {
    #print(i) # uncomment if need to find where expectation is not met
    tmp <- makeGrowthFun(type="logistic",param=as.numeric(i))
    expect_equal(mode(tmp),"function")
    expect_equal(names(formals(tmp)),ptmp[[i]])
    expect_true(all(sapply(formals(tmp),FUN=is.null)[-nnull]))
    tmp2 <- makeGrowthFun(type="logistic",param=as.numeric(i),simple=TRUE)
    expect_equal(mode(tmp2),"function")
    expect_equal(names(formals(tmp2)),ptmp[[i]])
    expect_true(all(!sapply(formals(tmp2),FUN=is.null)))       # none NULL
    expect_message(makeGrowthFun(type="logistic",param=as.numeric(i),msg=TRUE),
                   paste("You have chosen paramaterization",i))
  }
})

test_that("makeGrowthFun() Richards output",{
  ptmp <- list("1"=c("t","Linf","k","ti","b"),
               "2"=c("t","Linf","k","t0","b"),
               "3"=c("t","Linf","k","L0","b"))
  nnull <- 1:2
  itmp <- names(ptmp)
  for (i in itmp) {
    #print(i) # uncomment if need to find where expectation is not met
    tmp <- makeGrowthFun(type="Richards",param=as.numeric(i))
    expect_equal(mode(tmp),"function")
    expect_equal(names(formals(tmp)),ptmp[[i]])
    expect_true(all(sapply(formals(tmp),FUN=is.null)[-nnull]))
    tmp2 <- makeGrowthFun(type="Richards",param=as.numeric(i),simple=TRUE)
    expect_equal(mode(tmp2),"function")
    expect_equal(names(formals(tmp2)),ptmp[[i]])
    expect_true(all(!sapply(formals(tmp2),FUN=is.null)))       # none NULL
    expect_message(makeGrowthFun(type="Richards",param=as.numeric(i),msg=TRUE),
                   paste("You have chosen paramaterization",i))
  }
})

test_that("makeGrowthFun() Other Model output",{
  ptmp <- list("Schnute"=c("t","L1","L3","a","b","t1","t3"),
               "Schnute-Richards"=c("t","Linf","k","a","b","c"))
  nnull <- list("Schnute"=c(1:2,6),
                "Schnute-Richards"=c(1:2))
  itmp <- names(ptmp)
  for (i in seq_along(itmp)) {
    #print(i) # uncomment if need to find where expectation is not met
    tmp <- makeGrowthFun(type=itmp[i],param=1)
    expect_equal(mode(tmp),"function")
    expect_equal(names(formals(tmp)),ptmp[[i]])
    expect_true(all(sapply(formals(tmp),FUN=is.null)[-nnull[[i]]]))
    tmp2 <- makeGrowthFun(type=itmp[i],param=1,simple=TRUE)
    expect_equal(mode(tmp2),"function")
    expect_equal(names(formals(tmp2)),ptmp[[i]])
    expect_true(all(!sapply(formals(tmp2),FUN=is.null)))       # none NULL
    expect_message(makeGrowthFun(type=itmp[i],param=1,msg=TRUE),
                   paste("You have chosen the",itmp[i],"growth function"))
  }
})

test_that("showGrowthFun()outputs",{
  tmp <- c(1:8,10:17)
  for (i in tmp) expect_equal(class(showGrowthFun(type="von Bertalanffy",param=i,parse=TRUE)),
                              "expression")
  for (i in 1:7) expect_equal(class(showGrowthFun(type="Gompertz",param=i,parse=TRUE)),
                              "expression")
  for (i in 1:7) expect_equal(class(showGrowthFun(type="Gompertz",param=i,parse=TRUE)),
                              "expression")
  for (i in 1:3) expect_equal(class(showGrowthFun(type="Richards",param=i,parse=TRUE)),
                              "expression")
  for (i in 1:4) expect_equal(class(showGrowthFun(type="Schnute",case=i,parse=TRUE)),
                              "expression")
})

## Validate Results ----

test_that("findGrowthStarts() von Bertalanffy results",{
  # Get starting values from SSasymp
  sstmp <- stats::getInitial(tlv~stats::SSasymp(age,Asym,R0,lrc),data=df)
  # extract results according to "Growth_Starting_Values.qmd" document
  Linf <- sstmp[["Asym"]]
  L0 <- sstmp[["R0"]]
  K <- exp(sstmp[["lrc"]])
  t0 <- -log(Linf/(Linf-L0))/K
  omega <- K*Linf
  t50 <- t0+log(2)/K
  tr150 <- -log((Linf-150)/(Linf-L0))/K
  Lr5 <- Linf*(1-exp(-K*(5-t0)))
  L1 <- Linf*(1-exp(-K*(2-t0)))
  L3 <- Linf*(1-exp(-K*(12-t0)))
  L2 <- Linf*(1-exp(-K*(7-t0)))
  
  # Test findGrowthStarts against those values for each parameterization
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=1)
  expect_equal(tmp,c(Linf=Linf,K=K,t0=t0))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=2)
  expect_equal(tmp,c(Linf=Linf,K=K,L0=L0))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=3)
  expect_equal(tmp,c(omega=omega,K=K,t0=t0))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=4)
  expect_equal(tmp,c(Linf=Linf,L0=L0,omega=omega))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=5)
  expect_equal(tmp,c(Linf=Linf,t0=t0,t50=t50))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=6,
                          constvals=c(tr=5))
  expect_equal(tmp,c(Linf=Linf,K=K,Lr=Lr5))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=6,
                          constvals=c(Lr=150))
  expect_equal(tmp,c(Linf=Linf,K=K,tr=tr150))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=7,
                          constvals=c(t1=2,t3=12))
  expect_equal(tmp,c(L1=L1,L3=L3,K=K))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=8,
                          constvals=c(t1=2,t3=12))
  expect_equal(tmp,c(L1=L1,L2=L2,L3=L3))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=10)
  expect_equal(tmp,c(Linf=Linf,K=K,t0=t0,C=0.5,ts=0.3))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=11)
  expect_equal(tmp,c(Linf=Linf,K=K,t0=t0,C=0.5,WP=0.8))
  tmp <- findGrowthStarts(tlv~age,data=df,type="von Bertalanffy",param=12)
  expect_equal(tmp,c(Linf=Linf,Kpr=K/(1-0.3),t0=t0,ts=0.3,NGT=0.3))
})

test_that("findGrowthStarts() Gompertz results",{
  # Get starting values from SSgompertz
  sstmp <- stats::getInitial(tlg~stats::SSgompertz(age,Asym,b2,b3),data=df)
  # extract results according to "Growth_Starting_Values.qmd" document
  Linf <- sstmp[["Asym"]]
  gi <- -log(sstmp[["b3"]])
  a2 <- sstmp[["b2"]]
  a1 <- log(a2)
  ti <- log(a2)/gi
  t0 <- log(a2*gi)/gi
  L0 <- Linf/exp(a2)
  
  # Test findGrowthStarts against those values for each parameterization
  tmp <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=1)
  expect_equal(tmp,c(Linf=Linf,gi=gi,a1=a1))
  tmp <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=2)
  expect_equal(tmp,c(Linf=Linf,gi=gi,ti=ti))
  tmp <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=3)
  expect_equal(tmp,c(L0=L0,gi=gi,a2=a2))
  tmp <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=4)
  expect_equal(tmp,c(Linf=Linf,gi=gi,a2=a2))
  tmp <- findGrowthStarts(tlg~age,data=df,type="Gompertz",param=5)
  expect_equal(tmp,c(Linf=Linf,gi=gi,t0=t0))
})

test_that("findGrowthStarts() logistic results",{
  # Get starting values from SSlogis
  sstmp <- stats::getInitial(tll~stats::SSlogis(age,Asym,xmid,scal),data=df)
  # extract results according to "Growth_Starting_Values.qmd" document
  Linf <- sstmp[["Asym"]]
  gninf <- 1/sstmp[["scal"]]
  ti <- sstmp[["xmid"]]
  a <- sstmp[["xmid"]]/sstmp[["scal"]]
  L0 <- Linf/(1+exp(a))
 
  # Test findGrowthStarts against those values for each parameterization
  tmp <- findGrowthStarts(tll~age,data=df,type="logistic",param=1)
  expect_equal(tmp,c(Linf=Linf,gninf=gninf,ti=ti))
  tmp <- findGrowthStarts(tll~age,data=df,type="logistic",param=2)
  expect_equal(tmp,c(Linf=Linf,gninf=gninf,a=a))
  tmp <- findGrowthStarts(tll~age,data=df,type="logistic",param=3)
  expect_equal(tmp,c(Linf=Linf,gninf=gninf,L0=L0))
})

test_that("findGrowthStarts() Richards results",{
  # Get starting values from FlexParamCurve
  sstmp <- FlexParamCurve::modpar(df$age,df$tlr,
                                  pn.options="sstmp",width.bounds=2,force4par=TRUE,
                                  verbose=FALSE,suppress.text=TRUE)
  # extract results according to "Growth_Starting_Values.qmd" document
  Linf <- sstmp[["Asym"]]
  k <- sstmp[["K"]]
  ti <- sstmp[["Infl"]]
  b <- -1/sstmp[["M"]]
  t0 <- -(log(-b)/k)+ti
  L0 <- Linf*((1-(1/b)*exp(k*ti))^(b))
  
  # Test findGrowthStarts against those values for each parameterization
  tmp <- findGrowthStarts(tlr~age,data=df,type="Richards",param=1)
  expect_equal(tmp,c(Linf=Linf,k=k,ti=ti,b=b))
  tmp <- findGrowthStarts(tlr~age,data=df,type="Richards",param=2)
  expect_equal(tmp,c(Linf=Linf,k=k,t0=t0,b=b))
  tmp <- findGrowthStarts(tlr~age,data=df,type="Richards",param=3)
  expect_equal(tmp,c(Linf=Linf,k=k,L0=L0,b=b))
})
