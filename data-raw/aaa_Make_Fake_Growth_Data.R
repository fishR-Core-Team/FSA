#===== Prep
vb1 <- FSA::makeGrowthFun(type="von Bertalanffy")
g1 <- FSA::makeGrowthFun(type="Gompertz",param=2)
l1 <- FSA::makeGrowthFun(type="logistic")
r1 <- FSA::makeGrowthFun(type="Richards")

s1 <- FSA::makeGrowthFun(type="von Bertalanffy",pname="Somers")


#===== Make fake length at annual age data
# Setup ages, sample sizes (general reduction in numbers with increasing age),
#   and additive SD to model
set.seed(234234)
t <- 0:15
n <- c(5,10,40,35,25,12,10,10,8,6,5,3,3,3,2,2)
N <- sum(n)
sd <- 15
# Expand ages and put in a data.frame
GrowthData1 <- dplyr::tibble(age=rep(t,n)) |>
  dplyr::mutate(tlV=round(vb1(age,Linf=450,K=0.3,t0=-0.5)+rnorm(N,0,sd),0),
                tlG=round(g1(age,Linf=450,gi=0.3,ti=3)+rnorm(N,0,sd),0),
                tlL=round(l1(age,Linf=450,gninf=0.5,ti=3)+rnorm(N,0,sd),0),
                tlR=round(r1(age,Linf=450,k=0.5,ti=3,b=-0.5)+rnorm(N,0,sd),0)) |>
  as.data.frame()

# brief view of data
head(GrowthData1)
plot(tlV~age,data=GrowthData1)
plot(tlG~age,data=GrowthData1)
plot(tlL~age,data=GrowthData1)
plot(tlR~age,data=GrowthData1)

# write out the Rdata file
usethis::use_data(GrowthData1,internal=FALSE,overwrite=TRUE)


#===== Make fake length at seasonal age data
set.seed(234234)
t <- 0:6
n <- c(15,20,40,25,13,8,5)
N <- sum(n)
sd <- 5

# Expand ages and put in a data.frame
GrowthData2 <- dplyr::tibble(age=rep(t,n)+round(runif(sum(n),min=0.2,max=0.75),1)) |>
  dplyr::mutate(tlS=round(s1(age,Linf=450,K=0.5,t0=-0.5,C=0.9,ts=0.1)+rnorm(N,0,sd),0)) |>
  as.data.frame()

# brief view of data
head(GrowthData2)
plot(tlS~age,data=GrowthData2)

# write out the Rdata file
usethis::use_data(GrowthData2,internal=FALSE,overwrite=TRUE)



#===== Make fake tag-recapture age data
set.seed(234234)
t <- 1:8
n <- c(15,20,40,25,13,8,5,2)
N <- sum(n)
sd <- 5

# Generate data with (assuming sampling at roughly same time each year) ...
# ... fake tag number
# ... annual ages
# ... lengths at markig from VB with random error
# ... days-at-large of 1-3 years (more at 1, fewer at 3) + a small random days off year
# ... Length-at-recap predicted from VB using age+deltat + error
GrowthData3 <- dplyr::tibble(tag=sample(1:1000,N)) |>
  dplyr::mutate(age=rep(t,n),
                tlM=round(vb1(age,Linf=450,K=0.3,t0=-0.5)+rnorm(N,0,sd),0),
                deltat=sample(1:3,size=N,prob=c(0.5,0.33,0.17),replace=TRUE)+runif(N,min=-0.15,max=0.15),
                tlR=round(vb1(age+deltat,Linf=450,K=0.3,t0=-0.5)+rnorm(N,0,sd),0),
                deltaL=tlR-tlM) |>
  dplyr::arrange(tag) |>
  dplyr::select(tag,tlM,tlR,deltat,deltaL) |>
  as.data.frame()

# Brief view of data
head(GrowthData3)

plot(tlR~tlM,data=GrowthData3)
abline(a=0,b=1,col="red")

# write out the Rdata file
usethis::use_data(GrowthData3,internal=FALSE,overwrite=TRUE)
