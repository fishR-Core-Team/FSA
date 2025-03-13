# Used to show the results of showGrowthFun for all type-param combos
# Output is to a plot so not used in testthat format


tstrShowGrowthFun <- function(formula,data,type,param,prms,cv=NULL,SW=FALSE,cex=1) {
  f <- makeGrowthFun(type,ifelse(type=="Schnute",1,param))
  sv <- findGrowthStarts(formula,data,type,param,constvals=cv)
  mf <- stats::model.frame(formula,data=data,na.action=NULL)
  mfn <- names(mf)
  if (length(mfn)==2) form <- paste0(mfn[1],"~f(",mfn[2],",",paste(prms,collapse=","),")")
  else form <- paste0(mfn[1],"~f(",mfn[2],",",mfn[3],",",paste(prms,collapse=","),")")
  rv <- nls(as.formula(form),data=data,start=sv)
  
  tmp1 <- showGrowthFun(type,param,parse=TRUE,stackWhere=SW)
  tmp2 <- showGrowthFun(type,param,fit=rv,constvals=cv,parse=TRUE,stackWhere=SW)
  plot(0,type="n",ylim=c(0,1),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
  text(0.5,0.8,tmp1,cex=cex)
  text(0.5,0.3,tmp2,cex=cex)
}

typ <- "von Bertalanffy"
tstrShowGrowthFun(tlV~age,GrowthData1,typ,1,c("Linf","K","t0"),cex=1.5)
tstrShowGrowthFun(tlV~age,GrowthData1,typ,2,c("Linf","K","L0"),cex=1.5)
tstrShowGrowthFun(tlV~age,GrowthData1,typ,3,c("omega","K","t0"),cex=1.5)
tstrShowGrowthFun(tlV~age,GrowthData1,typ,4,c("Linf","L0","omega"),cex=1.5)
tstrShowGrowthFun(tlV~age,GrowthData1,typ,5,c("Linf","t0","t50"),cex=1.5)
tstrShowGrowthFun(tlV~age,GrowthData1,typ,6,c("Linf","K","Lr","tr=3"),c(tr=3),cex=1.25)
tstrShowGrowthFun(tlV~age,GrowthData1,typ,6,c("Linf","K","Lr=200","tr"),c(Lr=200),cex=1.25)
tstrShowGrowthFun(tlV~age,GrowthData1,typ,7,c("L1","L3","K","t1=1","t3=13"),c(t1=1,t3=13))
tstrShowGrowthFun(tlV~age,GrowthData1,typ,8,c("L1","L2","L3","t1=1","t3=13"),c(t1=1,t3=13))
tstrShowGrowthFun(tlV~age,GrowthData1,typ,8,c("L1","L2","L3","t1=1","t3=13"),c(t1=1,t3=13),SW=TRUE)
tstrShowGrowthFun(tlS~age,GrowthData2,typ,10,c("Linf","K","t0","C","ts"),cex=0.75)
tstrShowGrowthFun(tlS~age,GrowthData2,typ,10,c("Linf","K","t0","C","ts"),cex=1.25,SW=TRUE)
tstrShowGrowthFun(tlS~age,GrowthData2,typ,11,c("Linf","K","t0","C","WP"),cex=0.75)
tstrShowGrowthFun(tlS~age,GrowthData2,typ,11,c("Linf","K","t0","C","WP"),cex=1.25,SW=TRUE)
tstrShowGrowthFun(tlS~age,GrowthData2,typ,12,c("Linf","Kpr","t0","ts","NGT"),cex=0.75)
tstrShowGrowthFun(tlS~age,GrowthData2,typ,12,c("Linf","Kpr","t0","ts","NGT"),cex=1.25,SW=TRUE)
tstrShowGrowthFun(deltaL~deltat+tlM,GrowthData3,typ,13,c("Linf","K"))
tstrShowGrowthFun(deltaL~deltat+tlM,GrowthData3,typ,15,c("Linf","K","b"))
tstrShowGrowthFun(deltaL~deltat+tlM,GrowthData3,typ,16,c("K","a","b"))
tstrShowGrowthFun(deltaL~deltat+tlM,GrowthData3,typ,18,c("g1","g2","L1=100","L2=300"),c(L1=100,L2=300),cex=0.75)

# do separate b/c different response variable
p <- 14
f <- makeGrowthFun(typ,param=p)
cv <- NULL
sv <- findGrowthStarts(deltaL~deltat+tlM,GrowthData3,typ,param=p)
rv <- nls(tlR~f(deltat,tlM,Linf,K),data=GrowthData3,start=sv)
tmp1 <- showGrowthFun(typ,param=p,parse=TRUE)
tmp2 <- showGrowthFun(typ,param=p,fit=rv,constvals=cv,parse=TRUE)
plot(0,type="n",ylim=c(0,1),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
text(0.5,0.8,tmp1,cex=1.25)
text(0.5,0.3,tmp2,cex=1.1)

p <- 17
f <- makeGrowthFun(typ,param=p)
cv <- NULL
sv <- findGrowthStarts(deltaL~deltat+tlM,GrowthData3,typ,param=p)
rv <- nls(tlR~f(deltat,tlM,K,a,b),data=GrowthData3,start=sv)
tmp1 <- showGrowthFun(typ,param=p,parse=TRUE)
tmp2 <- showGrowthFun(typ,param=p,fit=rv,constvals=cv,parse=TRUE)
plot(0,type="n",ylim=c(0,1),xlim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
text(0.5,0.8,tmp1,cex=1.25)
text(0.5,0.3,tmp2,cex=1.1)


typ <- "Gompertz"
tstrShowGrowthFun(tlG~age,GrowthData1,typ,1,c("Linf","gi","a1"),cex=1.5)
tstrShowGrowthFun(tlG~age,GrowthData1,typ,2,c("Linf","gi","ti"),cex=1.5)
tstrShowGrowthFun(tlG~age,GrowthData1,typ,3,c("L0","gi","a2"),cex=1.5)
tstrShowGrowthFun(tlG~age,GrowthData1,typ,4,c("Linf","gi","a2"),cex=1.5)
tstrShowGrowthFun(tlG~age,GrowthData1,typ,5,c("Linf","gi","t0"),cex=1.5)
#tstrShowGrowthFun(deltaL~deltat+tlM,GrowthData3,typ,6,c("Linf","gi"),cex=1.5)

typ <- "logistic"
tstrShowGrowthFun(tlL~age,GrowthData1,typ,1,c("Linf","gninf","ti"),cex=1.5)
tstrShowGrowthFun(tlL~age,GrowthData1,typ,2,c("Linf","gninf","a"),cex=1.5)
tstrShowGrowthFun(tlL~age,GrowthData1,typ,3,c("Linf","gninf","L0"),cex=1.5)

typ <- "Richards"
tstrShowGrowthFun(tlR~age,GrowthData1,typ,1,c("Linf","k","ti","b"),cex=1.5)
tstrShowGrowthFun(tlR~age,GrowthData1,typ,2,c("Linf","k","t0","b"),cex=1.5)
tstrShowGrowthFun(tlR~age,GrowthData1,typ,3,c("Linf","k","L0","b"),cex=1.5)

typ <- "Schnute"
cv <- c(t1=0,t3=12)
tstrShowGrowthFun(tlR~age,GrowthData1,typ,1,c("L1","L3","a","b","t1"=0,"t3"=12),
                  cv=cv,cex=1.25)
tstrShowGrowthFun(tlR~age,GrowthData1,typ,2,c("L1","L3","a","b"=0,"t1"=0,"t3"=12),
                  cv=cv,cex=1.25)
tstrShowGrowthFun(tlR~age,GrowthData1,typ,3,c("L1","L3","a"=0,"b","t1"=0,"t3"=12),
                  cv=cv,cex=1.25)
tstrShowGrowthFun(tlR~age,GrowthData1,typ,4,c("L1","L3","a"=0,"b"=0,"t1"=0,"t3"=12),
                  cv=cv,cex=1.25)

tstrShowGrowthFun(tlV~age,GrowthData1,typ,1,c("L1","L3","a","b","t1"=0,"t3"=12),
                  cv=cv,cex=1.25)
tstrShowGrowthFun(tlV~age,GrowthData1,typ,2,c("L1","L3","a","b"=0,"t1"=0,"t3"=12),
                  cv=cv,cex=1.25)
tstrShowGrowthFun(tlV~age,GrowthData1,typ,3,c("L1","L3","a"=0,"b","t1"=0,"t3"=12),
                  cv=cv,cex=1.25)
tstrShowGrowthFun(tlV~age,GrowthData1,typ,4,c("L1","L3","a"=0,"b"=0,"t1"=0,"t3"=12),
                  cv=cv,cex=1.25)
