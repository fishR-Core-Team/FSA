#' @Title
#' 
#' @description
#' A short description...
#' 
#'
#' @param type 
#' @param param 
#' @param case 
#' @param plot 
#' @param ... 
#'
#' @returns
#'
#' @examples
#' # none yet
#' @export

showGrowthFun <- function(type=c("von Bertalanffy","Gompertz","Richards",
                                 "Logistic","Schnute","Schnute Richards"),
                          param=NULL,case=param,plot=FALSE,...) {
  
  type <- match.arg(type)
  switch(type,
         vonBertalanffy = { expr <- iSGF_VB(param) },
         Gompertz = { expr <- iSGF_GOMP(param) },
         Richards = { expr <- iSGF_RICHARDS(param) },
         Logistic = { expr <- iSGF_LOGISTIC(param) },
         Schnute = { expr <- iSGF_SCHNUTE(case) },
         SchnuteRichards = { expr <- iSGF_SCHNUTERICHARDS()})
  if (plot) {
    withr::local_par(list(mar=c(0.1,0.1,0.1,0.1)))
    graphics::plot(0,type="n",ylim=c(0,1),xlim=c(0,1),xaxt="n",yaxt="n",
                   xlab="",ylab="",bty="n",...)
    graphics::text(0.5,0.5,expr,...)
  }
  expr
}

################################################################################
## Internal functions for growth model expressions
################################################################################
iSGF_VB <- function(param=c("Original","original","vonBertalanffy",
                            "Typical","typical","Traditional","traditional","BevertonHolt",
                            "GallucciQuinn","GQ","Mooij","Weisberg","Ogle",
                            "Schnute","Francis","Laslett","Polacheck",
                            "Somers","Somers2","Pauly",
                            "Fabens","Fabens2","Wang","Wang2","Wang3")) {
  if(!is.character(param)) STOP("'param' must be a character string.")
  param <- match.arg(param)
  switch(param,
         Ogle= {
           expr <- expression(E(L[t])==L[infinity]~-~(L[infinity]-L[r])*~e^{-K(t~-~t[r])})
         },
         Typical=,typical=,Traditional=,traditional=,BevertonHolt= {
           expr <- expression(E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])},")"))
         },
         Original=,original=,vonBertalanffy= {
           expr <- expression(E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-Kt})
         },
         GallucciQuinn=,GQ= {
           expr <- expression(E(L[t])==frac(omega,K)*~bgroup("(",1-e^{-K*(t~-~t[0])},")"))
         },
         Mooij= {
           expr <- expression(E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-frac(omega,L[infinity])*~t})
         },
         Weisberg= {
           expr <- expression(E(L[t])==L[infinity]*bgroup("(",1-e^{-frac(log(2),(t[50]~-~t[0]))*(t~-~t[0])},")"))
         },
         Schnute= {
           expr <- expression(E(L[t])==L[1]+(L[3]-L[1])*~frac(1-e^{-K*(~t~-~t[1])},1-e^{-K*(~t[3]~-~t[1])}))
         },
         Francis= {
           expr <- expression(atop(E(L[t])==L[1]+(L[3]-L[1])*~frac(1-r^{2*frac(t-t[1],t[3]-t[1])},1-r^{2}),
                                   plain("where" )~r==frac(L[3]-L[2],L[2]-L[1])))
         },
         Laslett= {
           expr <- expression(plain("Not Yet Implemented"))
         },
         Polacheck= {
           expr <- expression(plain("Not Yet Implemented"))
         },
         Somers= {
           expr <- expression(atop(E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])-S(t)+S(t[0])},")"),
                                   plain("where" )~S(t)==bgroup("(",frac(C*K,2*~pi),")")*~sin(2*pi*(t-t[s]))))
         },
         Somers2= {
           expr <- expression(atop(E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])-R(t)+R(t[0])},")"),
                                   plain("where" )~R(t)==bgroup("(",frac(C*K,2*~pi),")")*~sin(2*pi*(t-WP+0.5))))
         },
         Pauly= {
           expr <- expression(atop(E(L[t])==L[infinity]*bgroup("(",1-e^{-Kpr*(tpr~-~t[0])-V(tpr)+V(t[0])},")"),
                                   plain("where" )~V(t)==bgroup("(",frac(Kpr(1-NGT),2*~pi),")")*~sin(frac(2*pi,1-NGT)*(t-t[s]))))
         },
         Fabens= {
           expr <- expression(E(L[r]-L[m])==(L[infinity]-L[m])*bgroup("(",1-e^{-K*Delta*t},")"))
         },
         Fabens2= {
           expr <- expression(E(L[r])==L[m]+(L[infinity]-L[m])*bgroup("(",1-e^{-K*Delta*t},")"))
         },
         Wang= {
           expr <- expression(E(L[r]-L[m])==(L[infinity]+beta*(L[t]-L[t])-L[m])*bgroup("(",1-e^{-K*Delta*t},")"))
         },
         Wang2= {
           expr <- expression(E(L[r]-L[m])==(alpha+beta*L[t])*bgroup("(",1-e^{-K*Delta*t},")"))
         },
         Wang3= {
           expr <- expression(E(L[r])==L[m]+(alpha+beta*L[t])*bgroup("(",1-e^{-K*Delta*t},")"))
         })
  expr
}

iSGF_GOMP <- function(param=c("Original","original","Ricker1","Ricker2","Ricker3",
                              "QuinnDeriso1","QuinnDeriso2","QuinnDeriso3","QD1","QD2","QD3",
                              "Troynikov1","Troynikov2")) {
  if(!is.character(param)) STOP("'param' must be a character string.")
  param <- match.arg(param)
  switch(param,
         Original=,original= {
           expr <- expression(E(L[t])==L[infinity]*~e^{-e^{a-g[i]*t}})
         },
         Ricker1= {
           expr <- expression(E(L[t])==L[infinity]*~e^{-e^{-g[i]*(t-t[i])}})
         },
         Ricker2=,QuinnDeriso1=,QD1= {
           expr <- expression(E(L[t])==L[0]*~e^{a*bgroup("(",1-e^{-g[i]*t},")")})
         },
         Ricker3=,QuinnDeriso2=,QD2= {
           expr <- expression(E(L[t])==L[infinity]*~e^{-a*~e^{-g[i]*t}})
         },
         QuinnDeriso3=,QD3= {
           expr <- expression(E(L[t])==L[infinity]*~e^{-~frac(1,g[i])*~e^{-g[i]*~(~t~-~t[0])}})
         },
         Troynikov1= {
           expr <- expression(E(L[r]-L[m])==L[infinity]*~bgroup("(",frac(L[m],L[infinity]),")")^{e^{-g[i]*Delta*t}}-L[m])
         },
         Troynikov2= {
           expr <- expression(E(L[r])==L[infinity]*~bgroup("(",frac(L[m],L[infinity]),")")^{e^{-g[i]*Delta*t}})
         })
  expr
}

iSGF_RICHARDS <- function(param=1:6) {
  if (!is.numeric(param)) STOP("'param' must be numeric when type='Richards'.")
  if (!param %in% 1:6) STOP("'param' must be from 1-6 when type='Richards'.")
  if(param==1){
    expr <- expression(E(L[t])==L[infinity]*~bgroup("(",1-a*e^{-kt},")")^{b})
  } else if (param==2) {
    expr <- expression(E(L[t])==L[infinity]*~bgroup("(",1-frac(1,b)*~e^{-k*(t-t[i])},")")^{~b})
  } else if (param==3) {
    expr <- expression(E(L[t])==frac(L[infinity],bgroup("(",1+b*e^{-k*(t-t[i])},")")^{~frac(1,b)}))
  } else if (param==4) {
    expr <- expression(E(L[t])==L[infinity]*~bgroup("(",1+(b-1)*~e^{-k*(t-t[i])},")")^{~frac(1,1-b)})
  } else if (param==5) {
    expr <- expression(E(L[t])==L[infinity]*~bgroup("[",bgroup("(",1+bgroup("(",frac(L[0],L[infinity]),")")^{1-b}-1,")")*~e^{-k*t},"]")^{~frac(1,1-b)})
  } else {
    expr <- expression(E(L[t])==L[-infinity]+(L[infinity]-L[-infinity])*~bgroup("(",1+(b-1)*~e^{-k*(t-t[i])},")")^{~frac(1,1-b)})
  }
  expr
}

iSGF_LOGISTIC <- function(param=c("CJ1","CJ2","Karkach","Haddon","CampanaJones1","CampanaJones2")) {
  if(!is.character(param)) STOP("'param' must be a character string.")
  param <- match.arg(param)
  switch(param,
         CJ1=,CampanaJones1= {
           expr <- expression(E(L[t])==frac(L[infinity],1+e^{-g[-infinity]*(t-t[i])}))
         },
         CJ2=,CampanaJones2= {
           expr <- expression(E(L[t])==frac(L[infinity],1+~ae^{-g[-infinity]*t}))
         },
         Karkach= {
           expr <- expression(E(L[t])==frac(L[0]*L[infinity],L[0]+(L[infinity]-L[0])*e^{-g[-infinity]*t}))
         },
         Haddon= {
           expr <- expression(E(L[r]-L[m])==frac(Delta*L[max],1+e^{log(19)*frac(L[m]~-~L[50],L[95]~-~L[50])}))
         })
  expr
}

iSGF_SCHNUTE <- function(case=1:4) {
  if (!is.numeric(case)) STOP("'case' must be numeric when type='Schnute'.")
  if (!case %in% 1:4) STOP("'case' must be from 1-4 when type='Schnute'.")
  if(case==1){
    expr <- expression(E(L[t])==bgroup("[",L[1]^{b}+(L[3]^{b}-L[1]^{b})*~frac(1-e^{-a*(~t~-~t[1])},1-e^{-a*(~t[3]~-~t[1])}),"]")^{~frac(1,b)})
  } else if (case==2) {
    expr <- expression(E(L[t])==L[1]*e^{log~bgroup("(",frac(L[3],L[1]),")")*~frac(1-e^{-a*(~t~-~t[1])},1-e^{-a*(~t[3]~-~t[1])})})
  } else if (case==3) {
    expr <- expression(E(L[t])==bgroup("[",L[1]^{b}+(L[3]^{b}-L[1]^{b})*~frac(~t~-~t[1],~t[3]~-~t[1]),"]")^{~frac(1,b)})
  } else  {
    expr <- expression(E(L[t])==L[1]*e^{log~bgroup("(",frac(L[3],L[1]),")")*~frac(~t~-~t[1],~t[3]~-~t[1])})
  }
  expr
}

iSGF_SCHNUTERICHARDS <- function() {
  expression(E(L[t])==L[infinity]*~bgroup("(",1-a*e^{-kt^{c}},")")^{frac(1,b)})
}
