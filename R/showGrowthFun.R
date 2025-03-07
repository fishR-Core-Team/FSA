#' @title Creates an expression for a specific parameterization of the von Bertalanffy, Gompertz, Richards, and logistic growth functions, as well as the Schnute and Schnute-Richards growth functions.
#' 
#' @description Creates an expression for a specific parameterization of the von Bertalanffy, Gompertz, Richards, and logistic growth functions, as well as the Schnute and Schnute-Richards growth functions. The expression can be plotted with \code{plot=TRUE} to see the equation of the growth function. The expressions can also be added to user plots as titles, annotations, etc.
#'
#' @inheritParams makeGrowthFun
#' @param case A numeric that indicates the specific case of the Schnute function to use.
#' @param plot A logical for whether the expression should be shown on a \dQuote{blank} plot. See examples.
#' @param \dots Arguments for \code{plot}. In particular use \code{cex=} to make the expression larger and easier to read. See examples. 
#'
#' @returns An expression representing the equation of the growth function given in \code{type} and \code{param}.
#'
#' @seealso See \code{\link{makeGrowthFun}} to make functions that correspond to these expressions.
#' 
#' @examples
#' #===== The expression (default is type="von Bertalanffy")
#' showGrowthFun()
#' showGrowthFun(pname="Typical")
#' 
#' #===== Shown on the plot, and then larger
#' showGrowthFun(plot=TRUE)
#' showGrowthFun(plot=TRUE,cex=2)
#' 
#' #===== Examples of other growth odels
#' showGrowthFun(type="Richards",param=3,plot=TRUE,cex=1.5)
#' showGrowthFun(type="Schnute",case=2,plot=TRUE,cex=1.5)
#' 
#' #===== Multiple expressions in one plot
#' op <- par(mar=c(0.1,0.1,0.1,0.1))
#' plot(0,type="n",xlab="",ylab="",xlim=c(0,1),ylim=c(0,3),xaxt="n",yaxt="n")
#' text(0,2.5,"Original:",pos=4)
#' text(0.5,2.5,showGrowthFun(type="von Bertalanffy",pname="Original"))
#' text(0,1.5,"Typical:",pos=4)
#' text(0.5,1.5,showGrowthFun(type="von Bertalanffy",pname="Typical"))
#' text(0,0.5,"Francis:",pos=4)
#' text(0.5,0.5,showGrowthFun(type="von Bertalanffy",pname="Francis"))
#' par(op)
#' 
#' #===== Put expression in title or otherwise on the plot
#' # Make a von Bertalanffy function
#' vb1 <- makeGrowthFun()
#'  # Get and save the expression of the von Bertalanffy growth function
#' tmp <- showGrowthFun()
#' 
#' # Make plot and put expression in plot title
#' ages <- 1:20
#' plot(vb1(ages,Linf=20,K=0.3,t0=-0.2)~ages,type="b",pch=19,ylab="Length",main=tmp)
#' 
#' # Put expression in plot body (as demo)
#' text(15,10,tmp)
#' 
#' @rdname showGrowthFun
#' @export

showGrowthFun <- function(type=c("von Bertalanffy","Gompertz","Richards",
                                 "logistic","Schnute","Schnute-Richards"),
                          param=1,pname=NULL,case=NULL,plot=FALSE,...) {
  #===== Checks
  # Schnute uses "case" instead of "param" ... convert to "param"
  if (!is.null(case)) {
    if(type=="Schnute") param <- case
    else STOP("'case' only used when 'type' is 'Schnute'")
  }
  
  # Handle checks on type, param, and pname
  type <- match.arg(type)
  param <- iHndlGrowthModelParams(type,param,pname,SGF=TRUE)
  
  #===== Find expression to return
  tpexpr <- iGrowthModelExpression(type,param)
  if (is.null(tpexpr)) STOP("Not yet implemented for ",type," parameterization #",param)
  
  #===== Write expression on a plot if asked for 
   if (plot) {
    withr::local_par(list(mar=c(0.1,0.1,0.1,0.1)))
    graphics::plot(0,type="n",ylim=c(0,1),xlim=c(0,1),xaxt="n",yaxt="n",
                   xlab="",ylab="",bty="n",...)
    graphics::text(0.5,0.5,tpexpr,...)
   }
  
  #===== Return expression
  tpexpr
}

iGrowthModelExpression <- function(type,param) {
  # make a combined parameter name ... remove spaces and hyphens from type
  pnm <- paste0(gsub(" ","",type),param)
  pnm <- gsub("-","",pnm)

  #Find expression to return 
  switch(pnm,
    "vonBertalanffy1" = {
      expression(E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])},")")) },
    "vonBertalanffy2" = {
      expression(E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-Kt}) },
    "vonBertalanffy3" = {
      expression(E(L[t])==frac(omega,K)*~bgroup("(",1-e^{-K*(t~-~t[0])},")")) },
    "vonBertalanffy4" = {
      expression(E(L[t])==L[infinity]~-~(L[infinity]-L[0])*~e^{-frac(omega,L[infinity])*~t}) },
    "vonBertalanffy5" = {
      expression(E(L[t])==L[infinity]*bgroup("(",1-e^{-frac(log(2),(t[50]~-~t[0]))*(t~-~t[0])},")")) },
    "vonBertalanffy6" = {
      expression(E(L[t])==L[infinity]~-~(L[infinity]-L[r])*~e^{-K(t~-~t[r])}) },
    "vonBertalanffy7" = {
      expression(E(L[t])==L[1]+(L[3]-L[1])*~frac(1-e^{-K*(~t~-~t[1])},1-e^{-K*(~t[3]~-~t[1])})) },
    "vonBertalanffy8" = {
      expression(atop(E(L[t])==L[1]+(L[3]-L[1])*~frac(1-r^{2*frac(t-t[1],t[3]-t[1])},1-r^{2}),
                      plain("where" )~r==frac(L[3]-L[2],L[2]-L[1]))) },
    "vonBertalanffy9" = { NULL },
    "vonBertalanffy10"= {
      expression(atop(E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])-S(t)+S(t[0])},")"),
                      plain("where" )~S(t)==frac(C*K,2*pi)*~sin(2*pi*(t-t[s])))) },
    "vonBertalanffy11"= {
      expression(atop(E(L[t])==L[infinity]*bgroup("(",1-e^{-K*(t~-~t[0])-R(t)+R(t[0])},")"),
                      plain("where" )~R(t)==frac(C*K,2*pi)*~sin(2*pi*(t-WP+0.5)))) },
    "vonBertalanffy12"= {
      expression(atop(E(L[t])==L[infinity]*bgroup("(",1-e^{-Kpr*(tpr~-~t[0])-V(tpr)+V(t[0])},")"),
                      plain("where" )~V(t)==frac(Kpr(1-NGT),2*pi)*~sin~bgroup("(",frac(2*pi,1-NGT*(t-t[s])),")"))) },
    "vonBertalanffy13"= {
      expression(E(L[r]-L[m])==(L[infinity]-L[m])*bgroup("(",1-e^{-K*Delta*t},")")) },
    "vonBertalanffy14"= {
      expression(E(L[r])==L[m]+(L[infinity]-L[m])*bgroup("(",1-e^{-K*Delta*t},")")) },
    "vonBertalanffy15"= {
      expression(E(L[r]-L[m])==(L[infinity]+beta*(bar(L)[m]-L[m])-L[m])*bgroup("(",1-e^{-K*Delta*t},")")) },
    "vonBertalanffy16"= {
      expression(E(L[r]-L[m])==(alpha+beta*L[m])*bgroup("(",1-e^{-K*Delta*t},")")) },
    "vonBertalanffy17"= {
      expression(E(L[r])==L[m]+(alpha+beta*L[m])*bgroup("(",1-e^{-K*Delta*t},")")) },
    "vonBertalanffy18"= { NULL },
    "vonBertalanffy19"= { NULL },
    "Gompertz1" = {
      expression(E(L[t])==L[infinity]*~e^{-e^{a[1]-g[i]*t}}) },
    "Gompertz2" = {
      expression(E(L[t])==L[infinity]*~e^{-e^{-g[i]*(t-t[i])}}) },
    "Gompertz3" = {
      expression(E(L[t])==L[0]*~e^{a[2]*bgroup("(",1-e^{-g[i]*t},")")}) },
    "Gompertz4" = {
      expression(E(L[t])==L[infinity]*~e^{-a[2]*~e^{-g[i]*t}}) },
    "Gompertz5" = {
      expression(E(L[t])==L[infinity]*~e^{-~frac(1,g[i])*~e^{-g[i]*~(~t~-~t[0])}}) },
    "Gompertz6" = {
      expression(E(L[r]-L[m])==L[infinity]*~bgroup("(",frac(L[m],L[infinity]),")")^{e^{-g[i]*Delta*t}}-L[m]) },
    "Gompertz7" = {
      expression(E(L[r])==L[infinity]*~bgroup("(",frac(L[m],L[infinity]),")")^{e^{-g[i]*Delta*t}}) },
    "logistic1" = {
      expression(E(L[t])==frac(L[infinity],1+e^{-g[-infinity]*(t-t[i])})) },
    "logistic2" = {
      expression(E(L[t])==frac(L[infinity],1+~ae^{-g[-infinity]*t})) },
    "logistic3" = {
      expression(E(L[t])==frac(L[0]*L[infinity],L[0]+(L[infinity]-L[0])*e^{-g[-infinity]*t})) },
    "logistic4" = {
      expression(E(L[r]-L[m])==frac(Delta*L[max],1+e^{log(19)*frac(L[m]~-~L[50],L[95]~-~L[50])})) },
    "Richards1" = {
      expression(E(L[t])==frac(L[infinity],bgroup("(",1+b[1]*e^{-k*(t-t[i])},")")^{~frac(1,b[1])})) },
    "Richards2" = {
      expression(E(L[t])==frac(L[infinity],bgroup("(",1+e^{-k*(t-t[0])},")")^{~-b[2]})) },
    "Richards3" = {
      expression(E(L[t])==L[infinity]*~bgroup("[",1+bgroup("(",bgroup("(",frac(L[0],L[infinity]),")")^{1-b[3]}-1,")")*~e^{-k*t},"]")^{~frac(1,1-b[3])}) },
    "Richards4" = {
      expression(E(L[t])==L[infinity]*~bgroup("(",1-frac(1,b[2])*~e^{-k*(t-t[i])},")")^{~b[2]}) },
    "Richards5" = {expression(E(L[t])==L[infinity]*~bgroup("(",1+(b[3]-1)*~e^{-k*(t-t[i])},")")^{~frac(1,1-b[3])}) },
    "Schnute1" = {
      expression(E(L[t])==bgroup("[",L[1]^{b}+(L[3]^{b}-L[1]^{b})*~frac(1-e^{-a*(~t~-~t[1])},1-e^{-a*(~t[3]~-~t[1])}),"]")^{~frac(1,b)}) },
    "Schnute2" = {
      expression(E(L[t])==L[1]*e^{log~bgroup("(",frac(L[3],L[1]),")")*~frac(1-e^{-a*(~t~-~t[1])},1-e^{-a*(~t[3]~-~t[1])})}) },
    "Schnute3" = {
      expression(E(L[t])==bgroup("[",L[1]^{b}+(L[3]^{b}-L[1]^{b})*~frac(~t~-~t[1],~t[3]~-~t[1]),"]")^{~frac(1,b)}) },
    "Schnute4" = {expression(E(L[t])==L[1]*e^{log~bgroup("(",frac(L[3],L[1]),")")*~frac(~t~-~t[1],~t[3]~-~t[1])}) },
    "SchnuteRichards1" = {
      expression(E(L[t])==L[infinity]*~bgroup("(",1-a*e^{-kt^{c}},")")^{frac(1,b)}) }
  )
}