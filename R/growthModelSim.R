#' @title Dynamics plots to explore typical fisheries growth models.
#'
#' @description Plots hypothetical size-at-age for one of seven possible parameterizations of the von Bertalanffy, three possible parameterizations of the Gompertz, and the Schnute growth models.  Slider bars are used to dynamically alter the parameters of each model.
#'
#' @details This function can be used to explore the \dQuote{shape} of the growth models for various choices of the parameters.  In this usage the \code{x} and \code{y} arguments should be (left) set at \code{NULL}.  This function can also be used to visually \dQuote{fit} a growth model to a set of observed lengths and ages.  This usage may be used to provide reasonable starting values for the parameters when fitting the growth model to the data with non-linear least-squares.  The observed data are plotted by including a formula of the form \code{length~age} in \code{x} and a data frame from which to draw the variables in the formula in the \code{data} arguments.
#'
#' The \code{type} argument is used to choose a type of growth model and must be one of the following (the models can be seen with \code{\link{growthModels}} and \code{\link{vbModels}}):
#'
#' \tabular{ll}{
#' \code{"vbTypical"} \tab The "typical" Beverton-Holt parameterized von Bertalanffy model.\cr
#' \code{"vbOriginal"} \tab The original parameterization from von Bertalanffy.\cr
#' \code{"vbMooij"} \tab The Mooij et al (1999) paramaterization of the von Bertalanffy model.\cr
#' \code{"vbGQ"} \tab The Gallucci & Quinn (1979) parameterization of the von Bertalanffy model.\cr
#' \code{"vbWeisberg"} \tab The Weisberg et al. (2010) parameterization of the von Bertalanffy model.\cr
#' \code{"vbSchnute"} \tab The Schnute-like paramaterization of the von Bertalanffy model.\cr 
#' \code{"vbTypicalW"} \tab The "typical" Beverton-Holt parameterized von Bertalanffy model, but for weights rather than lengths (thus, includes one more parameter).\cr
#' \code{"vbOriginalW"} \tab The original parameterization from von Bertalanffy, but for weights rather than lengths (thus, includes one more parameter).\cr
#' \code{"Gompertz1"} \tab The "first" parameterization of the Gompertz model.\cr 
#' \code{"Gompertz2"} \tab The "second" parameterization of the Gompertz model.\cr 
#' \code{"Gompertz3"} \tab The "third" parameterization of the Gompertz model.\cr
#' \code{"schnute"} \tab The Schnute(1981) four-parameter general growth model.
#' }
#'
#' @param formula An optional formula of the form \code{len~age}.  See details.
#' @param data A data frame from which the variables in the formula should be drawn.
#' @param type A single character string that indicates which growth model to use.  See details.
#' @param max.len A single numeric that indicates the maximum length to use in the simulations.
#' @param max.wt A single numeric that indicates the maximum weight to use in the simulations (only used of \code{type=} \code{"vbTypicalW"} or \code{"vbOriginalW"}.
#'
#' @return None.  However a dynamic graphic connected to slider bar controls in which the user can change the maximum age over which the growth model is evaluated and change the parameters specific to the chosen growth model.
#'
#' @author Derek H. Ogle, \email{dogle@@northland.edu}
#'
#' @seealso \code{\link{growthModels}}, \code{\link{vbModels}}, and \code{\link{vbStarts}}.
#'
#' @references Francis, R.I.C.C.  1988.  Are growth parameters estimated from tagging and age-length data comparable?  Canadian Journal of Fisheries and Aquatic Sciences, 45:936-942.
#'
#' Gallucci, V.F. and T.J. Quinn II. 1979.  Reparameterizing, fitting, and testing a simple growth model.  Transactions of the American Fisheries Society, 108:14-25.
#'
#' Mooij, W.M., J.M. Van Rooij, and S. Wijnhoven.  1999.  Analysis and comparison of fish growth from small samples of length-at-age data: Detection of sequal dimorphism in Eurasian perch as an example.  Transactions of the American Fisheries Society 128:483-490.
#'
#' Schnute, J.  1981.  A versatile growth model with statistically stable parameters. Canadian Journal of Fisheries & Aquatic Sciences, 38:1128-1140.
#'
#' Schnute, J. and D. Fournier. 1980.  A new approach to length-frequency analysis: Growth structure.  Canadian Journal of Fisheries and Aquatic Sciences, 37:1337-1351.
#'
#' Weisberg, S., G.R. Spangler, and L. S. Richmond. 2010. Mixed effects models for fish growth. Canadian Journal of Fisheries And Aquatic Sciences 67:269-277.
#'
#' @keywords iplot
#'
#' @examples
#' ## ONLY RUN IN INTERACTIVE MODE
#' if (interactive()) {
#'
#' # Explore growth models (no data) -- use the defaults
#' growthModelSim()
#'
#' # Schnute parameterization of the von Bertalanffy model
#' growthModelSim(type="vbSchnute")
#'
#' ## Explore growth models superimposed on length-at-age data
#' # get Smallmouth Bass data from FSA package
#' data(SMBassWB)
#'
#' # interactively "fit" the typical paramaterization of the von Bertalanffy model to the data
#' growthModelSim(lencap~agecap,data=SMBassWB)
#'
#' # interactively "fit" the second paramaterization of the Gompertz model to the data
#' growthModelSim(lencap~agecap,data=SMBassWB,type="Gompertz2")
#'
#' } ## END IF INTERACTIVE MODE
#'
#' @export growthModelSim
#' 
## Main function
growthModelSim <- function(formula=NULL,data=NULL,
                           type=c("vbTypical","vbOriginal","vbGQ","vbGallucciQuinn","vbMooij",
                                  "vbWeisberg","vbSchnute","vbTypicalW","vbOriginalW",
                                  "Gompertz1","Gompertz2","Gompertz3",
                                  "Schnute"),
                           max.len=500,max.wt=500) {
  ## Internal refresh function
  refresh <- function(...) {
    p1 <- relax::slider(no=1)
    p2 <- relax::slider(no=2)
    p3 <- relax::slider(no=3)
    if (type %in% c("vbSchnute","vbTypicalW","vbOriginalW","Schnute")) {
      p4 <- relax::slider(no=4)
      t.max <- relax::slider(no=5)
    } else {
      p4 <- 1
      t.max <- relax::slider(no=4)
    }
  iGrowthSimPlot(type,x,y,max.y,t.max,p1,p2,p3,p4)
  } ## end Internal refresh

  ## begin main growthModelSim
  # some checks and handle the formula
  if (!is.null(formula)) {
    # this allows the user to enter a type as the first argument as long as data is not given
    if (is.character(formula) & is.null(data)) type <- formula
    else {
      tmp <- iHndlFormula(formula,data,expNumR=1,expNumE=1)
      if (!tmp$metExpNumR) stop("'vbStarts' must have only one LHS variable.",call.=FALSE)
      if (!tmp$Rclass %in% c("numeric","integer")) stop("LHS variable must be numeric.",call.=FALSE)
      if (!tmp$metExpNumE) stop("'vbStarts' must have only one RHS variable.",call.=FALSE)
      if (!tmp$Eclass %in% c("numeric","integer")) stop("RHS variable must be numeric.",call.=FALSE)
      # get the length and age vectors
      y <- tmp$mf[,tmp$Rname[1]]
      x <- tmp$mf[,tmp$Enames[1]]
      # set maximum size to max observed data
      max.len <- max.wt <- max(y,na.rm=TRUE)
    }
  } else {
    # if no formula given then set x, y vectors to NULL
    x <- y <- NULL
  }
  type <- match.arg(type)
  t.max <- ifelse(!is.null(x),max(x,na.rm=TRUE),20)
  max.y <- ifelse (type %in% c("vbTypicalW","vbOriginalW"),max.wt,max.len)
  if (!is.null(y)) max.y <- 1.1*max(y,na.rm=TRUE)
  min.y <- floor(max.y/10)
  delta.y <- (max.y-min.y)/50
  switch(type,
    vbOriginal= {
      relax::gslider(refresh,prompt=TRUE,
             sl.names=   c("L_inf",    "L_0",  "K", "Max Age"),
             sl.mins=    c(  min.y,        0,    0,         5),
             sl.maxs=    c(  max.y,    min.y,  0.5,       100),
             sl.deltas=  c(delta.y, min.y/50,  0.01,        1),
             sl.defaults=c(  max.y,    min.y,  0.3,     t.max),
             title = "Original Von Bertalanffy",pos.of.panel="left")    
      }, # end vbOriginal
    vbTypical= {
      relax::gslider(refresh,prompt=TRUE,
             sl.names=   c("L_inf",  "K", "t_0", "Max Age"),
             sl.mins=    c(  min.y,    0,   -10,         5),
             sl.maxs=    c(  max.y,  0.5,    10,       100),
             sl.deltas=  c(delta.y, 0.01,   0.1,         1),
             sl.defaults=c(  max.y,  0.3,     0,     t.max),
             title = "Typical Von Bertalanffy",pos.of.panel="left")    
      }, # end vbTypical
    vbOriginalW= {
      relax::gslider(refresh,prompt=TRUE,
             sl.names=   c("W_inf",    "W_0",  "K",  "b", "Max Age"),
             sl.mins=    c(  min.y,        0,    0, 0.25,         5),
             sl.maxs=    c(  max.y,    min.y,  0.5,    4,       100),
             sl.deltas=  c(delta.y, min.y/50, 0.01, 0.05,         1),
             sl.defaults=c(  max.y,    min.y,  0.3,    1,     t.max),
             title = "Original Von Bertalanffy Weight",pos.of.panel="left")
      }, # end vbOriginal
    vbTypicalW= {
      relax::gslider(refresh,prompt=TRUE,
             sl.names=   c("W_inf",  "K", "t_0",  "b", "Max Age"),
             sl.mins=    c(  min.y,    0,   -10, 0.25,         5),
             sl.maxs=    c(  max.y,  0.5,    10,    4,       100),
             sl.deltas=  c(delta.y, 0.01,   0.1, 0.05,         1),
             sl.defaults=c(  max.y,  0.3,     0,    1,     t.max),
             title = "Typical Von Bertalanffy Weight",pos.of.panel="left")
      }, # end vbTypical
    vbGQ=, vbGallucciQuinn= {
      relax::gslider(refresh,prompt=TRUE,
             sl.names=   c("omega",  "K", "t_0", "Max Age"),
             sl.mins=    c(      0,    0,   -10,         5),
             sl.maxs=    c(    250,  0.5,    10,       100),
             sl.deltas=  c(      5, 0.01,   0.1,         1),
             sl.defaults=c(     75,  0.3,     0,     t.max),
             title = "Gallucci & Quinn Von Bertalanffy",pos.of.panel="left")
      }, # end vbGallucciQuinn
    vbMooij= {
      relax::gslider(refresh,prompt=TRUE,
             sl.names=   c("L_inf",    "L_0", "omega", "Max Age"),
             sl.mins=    c(  min.y,        0,       0,         5),
             sl.maxs=    c(  max.y,    min.y,     250,       100),
             sl.deltas=  c(delta.y, min.y/50,       5,         1),
             sl.defaults=c(  max.y,    min.y,      75,     t.max),
             title = "Mooij et al. Von Bertalanffy",pos.of.panel="left")
      }, # end vbMooij
    vbWeisberg= {
      relax::gslider(refresh,prompt=TRUE,
                     sl.names=   c("L_inf",           "t50", "t_0", "Max Age"),
                     sl.mins=    c(  min.y,               1,   -10,         5),
                     sl.maxs=    c(  max.y,         t.max-1,    10,       100),
                     sl.deltas=  c(delta.y,             0.1,   0.1,         1),
                     sl.defaults=c(  max.y,round(t.max/2,0),     0,     t.max),
                     title = "Weisberg et al. Von Bertalanffy",pos.of.panel="left")    
    }, # end vbWeisberg
    vbSchnute= {
      relax::gslider(refresh,prompt=TRUE,
             sl.names=   c(            "L_1",            "L_2",  "K",  "b", "Max Age"),
             sl.mins=    c(            min.y, floor(0.5*max.y),    0, 0.25,         5),
             sl.maxs=    c(floor(0.75*max.y),            max.y,  0.5,    4,       100),
             sl.deltas=  c(                5,               10, 0.01, 0.05,         1),
             sl.defaults=c(               50,              200,  0.3,    1,     t.max),
             title = "Schnute-like Von Bertalanffy",pos.of.panel="left")
      }, # end vbSchnute
    Gompertz1= {
      relax::gslider(refresh,prompt=TRUE,
             sl.names=   c(  "L_0",  "G",  "g", "Max Age"),
             sl.mins=    c(  min.y,    0,    0,         5),
             sl.maxs=    c(  max.y,    2,    2,       100),
             sl.deltas=  c(delta.y, 0.01, 0.01,         1),
             sl.defaults=c(  min.y,  1.5,  0.5,     t.max),
             title = "Gompertz Growth #1",pos.of.panel="left")    
    }, # end g1
    Gompertz2= {
      relax::gslider(refresh,prompt=TRUE,
             sl.names=   c("L_inf",  "g", "t*", "Max Age"),
             sl.mins=    c(  min.y,    0,    0,         5),
             sl.maxs=    c(  max.y,    2,   10,       100),
             sl.deltas=  c(delta.y, 0.01, 0.01,         1),
             sl.defaults=c(  max.y,  0.5,    2,     t.max),
             title = "Gompertz Growth #2",pos.of.panel="left")    
    }, # end g2
    Gompertz3= {
      relax::gslider(refresh,prompt=TRUE,
             sl.names=   c("L_inf",  "g", "t_0", "Max Age"),
             sl.mins=    c(  min.y,    0,   -10,         5),
             sl.maxs=    c(  max.y,    2,    10,       100),
             sl.deltas=  c(delta.y, 0.01,  0.01,         1),
             sl.defaults=c(  max.y,  0.5,     0,     t.max),
             title = "Gompertz Growth #3",pos.of.panel="left")    
    }, # end g3
    Schnute= {
      relax::gslider(refresh,prompt=TRUE,
             sl.names=   c(  "L_1",   "L_2",   "c", "d", "Max Age"),
             sl.mins=    c(  min.y,     100,   -3,   -3,         5),
             sl.maxs=    c(    200,   max.y,    3,    3,       100),
             sl.deltas=  c(delta.y, delta.y, 0.01, 0.01,         1),
             sl.defaults=c(     50,   max.y,    1,  0.5,     t.max),
             title = "Schnute 4-Parameter Model",pos.of.panel="left") 
    } # end Schnute
  ) # end switch
}




## Internal function to predict length/weight for plotting -- used in iGrowthSimPlot
iPredLength <- function(type,t,p1,p2,p3,p4) {
  switch(type,
         # p1=Linf, p2=K,  p3=to, p4 not used
         vbTypical= {  sd <- p1*(1-exp(-p2*(t-p3))) },
         # p1=Winf, p2=K,  p3=to, p4=b
         vbTypicalW= { sd <- p1*(1-exp(-p2*(t-p3)))^p4 },         
         # p1=Linf, p2=L0, p3=K,  p4 not used
         vbOriginal= { sd <- (p1-(p1-p2)*exp(-p3*t)) },
         # p1=Winf, p2=W0, p3=K,  p4=b
         vbOriginalW= { sd <- (p1-(p1-p2)*exp(-p3*t))^p4 },
         # p1=omega,p2=K,  p3=t0, p4 not used
         vbGQ=, vbGallucciQuinn= { sd <- (p1/p2)*(1-exp(-p2*(t-p3))) },
         # p1=Linf, p2=t50,  p3=to, p4 not used
         vbWeisberg= {  sd <- p1*(1-exp(-(log(2)/(p2-p3))*(t-p3))) },
         # p1=Linf, p2=L0, p3=ome,p4 not used
         vbMooij= { sd <- p1-(p1-p2)*exp(-(p3/p1)*t) },
         # p1=L1,   p2=L2, p3=K,  p4=b
         vbSchnute= {
           sd <- ((p1^p4)+((p2^p4)-(p1^p4))*((1-exp(-p3*(t-min(t))))/(1-exp(-p3*(max(t)-min(t))))))^(1/p4)
         },
         # p1=Lo,   p2=G,  p3=g,  p4 not used
         Gompertz1= { sd <- p1*(exp(p2*(1-exp(-p3*t)))) },
         # p1=Linf, p2=g,  p3=t*, p4 not used
         Gompertz2= { sd <- p1*(exp(-exp(-p2*(t-p3)))) },
         # p1=Linf, p2=g,  p3=t0, p4 not used
         Gompertz3= { sd <- p1*(exp((-1/p2)*exp(-p2*(t-p3)))) },
         # p1=L1,   p2=L2, p3=c,  p4=d
         Schnute= {
           minage <- min(t)
           maxage <- max(t)
           diffage <- maxage-minage
           if (p3==0) {
             if (p4==0) {
               # Case 4
               sd <- p1*exp(log(p2/p1)*(t-minage)/diffage)
             } else { 
               # Case 3
               sd <- (p1^p4+(p2^p4-p1^p4)*(t-minage)/diffage)^(1/p4)
             }
           } else {
             if (p4==0) {
               # Case 2
               sd <- p1*exp(log(p2/p1)*(1-exp(-p3*(t-minage)))/(1-exp(-p3*diffage)))
             } else { 
               # Case 1
               sd <- (p1^p4+(p2^p4-p1^p4)*((1-exp(-p3*(t-minage)))/
                                             (1-exp(-p3*diffage))))^(1/p4) 
             }
           }
         } # end Schnute
  ) # end switch 
  sd
} ## end iPredLength internal function

## internal function for constructing the plot
iGrowthSimPlot <- function(type,x,y,max.y,t.max,p1,p2,p3,p4) {
  t <- seq(0,t.max,length.out=t.max*20)
  vals <- iPredLength(type,t,p1,p2,p3,p4)
  ylbl <- ifelse (type %in% c("vbTypicalW","vbOriginalW"),"Weight","Length")
  opar <- par(mar=c(3.5,3.5,1,1), mgp=c(2,0.4,0), tcl=-0.2)
  if (is.null(x)||is.null(y)) plot(t,vals,xlab="Age",ylab=paste("Mean",ylbl),type="l",lwd=2,col="blue",ylim=c(0,max.y))
  else {
    plot(x,y,xlab="Age",ylab=ylbl,ylim=c(0,max.y),xlim=c(0,t.max))
    lines(t,vals,type="l",lwd=2,col="blue")
  }
  par(opar)
} ## end iGrowthSimPlot internal function
