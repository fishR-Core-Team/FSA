#'Dynamics plots to explore typical fisheries growth models.
#'
#'Plots hypothetical size-at-age for one of seven possible parameterizations of
#'the von Bertalanffy, three possible parameterizations of the Gompertz, and the
#'Schnute growth models.  Slider bars are used to dynamically alter the parameters
#'of each model.
#'
#'This function can be used to explore the \dQuote{shape} of the growth models for
#'various choices of the parameters.  In this usage the \code{x} and \code{y}
#'arguments should be (left) set at \code{NULL}.  This function can also be used
#'to visually \dQuote{fit} a growth model to a set of observed lengths and ages.
#'This usage may be used to provide reasonable starting values for the parameters
#'when fitting the growth model to the data with non-linear least-squares.  The
#'observed data are plotted by including a formula of the form \code{length~age} in
#'\code{x} and a data frame from which to draw the variables in the formula in the
#'\code{data} arguments.
#'
#'The \code{type} argument is used to choose a type of growth model and must be
#'one of the following (the models can be seen with \code{\link{growthModels}} and
#'\code{\link{vbModels}}):
#'
#'\tabular{ll}{
#'\code{"vbTypical"} \tab The "typical" Beverton-Holt parameterized von Bertalanffy model.\cr
#'\code{"vbOriginal"} \tab The original parameterization from von Bertalanffy.\cr
#'\code{"vbMooij"} \tab The Mooij et al (1999) paramaterization of the von Bertalanffy model.\cr
#'\code{"vbGalucciQuinn"} \tab The Galucci & Quinn (1979) parameterization of the von Bertalanffy model.\cr 
#'\code{"vbSchnute"} \tab The Schnute-like paramaterization of the von Bertalanffy model.\cr 
#'\code{"vbTypicalW"} \tab The "typical" Beverton-Holt parameterized von 
#'Bertalanffy model, but for weights rather than lengths (thus, includes one more parameter).\cr
#'\code{"vbOriginalW"} \tab The original parameterization from von Bertalanffy,
#'but for weights rather than lengths (thus, includes one more parameter).\cr
#'\code{"Gompertz1"} \tab The "first" parameterization of the Gompertz model.\cr 
#'\code{"Gompertz2"} \tab The "second" parameterization of the Gompertz model.\cr 
#'\code{"Gompertz3"} \tab The "third" parameterization of the Gompertz model.\cr
#'\code{"schnute"} \tab The Schnute(1981) four-parameter general growth model.
#'}
#'
#'@param type A single character string that indicates which growth model to use.  See details.
#'@param x An optional vector that contains observed ages or a formula.  See details.
#'@param y An optional vector that contains observed lengths.  See details.
#'@param data A data frame from which the variables in the formula should be drawn.
#'@param max.len A single numeric that indicates the maximum length to use in the simulations.
#'@param max.wt A single numeric that indicates the maximum weight to use in the simulations
#'(only used of \code{type=} \code{"vbTypicalW"} or \code{"vbOriginalW"}.
#'@return None.  However a dynamic graphic connected to slider bar controls in which
#'the user can change the maximum age over which the growth model is evaluated
#'and change the parameters specific to the chosen growth model.
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@seealso \code{\link{growthModels}}, \code{\link{vbModels}}, and \code{\link{vbStarts}}.
#'@references Francis, R.I.C.C.  1988.  Are growth parameters estimated from
#'tagging and age-length data comparable?  Canadian Journal of Fisheries and
#'Aquatic Sciences, 45:936-942.
#'
#'Galucci, V.F. and T.J. Quinn II. 1979.  Reparameterizing, fitting, and testing a 
#'simple growth model.  Transactions of the American Fisheries Society, 108:14-25.
#'
#'Mooij, W.M., J.M. Van Rooij, and S. Wijnhoven.  1999.  Analysis and comparison of 
#'fish growth from small samples of length-at-age data: Detection of sequal dimorphism 
#'in Eurasian perch as an example.  Transactions of the American Fisheries Society 128:483-490.
#'
#'Schnute, J.  1981.  A versatile growth model with statistically stable
#'parameters. Canadian Journal of Fisheries & Aquatic Sciences, 38:1128-1140.
#'
#'Schnute, J. and D. Fournier. 1980.  A new approach to length-frequency analysis: 
#'Growth structure.  Canadian Journal of Fisheries and Aquatic Sciences, 37:1337-1351.
#'@keywords iplot
#'@examples
#'if (interactive()) {
#'
#'# Explore growth models (no data) -- use the defaults
#'growthModelSim()
#'
#'# Schnute parameterization of the von Bertalanffy model
#'growthModelSim(type="vbSchnute")
#'
#'## Explore growth models superimposed on length-at-age data
#'# get Smallmouth Bass data from FSA package
#'library(FSA)
#'data(SMBassWB)
#'
#'# interactively "fit" the second paramaterization of the Gompertz model to the data
#'growthModelSim(type="Gompertz2",lencap~agecap,data=SMBassWB,max.len=500)
#'
#'} # end if interactive
#'@export
#'
growthModelSim <- function(type=c("vbTypical","vbOriginal","vbGalucciQuinn","vbMooij",
                                  "vbSchnute","vbTypicalW","vbOriginalW","Gompertz1",
                                  "Gompertz2","Gompertz3","Schnute"),
                           x=NULL,y=NULL,data=NULL,max.len=500,max.wt=500) {
  type <- match.arg(type)
  if (class(x)=="formula") {
    DF <- model.frame(x,data=data)
    x <- DF[,2]
    y <- DF[,1]
    max.len <- max.wt <- max(y,na.rm=TRUE)
  }
  if ((!is.null(x) & is.null(y)) | (is.null(x) & !is.null(y))) stop("Both x (age) and y (length/weight) must be supplied if one is given.",call.=FALSE)
  growthModelSim.default(type,x,y,max.len,max.wt)
}

## Main function
growthModelSim.default <- function(type,x,y,max.len,max.wt) {
  ## internal function to predict length/weight for plotting -- used in growSimPlot
  predLength <- function(type,t,p1,p2,p3,p4) {
    switch(type,
           vbTypical= {  sd <- p1*(1-exp(-p2*(t-p3))) },                         # p1=Linf, p2=K,  p3=to, p4 not used
           vbTypicalW= { sd <- p1*(1-exp(-p2*(t-p3)))^p4 },                      # p1=Winf, p2=K,  p3=to, p4=b         
           vbOriginal= { sd <- (p1-(p1-p2)*exp(-p3*t)) },                        # p1=Linf, p2=L0, p3=K,  p4 not used
           vbOriginalW= { sd <- (p1-(p1-p2)*exp(-p3*t))^p4 },                    # p1=Winf, p2=W0, p3=K,  p4=b
           vbGalucciQuinn= { sd <- (p1/p2)*(1-exp(-p2*(t-p3))) },                # p1=omega,p2=K,  p3=t0, p4 not used
           vbMooij= { sd <- p1-(p1-p2)*exp(-(p3/p1)*t) },                        # p1=Linf, p2=L0, p3=ome,p4 not used
           vbSchnute= {                                                          # p1=L1,   p2=L2, p3=K,  p4=b
             sd <- ((p1^p4)+((p2^p4)-(p1^p4))*((1-exp(-p3*(t-min(t))))/(1-exp(-p3*(max(t)-min(t))))))^(1/p4)
           },
           Gompertz1= { sd <- p1*(exp(p2*(1-exp(-p3*t)))) },                     # p1=Lo,   p2=G,  p3=g,  p4 not used
           Gompertz2= { sd <- p1*(exp(-exp(-p2*(t-p3)))) },                      # p1=Linf, p2=g,  p3=t*, p4 not used
           Gompertz3= { sd <- p1*(exp((-1/p2)*exp(-p2*(t-p3)))) },               # p1=Linf, p2=g,  p3=t0, p4 not used
           Schnute= {                                                            # p1=L1,   p2=L2, p3=c,  p4=d
             minage <- min(t)
             maxage <- max(t)
             diffage <- maxage-minage
             if (p3==0) {
               if (p4==0) { sd <- p1*exp(log(p2/p1)*(t-minage)/diffage) }                                 # Case 4
               else { sd <- (p1^p4+(p2^p4-p1^p4)*(t-minage)/diffage)^(1/p4) }                             # Case 3
             } else {
               if (p4==0) { sd <- p1*exp(log(p2/p1)*(1-exp(-p3*(t-minage)))/(1-exp(-p3*diffage))) }       # Case 2
               else { sd <- (p1^p4+(p2^p4-p1^p4)*((1-exp(-p3*(t-minage)))/(1-exp(-p3*diffage))))^(1/p4) } # Case 1
             }
           } # end Schnute
    ) # end switch 
    sd
  } ## end predLength internal function
  
  ## internal function for constructing the plot
  growthSimPlot <- function(type,x,y,max.y,t.max,p1,p2,p3,p4) {
    t <- seq(0,t.max,length.out=t.max*20)
    vals <- predLength(type,t,p1,p2,p3,p4)
    ylbl <- ifelse (type %in% c("vbTypicalW","vbOriginalW"),"Weight","Length")
    old.par <- par(mar=c(3.5,3.5,1,1), mgp=c(2,0.4,0), tcl=-0.2); on.exit(par(old.par))
    if (is.null(x)||is.null(y)) plot(t,vals,xlab="Age",ylab=paste("Mean",ylbl),type="l",lwd=2,col="blue",ylim=c(0,max.y))
    else {
      plot(x,y,xlab="Age",ylab=ylbl,ylim=c(0,max.y),xlim=c(0,t.max))
      lines(t,vals,type="l",lwd=2,col="blue")
    }
  } ## end growthSimPlot internal function
  
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
  growthSimPlot(type,x,y,max.y,t.max,p1,p2,p3,p4)
  } ## end internal refresh

  ## begin main growthModelSim
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
    vbGalucciQuinn= {
      relax::gslider(refresh,prompt=TRUE,
             sl.names=   c("omega",  "K", "t_0", "Max Age"),
             sl.mins=    c(      0,    0,   -10,         5),
             sl.maxs=    c(    250,  0.5,    10,       100),
             sl.deltas=  c(      5, 0.01,   0.1,         1),
             sl.defaults=c(     75,  0.3,     0,     t.max),
             title = "Galucci & Quinn Von Bertalanffy",pos.of.panel="left")
      }, # end vbGalucciQuinn
    vbMooij= {
      relax::gslider(refresh,prompt=TRUE,
             sl.names=   c("L_inf",    "L_0", "omega", "Max Age"),
             sl.mins=    c(  min.y,        0,       0,         5),
             sl.maxs=    c(  max.y,    min.y,     250,       100),
             sl.deltas=  c(delta.y, min.y/50,       5,         1),
             sl.defaults=c(  max.y,    min.y,      75,     t.max),
             title = "Mooij et al. Von Bertalanffy",pos.of.panel="left")
      }, # end vbMooij
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