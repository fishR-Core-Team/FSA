#'Dynamics plots to explore numbers in a cohort over time.
#'
#'Constructs a plot of the hypothetical number of individuals in a cohort over
#'time.  The initial size of the cohort and the instantaneous mortality rate
#'are controlled by slider bars.
#'
#'@param age.max A single numeric that indicates the maximum age to use in the simulations.
#'@return None.  An interactive graphic connected to slider controls is produced.
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@keywords iplot
#'@examples
#'if (interactive()) {
#'
#'cohortSim()
#'
#'} # end if interactive
#'@export
#'
cohortSim <- function(age.max=15) {
  cohortSimPlot <- function(age.max,No,Z) {
    Nt <- No*exp(-Z*1:age.max)
    old.par <- par(mar=c(3.5,3.5,1,1), mgp=c(2,0.4,0), tcl=-0.2); on.exit(par(old.par))
    plot(0:age.max,c(No,Nt),xlab="Time / Age",ylab="Population Size",type="l",lwd=2,col="blue",ylim=c(0,10000))    
  } # end cohortSimPlot internal function
  
  refresh <- function(...) {
    No <- relax::slider(no=1)
    Z <- relax::slider(no=2)
    cohortSimPlot(age.max,No,Z)
  } # end refresh internal function

  relax::gslider(refresh,prompt=TRUE,
         sl.names=   c("Initial Numbers (No)", "Instantaneous Mortality (Z)"),
         sl.mins=    c(       1000,           0.05),
         sl.maxs=    c(      10000,           2.00),
         sl.deltas=  c(        500,           0.05),
         sl.defaults=c(      10000,           0.50),
         title = "Cohort Numbers-at-Age Simulator",
         pos.of.panel="left")
}
