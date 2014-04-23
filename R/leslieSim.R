#'Dynamic plots to explore the Leslie depletion model.
#'
#'Constucts hypothetical catch and effort data given choices for number of removal
#'events, initial population size, effort, catchability, survival, and recruitment
#'for a hypothetical depletion fishery.  Various plots are produced (see details)
#'with corresponding slider bars that allow the user to modify parameter values
#'to explore the effect of these modifications on the Leslie model dynamics or results.
#'This function is used primarily to explore the effects of a parameter on the model
#'and the effects of assumptions violations on modely dynamics.
#'
#'Three versions of the simulation are allowed.  First, the model is purely \bold{\code{deterministic}}
#'(i.e., without randomization) such that the shape of the Leslie model can be easily
#'explored.  Second, the model includes a \bold{\code{random}} component and one collection
#'of data is plotted with the Leslie model and the estimates of the catchabitilty
#'coefficient (q) and initial population size (No) superimposed.  This version
#'allows the user to explore the effect of variability on the model dynamics and
#'how this variability and assumption violations effect the parameter estimates.
#'The third version is like the second except that the results are repeated \code{rsmpls}
#'times, the q and No are computed from each resample, and these results are plotted.
#'This version, called the \bold{\code{resampling}} version allows the user to examine
#'the sampling distributions of the parameter estimates and compare those to the
#'known parameters to explore the bias caused by assumption violations.  The plots
#'produced are described further in the \dQuote{returns} section below.
#'
#'In the \code{random} and \code{resampling} versions, randomness is included in the
#'model by including binomial stochasticity in the catch and survival functions.
#'Specifically, the number captured is, effectively, computed by assigning a uniform
#'random number from between 0 and 1 to each individual in the population and then
#'\dQuote{catching} those individuals where this value is less than q*E (where E
#'is effort expended).  A similar method is used for survivorship, but with q*E
#'replaced with the user chosen probability of survival.
#
#'For each version, a set of plots is produced that are linked to a set of
#'slider bars that allows the user to change model parameters or create assumption
#'violations.  A slider is created to control the number of removal events, initial
#'population size (No), effort, and catchability coefficent (q).  The remaining
#'sliders allow for simulating specific violations to the assumption of a Leslie
#'model.  These sliders are further described below.
#'
#'The \bold{\sQuote{q factor}} value is a constant that modifies the catchability
#'coefficient (q) for each subsequent sample.  For example, if \bold{\sQuote{q.factor}}
#'is set to 0.8 then the catchability decreases by a constant multiplier of 0.8 for
#'each sample.  In other words, the catchability set with the catchability slider
#'is multiplied by the vector \code{c(1,0.8,0.8^2,0.8^3,...)} to determine a
#'catchability for each sample.
#'
#'The \bold{\sQuote{Survival}} value is a constant used as a proportion of fish
#'alive at time t that survive to time t+1 or, if \code{use.rand=TRUE}, is the
#'probability that a fish survives from time t to time t+1.  The survival
#'function is applied to the population after the catch at time t has already
#'been removed from the population.
#'
#'The \bold{\sQuote{Recruitment}} value is a constant used to determine the
#'number of \dQuote{new} fish to recruit to the population from time t to time
#'t+1.  The number to recruit is equal to the recruitment proportion of the extant
#'number of fish alive at time t.  For example, if 100 fish are alive at time t
#'and the recruitment factor is 0.2 then 100*0.2=20 fish will be added to the
#'population just before time t+1.  The number of fish to recruit is computed
#'after the catch at time t and any natural mortality at time t have been
#'removed from the population.
#'
#'@note The range of values allowed for each of the parameters were chosen to allow
#'a wide variety of model values.  However, it is highly likely that these ranges
#'do not encompass every possible set of values that a user may wish to view.
#'Thus, this simulation should not be used for research-grade simulations.
#'@param type A single string that indicates the type or version of simulation that
#'should be used.  See the details.
#'@param ricker.mod A single logical value that indicates whether the modification
#'proposed by Ricker should be used (\code{=TRUE}) or not (\code{=FALSE}, default).
#'@param rsmpls A single numeric for the number of simulations to run.
#'@return None.  An interactive graphic with corresponding slider bars, which differs
#'depending on the version (as defined by \code{type}) of simulation used, is produced.
#'
#'In the deterministic and random versions a plot of catch-per-unit-effort (CPE) against
#'cumulative catch (i.e., the Leslie plot) is displayed.  In the deterministic version,
#'as many as three lines may be seen.  The gray line is the Leslie model for the
#'default values from the slider bars.  This line is used simply as a basis for
#'examining changes in parameters.  The blue line is the Leslie model for current
#'choices of \bold{\sQuote{Removals}}, \bold{\sQuote{Initial Size}}, \bold{\sQuote{Effort}},
#'and \bold{\sQuote{Catchability}} but NOT for \bold{\sQuote{q factor}}, \bold{\sQuote{Survival}},
#'or \bold{\sQuote{Recruitment}}.  In other words, the blue line reflects the model
#'for other than default parameter choices but with NO assumption violations.  This
#'line serves as a basis for judging different parameter choices without any assumption
#'violations.  The red line is the Leslie model for all current choices of sliders.  The
#'lines are plotted in the order of \dQuote{gray}, \dQuote{red}, \dQuote{blue} so,
#'if any two are equal then the color first plotted will not be seen.
#'
#'In the random version, the graphic is simply the traditional Leslie model graphic
#'(see \code{\link{depletion}}) with the \dQuote{random} catch-per-unit-effort values
#'plotted against total catch with a best-fit linear regression line shown in blue.
#'The current estimaes of q and No from the random data are also printed on the graph.
#'A \bold{\sQuote{Re-Randomize}} button is included with the sliders which can be used
#'to evaluate the model again (with a different random seed) at the current slider choices.
#'
#'In the resampling version, three side-by-side graphs will be produced.  The left-most
#'graph is a histogram of the estimates of the initial population size (No) from all
#'resamples.  The middle graph is a histogram of the estiamtes of the catchability
#'coefficient from all resamples.  Both histograms will have a vertical red dashed
#'line at the true value of the parameter (No or q, as provided by the user) and a
#'vertical blue solid line at the mean value of the estimate from all resamples.
#'The right-most graph is a scatterplot of the paired catchability and initial
#'population size estimates with red lines showing the true values of the
#'catchability and initial population size and blue lines at the means of the
#'respective estimates.
#'
#'@seealso \code{\link{removal}}.
#'@keywords iplot
#'@examples
#'if (interactive()) {
#'
#'## Deterministic exploration of model dynamics
#'leslieSim()              
#'
#'## Stochastic exploration of model dynamics -- Leslie model plot
#'leslieSim(type="random")
#'
#'## Stochastic exploration of model dynamics -- sampling distribution plots
#'leslieSim(type="resampling")
#'
#'} # end if interactive
#'@export
#'
leslieSim <- function(type=c("deterministic","random","resampling","montecarlo"),ricker.mod=FALSE,rsmpls=100) {
  graphLabel <- function(q.adj.const,p.surv,r.prop) {
    viol.q <- viol.surv <- viol.Nocv <- viol.recruits <- FALSE
    if (q.adj.const != 1) {
      viol.q <- TRUE
      if (q.adj.const < 1) lbl <- "VIOLATION: decreasing catchability"
      else lbl <- "VIOLATION: increasing catchability"
    }
    if (any(p.surv < 1)) {
      viol.surv <- TRUE
      lbl <- "VIOLATION: natural mortality is occurring"
    }
    if (any(r.prop > 0)) {
      viol.recruits <- TRUE
      lbl <- "VIOLATION: recruitment is occurring"
    }
    viol.sum <- sum(c(viol.q,viol.surv,viol.recruits))
    if(viol.sum==0) lbl <- "No Assumptions Violations"
    else if (viol.sum > 1) lbl <- "VIOLATION: Multiple Assumptions"
    lbl
  } # end graphLabel internal function
  
  leslieRandRun <- function(plist) {
    k <- plist$k
    Nt <- plist$Nt
    Et <- plist$Et
    qt <- plist$qt
    p.surv <- plist$p.surv
    r.prop <- plist$r.prop  
    Ct <- Mt <- Rt <- rep(0,k)                                 # Initialize catch, mortality, and recruitment vectors      
    for (i in 1:k) {
      Ct[i] <- length(which(runif(Nt[i])<qt[i]*Et[i]))         # Determine whether caught or not -- capture probability is q*E*N/N or just q*E
      if(i<k) {                                                # Update population size after catch for next pass
        Nt[i+1] <- Nt[i]-Ct[i]                                 # Reduce by catch in current time
        if (any(p.surv<1)) {                                   # Reduce by mortality, if any, in current time
          Mt[i] <- length(which(runif(Nt[i+1])<(1-p.surv[i]))) # Mortality
          Nt[i+1] <- Nt[i+1]-Mt[i]
        }  
        if (any(r.prop>0)) {                                   # Increase by recruitment, if any, in current time
          Rt[i] <- round(Nt[i+1]*r.prop[i],0)
          Nt[i+1] <- Nt[i+1]+Rt[i]
        }
      }
    }
    data.frame(Nt,qt=round(qt,4),Mt,Rt,Et,Ct,Pt=round(Ct/Nt,3))
  } # end leslieRandRun internal
  
  detLeslieSimPlot <- function(params.def, params.mod, params.ass,q.adj.const,ricker.mod) {
    detRun <- function(plist) {
      k <- plist$k
      Nt <- plist$Nt
      Et <- plist$Et
      qt <- plist$qt
      p.surv <- plist$p.surv
      r.prop <- plist$r.prop
      Ct <- Mt <- Rt <- rep(0,k)                  # Initialize catch, mortality, and recruitment vectors      
      for (i in 1:k) {
        Ct[i] <- qt[i]*Et[i]*Nt[i]
        if(i<k) {                                 # Update population size after catch for next pass
          Nt[i+1] <- Nt[i]-Ct[i]                  # Reduce by catch in current time
          if (any(p.surv<1)) {                    # Reduce by mortality, if any, in current time
            Mt[i] <- (1-p.surv[i])*Nt[i+1]
            Nt[i+1] <- Nt[i+1]-Mt[i]
          }  
          if (any(r.prop>0)) {                    # Increase by recruitment, if any, in current time
            Rt[i] <- round(Nt[i+1]*r.prop[i],0)
            Nt[i+1] <- Nt[i+1]+Rt[i]
          }
        }
      }
      data.frame(Nt,qt=round(qt,4),Mt,Rt,Et,Ct,Pt=round(Ct/Nt,3))
    } # end detRun internal function inside of detLeslieSimPlot
    
    detPlot <- function(resdef,res1,res2,ricker.mod=FALSE,glbl) {
      resdef$cpe <- resdef$Ct/resdef$Et
      res1$cpe <- res1$Ct/res1$Et
      res2$cpe <- res2$Ct/res2$Et
      if (!ricker.mod) {
        resdef$K <- cumsum(resdef$Ct)-resdef$Ct
        res1$K <- cumsum(res1$Ct)-res1$Ct
        res2$K <- cumsum(res2$Ct)-res2$Ct
      } else {
        resdef$K <- cumsum(resdef$Ct)-(resdef$Ct/2)
        res1$K <- cumsum(res1$Ct)-(res1$Ct/2)
        res2$K <- cumsum(res2$Ct)-(res2$Ct/2)
      }
      plot(resdef$K,resdef$cpe,type="o",main=glbl,xlab="Cumulative Catch",ylab="CPE",pch=19,lwd=2,col="gray",xlim=c(0,1.75*max(resdef$K)),ylim=c(0,1.75*max(resdef$cpe)))  # default
      points(res2$K,res2$cpe,type="o",pch=19,lwd=2,col="red")           # final
      points(res1$K,res1$cpe,type="o",pch=19,lwd=2,col="blue")          # modified but if no assumptions violated
    } # end detPlot internal function inside of detLeslieSimPlot
    
    # begin main detLeslieSimPlot
    res.def <- detRun(params.def)
    res.mod <- detRun(params.mod)
    res.ass <- detRun(params.ass)
    glbl <- graphLabel(q.adj.const,res.ass$p.surv,res.ass$r.prop)
    old.par <- par(mar=c(3.5,3.5,1.5,1.5), mgp=c(2,0.4,0),tcl=-0.2); on.exit(par(old.par))    
    detPlot(res.def,res.mod,res.ass,ricker.mod,glbl)  # Plot total results
    lgndtxt <- c("default","assumptions not met","if assumptions met")
    legend(x="topright",legend=lgndtxt,lwd=2,col=c("gray","red","blue"),pch=19,cex=0.8)
  } # end detLeslieSimPlot internal function
  
  randLeslieSimPlot <- function(params,ricker.mod,q.adj.const) {
    res.ass <- leslieRandRun(params)                      # Results for possible assumptions violations
    res <- depletion(type="Leslie",res.ass$Ct,res.ass$Et,ricker.mod=ricker.mod)
    old.par <- par(mar=c(3.5,3.5,1.5,1.5), mgp=c(2,0.4,0),tcl=-0.2); on.exit(par(old.par))
    plot(res,xlim=c(0,1000),ylim=c(0,50),main=graphLabel(q.adj.const,params$p.surv,params$r.prop))    
  } # end randLeslieSimPlot internal function
  
  mcLesliePlot <- function(rsmpls,ricker.mod,plist) {
    res.qhat <- res.N0hat <- NULL
    for (i in 1:rsmpls) {
      res.ass <- leslieRandRun(plist)       # Results for possible assumptions violations
      if (!ricker.mod) res <- depletion(res.ass$Ct,res.ass$Et,"Leslie")
      else res <- depletion(res.ass$Ct,res.ass$Et,"Leslie",ricker.mod=ricker.mod)
      res.qhat[i] <- res$est["q","Estimate"]
      res.N0hat[i] <- res$est["No","Estimate"]
    }
    
    # Histogram of population estimates
    old.par <- par(mar=c(3.5,1,1,1), mgp=c(2.25,0.4,0), tcl=-0.2, mfrow=c(1,3))
    h.N <- hist(res.N0hat,plot=FALSE)
    hist(res.N0hat,main="",xlab="Estimated Population Size",yaxt="n",ylab="",xlim=range(c(plist$Nt,h.N$breaks)),col="gray90")
    abline(v=plist$Nt[1],col="red",lwd=4,lty=3)
    abline(v=mean(res.N0hat),col="blue",lwd=3) 
    legend("topright",c("truth","mean"),col=c("red","blue"),lty=c(3,1),lwd=4)
    # Histogram of catchability estimates
    h.q <- hist(res.qhat,plot=FALSE)
    hist(res.qhat,main="",xlab="Catchability Estimate",yaxt="n",ylab="",xlim=range(c(plist$qt[1],h.q$breaks)),col="gray90")
    abline(v=plist$qt[1],col="red",lwd=4,lty=3)
    abline(v=mean(res.qhat),col="blue",lwd=3)
    # Scatterplot of No vs q
    old.par <- par(mar=c(3.5,2,1,1), mgp=c(1,0,0)); on.exit(par(old.par)) 
    plot(res.qhat,res.N0hat,xlab="Catchability Estimate",ylab="Estimated Population Size",xaxt="n",yaxt="n",pch=19,xlim=range(c(plist$qt[1],res.qhat)),ylim=range(c(plist$Nt,res.N0hat)))
    abline(h=plist$Nt[1],col="red",lwd=4,lty=3)
    abline(v=plist$qt[1],col="red",lwd=3,lty=3)
    abline(h=mean(res.N0hat),col="blue",lwd=3,lty=1)
    abline(v=mean(res.qhat),col="blue",lwd=3,lty=1)
  } # end mcLesliePlot internal function
  
  detRefresh <- function(...) {
    # put default parameters for 8 removal events into a parameter list
    params.def <- list(k=8,Nt=rep(500,8),Et=rep(5,8),qt=rep(0.05,8),p.surv=rep(1,8),r.prop=rep(0,8))
    # get parameter values from sliders and put in parameter list for no violations lines
    k <- relax::slider(no=1)
    No <- relax::slider(no=2)
    E <- relax::slider(no=3)
    q <- relax::slider(no=4)
    params.mod <- list(k=k,Nt=rep(No,k),Et=rep(E,k),qt=rep(q,k),p.surv=rep(1,k),r.prop=rep(0,k))
    # get assumption violation values from sliders and put in a parameter list for assumption violations line
    p.surv <- relax::slider(no=6)          # Get probability of survival value and put into a vector
    r.prop <- relax::slider(no=7)          # Get proportional recruitment addition value and put into a vector
    q.adj.const <- relax::slider(no=5)     # Get catchabitity constant adjustment value
    q <- q*q.adj.const^(0:(k-1))                   # Create vector of catchabilities based on q and q.adj.prop
    params.ass <- list(k=k,Nt=rep(No,k),Et=rep(E,k),qt=q,p.surv=rep(p.surv,k),r.prop=rep(r.prop,k))
    detLeslieSimPlot(params.def, params.mod, params.ass,q.adj.const,ricker.mod)
  } # end detRefresh internal function
  
  randRefresh <- function(...) {
    k <- relax::slider(no=1)                      # Number of removal events 
    Nt <- rep(relax::slider(no=2),k)              # Initialize population size vector
    Et <- rep(relax::slider(no=3),k)              # Initialize effort vector (constants)
    qt <- rep(relax::slider(no=4),k)              # Get catchability constant and put into a vector
    p.surv <- rep(relax::slider(no=6),k)          # Get probability of survival value and put into a vector
    r.prop <- rep(relax::slider(no=7),k)          # Get proportional recruitment addition value and put into a vector
    q.adj.const <- relax::slider(no=5)            # Get catchabitity constant adjustment value
    qt <- qt*q.adj.const^(0:(k-1))                        # Create vector of catchabilities based on q and q.adj.prop
    randLeslieSimPlot(list(k=k,Nt=Nt,Et=Et,qt=qt,p.surv=p.surv,r.prop=r.prop),ricker.mod,q.adj.const)
  } # end randRefresh internal function

  mcRefresh <- function(...) {
    k <- relax::slider(no=1)                     # Number of removal events 
    Nt <- rep(relax::slider(no=2),k)             # Initialize population size vector
    Et <- rep(relax::slider(no=3),k)             # Initialize effort vector (constants)
    qt <- rep(relax::slider(no=4),k)             # Get catchability constant and put into a vector
    p.surv <- rep(relax::slider(no=6),k)         # Get probability of survival value and put into a vector
    r.prop <- rep(relax::slider(no=7),k)         # Get proportional recruitment addition value and put into a vector
    q.adj.const <- relax::slider(no=5)           # Get catchabitity constant adjustment value
    qt <- qt*q.adj.const^(0:(k-1))                       # Create vector of catchabilities based on q and q.adj.prop
    params <- list(k=k,Nt=Nt,Et=Et,qt=qt,p.surv=p.surv,r.prop=r.prop)
    mcLesliePlot(rsmpls,ricker.mod,params)
  } # end sim2Refresh internal function
  
  
  # begin main leslieSim function
  sl.names <-    c("Removals", "Initial Size (No)", "Effort (E)", "Catchability(q)", "q factor", "Survival","Recruitment")
  sl.mins <-     c(         3,                100,             1,             0.01,         0.7,       0.7,          0.0)
  sl.maxs <-     c(        15,               1000,            20,             0.15,          1.3,       1.0,          0.2)
  sl.deltas <-   c(         1,                 50,             1,             0.01,         0.01,      0.02,         0.02)
  sl.defaults <- c(         8,                500,             5,             0.05,         1.0,       1.0,          0.0)
  sl.title <- 
  
  type <- match.arg(type)
  switch(type,
         deterministic={
           ttl <- "deterministic Leslie Model"
           relax::gslider(detRefresh,prompt=TRUE,sl.names=sl.names,sl.mins=sl.mins,sl.maxs=sl.maxs,sl.deltas=sl.deltas,sl.defaults=sl.defaults,title=ttl,pos.of.panel="left")
           },
         random={
           ttl <- "random Leslie Model"
           relax::gslider(randRefresh,prompt=TRUE,sl.names=sl.names,sl.mins=sl.mins,sl.maxs=sl.maxs,sl.deltas=sl.deltas,sl.defaults=sl.defaults,title=ttl,
                          but.functions= function(...){relax::slider(obj.name="rerand",obj.value="Y");randRefresh()}, but.names=c("Re-Randomize"),pos.of.panel="left")
           },
         resampling=,montecarlo={
           ttl <- "Monte Carlo Leslie Model"
           relax::gslider(mcRefresh,prompt=TRUE,sl.names=sl.names,sl.mins=sl.mins,sl.maxs=sl.maxs,sl.deltas=sl.deltas,sl.defaults=sl.defaults,title=ttl,
                          but.functions= function(...){relax::slider(obj.name="rerand",obj.value="Y");mcRefresh()}, but.names=c("Re-Randomize"),
                          pos.of.panel="left",hscale=1.5)
         }
  ) 
}
