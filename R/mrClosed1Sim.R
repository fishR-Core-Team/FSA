#'Dynamics plots to explore single-census mark-recapture models.
#'
#'This function is used to simulate multiple results for single census mark-recapture
#'study.  The user can control many aspects of the simulation including simulating
#'the loss of marks, differential survival rates for tagged and untagged fish, the
#'addition of recruits, and differential probabilities of capture for tagged and
#'untagged fish.  In addition, Petersen, Chapman, and Bailey estimates of population
#'size can be made.
#'
#'The user can use slider bars to choose values for the expected number of fish
#'to capture in the first (i.e., the marking or tagging) sample, the expected
#'number of fish to be captured in the second or final (i.e., the recapture)
#'sample, a probablity that a tagged fish loses the tag, the probability of
#'survival for tagged fish, the probability of survival for untagged fish, a
#'proportion of the initial population size to recruit to the population
#'between the first and final samples, and the ratio of the probability of
#'capture of tagged fish to the probability of capture of untagged fish in the
#'final sample.  The user selects the \dQuote{true} initial population size,
#'the method of population estimator, and the number of resamples to use with
#'arguments to the function.  The simulation can be re-run without changing any
#'of the slider buttons by pressing the \bold{\sQuote{Re-Randomize}} button.
#'
#'In general, the simulation follows these steps:
#'\enumerate{
#'\item a population of N fish is created;
#'\item randomly select M fish in the first sample to be tagged such that the average
#'of all M values should be equal to the user-supplied \bold{\sQuote{Tagged (M)}} value;
#'\item randomly select m fish to lose tags according to the user-supplied probability of
#'tag loss (\bold{\sQuote{PR(Tag Loss)}});
#'\item randomly select fish to die according to the user-supplied probabilities of
#'survival for tagged (\bold{\sQuote{PR(Surv Tagged)}}) and untagged fish
#'(\bold{\sQuote{PR(Surv UNTagged)}});
#'\item add recruits to the population in proportion to N and in accordance to the
#'user-supplied proportion of recruits (\bold{\sQuote{Proportion Recruit}}) to add;
#'\item identify the actual population size just before taking the final sample (N1);
#'\item randomly select n fish in the second sample such that the average of all
#'n values should be equal to the user-supplied \bold{\sQuote{Captured (n)}}
#'value (see note below about how differential probabilties of capture are
#'incorporated into the model);
#'\item count the number of tagged fish in the second sample; and 
#'\item compute the population estimate using the chosen method (see below).
#'}
#'
#'The methods for estimating the population size are
#'
#'\tabular{ll}{
#'\code{type="P"} \tab naive Petersen.\cr
#'\code{type="C"} \tab Chapman(1951) modification of the Petersen.\cr
#'\code{type="CR"} \tab Ricker(1975) modification of the Chapman modification.\cr
#'\code{type="B"} \tab Bailey(1951,1952) modification of the Petersen. 
#'}
#'
#'The effect of violating the assumption of tag loss is simulated by changing the 
#'probability of tag loss slider to a value greater than 0 but less than 1.  For
#'example, setting \bold{\sQuote{PR(Tag Loss)}} to 0.1 is used to simulate a 10 
#'percent probability of losing the tag.
#'
#'The effect of violating the assumption of no mortality from the first to
#'second sample is simulated by changing one or both of the probabilities
#'of survival for tagged and untagged fish to values less than 1 (but greater
#'than 0).  For example, setting \bold{\sQuote{PR(Surv Tagged)}} AND
#'\bold{\sQuote{PR(Surv UNTagged)}} to 0.8 will simulate mortality between the
#'first and final sample but NOT differential mortality between the tagged and
#'untagged fish (i.e., the mortalities are the same for both groups of fish).
#'The effect of differential mortalities between tagged and untagged fish can
#'be simulated by using different survival probabilities for tagged and untagged fish.
#'
#'The effect of violating the assumption of no recruitment from the first to
#'final sample is simulated by changing the Proportion Recruit slider to a
#'value greater than 0.  For example, setting \bold{\sQuote{Proportion Recruit}}
#'to 0.1 will simulate 10 percent of N recruiting to the population just before 
#'the final sample.
#'
#'The effect of violating the assumption of equal catchabilities in the final
#'sample for tagged and untagged fish is simulated by changing the
#'\bold{\sQuote{Ratio PR(Capture)}} slider to a value different than 1.  Values
#'greater than 1 indicate that the catchability of tagged fish is greater than
#'the catchability of untagged fish.  Values less than 1 indicate that the
#'catchability of tagged fish is less than that of untagged fish.  For example,
#'setting \bold{\sQuote{Ratio PR(Capture)}} to 0.8 will simulate a situation
#'where the capture probability of tagged fish is 80 percent of the capture
#'probablity of untagged fish (i.e., simulates the situation where marked fish
#'are less likely to be captured).
#'
#'The probability of capture in the final sample is equal to the expected number
#'of fish to be collected in the final sample (set with \bold{\sQuote{Captured (n)}})
#'divided by the actual population size just prior to the final sample (N1) as long
#'as \bold{\sQuote{Ratio PR(Capture)}} is 1.  The probabilities of capture for the
#'tagged and untagged fish are carefully adjusted if the \bold{\sQuote{Ratio PR(Capture)}}
#'value is different than 1.  Because of the different numbers of tagged and untagged
#'individuals in the population, the probability of capture for tagged and
#'untagged individuals must be computed by adjusting the overall probability of
#'capture to assure that, on average, the user-provided expected number
#'captured in the final sample is met.  This modification is found by solving
#'the following system of equations for the probabilities of capture for the
#'tagged (PM) and untagged fish (PU), respectively,
#'
#'\deqn{PM*M + PU*(N-M) = P*N}
#'
#'\deqn{\frac{PM}{PU} = k}
#'
#'where M is the number of tagged animals, and N-M is the number of untagged
#'animals, k is the \bold{\sQuote{Ratio PR(Capture)}} value, and P is the
#'overall probability of capture if there was no difference in catchability
#'between tagged and untagged animals.  The solutions to this system, which are 
#'used in this function, are
#'
#'\deqn{PU = \frac{PN}{M+N}}
#'
#'\deqn{PM = PU*k}
#'
#'@param type A single string that identifies the type of calculation method to use (see details).
#'@param N A single number that represents the \dQuote{known} size of the simulated population
#'just prior to the first sample.
#'@param rsmpls A single number that indicates the number of simulations to run.
#'@param incl.final A single logical that indicates whether the mean final population size
#'and histogram of percent error from the final population size should be shown.
#'@return None.  However, a dynamic graphic is produced that is controlled by
#'slider bars as described in the details.  The dynamic graphic is a histogram
#'of the population estimate from all resamples with a red vertical line at the
#'initial population size (provided by the user), a blue vertical line at the
#'population size just prior to the final sample (N1; if \code{incl.final=TRUE}),
#'and a green vertical line at the mean population estimate.  The vertical blue 
#'and red lines may not be visible under some scenarios because of overprinting.
#'@author Derek H. Ogle, \email{dogle@@northland.edu}
#'@seealso \code{\link{mrClosed}}
#'@keywords iplot
#'@examples
#'if (interactive()) {
#'
#'mrClosed1Sim()
#'mrClosed1Sim(type="C")  # use Chapman modification
#'
#'} # end if interactive
#'
#'@export
#'
mrClosed1Sim <- function(type=c("Petersen","P","Chapman","C","Ricker","R","Bailey","B"),N=1000,rsmpls=500,incl.final=TRUE) {
  mrClosedSimPlot <- function(type,N,rsmpls,incl.final,EM,En,mark.loss,surv.mark,surv.unmark,recruits,cap.ratio) {
    popGen <- function(type,N,EM,En,mark.loss,surv.mark,surv.unmark,recruits,cap.ratio,rsmpls) { # Run population simulations
      M <- n <- m <- N0 <- N1 <- rep(0,rsmpls)                                                # Initialize vector sizes
      for (i in 1:rsmpls) {
        adj.M <- M[i] <- length(which(runif(N)<(EM/N)))                                       # Number marked in first sample (EM/N is probability of being marked)
        adj.U <- N-M[i]                                                                       # Unmarked fish
        if (mark.loss>0) {
          lost.marks <- length(which(runif(M[i])<mark.loss))                                  # Apply tag loss probability
          adj.M <- adj.M - lost.marks                                                         # Removed lost mark fish from marked popn
          adj.U <- adj.U + lost.marks                                                         # Put lost mark fish back into unmarked popn
        }
        if (!surv.mark==1)   adj.M <- adj.M - length(which(runif(M[i])>surv.mark))            # Marked fish survival -- greater than survival prob is mortality
        if (!surv.unmark==1) adj.U <- adj.U - length(which(runif(adj.U)>surv.unmark))         # UnMarked fish survival
        if (!recruits==0)    adj.U <- adj.U + recruits                                        # Add recruits to unmarked population
        N1[i] <- adj.M + adj.U                                                                # Population size just before second capture
        p.unmark <- En/N1[i]*N1[i]/(M[i]+N1[i])                                               # Probability of capturing unmarked fish in second sample
        p.mark <- p.unmark*cap.ratio                                                          # Probability of capturing marked fish in second sample
        m[i] <- length(which(runif(adj.M)<p.mark))                                            # Number of marked fish recaptured in second sample
        n[i] <- m[i] + length(which(runif(adj.U)<p.unmark))                                   # Total number of fish captured in second sample
        # Compute population estimates
        switch(type,
               P=,Petersen={ N0[i] <- round((M[i]*n[i])/m[i],0) },
               C=,Chapman={ N0[i] <- round((M[i]+1)*(n[i]+1)/(m[i]+1)-1,0) },
               R=,Ricker={ N0[i] <- round((M[i]+1)*(n[i]+1)/(m[i]+1),0) },
               B=,Bailey={ N0[i] <- round(M[i]*(n[i]+1)/(m[i]+1),0) }
        )
      }
      data.frame(M,n,m,N,N1,N0)
    } # end popGen internal function in mrCSimPlot internal function
    
    graphLabel <- function(mark.loss,surv.mark,surv.unmark,recruits,cap.ratio) {
      viol.markloss <- viol.diffmort <- viol.surv <- viol.recruits <- viol.capratio <- FALSE
      if(mark.loss > 0) { viol.markloss <- TRUE; lbl <- "VIOLATION: Loss of Tags" }
      if(surv.mark < 1 || surv.unmark < 1) {
        if (surv.mark == surv.unmark) { viol.surv <- TRUE ; lbl <- "VIOLATION: Mortality" }
        else if (surv.unmark == 1) { viol.surv <- TRUE; lbl <- "VIOLATION: Mortality of Tagged Fish" }
        else if (surv.mark ==1) { viol.surv <- TRUE; lbl <- "VIOLATION: Mortality of UNtagged Fish" }
        else { viol.surv <- TRUE; lbl <- "VIOLATION: Mortality (Differential)" }
      }
      if(recruits > 0) { viol.recruits <- TRUE; lbl <- "VIOLATION: Recruitment" }
      if(cap.ratio > 1) { viol.capratio <- TRUE; lbl <- "VIOLATION: Trap-Happy" } 
      if(cap.ratio < 1) { viol.capratio <- TRUE; lbl <- "VIOLATION: Trap-Shy" }
      viol.sum <- sum(c(viol.markloss,viol.diffmort,viol.surv,viol.recruits,viol.capratio))
      if(viol.sum==0) lbl <- "No Assumption Violations"
        else if (viol.sum > 1) lbl <- "VIOLATION: Multiple Assumptions"
      lbl
    } # end graphLabel internal function inside of mrCSimPlot internal function
    
    graphGen <- function(df,N,incl.final,hlbl) {
      mn.N0.perr <- paste("% Error Initial = ",round(mean(100*(df$N0-N)/N),1),sep="")
      mn.N1.perr <- paste("% Error Final = ",round(mean(100*(df$N0-df$N1)/df$N1),1),sep="")
      # Histogram of population estimates
      old.par <- par(mar=c(3.5,1.5,1.5,1.5), mgp=c(2,0.4,0), tcl=-0.2); on.exit(par(old.par))
      h <- hist(df$N0,plot=FALSE)
      hist(df$N0,main=hlbl,xlab="Population Estimate",yaxt="n",ylab="",xlim=range(c(mean(df$N1),mean(df$N0),N,h$breaks)),col="gray90")
      abline(v=N,col="red",lwd=4,lty=2); abline(v=mean(df$N0),col="green",lwd=4)
      if (incl.final) {
        abline(v=mean(df$N1),col="blue",lwd=4,lty=3)
        legend("topright",legend=c(paste("Initial Pop (",N,")",sep=""),"Mean Final Pop","Mean Pop Est","",mn.N0.perr,mn.N1.perr),col=c("red","blue","green",NA,NA,NA),lwd=2,lty=c(2,3,1,NA,NA,NA),bty="n")
      } else {
        legend("topright",legend=c(paste("Initial Pop (",N,")",sep=""),"Mean Pop Est","",mn.N0.perr),col=c("red","green",NA,NA),lwd=2,lty=c(2,1,NA,NA),bty="n")
      }
    } # end graphGen internal function inside of mrCSimPlot internal function
    
    # begin main mrCsimPlot internal function
    mc.df <- popGen(type,N,EM,En,mark.loss,surv.mark,surv.unmark,recruits,cap.ratio,rsmpls)  # Run population simulations
    hlbl <- graphLabel(mark.loss,surv.mark,surv.unmark,recruits,cap.ratio)                   # Find label for graph
    graphGen(mc.df,N,incl.final,hlbl)                                                        # Graph the results    
  } # end mrCSimPlot internal function
  
  refresh <- function(...) {
    EM <- relax::slider(no=1)            # Expected number to mark on first sample
    En <- relax::slider(no=2)            # Expected number captured on second sample
    mark.loss <- relax::slider(no=3)     # Probability of losing a mark
    surv.mark <- relax::slider(no=4)     # Probability of survival for marked fish
    surv.unmark <- relax::slider(no=5)   # Probability of survival for unmarked fish
    recruits <- N*relax::slider(no=6)    # Number of recruits to add (No times proportion to add)
    cap.ratio <- relax::slider(no=7)     # Ratio of probability of capture for marked to unmarked
    mrClosedSimPlot(type,N,rsmpls,incl.final,EM,En,mark.loss,surv.mark,surv.unmark,recruits,cap.ratio)
  }

  # begin main mrClosed1Sim function
  type <- match.arg(type)
  N.1 <- round(0.01*N)
  sl.names <-    c("Tagged (M)", "Captured (n)", "PR(Tag Loss)", "PR(Surv Tagged)", "PR(Surv UNtagged)", "Proportion Recruit", "Ratio PR(Capture)")
  sl.mins <-     c(       5*N.1,          5*N.1,              0,               0.1,                 0.1,                    0,                 0.2)
  sl.maxs <-     c(      40*N.1,         40*N.1,            0.9,                 1,                   1,                  0.5,                 5  )
  sl.deltas <-   c(         N.1,            N.1,            0.1,               0.1,                 0.1,                  0.1,                 0.1)  
  sl.defaults <- c(      20*N.1,         20*N.1,              0,                 1,                   1,                    0,                 1  )
  sl.title <- "Single Closed Population M/R Simulator"
  relax::gslider(refresh,prompt=TRUE,sl.names=sl.names,sl.mins=sl.mins,sl.maxs=sl.maxs,sl.deltas=sl.deltas,sl.defaults=sl.defaults,title=sl.title,
  but.functions= function(...){relax::slider(obj.name="rerand",obj.value="Y");refresh()}, but.names=c("Re-Randomize"),pos.of.panel="left",hscale=1.4,vscale=1.2)
}
