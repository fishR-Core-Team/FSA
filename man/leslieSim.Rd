\name{leslieSim}
\alias{leslieSim}
\title{Dynamic plots to explore the Leslie depletion model.}
\usage{
leslieSim(type = c("deterministic", "random", "resampling", "montecarlo"),
  ricker.mod = FALSE, rsmpls = 100)
}
\arguments{
  \item{type}{A single string indicating the type or
  version of simulation that should be used.  See the
  details.}

  \item{ricker.mod}{A single logical value that indicates
  whether the modification proposed by Ricker should be
  used (\code{=TRUE}) or not (\code{=FALSE}, default).}

  \item{rsmpls}{A single numeric for the number of
  simulations to run.}
}
\value{
None.  An interactive graphic with corresponding slider
bars, which differs depending on the version (as defined by
\code{type}) of simulation used, is produced.

In the deterministic and random versions a plot of
catch-per-unit-effort (CPE) against cumulative catch (i.e.,
the Leslie plot) is displayed.  In the deterministic
version, as many as three lines may be seen.  The gray line
is the Leslie model for the default values from the slider
bars.  This line is used simply as a basis for examining
changes in parameters.  The blue line is the Leslie model
for current choices of \bold{\sQuote{Removals}},
\bold{\sQuote{Initial Size}}, \bold{\sQuote{Effort}}, and
\bold{\sQuote{Catchability}} but NOT for \bold{\sQuote{q
factor}}, \bold{\sQuote{Survival}}, or
\bold{\sQuote{Recruitment}}.  In other words, the blue line
reflects the model for other than default parameter choices
but with NO assumption violations.  This line serves as a
basis for judging different parameter choices without any
assumption violations.  The red line is the Leslie model
for all current choices of sliders.  The lines are plotted
in the order of \dQuote{gray}, \dQuote{red}, \dQuote{blue}
so, if any two are equal then the color first plotted will
not be seen.

In the random version, the graphic is simply the
traditional Leslie model graphic (see
\code{\link{depletion}}) with the \dQuote{random}
catch-per-unit-effort values plotted against total catch
with a best-fit linear regression line shown in blue. The
current estimaes of q and No from the random data are also
printed on the graph. A \bold{\sQuote{Re-Randomize}} button
is included with the sliders which can be used to evaluate
the model again (with a different random seed) at the
current slider choices.

In the resampling version, three side-by-side graphs will
be produced.  The left-most graph is a histogram of the
estimates of the initial population size (No) from all
resamples.  The middle graph is a histogram of the
estiamtes of the catchability coefficient from all
resamples.  Both histograms will have a vertical red dashed
line at the true value of the parameter (No or q, as
provided by the user) and a vertical blue solid line at the
mean value of the estimate from all resamples. The
right-most graph is a scatterplot of the paired
catchability and initial population size estimates with red
lines showing the true values of the catchability and
initial population size and blue lines at the means of the
respective estimates.
}
\description{
Constucts hypothetical catch and effort data given choices
for number of removal events, initial population size,
effort, catchability, survival, and recruitment for a
hypothetical depletion fishery.  Various plots are produced
(see details) with corresponding slider bars that allow the
user to modify parameter values to explore the effect of
these modifications on the Leslie model dynamics or
results. This function is used primarily to explore the
effects of a parameter on the model and the effects of
assumptions violations on modely dynamics.
}
\details{
Three versions of the simulation are allowed.  First, the
model is purely \bold{\code{deterministic}} (i.e., without
randomization) such that the shape of the Leslie model can
be easily explored.  Second, the model includes a
\bold{\code{random}} component and one collection of data
is plotted with the Leslie model and the estimates of the
catchabitilty coefficient (q) and initial population size
(No) superimposed.  This version allows the user to explore
the effect of variability on the model dynamics and how
this variability and assumption violations effect the
parameter estimates. The third version is like the second
except that the results are repeated \code{rsmpls} times,
the q and No are computed from each resample, and these
results are plotted. This version, called the
\bold{\code{resampling}} version allows the user to examine
the sampling distributions of the parameter estimates and
compare those to the known parameters to explore the bias
caused by assumption violations.  The plots produced are
described further in the \dQuote{returns} section below.

In the \code{random} and \code{resampling} versions,
randomness is included in the model by including binomial
stochasticity in the catch and survival functions.
Specifically, the number captured is, effectively, computed
by assigning a uniform random number from between 0 and 1
to each individual in the population and then
\dQuote{catching} those individuals where this value is
less than q*E (where E is effort expended).  A similar
method is used for survivorship, but with q*E replaced with
the user chosen probability of survival. For each version,
a set of plots is produced that are linked to a set of
slider bars that allows the user to change model parameters
or create assumption violations.  A slider is created to
control the number of removal events, initial population
size (No), effort, and catchability coefficent (q).  The
remaining sliders allow for simulating specific violations
to the assumption of a Leslie model.  These sliders are
further described below.

The \bold{\sQuote{q factor}} value is a constant that
modifies the catchability coefficient (q) for each
subsequent sample.  For example, if
\bold{\sQuote{q.factor}} is set to 0.8 then the
catchability decreases by a constant multiplier of 0.8 for
each sample.  In other words, the catchability set with the
catchability slider is multiplied by the vector
\code{c(1,0.8,0.8^2,0.8^3,...)} to determine a catchability
for each sample.

The \bold{\sQuote{Survival}} value is a constant used as a
proportion of fish alive at time t that survive to time t+1
or, if \code{use.rand=TRUE}, is the probability that a fish
survives from time t to time t+1.  The survival function is
applied to the population after the catch at time t has
already been removed from the population.

The \bold{\sQuote{Recruitment}} value is a constant used to
determine the number of \dQuote{new} fish to recruit to the
population from time t to time t+1.  The number to recruit
is equal to the recruitment proportion of the extant number
of fish alive at time t.  For example, if 100 fish are
alive at time t and the recruitment factor is 0.2 then
100*0.2=20 fish will be added to the population just before
time t+1.  The number of fish to recruit is computed after
the catch at time t and any natural mortality at time t
have been removed from the population.
}
\note{
The range of values allowed for each of the parameters were
chosen to allow a wide variety of model values.  However,
it is highly likely that these ranges do not encompass
every possible set of values that a user may wish to view.
Thus, this simulation should not be used for research-grade
simulations.
}
\examples{
if (interactive()) {

## Deterministic exploration of model dynamics
leslieSim()

## Stochastic exploration of model dynamics -- Leslie model plot
leslieSim(type="random")

## Stochastic exploration of model dynamics -- sampling distribution plots
leslieSim(type="resampling")

} # end if interactive
}
\seealso{
\code{\link{removal}}.
}
\keyword{iplot}

