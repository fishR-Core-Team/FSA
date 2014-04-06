\name{mrClosed1Sim}
\alias{mrClosed1Sim}
\title{Dynamics plots to explore single-census mark-recapture models.}
\usage{
mrClosed1Sim(type = c("Petersen", "P", "Chapman", "C", "Ricker", "R",
  "Bailey", "B"), N = 1000, rsmpls = 500, incl.final = TRUE)
}
\arguments{
  \item{type}{A single string that identifies the type of
  calculation method to use (see details).}

  \item{N}{A single number that represents the
  \dQuote{known} size of the simulated population just
  prior to the first sample.}

  \item{rsmpls}{A single number that indicates the number
  of simulations to run.}

  \item{incl.final}{A single logical that indicates whether
  the mean final population size and histogram of percent
  error from the final population size should be shown.}
}
\value{
None.  However, a dynamic graphic is produced that is
controlled by slider bars as described in the details.  The
dynamic graphic is a histogram of the population estimate
from all resamples with a red vertical line at the initial
population size (provided by the user), a blue vertical
line at the population size just prior to the final sample
(N1; if \code{incl.final=TRUE}), and a green vertical line
at the mean population estimate.  The vertical blue and red
lines may not be visible under some scenarios because of
overprinting.
}
\description{
This function is used to simulate multiple results for
single census mark-recapture study.  The user can control
many aspects of the simulation including simulating the
loss of marks, differential survival rates for tagged and
untagged fish, the addition of recruits, and differential
probabilities of capture for tagged and untagged fish.  In
addition, Petersen, Chapman, and Bailey estimates of
population size can be made.
}
\details{
The user can use slider bars to choose values for the
expected number of fish to capture in the first (i.e., the
marking or tagging) sample, the expected number of fish to
be captured in the second or final (i.e., the recapture)
sample, a probablity that a tagged fish loses the tag, the
probability of survival for tagged fish, the probability of
survival for untagged fish, a proportion of the initial
population size to recruit to the population between the
first and final samples, and the ratio of the probability
of capture of tagged fish to the probability of capture of
untagged fish in the final sample.  The user selects the
\dQuote{true} initial population size, the method of
population estimator, and the number of resamples to use
with arguments to the function.  The simulation can be
re-run without changing any of the slider buttons by
pressing the \bold{\sQuote{Re-Randomize}} button.

In general, the simulation follows these steps: \enumerate{
\item a population of N fish is created; \item randomly
select M fish in the first sample to be tagged such that
the average of all M values should be equal to the
user-supplied \bold{\sQuote{Tagged (M)}} value; \item
randomly select m fish to lose tags according to the
user-supplied probability of tag loss (\bold{\sQuote{PR(Tag
Loss)}}); \item randomly select fish to die according to
the user-supplied probabilities of survival for tagged
(\bold{\sQuote{PR(Surv Tagged)}}) and untagged fish
(\bold{\sQuote{PR(Surv UNTagged)}}); \item add recruits to
the population in proportion to N and in accordance to the
user-supplied proportion of recruits
(\bold{\sQuote{Proportion Recruit}}) to add; \item identify
the actual population size just before taking the final
sample (N1); \item randomly select n fish in the second
sample such that the average of all n values should be
equal to the user-supplied \bold{\sQuote{Captured (n)}}
value (see note below about how differential probabilties
of capture are incorporated into the model); \item count
the number of tagged fish in the second sample; and \item
compute the population estimate using the chosen method
(see below). }

The methods for estimating the population size are

\tabular{ll}{ \code{type="P"} \tab naive Petersen.\cr
\code{type="C"} \tab Chapman(1951) modification of the
Petersen.\cr \code{type="CR"} \tab Ricker(1975)
modification of the Chapman modification.\cr
\code{type="B"} \tab Bailey(1951,1952) modification of the
Petersen. }

The effect of violating the assumption of tag loss is
simulated by changing the probability of tag loss slider to
a value greater than 0 but less than 1.  For example,
setting \bold{\sQuote{PR(Tag Loss)}} to 0.1 is used to
simulate a 10 percent probability of losing the tag.

The effect of violating the assumption of no mortality from
the first to second sample is simulated by changing one or
both of the probabilities of survival for tagged and
untagged fish to values less than 1 (but greater than 0).
For example, setting \bold{\sQuote{PR(Surv Tagged)}} AND
\bold{\sQuote{PR(Surv UNTagged)}} to 0.8 will simulate
mortality between the first and final sample but NOT
differential mortality between the tagged and untagged fish
(i.e., the mortalities are the same for both groups of
fish). The effect of differential mortalities between
tagged and untagged fish can be simulated by using
different survival probabilities for tagged and untagged
fish.

The effect of violating the assumption of no recruitment
from the first to final sample is simulated by changing the
Proportion Recruit slider to a value greater than 0.  For
example, setting \bold{\sQuote{Proportion Recruit}} to 0.1
will simulate 10 percent of N recruiting to the population
just before the final sample.

The effect of violating the assumption of equal
catchabilities in the final sample for tagged and untagged
fish is simulated by changing the \bold{\sQuote{Ratio
PR(Capture)}} slider to a value different than 1.  Values
greater than 1 indicate that the catchability of tagged
fish is greater than the catchability of untagged fish.
Values less than 1 indicate that the catchability of tagged
fish is less than that of untagged fish.  For example,
setting \bold{\sQuote{Ratio PR(Capture)}} to 0.8 will
simulate a situation where the capture probability of
tagged fish is 80 percent of the capture probablity of
untagged fish (i.e., simulates the situation where marked
fish are less likely to be captured).

The probability of capture in the final sample is equal to
the expected number of fish to be collected in the final
sample (set with \bold{\sQuote{Captured (n)}}) divided by
the actual population size just prior to the final sample
(N1) as long as \bold{\sQuote{Ratio PR(Capture)}} is 1.
The probabilities of capture for the tagged and untagged
fish are carefully adjusted if the \bold{\sQuote{Ratio
PR(Capture)}} value is different than 1.  Because of the
different numbers of tagged and untagged individuals in the
population, the probability of capture for tagged and
untagged individuals must be computed by adjusting the
overall probability of capture to assure that, on average,
the user-provided expected number captured in the final
sample is met.  This modification is found by solving the
following system of equations for the probabilities of
capture for the tagged (PM) and untagged fish (PU),
respectively,

\deqn{PM*M + PU*(N-M) = P*N}

\deqn{\frac{PM}{PU} = k}

where M is the number of tagged animals, and N-M is the
number of untagged animals, k is the \bold{\sQuote{Ratio
PR(Capture)}} value, and P is the overall probability of
capture if there was no difference in catchability between
tagged and untagged animals.  The solutions to this system,
which are used in this function, are

\deqn{PU = \frac{PN}{M+N}}

\deqn{PM = PU*k}
}
\examples{
if (interactive()) {

mrClosed1Sim()
mrClosed1Sim(type="C")  # use Chapman modification

} # end if interactive
}
\seealso{
\code{\link{mrClosed}}
}
\keyword{iplot}

