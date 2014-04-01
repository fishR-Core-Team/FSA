\name{fishR}
\alias{fishR}
\title{Opens web pages associated with the fishR site.}
\usage{
fishR(where = c("home", "general", "books", "AIFFD", "news"))
}
\arguments{
  \item{where}{A string indicating a particular page on the
  fishR site to open.}
}
\value{
None, but a webpage will be opened in the default browswer.
}
\description{
Opens web pages associated with the fishR site.
}
\examples{
\dontrun{
fishR()            # home page
fishR("general")   # general examples page
fishR("books")     # books examples page
fishR("AIFFD")     # Analysis & Interpretation of Freshwater Fisheries Data page
fishR("news")      # News page
}
}
\keyword{misc}

