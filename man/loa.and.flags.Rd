\name{loa.and.flags}
\alias{loa.and.flags}
\title{Q methodology: show factor loadings next to flags}
\description{Prints a table with factor loadings and flagged Q-sorts are indicated with a star.}
\usage{loa.and.flags(results, nload = FALSE)}

\arguments{
  \item{results}{an object of Q method results.}
  \item{nload}{logical; print number of flagged Q-sorts.}
}
\details{
Simple function to explore the Q-sorts that are automatically pre-flagged, using the standard criteria implemented in function \code{\link{qflag}}
}

\author{Aiora Zabala}

\examples{
data(lipset)
results <- qmethod(lipset[[1]], nfactors = 3, rotation = "varimax")
loa.and.flags(results)
}