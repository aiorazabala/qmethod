\name{print.QmethodRes}
\alias{print.QmethodRes}
\title{Q Method: print method for results}
\description{Takes an object \code{QmethodRes} resulting from \code{\link{qmethod}} and prints it in a synthetic way.}
\usage{
\method{print}{QmethodRes}(x, length = 10, digits = 2, ...)}
\arguments{
  \item{x}{an object of class \code{QmethodRes}.}
  \item{length}{maximum number of rows to print from the data frames within \code{QmethodRes}. Defaults to 10. Set to \code{NULL} to see the full results.}
  \item{digits}{minimum number of significant digits, see \code{\link[base]{print.default}.}}
  \item{...}{further arguments passed to or from other methods.}
  }

\author{Aiora Zabala}

\examples{
data(lipset)
results <- qmethod(lipset[[1]], nfactors = 3, rotation = "varimax")
print(results, length = 5, digits = 1)
}
\keyword{print}