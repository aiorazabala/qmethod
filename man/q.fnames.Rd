\name{q.fnames}
\alias{q.fnames}
\title{Change factor names in the results of Q methodology analysis}
\description{This function replaces the automatic names created in an object of Q method results returned by \code{\link{qmethod}}.}
\usage{
q.fnames(results, fnames)}

\arguments{
  \item{results}{an object of class \code{QmethodRes}.}
  \item{fnames}{a vector with the names of the factors. The number of names provided has to match the number of factors extracted in the object \code{results}. The names cannot begin with a number. A limit of 50 characters is set, to avoid excessively wide columns. Names should ideally contain no spaces or symbols that are used for other purposes in R (e.g. '-', '+', '/' , ). However '.' are fine.}
}

\value{
Returns the object \code{results} of class \code{QmethodRes}, with the new factor names.
}

\author{Aiora Zabala}

\seealso{
\code{\link{qmethod}}
}
\examples{
data(lipset)
results <- qmethod(lipset[[1]], nfactors = 3, rotation = "varimax")
factor.names <- c("good", "bad", "ugly") 
results2 <- q.fnames(results, fnames = factor.names)
results #shows all results
}
