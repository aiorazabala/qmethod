\name{qdc.zsc}
\alias{qdc.zsc}
\title{Q methodology: distinguishing and consensus statements}
\description{Extracts the z-score of distinguishing statements, in order to plot.}
\usage{qdc.zsc(results)}

\arguments{
  \item{results}{an object of class \code{QmethodRes}.}
}

\note{This is a function used within \code{\link{plot.QmethodRes}}. Rarely to be used independently.}

\author{Aiora Zabala}

\examples{
data(lipset)
results <- qmethod(lipset[[1]], nfactors = 3, rotation = "varimax")
qdc.zsc(results)
}