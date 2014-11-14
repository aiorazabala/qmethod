\name{import.pqmethod}
\alias{import.pqmethod}
\title{Q methodology: import PQMethod *.DAT files}
\description{Imports data from *.DAT files created in PQMethod software.}
\usage{import.pqmethod(file, ...)}

\arguments{
  \item{file}{a file with extension *.DAT (see full description of the file below in References).}
  \item{...}{further arguments to be passed to \code{\link[utils]{read.table}} and \code{\link[utils]{read.fwf}}.}
}
\details{
Extracts the raw data of a Q methodology study from the native format used in PQMethod. Returns a data frame with statements as rows and Q-sorts as columns.

If the following error occurs: \code{"invalid multibyte string"}, a possible solution is to either set the right file encoding with the argument \code{fileEncoding} or inspect the file for uncommon characters (see details in \code{\link[utils]{read.table}}).
}

\references{Schmolck, Peter, 2014. \emph{PQMethod Software}, Available at: \url{http://schmolck.org/qmethod/}

File descriptions in \emph{PQMethod Manual}: \url{http://schmolck.org/qmethod/pqmanual.htm#appdxa}}

\author{Aiora Zabala}

\examples{
data(lipset)
results <- qmethod(lipset[[1]], nfactors=3, rotation="varimax")
export.qm(results, "lipset_R.txt")
export.qm(results, "lipset_PQMethod.txt", style="PQMethod")
}