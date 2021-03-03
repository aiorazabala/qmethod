\name{export.pqmethod}
\alias{export.pqmethod}
\title{Q methodology: export to PQMethod *.DAT and *.STA files}
\description{Exports Q data to *.DAT and *.STA files readable in PQMethod software.}
\usage{export.pqmethod(dataset, study.name, 
     study.description, col.range, 
     filename='Q_data_forPQmethod', 
     left.zeros, right.zeros, statements)}

\arguments{
  \item{dataset}{a matrix or data frame qwith Q data: Q-sorts as columns and statements as rows. The names of the columns will be used as Q-sort IDs in the *.DAT file.}
   \item{study.name}{a string with a short name of the study. No space characters are allowed.}
  \item{study.description}{a string with a one-sentence description of the study).}
  \item{col.range}{a two-element numerical vector with the values at the two extremes of the Q distribution (e.g. \code{c(-4, 4)}).}
  \item{filename}{a filename. The extension *.DAT is added automatically).}
  \item{left.zeros}{number of zeros before the distribution, in the second line of *.DAT file.}
  \item{right.zeros}{number of zeros after the distribution, in the second line of *.DAT file).}
  \item{statements}{a matrix with statements, one in each row).}
}
\details{
Exports the raw data of a Q methodology study into the native format used in PQMethod. Returns a message with some basic information about the data.

Note that \bold{no checks are made on the data}, such as whether there are duplicated or non-forced Q-sorts.

This function is not applicable to non-forced distributions.

}

\references{Schmolck, Peter, 2014. \emph{PQMethod Software}, Available at: \url{http://schmolck.org/qmethod/}

File descriptions in \emph{PQMethod Manual}: \url{http://schmolck.org/qmethod/pqmanual.htm#appdxa}}

\author{Aiora Zabala}

\note{This function is experimental. Use with caution and verify that the output is as desired.}

\examples{
# data(lipset)
# db <- lipset[[1]]
# export.pqmethod(dataset = db, 
#                study.name = 'mystudy', 
#                study.description = 'great study', 
#                col.range = c(-4, 4), 
#                filename = 'mystudy',
#                statements=lipset[[2]])
}
