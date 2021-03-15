\name{export.qm}
\alias{export.qm}
\title{Q Methodology: export results to a plain text document}
\description{Exports an object of class \code{QmethodRes} to a plain text file (*.TXT). All the objects within the list resulting from \code{\link{qmethod}} are exported as they are. This is intended for interpretation rather than for further analysis.}
\usage{export.qm(qmobject, file, style= c("R", "PQMethod"))}

\arguments{
  \item{qmobject}{an object of Q methodology results, obtained from the function \code{\link{qmethod}}.}
  \item{file}{the file name. Note that in some operating systems, the file name will need an extension *.TXT so that other software opens it.}
  \item{style}{the structure and formatting of the results in the exported document. Defaults to \code{"R"} where the \code{qmobject} will be written as is. Option \code{"PQMethod"} provides an output with similar structure and elements as those provided by PQMethod software in the *.LIS files (see details of *.LIS files in the References below). Note that the latter creates a much longer document.}
}

\references{Schmolck. \emph{PQMethod Software (Version 2.35)}, 2014. \url{http://schmolck.org/qmethod/}

File descriptions in \emph{PQMethod Manual}: \url{http://schmolck.org/qmethod/pqmanual.htm#appdxa}
}

\author{Aiora Zabala}