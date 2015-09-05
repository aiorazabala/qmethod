\name{make.distribution}
\alias{make.distribution}
\title{Q methodology: create Q normal distribution}
\description{
  Creates a distribution close to a standard normal distribution given a number of statements and a maximum Q sort rank.
}
\usage{
  make.distribution(
  	nstat,
  	max.bin = 5
  )
}

\arguments{
  \item{nstat}{
    Number of desired statements in a Q sample for a given study.
    Must be a positive integer, vector of length 1.
  }
  \item{max.bin}{
  	Maximum positive value to be entered by participants.
    Must be a positive integer, vector of length 1.
    Defaults to frequent value \code{5}.
  }
}
\details{
	Participants in Q studies are frequently asked to sort Q items under a quasi-normal distribution.
	This function generates such a Q distribution, given a number of statements \code{nstat} and a desired maximum positive value \code{max.bin} in the Q distribution.

  The function always returns an \emph{uneven} number of columns, ensuring that there is always a modal column at the zero mark.

  Not every combination of \code{nstat} and \code{max.bin} can be neatly fit under a standard normal distribution, in which case the function returns a vector of unequal length to the specified \code{nstat}.
  The function will issue a warning in that case.
  Researchers can either accept the different \code{nstat}, or try again with a different \code{max.bin}.
}

\value{
	Returns a vector of positive integers (column heights), of the length \code{max.column * 2 + 1}.
	An object of this kind is expected in \code{\link{import.q.sorts}}, \code{\link{build.q.set}} and other convenience functions.
}

\author{Maximilian Held}

\seealso{
	\code{\link{build.q.set}},
	\code{\link{import.q.sorts}}
}

\examples{
  ## Make Q distribution
  make.distribution(nstat=76, max.bin=7)
}
