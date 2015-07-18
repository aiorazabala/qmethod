\name{q.compplot}
\alias{q.compplot}
\title{Q methodology: a factor composition plot.}
\description{
	Produces a factor composition plot, with the squared loadings of every Q-sort on all extracted factors.
}

\usage{
	q.compplot(results, quietly = FALSE)
}

\arguments{
  \item{results}{
    A results object from \code{\link{qmethod}}.
  }
  \item{quietly}{
    Logical, indicating whether a summary plot should be printed.
    Defaults to \code{FALSE}.
  }
}

\details{
	During rotation procedures and factor interpretation, Q researchers may wish to inspect how much the variance of each Q-sort is explained by each retained factor.
	Steven Brown (1980: 234) suggested a factor composition plot for this purpose, of which a simpler variant is produced by this function.
}

\value{
	Returns a factor composition plot as a list of class \code{ggplot}.
}

\author{Maximilian Held}

\references{
  Brown, S. R., 1980: \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press.
}

\note{
  This plot uses colors to identify the different factors, even if no colors are defined in \code{results}, as created by \code{\link{q.fcolors}}.
  These colors use a different color scheme than other default colors in this package, but may still overlap with other plots produced by this package.
  Similar colors for factors do not indicate substantive similarity, especially when the extraction and rotation procedure has changed.

  If colors \emph{are} defined in \code{results}, as created by \code{\link{q.fcolors}}, they are used in this plot, too.
}

\seealso{
	\code{\link{q.loaplot}}
	\code{\link{qmethod}}
}

\examples{
  data(lipset)  # use data included with this package
  results <- qmethod(lipset[[1]], nfactors=3, rotation="varimax")  # run analysis
  q.compplot(results = results)
}
