\name{q.corrplot}
\alias{q.corrplot}
\title{Q methodology: correlation plot}
\description{
	Produces a correlation plot from a given correlation matrix.
}
\usage{
	q.corrplot(corr.matrix)
}
\arguments{
  \item{corr.matrix}{
  a correlation matrix as produced by \code{\link[stats]{cor}}.
  }
}
\details{
	This is a helper function to visualize correlation matrices and residual correlation coefficients as part of \code{\link{q.nfactors}}.
	Rarely to be used independently.
}
\value{
	Returns a plot of the correlation matrix as a list of class \code{ggplot}.
}
\author{Maximilian Held}

\seealso{
	\code{\link{q.nfactors}}
}
\examples{
	data(lipset)
	q.corrplot(corr.matrix = cor(x = lipset$ldata, method = "spearman"))
}
