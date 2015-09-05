\name{q.corrplot}
\alias{q.corrplot}
\title{Q methodology: creates correlation matrix heatmap plot}
\description{
	Produces a heatmap from a given correlation matrix.
}
\usage{
	q.corrplot(corr.matrix, quietly = FALSE)
}
\arguments{
  \item{corr.matrix}{
  a correlation matrix as produced by \code{\link[stats]{cor}}.
  }
  \item{quietly}{
    Logical, indicating whether a summary plot should be printed.
    Defaults to \code{FALSE}.
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
