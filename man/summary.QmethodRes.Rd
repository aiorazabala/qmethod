\name{summary.QmethodRes}
\alias{summary.QmethodRes}
\title{Q methodology: summary for class 'QmethodRes'}
\description{Shows a summary of the results of Q methodology from the \code{\link{qmethod}} function: factor scores and factor characteristics.}
\usage{
\method{summary}{QmethodRes}(object, ...)
}

\arguments{
  \item{object}{an object of class \code{QmethodRes} created after \code{\link{qmethod}} function.}
  \item{...}{any other argument for the \code{\link{summary}} function.}
}
\value{
Returns the summary of the analysis:
\itemize{
\item Statements factor scores normalized to the values in the first row of the original dataset, and 
\item Factor characteristics: Average reliability coefficient, Number of loading Q-sorts, Eigenvalues, Percentage of explained variance, Composite reliability, Standard error of factor scores, Correlation coefficients between factors z-scores, Standard errors of differences
}
}
\references{Brown, S. R., 1980 \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press.}

\author{Aiora Zabala}

\seealso{
\code{\link{qmethod}} in this package
}
\examples{
data(lipset)
results <- qmethod(lipset[[1]], nfactors = 3, rotation = "varimax")
summary(results)
}