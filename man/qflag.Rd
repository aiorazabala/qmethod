\name{qflag}
\alias{qflag}
\title{Q methodology: automatic flagging of Q-sorts}
\description{Applies the two standard algorithms to pre-flag Q-sorts automatically, for posterior calculation of the statement scores.}
\usage{qflag(loa, nstat)}

\arguments{
  \item{loa}{a Q-sort factor loading matrix obtained, for example from\cr \code{unclass(\link[psych]{principal}(...)$loadings)}, or from \code{\link{qmethod}(...)$loa}.}
  \item{nstat}{number of statements in the study.}
}
\details{
These are the two standard criteria for automatic flagging used in Q method analysis:
\enumerate{
\item{Q-sorts which factor loading is higher than the threshold for p-value < 0.05, and}
\item{Q-sorts which square loading is higher than the sum of square loadings of the same Q-sort in all other factors.}
}
Returns a logical matrix with Q-sorts as rows, and factors as columns.
}

\references{Brown, S. R., 1980 \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press.

Van Exel, J., de Graaf, G., Rietveld, P., 2011. "'I can do perfectly well without a car!'" \emph{Transportation} 38, 383-407 (Page 388, footnote 8).

See further references on the methodology in \code{\link{qmethod-package}}.}

\note{This is a function used within \code{\link{qmethod}}. Rarely to be used independently.}

\author{Aiora Zabala}

\examples{
data(lipset)
library(psych)
loa <- as.data.frame(unclass(principal(lipset[[1]], nfactors = 3, 
                                       rotate = "varimax")$loadings))
flagged <- qflag(loa = loa, nstat = nrow(lipset[[1]]))
summary(flagged)
}