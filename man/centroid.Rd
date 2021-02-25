\name{centroid}
\alias{centroid}
\title{Q methodology: centroid extraction}
\description{Extracts factors using the centroid approach.}
\usage{centroid(tmat, nfactors = 7, spc)}

\arguments{
  \item{tmat}{a correlation matrix between Q-sorts.}
  \item{nfactors}{number of factors/ components to extract.}
  \item{spc}{the threshold to accept factor results, set to 0.005 by default (in Brown 1980, this is set to 0.02).}
}

\details{In order to implement manual flagging, use a manually created data frame for \code{flagged}. See an example of code to perform manual flagging or to manipulate the loadings in \url{https://github.com/aiorazabala/qmethod/wiki/Advanced-analysis}.

The loadings from \code{principal(...)$loadings} can be explored to decide upon flagging. The \code{loa} data frame should have Q-sorts as rows, and factors as columns, where \code{TRUE} are the flagged Q-sorts.}
\value{
Returns a matrix with Q-sorts as rows, and rotated factors as columns
}
\references{Brown, S. R., 1980 \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press, pages 208-224.

See further references on the methodology in \code{\link{qmethod-package}}.}

\note{This is a function used within \code{\link{qmethod}}. Rarely to be used independently.}

\author{Frans Hermans}

\examples{
#### Example
require('qmethod')
require ("psych")

data("lipset")
lip <- lipset[[1]]
corlip <-cor(lip)

lipcent <- centroid(corlip)
lipcent

# varimax rotation over 3 factors
vmax <- varimax(lipcent[,1:3])
qflag (unclass(vmax$loadings), nstat = 33)}
