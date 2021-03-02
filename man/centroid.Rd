\name{centroid}
\alias{centroid}
\title{Q methodology: centroid extraction}
\description{Extracts factors/ components using the centroid approach as an alternative to Principal Components Analysis.}
\usage{centroid(tmat, nfactors = 7, spc)}

\arguments{
  \item{tmat}{a correlation matrix between Q-sorts.}
  \item{nfactors}{number of factors/ components to extract. Defaults to 7.}
  \item{spc}{the threshold to accept factor results, set to 0.00001 by default (in Brown 1980, this is set to 0.02).}
}

\details{This functions implement the centroid method for extraction of factors, an alternative to Principal Components that can be used in Q methodology. The calculations are based in Brown (1980; below). 

The function is called from within \code{\link{qmethod}} where the attribute \code{extraction} is set to \code{centroid}.

This function can be used independently where conducting each step of the analysis separately, preceded by a correlation between Q-sorts and followed by the rotation of factors/ components (see below), calculation of z-scores, etc.
}
\value{
Returns a matrix with Q-sorts as rows, and rotated factors as columns.
}
\references{Brown, S. R., 1980 \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press, pages 208-224.

See further references on the methodology in \code{\link{qmethod-package}}.}

\note{This is a function used within \code{\link{qmethod}}. Rarely to be used independently.}

\author{Frans Hermans}

\examples{
#### Example
require('qmethod')
require ("psych")

# Load data
data("lipset")
lip <- lipset[[1]]

# Correlation matrix
corlip <-cor(lip)

# Centroid extraction
lipcent <- centroid(corlip)
lipcent


## To finalise the full analysis, continue with the following steps
# Rotation (in this example, varimax over 3 factors)
vmax <- varimax(lipcent[,1:3])

# Automatic pre-flagging of Q-sorts
flags <- qflag(unclass(vmax$loadings), nstat = 33)

# Calculate z-scores and general characeristics
results <- qzscores(lip, 3, loa=vmax$loadings, flagged=flags)
summary(results)

# Consensus and distinguishing statements
results$qdc <- qdc(lip, 3, zsc=results$zsc, sed=results$f_char$sd_dif)

plot(results)

## All of the above can be done with:
results2 <- qmethod(lip, 3, extraction="centroid")
}