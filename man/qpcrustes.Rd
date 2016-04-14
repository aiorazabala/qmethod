\name{qpcrustes}
\alias{qpcrustes}
\title{Q Methodology: Procrustes rotation of loadings}
\description{This is a wrap of \code{procrustes} rotation from \pkg{GPArotation} for bootstrapping Q methodology in the function \code{\link{qmboots}}.}
\usage{qpcrustes(loa, target, nfactors)}

\arguments{
  \item{loa}{factor loadings from the analysis of a resample.}
  \item{target}{factor loadings from the analysis of a subsample.}
  \item{nfactors}{fumber of factors}
}

\details{Returns the factor loadings for the subsample after applying Procrustes rotation to correct the indeterminacy issue. Use \code{procrustes} from \pkg{GPArotation}. Used within the function \code{\link{qmboots}}, not intended for independent use.
}

\references{Zabala, Pascual (2016) Bootstrapping Q Methodology to Improve the Understanding of Human Perspectives. PLoS ONE 11(2): e0148087.}

\author{Aiora Zabala}

\note{this function is called within the function \code{\link{qmboots}}. Not intended to be used separately. The function calls \code{procrustes} from \pkg{GPArotation}, a package that requires the package \code{graph}. As from April 2016 the package has been moved to Bioconductor, and therefore it needs to be installed manually. If you get errors of missing packages when using this function or \code{\link{qmboots}}, install \code{graph} manually:
\code{source("https://bioconductor.org/biocLite.R")
biocLite("graph")}
}

\seealso{
Function \code{procrustes} from \pkg{GPArotation} package.
}
\examples{
# This example requires installing 'MCMCpack', check warning when running line 10 below:
data(lipset)
qm <- qmethod(lipset[[1]], nfactors=3, rotation="varimax")
qselim <- sample(1:3, 2, replace=FALSE) ##q sorts to eliminate
subdata <- lipset[[1]][ , -qselim]
library(psych)
loa <- as.data.frame(unclass(principal(subdata, 
         nfactors=3, rotate="varimax")$loadings))
target <- as.matrix(as.data.frame(qm[3]))
colnames(target) <- paste("target_f", 1:3, sep="")
subtarget <- target[c(-qselim),]
# qindt <- qpcrustes(loa, subtarget, 3)
# qindt
}

\keyword{multivariate}
\keyword{Q methodology}
\keyword{Procrustes rotation}
