\name{qindtest}
\alias{qindtest}
\title{Q Methodology: PCA bootstrap indeterminacy tests}
\description{This is a simple test and solution for the indeterminacy problem (alignment problem) in PCA bootstrapping that causes factor order swaps and factor sign swaps.}
\usage{qindtest(loa, target, nfactors)}

\arguments{
  \item{loa}{data frame with factor loadings from subsample analysis.}
  \item{target}{data frame with factor loadings from full sample analysis, excluding qsorts that are not present in the bootstrap step.}
  \item{nfactors}{number of factors extracted.}
}

\details{This function tests whether there is any or both of the indeterminacy issues in bootstrapped PCA factor loading values, using correlation coefficients between the target factor loadings and the bootstrapped factor loadings for each factor.
First, if \emph{factor swap} is detected (Is the absolute value of diagonal coefficients bigger than non-diagonal coefficients for the same factor?) and it is only between two factors, these are swaped. After, the test is again performed to ensure that the swap had the desired effect, if the test fails, then the original factor loadings are recovered and the failure is reported. If the factor swap is detected for 1, 3 or more factors, this is reported and left unresolved, for it is yet to develop a method to determine which factors should swap with which. Second, \emph{sign swap} is tested for (Are all diagonal coefficients positive?), if it is detected, the sign of factor loadings is shifted. This is not tested, for it is given for granted that swaping signs will solve the issue.}
\value{
\item{qindtest}{Returns a list with three data frames: the factor loadings of the corrected bootstrap step, results from order swap and sign swap tests, and report of errors}
}

\author{Aiora Zabala}

\examples{
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
qindt <- qindtest(loa, subtarget, 3)
qindt
}

\keyword{PCA}
\keyword{Q methodology}
\keyword{indeterminacy}
\keyword{bootstrapping}