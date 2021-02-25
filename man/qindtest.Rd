\name{qindtest}
\alias{qindtest}
\title{Q Methodology: PCA bootstrap indeterminacy tests}
\description{This is a simple test and implementation of the 'reordering-reflection' solution for the indeterminacy problem (alignment problem) when bootstrapping Principal Components Analysis (PCA) that causes factor order swaps and factor sign swaps.}
\usage{qindtest(loa, target, nfactors)}

\arguments{
  \item{loa}{data frame with factor loadings from the subsample analysis.}
  \item{target}{data frame with factor loadings from the full sample analysis, excluding qsorts that are not present in the bootstrap step.}
  \item{nfactors}{number of factors extracted.}
}

\details{This function tests whether there is any or both of the indeterminacy issues in bootstrapped PCA factor loading values. For testing, it looks at correlation coefficients between the target factor loadings and the bootstrapped factor loadings for each factor.

First, if \emph{factor swap} is detected (Is the absolute value of diagonal coefficients bigger than non-diagonal coefficients for the same factor?) and it is only between two factors, these are swaped. After, the test is again performed to ensure that there is no need for further swaps. If the test fails, then the original factor loadings are recovered and the failure is reported. If the need for factor swap is detected for 1, 3 or more factors, this is reported and left unresolved. This is because an algorithm to determine which factors should swap with which has not been implemented.

Second, \emph{sign swap} is tested for (Are all diagonal coefficients positive?). If it is detected, then the sign of factor loadings is shifted. This is not tested again afterwards, for it is given for granted that swaping signs will solve the issue.}
\value{
\item{qindtest}{returns a list with three data frames: the factor loadings of the corrected bootstrap step, results from order swap and sign swap tests, and report of errors.}
}

\note{this function is called within the function \code{\link{qmboots}}. Not intended to be used separately.}

\references{Zabala, Pascual (2016) Bootstrapping Q Methodology to Improve the Understanding of Human Perspectives. PLoS ONE 11(2): e0148087.

See also:

- Timmerman, M.E., Kiers, H. a L., Smilde, A.K., 2007. Estimating confidence intervals for principal component loadings: a comparison between the bootstrap and asymptotic results. The British journal of mathematical and statistical psychology 60, 295-314.

- Zhang, G., Preacher, K.J., Luo, S., 2010. Bootstrap Confidence Intervals for Ordinary Least Squares Factor Loadings and Correlations in Exploratory Factor Analysis. Multivariate Behavioral Research 45, 104-134.
}

\author{Aiora Zabala}

\examples{
data(lipset)
nf <- 3

# 1. Create target matrix
qm <- qmethod(lipset[[1]], nfactors = nf, rotation = "varimax")

# 2. Resample
qselim <- sample(1:3, 2, replace = FALSE) ##q sorts to eliminate
subdata <- lipset[[1]][ , -qselim]

# 3. Calculate factor loadings with the resample
library(psych)
loa <- as.data.frame(unclass(principal(subdata, 
           nfactors = nf, rotate = "varimax")$loadings))

# 4. Reorder target matrix 
target <- as.matrix(as.data.frame(qm[3]))
colnames(target) <- paste0("target_f", 1:nf)
subtarget <- target[c(-qselim),]

# 5. Apply test and solution for indeterminacy issue
qindt <- qindtest(loa, subtarget, nf)
qindt
}

\keyword{PCA}
\keyword{Q methodology}
\keyword{indeterminacy}
\keyword{bootstrapping}