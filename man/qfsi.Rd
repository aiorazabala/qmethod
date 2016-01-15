\name{qfsi}
\alias{qfsi}
\title{Q Methodology: Factor Stability index}
\description{Calculates a Factor Stability index and a Normalised Factor Stability index to bootstrapped Q method results (experimental).}
\usage{qfsi(nfactors, nstat, qscores, zsc_bn, qm)}

\arguments{
  \item{nfactors}{number of factors to extract.}
  \item{nstat}{number of statements in the study.}
  \item{qscores}{all possible factor score values in the Q grid distribution.}
  \item{zsc_bn}{bootstrapped factor scores.}
  \item{qm}{original Q method results from \code{\link{qmethod}} function.}
}
\details{
Applies the Factor Stability index to a bootstrapped Q method results. Returns a data frame with two variables and as many rows as factors extracted. The first variable is the raw Factor Stability index. The second variable is the Normalised Factor Stability index which ranges from 0 to 1.
}

\author{Aiora Zabala}

\seealso{
\code{\link{qmboots}}.
}
\note{IMPORTANT: This function is experimental. Please contact the author for details.}
\examples{
data(lipset)
boots <- qmboots(lipset[[1]], nfactors=3, nsteps=10, 
           rotation="varimax", indet="procrustes",
           fsi=FALSE)
fsi <- qfsi(nfactors=3, nstat=33, qscores=boots[[6]], 
           zsc_bn=boots[[1]][[1]], qm=boots[[5]])
fsi
}

\keyword{Q methodology}
