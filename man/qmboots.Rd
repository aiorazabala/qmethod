\name{qmboots}
\alias{qmboots}
\title{Q Methodology: Bootstrap}
\description{Implementation of the bootstrap to Q methodology using PCA}
\usage{qmboots(dataset, nfactors, nsteps, load="auto",
        rotation="varimax", indet="qindtest", fsi=TRUE, 
        forced=T, distribution=NULL, ...)}

\arguments{
  \item{dataset}{a matrix or dataframe containing original data, with statements as rows, Q sorts as columns, and Q board column values in each cell.}
  \item{nfactors}{number of factors to extract using PCA.}
  \item{load}{a matrix of factor loadings to be used as target. If "auto", the target matrix is generated using the rotation indicated ("varimax" by default.)}
  \item{nsteps}{number of steps (iterations) for the bootstraping}
  \item{rotation}{rotation method for the PCA, set to "varimax" by default. Other possible rotations from \pkg{psych} \code{principal} function "none", "varimax", "quatimax", "promax", "oblimin", "simplimax", and "cluster" are possible.}
  \item{indet}{method to solve the double indeterminacy issue in PCA bootstrapping. \code{"procrustes"} for procrustes rotation, \code{"qindtest"} for simple solution valid for up to 3 factors extracted, \code{"both"} for a qindtest and a procrustes rotation, or \code{"none"} for no rotation. The latter is not recommended for it introduces inflated variability. If \code{"none"} is selected, each replication is rotated using varimax.}
 \item{fsi}{logical; Shall the Factor Stability index be calculated?}
 \item{forced}{logical; Is the ranking of the items forced to match the distributions? Set to \code{TRUE} if all respondents ranked the items strictly following the distribution scores, in which case the values of the distribution are calculated automatically. Set to \code{FALSE} if respondents were able to rank the items without following the distribution, and the values of the distribution have to be provided as an array in the argument \code{distribution}.}
  \item{distribution}{logical; when forced = \code{FALSE}, the distribution has to be provided as a vector of numbers, such as \code{c(-2, -1, -1, 0, 1, 1, 2, 2)}.}
  \item{...}{Other arguments for qmethod().}
}

\value{
\item{brief}{summary of the analysis.}
\item{loa}{factor loadings for Q-sorts.}
\item{flagged}{binary dataframe of flagged Q-sorts.}
\item{zsc}{statements' z-scores.}
\item{zsc_n}{statements' factor scores, normalized to the values in the first row of the original dataset.}
\item{f_char}{factor characteristics. See details in \code{\link{qfcharact}}.}
}
\references{Brown, S. R., 1980 \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press.}

\author{Aiora Zabala}
\note{This is a preliminary version of the documentation. The section of 'values' above needs to be corrected.}

\seealso{
\code{\link{qmethod}}
}
\examples{
data(lipset)
boots <- qmboots(lipset[[1]], nfactors=3, nsteps=10, load="auto",
           rotation="varimax", indet="procrustes",
           fsi=TRUE)
boots
boxplot(t(boots[[2]][[1]][[2]]), horizontal=TRUE, 
main="Statement z-score boxplot for the first factor", las=1)
}
\keyword{multivariate}
\keyword{Q methodology}
\keyword{bootstrapping}
\keyword{PCA}