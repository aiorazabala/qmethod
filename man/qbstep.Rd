\name{qbstep}
\alias{qbstep}
\title{Q Methodology: Single step for the bootstrap}
\description{Bootstraping of Q methodology using PCA.}
\usage{qbstep(subdata, subtarget, indet, nfactors, nqsorts, nstat, 
              qmts=qmts, qmts_log=qmts_log, rotation="unknown", 
              flagged=flagged, ...)}

\arguments{
  \item{subdata}{resampled dataset.}
  \item{subtarget}{target matrix adapted to the rows of the resampled dataset.}
  \item{indet}{method to solve the double indeterminacy issue in PCA bootstrapping. \code{'procrustes'} for procrustes rotation, \code{qindtest} for simple solution valid for up to 3 factors extracted, \code{both} for a \emph{qindtest} and a \emph{procrustes} rotation, or \code{'none'} for no rotation. The latter is not recommended for it introduces inflated variability. If \code{'none'} is selected, each replication is rotated using varimax.}
  \item{nfactors}{number of factors in the study.}
  \item{nqsorts}{number of Q-sorts in the study.}
  \item{nstat}{number of statements in the study.}
  \item{qmts}{number of steps (iterations) for the bootstraping}
  \item{qmts_log}{.}
  \item{rotation}{.}
  \item{flagged}{.}
  \item{...}{.}
}

\value{
\item{step_res}{Summary of the analysis}
}
\references{Brown, S. R., 1980 \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press.}

\author{Aiora Zabala}
\note{This is a function used within \code{\link{qmboots}}. Not intended to be used independently.}

\seealso{
\code{\link{qmethod}} in this package
}
\examples{
data(lipset)
boots <- qmboots(lipset[[1]], nfactors=3, nsteps=10, load="auto",
         rotation="varimax", indet="procrustes",
         fsi=TRUE)
boots
boxplot(t(boots[[2]][[1]][[2]]), horizontal=TRUE, 
        main="Statement z-score boxplot for the first factor",
        las=1)
}
\keyword{multivariate}
\keyword{Q methodology}
\keyword{bootstrapping}
\keyword{PCA}