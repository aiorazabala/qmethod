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
  \item{qmts}{data frame with two rows and at least one column.}
  \item{qmts_log}{data frame with two rows and at least one column.}
  \item{rotation}{rotation method, defaults to \code{"varimax"}. Other possible rotations allowed in \code{\link[psych]{principal}} function can be used: \code{"none"}, \code{"varimax"}, \code{"quartimax"}, \code{"promax"}, \code{"oblimin"}, \code{"simplimax"}, and \code{"cluster"}.}
  \item{flagged}{matrix or data frame of \code{nqsorts} rows and \code{nfactors} columns, with \code{TRUE} values for the Q-sorts that are flagged. Automatic flagging can be aplied using \code{\link{qflag}}. Manual flagging can be done by providing a logical matrix with \code{nqsorts} rows and \code{nfactors} columns to the argument \code{flagged}.}
  \item{...}{.}
}

\value{
\item{step_res}{Summary of the analysis}
}

\details{
This function performs a single step within a bootstrap of Q methodology data. It takes one resample, performs the Q method analysis, checks for indeterminacy issues, and corrects them if necessary by calling the function \code{\link{qindtest}} or \code{\link{qpcrustes}}.
}

\author{Aiora Zabala}
\note{This function is experimental, it is called by \code{\link{qmboots}} and is not intended to be used separately from it.
}

\seealso{
\code{\link{qmethod}} and \code{\link{qmethod}} in this package.
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