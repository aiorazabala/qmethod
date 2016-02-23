\name{qbstep}
\alias{qbstep}
\title{Q Methodology: Single step for the bootstrap}
\description{Bootstraping of Q methodology using PCA.}
\usage{qbstep(subdata, subtarget, indet, 
       nfactors, nqsorts, nstat, 
       qmts = qmts, qmts_log = qmts_log, 
       rotation = "unknown", 
       flagged = flagged, ...)}

\arguments{
  \item{subdata}{resampled dataset of Q-sorts.}
  \item{subtarget}{target matrix, adapted to match the rows of the resampled dataset.}
  \item{indet}{method to solve the double indeterminacy issue when bootstrapping Principal Components Analysis (PCA). \code{"procrustes"} for procrustes rotation from \pkg{GPArotation}, \code{"qindtest"} for simple solution valid for at least up to 3 factors extracted (see references), \code{"both"} for a \emph{\code{"qindtest"}} and a \emph{\code{"procrustes"}} rotation, or \code{"none"} for no solution. The latter is not recommended because it introduces inflated variability. If \code{"none"} is selected, each replication is rotated using the method set in \code{rotation}.}
  \item{nfactors}{number of factors in the study.}
  \item{nqsorts}{number of Q-sorts in the study.}
  \item{nstat}{number of statements in the study.}
  \item{qmts}{data frame with two rows and at least one column. This is automatically created when this function is called from \code{\link{qmboots}}(see \emph{Note} below).}
  \item{qmts_log}{data frame with two rows and at least one column. This is automatically created when this function is called from \code{\link{qmboots}}(see \emph{Note} below).}
  \item{rotation}{rotation method, defaults to \code{"none"}.}
  \item{flagged}{matrix or data frame of \code{nqsorts} rows and \code{nfactors} columns, with \code{TRUE} values for the Q-sorts that are flagged. Automatic flagging can be aplied using \code{\link{qflag}}. Manual flagging can be done by providing a logical matrix with \code{nqsorts} rows and \code{nfactors} columns to the argument \code{flagged}.}
  \item{...}{other arguments to be passed on to \code{\link{qzscores}} or to \code{\link[psych]{principal}}.}
}

\value{
\item{step_res}{summary of the analysis.}
}

\details{
This function performs a single step within a bootstrap of Q methodology data. It takes one resample, performs the Q method analysis, checks for indeterminacy issues, and corrects them if necessary by calling the function \code{\link{qindtest}} or \code{\link{qpcrustes}}.
}

\author{Aiora Zabala}

\note{this function is called within the function \code{\link{qmboots}}. Not intended to be used separately.}

\seealso{
\code{\link{qmethod}} and \code{\link{qmboots}} in this package.
}

\references{Zabala, Pascual (2016) Bootstrapping Q Methodology to Improve the Understanding of Human Perspectives. PLoS ONE 11(2): e0148087.}

\keyword{multivariate}
\keyword{Q methodology}
\keyword{bootstrapping}
\keyword{PCA}
