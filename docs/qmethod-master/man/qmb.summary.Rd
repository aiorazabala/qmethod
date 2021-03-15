\name{qmb.summary}
\alias{qmb.summary}
\title{Q Methodology: Summary of bootstrap results}
\description{Summarises bootstrap results for Q-sorts and statements into two tables.}
\usage{qmb.summary(qmboots)}

\arguments{
  \item{qmboots}{an object of bootstrap results, as produced by \code{\link{qmboots}}.}
}

\value{
Returns a list with two data frames:
\item{qsorts}{data frame with Q-sort as rows, and the following columns: the factor loadings from the standard analysis (*.std), the bootstrap (*.loa), the bootstrap SE (*.SE), the frequency of flagging (*.freq*) and the estimate of bias (*.bias).}
\item{statements}{data frame with statements as rows, and the following columns: the z-scores from the standard analysis (*.std), from the bootstrap (*.bts), bootstrap SE (*.SE), estimate of bias of z-scores (*.bias), factor scores from the standard analysis (fsc_f*), from the bootstrap (fsc.bts.*), estimate of bias of factor scores, distinguishing and consensus statements from the standard results (see \code{\link{qdc}}) and from the bootstrap values.}
}

\author{Aiora Zabala}

\references{Zabala, Pascual (2016) Bootstrapping Q Methodology to Improve the Understanding of Human Perspectives. PLoS ONE 11(2): e0148087.}

\seealso{
\code{\link{qmethod}}, \code{\link{qmboots}}
}
\examples{
data(lipset)
boots <- qmboots(lipset[[1]], nfactors = 3, nsteps = 50, 
                 load = "auto", rotation = "varimax",
                 indet = "qindet", fsi = TRUE)

boots.summary <- qmb.summary(boots)

# First rows of the summary for Q-sorts:
head(boots.summary$qsorts)

# First rows of the summary for statements:
head(boots.summary$statements)
}
\keyword{Q methodology}
\keyword{bootstrapping}
\keyword{summary}
