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
\item{qsorts}{data frame with Q-sort as rows, and the following columns: the factor loadings from the standard analysis, the bootstrap, the bootstrap SE, the estimate of bias, and the frequency of flagging.}
\item{statements}{data frame with statements as rows, and the following columns: the z-scores from the standard analysis, from the bootstrap, bootstrap SE, estimate of bias of z-scores, factor scores from the standard analysis, from the bootstrap, estimate of bias of factor scores, distinguishing and consensus statements from the standard results (see \code{\link{qdc}}) and from the bootstrap values.}
}

\author{Aiora Zabala}

\references{Zabala, accepted. Bootstrapping Q Methodology to Improve the Understanding of Human Perspectives.}

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
