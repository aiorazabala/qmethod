\name{qmboots}
\alias{qmboots}
\title{Q Methodology: Bootstrap}
\description{Implementation of the bootstrap to Q methodology using Principal Components Analysis (PCA).}
\usage{qmboots(dataset, nfactors, nsteps, load = "auto",
        rotation = "varimax", indet = "qindtest", fsi = TRUE, 
        forced = T, distribution = NULL, 
        cor.method="pearson", ...)}

\arguments{
  \item{dataset}{a matrix or dataframe containing original data, with statements as rows, Q sorts as columns, and Q board column values in each cell.}
  \item{nfactors}{number of factors to extract using PCA.}
  \item{load}{a matrix of factor loadings to be used as target. If "auto", the target matrix is generated using the rotation indicated ("varimax" by default).}
  \item{nsteps}{number of steps (repetitions) for the bootstraping.}
  \item{rotation}{rotation method, set to "varimax" by default. Other possible rotations from \pkg{psych} \code{\link[psych]{principal}} function "none", "varimax", "quatimax", "promax", "oblimin", "simplimax", and "cluster" are possible.}
  \item{indet}{method to solve the double indeterminacy issue in PCA bootstrapping. \code{"procrustes"} for procrustes rotation, \code{"qindtest"} for simple solution valid for up to 3 factors extracted, \code{"both"} for a qindtest and a procrustes rotation, or \code{"none"} for no rotation. The latter is not recommended for it introduces inflated variability. If \code{"none"} is selected, each replication is rotated using varimax.}
 \item{fsi}{logical; Shall the Factor Stability index be calculated? (experimental index).}
 \item{forced}{logical; Is the ranking of the items forced to match the distributions? Set to \code{TRUE} if all respondents ranked the items strictly following the distribution scores, in which case the values of the distribution are calculated automatically. Set to \code{FALSE} if respondents had the possibility to rank the items without following the distribution, and the values of the distribution have to be provided as an array in the argument \code{distribution}.}
  \item{distribution}{logical; when forced = \code{FALSE}, the distribution has to be provided as a vector of numbers, such as \code{c(-2, -1, -1, 0, 1, 1, 2, 2)}.}
  \item{cor.method}{character string indicating which correlation coefficient is to be computed, to be passed on to the function \code{\link[stats]{cor}}: \code{"pearson"} (default), \code{"kendall"}, or \code{"spearman"}. }
  \item{...}{Other arguments passed on to \code{\link{qmethod}}.}
}

\value{
\item{zscore-stats}{summary of the analysis. List of one object, plus as many objects as factors extracted: the bootstrapped factor scores, and the z-score statistics of the bootrstrap. The z-score statistics of interest are \code{mean} (the bootstrap estimate of the z-score), and \code{sd} (the bootstrap estimate of the SE).}
\item{full.bts.res}{full bootstrap results. List with as many objects as factors extracted, each object containing three data frames: \code{flagged}, \code{zsc} and \code{loa}. These data frames have as many columns as bootstrap steps, and contain the results of the analysis of each iteration. See description of these three data frames in \code{\link{qmethod}}.}
\item{indet.tests}{indeterminacy tests.}
\item{resamples}{index of the Q-sorts selected for each step.}
\item{orig.res}{original results. See details of all the objects in \code{\link{qmethod}}.}
\item{q.array}{array of values in the distribution grid.}
\item{loa.stats}{statistics of factor loadings. List with as many objects as factors extracted, each object containing one data frame with the factor loading statistics of the bootrstrap. The factor loading statistics of interest are \code{mean} (the bootstrap estimate of the factor loading), and \code{sd} (the bootstrap estimate of the SE). This table includes \code{flag_freq}, which indicates the frequency with which the given Q-sort was flagged for the given factor.}
\item{q.array}{array of values in the distribution grid.}
\item{fsi}{factor stability index (optional; experimental).}
}
\references{Zabala, Pascual (2016) Bootstrapping Q Methodology to Improve the Understanding of Human Perspectives. PLoS ONE 11(2): e0148087.}

\author{Aiora Zabala}

\seealso{
\code{\link{qmethod}}
}
\examples{
data(lipset)
boots <- qmboots(lipset[[1]], nfactors = 3, nsteps = 10, load = "auto",
           rotation = "varimax", indet = "qindtest",
           fsi = TRUE)
boots
boxplot(t(boots[[2]][[1]][[2]]), horizontal = TRUE, 
main = "Statement z-score boxplot for the first factor", las = 1)

#See the table summaries:
qms <- qmb.summary(boots)
round(qms$statements, digits=2) # statements
round(qms$qsorts, digits=2)     # Q-sorts

# A more synthetic visualisation:
# z-scores:
qmb.plot(qms, nfactors=3, type="zsc", sort=TRUE)
# factor loadings:
qmb.plot(qms, nfactors=3, type="loa", sort=TRUE)


}
\keyword{multivariate}
\keyword{Q methodology}
\keyword{bootstrapping}
\keyword{PCA}
