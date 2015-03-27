\name{qdc}
\alias{qdc}
\title{Q methodology: distinguishing and consensus statements}
\description{Indicates the distinguishing and consensus statements. It does so by comparing the z-scores between each pair factors.}
\usage{qdc(dataset, nfactors, zsc, sed)}

\arguments{
  \item{dataset}{a matrix or a dataframe containing original data, with statements as rows, Q sorts as columns, and grid column values in each cell.}
  \item{nfactors}{number of factors extracted.}
  \item{zsc}{a matrix with the factor z-scores for statements resulting from \code{\link{qzscores}}.}
  \item{sed}{a matrix with the standard error of differences resulting from \code{\link{qfcharact}}.}
}

\details{
	Finds the distinguishing and consensus statements, based on the absolute differences between factor z-scores being larger than the standard error of differences (SED, calculated in \code{\link{qfcharact}}) for a given pair of factors.

	Differences that are significant at a p-value < .05 are indicated with \code{'*'}, and differences significant at a p-value < .01 are indicated with \code{'**'}.

	Returns a single data frame with the differences in z-scores between each pair of factors and the variable \code{dist.and.cons}, indicating whether each statement is distinguishing or consensus and for which factor(s) it is distinguishing. These are the possible categories in the \code{dist.and.cons} variable:
	\itemize{
		\item Where all the comparisons between each pair of factors are significantly different at p-value < .05 the statement is labelled as \code{"Distinguishes all"}.
		\item Where the comparisons of a given factor with all other factors are significant at p-value < .05, and comparisons between all other factors are not significant, the statement is labeled as \code{"Distinguishes f*"}.
		\item Where none of the comparisons are significantly different, the statement is labeled as \code{"Consensus"}.
		\item Statements that have category \code{""} (empty) are not distinguishing for any of the factors in particularly. They distinguish one or more pairs of factors and the star indications may be inspected to understand their role.
	}
}

\references{Brown, S. R., 1980 \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press.

See further references on the methodology in \code{\link{qmethod-package}}.}

\note{This is a function used within \code{\link{qmethod}}. Rarely to be used independently.}

\author{Aiora Zabala}

\examples{
data(lipset)
results <- qmethod(lipset[[1]], nfactors = 3, rotation = "varimax")
sed <- as.data.frame(results[[7]][[3]])
zsc <- results[[5]]
qdc(lipset[[1]], nfactors = 3, zsc = zsc, sed = sed)
}