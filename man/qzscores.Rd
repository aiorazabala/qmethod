\name{qzscores}
\alias{qzscores}
\title{Q methodology: z-scores from loadings}
\description{Calculates factor characteristics, z-scores, and rounded scores, provided a matrix of loadings and a matrix of flagged Q-sorts.}
\usage{qzscores(dataset, nfactors, loa=loa,
        flagged=flagged, forced=TRUE, distribution=NA)}

\arguments{
  \item{dataset}{a matrix or a data frame containing raw data, with statements as rows, Q-sorts as columns, and the column scores in the distribution in each cell.}
  \item{nfactors}{number of factors to extract.}
  \item{loa}{matrix or data frame of \code{nqsorts} rows and \code{nfactors} columns, with values of factor loadings for Q-sorts, calculated using, e.g., \code{principal(...)$loadings}.}
  \item{flagged}{matrix or data frame of \code{nqsorts} rows and \code{nfactors} columns, with \code{TRUE} values for Q-sorts that are flagged. Automatic flagging can be aplied using \code{\link{qflag}} function. Manual flagging can be done by providing the corresponding matrix to \code{flagged}.}
  \item{forced}{logical; Is the distribution of items forced? Is the distribution of items forced? Set to \code{TRUE} if all respondents ranked the items following strictly the distribution scores, and the values of the distribution are calculated automatically. Set to \code{FALSE} if respondents were able to rank the items without following the distribution, and the values of the distribution have to be provided as an array in the argument \code{distribution}.}
  \item{distribution}{logical; when \code{forced = FALSE}, the distribution has to be provided as a vector of numbers, such as \code{c(-2, -1, -1, 0, 1, 1, 2, 2)}.}
}

\details{In order to implement manual flagging, use a manually created data frame for \code{flagged}. The loadings from \code{principal(...)$loadings} can be explored to decide upong flagging. The \code{loa} data frame should have Q-sorts as rows, and factors as columns, where \code{TRUE} are the flagged Q-sorts.}
\value{
\item{brief}{a list with the basic values of the analysis: date (\code{"date"}), number of statements (\code{"nstat"}), number of Q-sorts (\code{"nqsort"}), number of factors extracted (\code{"nfactors"}), type or rotation (\code{"rotation"}), and method for correlation in the PCA (\code{"cor.method"}).}
\item{rawdata}{original data.}
\item{loa}{factor loadings for Q-sorts.}
\item{flagged}{logical dataframe of flagged Q-sorts.}
\item{zsc}{statements z-scores.}
\item{zsc_n}{statements rounded scores, rounded to the values in the first row of the original dataset.}
\item{f_char}{factor characteristics obtained from \code{\link{qfcharact}}.}
\item{item_sd}{
  Population standard deviations of item scores \emph{across} all flagged q sorts for a given factor, weighted same as \code{zsc}.
  Used for visualization in \code{\link{array.viz}}
}
}
\references{Brown, S. R., 1980 \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press.

See further references on the methodology in \code{\link{qmethod-package}}.}

\note{This is a function used within \code{\link{qmethod}}. Rarely to be used independently.}

\author{Aiora Zabala}

\examples{
data(lipset)
library(psych)
loa <- as.data.frame(unclass(principal(lipset[[1]], 
                    nfactors=3, rotate="varimax")$loadings))
flagged <- qflag(nstat=33, loa=loa)
qmzsc <- qzscores(lipset[[1]], nfactors=3,
                    flagged=flagged, loa=loa)
qmzsc
}
