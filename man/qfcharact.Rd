\name{qfcharact}
\alias{qfcharact}
\title{Q methodology: factor characteristics}
\description{Calculates the general factor characteristics: number of flagged Q-sorts, composite reliability, standard errors of factor scores, and comparisons between factors.}
\usage{qfcharact(loa, flagged, zsc, nfactors, floa, av_rel_coef = 0.8)}

\arguments{
  \item{loa}{a matrix or a data frame containing raw data, with statements as rows, Q-sorts as columns, and the scores of the columns in the distribution in each cell.}
  \item{flagged}{a data frame of type \emph{logical}, indicating which Q-sorts are flagged for each factor. Provided manually or automatically using \code{\link{qflag}}.}
  \item{zsc}{a data frame with the z-scores for statements, calculated using \code{\link{qzscores}}.}
  \item{nfactors}{number of factors extracted.}
  \item{floa}{a data frame with the factor loadings, calculated using PCA or Centroid factor analysis.}
  \item{av_rel_coef}{average reliability coefficient (the individual variability of a respondent), estimated by default as 0.8.}
}

\value{
Returns a list with three objects:
\item{characteristics}{data frame with the following values for each factor:
\itemize{
        \item{\code{"av_rel_coef"}: average reliability coefficient.}
        \item{\code{"nload"}: number of loading Q-sorts.}
        \item{\code{"eigenvals"}: eigenvalues.}
        \item{\code{"expl_var"}: percentage of explained variance.}
        \item{\code{"reliability"}: composite reliability.}
        \item{\code{"se_fscores"}: standard error of factor scores (SE).}
        }
}
\item{cor_zsc}{matrix of correlation coefficients between factors z-scores.}
\item{sd_dif}{matrix of standard errors of differences (SED).}
}
\references{Brown, S. R., 1980 \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press.

See further references on the methodology in \code{\link{qmethod-package}}.}

\note{This is a function used within \code{\link{qmethod}}. Rarely to be used independently.}

\author{Aiora Zabala}