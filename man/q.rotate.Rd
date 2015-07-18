\name{q.rotate}
\alias{q.rotate}
\title{Q methodology: rotate factor loadings by hand}
\description{
  Lets user iterate over arbitrary rotation angles for each combination of factor pairs.
  Runs interactively from the console, produces informative plots.
}

\usage{
  q.rotate(results, plot.type = "q.rotplot", plot.all = TRUE, cache = FALSE)
}

\arguments{
  \item{results}{
    A results object from \code{\link{qmethod}}.
  }
  \item{plot.type}{
  	A character vector of length one, identifying the plotting function.
  	Must be either \code{\link{q.rotplot}}, \code{\link{q.loaplot}} or \code{"base"} for faster and simpler graphics in base R \code{\link[graphics]{plot}}.
  	Defaults to \code{"q.rotplot"}}.
  \item{plot.all}{
  	Logical, indicating whether plots should include all factor pairs during the iterative rotation.
  }
  \item{cache}{
  	Logical, indicating whether a cache of all possible 360 integer rotations should be built for any given factor pair to improve the responsiveness of the program.
  	Defaults to \code{FALSE}.
  }
}

\details{
  Q methodologists often prefer by-hand (or manual or judgmental) rotations over automatic procedures.
  This function allows users to rotate any pair of factors by any angle and provides informative plots for any rotation.
  Users are guided through an iterative rotation procedure by a command line interface in the R console.
  Arbitrary angles in degrees can be entered.
  Positive (negative) angles rotate clockwise (counterclockwise).

  Users can abort the rotation procedure at any time.

  The rotation is provided by \code{link[psych]{factor.rotate}}.

  Recall that any rotation will change the factor scores on \emph{all} factors (not merely the current factor pair), and may require a new factor interpretation.
  To aid in such an iterative process of rotation and interpretation, this function not provides rotated \emph{loadings}, but also calculates and plots resultant \emph{factor scores} by re-running \code{\link{qmethod}}.

  Recall further that any given completed Q analysis and factor interpretation depends on a \emph{particular} rotation, which cannot be easily inferred from the factor loadings.
  To be able to document and reproduce a by-hand rotation, this function appends a dataframe to the results object of class \code{QmethodRes} under \code{results$brief$rotation.angles}, with rows as factor pairs, and original and rotated angles as columns.
  On completion, \code{results$brief$rotation} is also changed to \code{"by-hand"} to reflect the manual procedure.

  Any given rotated solution, including \code{rotation = "varimax"}, \code{rotation = "none"} and others from \code{\link{qmethod}} can be used as a baseline for by-hand rotation.
  Notice that \emph{non-orthogonal} baseline rotations may not be meaningfully rotated by-hand.
}

\value{
  Returns the object \code{results} of class \code{QmethodRes}, re-calculated for the manually rotated loadings.
  The object is returned invisibly.
}

\author{
  Maximilian Held
}

\note{
  This function currently relies on static plotting functions and is not optimized for speed.
  As a result, the iterative rotation is quite fast, but does not refresh in real-time.
  If you would like a real-time version of this software, comment or contribute \href{https://github.com/aiorazabala/qmethod/issues/172}{here}.

  Only integer angles (in degrees) are cached, but users may enter any numerical angle interactively.
  Downstream rotations may be significantly slower.

  Different graphics devices in R appear to vary greatly in their plotting speed.
  Slow plotting speed can impair the responsiveness of interactive rotation.
  If you are experiencing poor refresh speeds, consider using an alternative graphics device.
  The default graphics device of the popular R IDE RStudio, in particular, appears to be slow on some platforms.

  Rotation angles are always simplified to be between -180 and 180 degrees.
}

\section{Warning}{
  This function has been recently developed with great care but has not been systematically validating against other programs or externally reviewed.
  Users beware.
  Note that a manual rotation changes \emph{all} downstream results in a Q methodological analysis.
  If you would like to help testing this function, comment or contribute \href{https://github.com/aiorazabala/qmethod/issues/206}{here}.
}

\seealso{
  \code{\link[psych]{factor.rotate}} in package \pkg{psych},
  \code{\link{qmethod}},
  \code{\link{q.rotplot}},
  \code{\link{q.loaplot}}
}

\examples{
  data(lipset)
  results <- qmethod(lipset[[1]], nfactors = 3, rotation = "varimax")
  # results2 <- q.rotate(results = results)  # run this!
  # follow on-screen instructions
}
