\name{q.mrot.choose}
\alias{q.mrot.choose}
\title{Q methodology: rotate factor loadings interactively}
\description{
  Lets user iterate over arbitrary rotation angles for each combination of factor pairs.
  Runs interactively from the console, produces informative plots.
}

\usage{
  q.mrot.choose(
    results,
    plot.type = "q.rotplot",
    plot.all = FALSE,
    file = "",
    label.scale = 300
  )
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
  	Defaults to \code{FALSE}.
  }
  \item{file}{
  	Either a character string naming a file or a connection open for writing the resultant \code{rot.mat}, as expected by \code{\link{q.mrot.do}}.
    Defaults to \code{""}, in which case the output will be returned to the console.
  }
    \item{label.scale}{
    An integer vector to scale the plot labels, passed on to \code{\link{q.scoreplot}}, defaults to \code{500}.
  }
}

\details{
  Q methodologists often prefer by-hand (or manual or judgmental) rotations over automatic procedures.
  This function allows users to rotate any pair of factors by any angle and provides informative plots for the rotations.
  Users are guided through an iterative rotation procedure by a command line interface in the R console.

  Arbitrary angles in degrees can be entered (transformed to radians).
  Negative (positive) angles rotate (counter) clockwise.
  Users can abort the rotation procedure at any time.

  Towards the end of the interactive procedure, it is recommended that users name the \emph{rotated} factors.
  These names will be included as column and row names of the returned rotation matrix, which can be passed on to \code{\link{q.mrot.do}}, to rename all results of the analysis accordingly.

  Recall that any rotation will change the factor scores, and may require a new factor interpretation.
  To aid in such an iterative process of rotation and interpretation, this function not only provides rotated \emph{loadings}, but also calculates and plots resultant \emph{factor scores} by re-running \code{\link{qmethod}}.

  Recall further that any given completed Q analysis and factor interpretation depends on a \emph{particular} rotation, which cannot be easily inferred from the factor loadings.

  To be able to document and reproduce a by-hand rotation, this function \emph{only} returns the rotation matrix, from which the by-hand rotation can be reproduced.
  To implement the by-hand rotation, users must pass this rotation matrix as \code{rot.mat} to \code{\link{q.mrot.do}}, which will then rerun the necessary functions to return a rotated \code{results} object of class \code{QmethodRes}.

  If users wish to go forward with a by-hand rotation, they should therefore carefully store the rotation matrix.

  Any given rotated solution, including \code{rotation = "varimax"}, \code{rotation = "none"} and others from \code{\link{qmethod}} can be used as a baseline for by-hand rotation.
  Notice that \emph{non-orthogonal} baseline rotations may not be meaningfully rotated by-hand.
}

\value{
  Returns a rotation matrix.
}

\author{
  Maximilian Held
}

\note{
  This function re-runs the Q analysis every time a new angle is provided, by invoking \code{\link{q.mrot.do}}, which in turn runs \code{\link{qzscores}}.
  Depending on the \code{plot.type} specified, these results are used for plotting.
  This function does \emph{not} return these results, but \emph{only} the rotation matrix to produce them.

  To calculate these results, pass the rotation matrix to \code{\link{q.mrot.do}}.

  This function currently relies on static plotting functions and is not optimized for speed.
  As a result, the iterative rotation is quite fast, but does not refresh in real-time.
  If you would like a real-time version of this software, comment or contribute \href{https://github.com/aiorazabala/qmethod/issues/172}{here}.

  Different graphics devices in R appear to vary greatly in their plotting speed.
  Slow plotting speed can impair the responsiveness of interactive rotation.
  If you are experiencing poor refresh speeds, consider using an alternative graphics device.
  The default graphics device of the popular R IDE RStudio, in particular, appears to be slow on some platforms.

  Rotation angles are always simplified to be between -180 and 180 degrees.

  The factor scores and other results presented during interactive rotation are computed using automated flagging.
  Flags can later be adjusted using \code{\link{qflag}}, but such manually flagged analyses will then differ from the preview rendered during interactive rotation.
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
  # results2 <- q.mrot.choose(results = results)  # run this!
  # follow on-screen instructions
}
