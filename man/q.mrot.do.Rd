\name{q.mrot.do}
\alias{q.mrot.do}
\title{Q methodology: implement rotation matrix}
\description{
  Implements a rotation matrix and recalculates all necessary qmethod results accordingly.
}

\usage{
  q.mrot.do(results, rot.mat, quietly = FALSE)
}

\arguments{
  \item{results}{
    A results object from \code{\link{qmethod}}.
  }
  \item{rot.mat}{
    An arbitrary rotation matrix, as produced by \code{\link{q.mrot.choose}}.
    A rotation matrix must have as many rows and columns as there are factors retained in \code{results} and must be orthogonal, but not an identity matrix.

    If the column and row names of the rotation matrix are named (manually, or by \code{\link{q.mrot.choose}}), these names will be passed to \code{\link{q.fnames}}, renaming all results accordingly.
  }
  \item{quietly}{
    Logical, indicating whether a summary and plot should be printed.
    Defaults to \code{FALSE}.
  }
}

\details{
  Q methodologists often prefer custom rotations, such as by-hand rotations by \code{\link{q.mrot.choose}}, a specialized rotation procedure or some other source.

  This function applies such a rotation matrix to the original loadings, and recalculates all downstream results as necessary.

  Recall that any rotation will change the factor scores on \emph{all} factors, and may require a new factor interpretation.

  Recall further that any given completed Q analysis and factor interpretation depends on a \emph{particular} rotation, which cannot be easily inferred from the factor loadings.
  To be able to document and reproduce a by-hand rotation, this function appends a rotation matrix to the results object of class \code{QmethodRes} under \code{results$brief$rotmat}.
  The function also sets \code{results$brief$rotation} to \code{"by-hand"}.

  Any given rotated solution, including \code{rotation = "varimax"}, \code{rotation = "none"} and others from \code{\link{qmethod}} can be used as a baseline for applying a custom rotation matrix on top.

  Notice that \emph{non-orthogonal} baseline rotations may not be meaningfully rotated by-hand.

  Notice that \code{\link{q.mrot.do}} \emph{does not} invert components with a negative average of loadings as both \code{\link{qmethod}} and \code{\link[psych]{principal}} do.
  In manual rotation, it is assumed that users \emph{want} a mostly negatively loading factor, though caution in interpreting the respective factor scores is advised.
}

\value{
  Returns the object \code{results} of class \code{QmethodRes}, re-calculated for the custom rotated loadings.
}

\author{
  Maximilian Held
}

\note{
  This function currently works only for forced distributions, because of a \href{https://github.com/aiorazabala/qmethod/issues/246}{technical reason}.
}

\seealso{
  \code{\link{qmethod}},
  \code{\link{q.rotplot}},
  \code{\link{q.mrot.choose}}
}

\examples{
  data(lipset)
  results.unrot <- invisible(qmethod(lipset[[1]], nfactors = 3, rotation = "none"))
  # to get unrotated loadings
  loa.unrot <- as.matrix(results.unrot$loa)
  rotmat.varimax <- varimax(x = loa.unrot)$rotmat  # produce some convenient rotation matrix
  # notice that users would usually provide a by-hand rotation matrix
  results.varimax <- q.mrot.do(results = results.unrot, rot.mat = rotmat.varimax, quietly = TRUE)
  # compute results object based on provided rotation matrix
}
