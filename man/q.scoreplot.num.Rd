\name{q.scoreplot.num}
\alias{q.scoreplot.num}
\title{Q methodology: create numerical Q factor score plot}
\description{
  Visualizes Q Method results, with one numerical Q factor score plot for each extracted factor.
}
\usage{
  q.scoreplot.num(
    results
    , factor
    , pointrange = "item.sd"
    , incl.qdc = TRUE
    , quietly = FALSE
    )
}
\arguments{
  \item{results}{
    A results object from \code{\link{qmethod}}.
  }
  \item{factor}{
    A vector of length one, indicating which factor should be plotted, either as integer or character factor name.
  }
  \item{pointrange}{
    A character vector of length one, choosing among available bases for plotting a point range around factor scores.
    Defaults to \code{"item.sd"}, calling \code{\link{q.item.sd}} with \code{standardize = TRUE}.
    Set to \code{NULL} to omit pointranges.
  }
  \item{incl.qdc}{
    Logical, indicating whether points for distinguishing items on other factors should be drawn.
    Defaults to \code{TRUE}.
  }
  \item{quietly}{
    Logical, indicating whether a summary plot should be printed.
    Defaults to \code{FALSE}.
  }
}
\details{
  During a factor interpretation in Q methodology, it is helpful to consider the \emph{ideal-typical} sort of a factor, as conveniently produced by \code{\link{qmethod}} in \code{zsc_n}.

  This visualization was inspired by the \emph{crib sheet} method for rigorous factor interpretation, as suggested by Watts and Stenner (2012).
  This plots combine \emph{all} information necessary for a crib sheet reading in one visualization.
  Items scoring higher (or lower) than on any other factor can be easily spotted as those items with no points on their left (right).

  For an easier to read alternative with some loss of information, users may consider \code{q.scoreplot.ord}.
}

\value{
  Returns a list of \pkg{ggplot2} objects.
}

\references{
  Watts, S., and and P. Stenner, 2012. \emph{Doing Q Methodological Research: Theory, Method & Interpretation}, London: Sage Publications Ltd.
}

\author{Maximilian Held}

\seealso{
  \code{\link{import.q.concourse}}
  , \code{\link{qmethod}}
  , \code{\link{qzscores}}
  , \code{\link{qdc}}
  , \code{\link{q.fcolors}}
  , \code{\link{q.fnames}}
  , \code{\link{pop.sd}}
  , \code{\link{q.item.sd}}
  , \code{\link{q.scoreplot.ord}}
}

\examples{
  data(lipset)  # use data included with this package
  results <- qmethod(lipset[[1]], nfactors=3, rotation="varimax")  # run analysis
  plots <- q.scoreplot.num(results = results, factor = 1)  # produces plot for first factor
}
