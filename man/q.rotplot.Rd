\name{q.rotplot}
\alias{q.rotplot}
\title{Q methodology: wrap plots for rotation.}
\description{
	Produces a grid of informative plots for all combinations of retained factors.
}

\usage{
	q.rotplot(results, quietly = FALSE, label.scale = 200)
}

\arguments{
  \item{results}{
    A results object from \code{\link{qmethod}}.
  }
  \item{quietly}{
    Logical, indicating whether a summary plot should be printed.
    Defaults to \code{FALSE}.
  }
  \item{label.scale}{
    An integer vector to scale the plot labels, passed on to \code{\link{q.scoreplot.ord}}, defaults to \code{500}.
  }
}

\details{
	During rotation procedures Q researchers may wish to simultaneously inspect three plots for each of the combinations of retained factors:
	\enumerate{
	  \item the loadings of all Q-sorts in the factor space (\code{\link{q.loaplot}}),
	  \item the share of explained variance by the retained factors for each of the Q-sorts (\code{\link{q.compplot}}),
	  \item and the factor scores for the pair of factors (\code{\link{q.rotplot}}).
	}
	This function arranges these plots in grids for convenient inspection on one screen.

	If no colors from \code{\link{q.fcolors}} are found in \code{results}, separate, consistent color schemes will be applied for the different available rotation methods in \code{\link{qmethod}} to help users distinguish different models.
	These color schemes are different from \code{\link{q.fcolors}} defaults.
	These colors are for temporary orientation during rotation procedures only and are \emph{not} written into \code{results}.

	To avoid confusion, final colors for factor interpretation and presentation should be decided together with factor names (\code{\link{q.fnames}}) only once rotation, flagging (\code{\link{qflag}}) and weighting (\code{\link{qfwe}}) procedures are completed.

}

\value{
	Returns a list of grobs.
}

\author{Maximilian Held}

\note{
  This grid of plots is not optimized for printing or publication and is best used during interactive rotation procedures.
  For better-quality plots, refer to the constituent plotting functions listed below.
}

\seealso{
	\code{\link{q.loaplot}}
	\code{\link{q.compplot}}
	\code{\link{q.scoreplot.ord}}
}

\examples{
  data(lipset)  # use data included with this package
  results <- qmethod(lipset[[1]], nfactors=3, rotation="varimax")  # run analysis
  # rotplots <- q.rotplot(results)  # prints (overwhelming) grid of grids
  # rotplots$`f1-f2`  # access individual factor pairs
}
