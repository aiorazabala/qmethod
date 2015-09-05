\name{q.item.sd}
\alias{q.item.sd}
\title{
	Q methodology: calculate item standard deviation.
}

\description{
	Calculates the standard deviation of items across flagged and weighted Q sorts for each factor.
}

\usage{
	q.item.sd(results)
}

\arguments{
  \item{results}{
  	An object of class \code{QMethodRes}, as produced by \code{\link{qmethod}}.
	}
}

\details{
	During factor interpretation of an ideal-typical Q sort (as computed by \code{\link{qzscores}}), researchers may want to consider the \emph{dispersion} of items on the factor \emph{across} the constituent Q sorts.
	This function calculates the standard deviation of items across flagged and weighted Q sorts for each factor.

	Notice that while items towards the extremes of the Q distribution are likely to have a smaller standard deviation, items towards the center may display either small or large standard deviations.
	On the one hand, an item may lie at the center of an ideal-typical Q sort because many of the constituent Q sorts have placed it \emph{at the center}.
	On the other hand, an item may lie at the center of an ideal-typical Q sort because it is widely, but \emph{symmetrically} dispersed across the constituent Q sorts.
	In an extreme example, a center item may result from half of the Q sorts ranking at the positive and negative maximum each, canceling each other out.
	Such differences in the dispersion across constituent Q sorts for an ideal-typical sort may be important information for a factor interpretation.

	%% TODO(maxheld83): need to discuss here how it differs/overlaps from qdc
}

\value{
	Returns a single data frame of item standard deviations, with factors as columns and items as rows.
}

\author{
	Maximilian Held
}

\note{
	Because the item standard deviation is a descriptive statistic about the actual Q sorts, with no intended inference to other cases, this function produces the \emph{population} standard deviation via \code{\link{pop.sd}}, not the \emph{sample} standard deviation.

	This is a function used within \code{\link{q.scoreplot}}.
	Rarely to be used independently.
}

\seealso{
	\code{\link{qzscores}},
	\code{\link{q.scoreplot}}
}

\examples{
  data(lipset)
  results <- qmethod(lipset[[1]], nfactors = 3, rotation = "varimax")
  item_sd <- q.item.sd(results)
  head(item_sd)
}
