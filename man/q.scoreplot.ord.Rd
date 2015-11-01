\name{q.scoreplot.ord}
\alias{q.scoreplot.ord}
\title{Q methodology: create ordinal Q factor score plot}
\description{
  Visualizes Q Method results, with one ordinal Q factor score plot for each extracted factor.
}
\usage{
  q.scoreplot.ord(
    results
    , factor
    , extreme.labels = c("negative", "positive")
    , incl.qdc = TRUE
    , quietly = FALSE
    , hyph.pattern = "en"
    , hyphenate = TRUE
    , split.string = "[[:punct:]]"
    , label.scale = 500
    )
}
\arguments{
  \item{results}{
    A results object from \code{\link{qmethod}}.
  }
  \item{factor}{
    A vector of length one, indicating which factor should be plotted, either as integer or character factor name.
  }
  \item{extreme.labels}{
    A character vector of length 2 with labels for the negative and positive extreme of the q sort array, ideally as presented to study participants.
    Defaults to \code{c("negative", "positive")}.
  }
  \item{incl.qdc}{
    Logical, indicating whether lines for relative position of distinguishing items on other factors should be drawn.
    Defaults to \code{TRUE}.
  }
  \item{quietly}{
    Logical, indicating whether a summary plot should be printed.
    Defaults to \code{FALSE}.
  }
  \item{hyph.pattern}{
    A character vector of length 1, indicating the language for hyphenation via \code{\link[koRpus]{hyphen}}.
    Must be either \code{"en"} (default, English), \code{"de"} (German), \code{"es"} (Spanish), \code{"fr"} (French), \code{"it"} (Italian) or \code{"ru"} (Russian).
  }
  \item{hyphenate}{
    Logical, indicating whether item handles should be hyphenated for plot labels.
    Defaults to \code{TRUE}.
  }
  \item{split.string}{
    Character vector of length 1, indicating the string on which to wrap item handles for plot labels.
    Defaults to \code{"[[:punct:]]"}, in which case all punctuation may be replaced by linebreaks.
    See \code{\link[base]{strsplit}} for details.
  }
  \item{label.scale}{
    An integer vector to scale the plot labels, defaults to \code{500}.
    To avoid overplotting, try a lower number or adjust the returned \code{ggplot} directly by passing additional options.
  }
}
\details{
  During a factor interpretation in Q methodology, it is helpful to consider the \emph{ideal-typical} sort of a factor, as conveniently produced by \code{\link{qmethod}} in \code{zsc_n}.
  Instead of a tabular view, this function creates a plot similar to the Q sorts in which participants originally expressed their viewpoints, with items stacked according to the (forced) Q distribution.

  The resulting graphs include the following information:
  \enumerate{
    \item \emph{Item Position.}
      The items are sorted as per \code{\link{qmethod}}'s \code{zsc_n}, that is, the standardized average of loadings-weighted item scores of flagged participants on a given factor, rank ordered.
    \item \emph{Item Names.}
      Items are named according to a line-wrapped version of the row names of the \code{"zsc_n"} array from \code{\link{qmethod}}.
    \item \emph{Tile Fill Color.}
      Tiles surrounding items are filled by a color identifying the current factor across all plots, as created by \code{\link{q.fcolors}}.
    \item \emph{Tile Fill Transparency.}
      The transparency of the fill color is determined by the loadings-weighted population (!) standard deviation of items \emph{across} flagged Q sorts, as produced by \code{\link{q.item.sd}}.
      The shade is scaled to the highest standard deviation of an item across all factors, implying that the scale \emph{can} be compared across factor plots \emph{within} the same \code{\link{qmethod}} analysis.
      This is a somewhat unconventional statistic in Q methodology, and is meant to give an impression of the \emph{dispersion} of items across sorts loading on a given factor.
    \item \emph{Lines Distinguishing Items Factors.}
      For any item that is significantly distinguishing between any pair of factors as per \code{\link{qdc}}, a vertical line is drawn in the color of that other factor.
      The line is offset from the horizontal center of the item tile by the difference in \code{zsc} between the two factors on that item.
      The offset is standardized to the highest difference on a given factor plot, implying that these offsets \emph{cannot} be compared across plots and visualize only \emph{relative} positions.
    \item \emph{Line Type for Distinguishing Items Factors.}
      Lines are dashed according to the conventional cut-off points of significance for the observed difference in position.
  }

  This visualization was inspired by the \emph{crib sheet} method for rigorous factor interpretation, as suggested by Watts and Stenner (2012).
  These plots combine \emph{all} information necessary for a crib sheet reading in one visualization.
  Items scoring higher (or lower) than on any other factor can be easily spotted as those items with no lines on their left (right) half.

  Compared to \code{zsc}, which are the unrounded z-scores, \code{zsc_n} constitute a \emph{loss} of information.
  In particular, the precise location of an item on an ideal-typical, shared factor is replaced by that items (binned) \emph{rank} order.
  Such summary may be easier to read and amenable to interpretation, because it reproduces Q sort data in its original form.
  Users should be careful to interpret the plot in \emph{ordinal} terms: distances \emph{between} (nearby) columns are not strictly meaningful.

  Users may consieder \code{q.scoreplot.num} as an alternative for a truthful representation of \emph{interval}-scaled factor score results.
}

\value{
  Returns a list of \pkg{ggplot2} objects.
}

\references{
    Watts, S., and and P. Stenner, 2012. \emph{Doing Q Methodological Research: Theory, Method & Interpretation}, London: Sage Publications Ltd.
}

\author{Maximilian Held}

\note{
  In rare cases \code{zsc_n} rounded scores from \code{\link{qzscores}} can violate a forced distribution when factor statement scores are tied between two columns.
  These ties are retained in the present plots.
  For more information, see \url{https://github.com/aiorazabala/qmethod/issues/196}.
}

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
  plots <- q.scoreplot.ord(results = results, factor = 1)  # produces plot for first factor
}
