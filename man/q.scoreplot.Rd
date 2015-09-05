\name{q.scoreplot}
\alias{q.scoreplot}
\title{Q methodology: create Q factor score plot}
\description{
  Visualizes Q Method results, with one Q factor score plot for each extracted factor.
}
\usage{
  q.scoreplot(
    results
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
  During a factor interpretation in Q methodology, it is helpful to consider the \emph{ideal-typical} sort of a factor, as conveniently produced by \code{\link{qmethod}} in \code{zsc}.
  Instead of a tabular view, this function creates a plot similar to the Q sorts in which participants originally expressed their viewpoints, with items stacked according to the (forced) Q distribution.

  The resulting graphs include the following information:
  \enumerate{
    \item \emph{Item Position.}
      The items are sorted as per \code{\link{qmethod}}'s \code{zsc_n}, that is, the standardized average of loadings-weighted item scores of flagged participants on a given factor, rank ordered.
      Compared to \code{zsc}, which are the unrounded z-scores, \code{zsc_n} constitute a \emph{loss} of information.
      Absolute (rather than zscored) positions are chosen here despite that loss to avoid colossal overplotting of labels, as well as to reproduce Q sort data in its original form.
    \item \emph{Item Names.}
      Items are named according to a line-wrapped version of the row names of the \code{"zsc_n"} array from \code{\link{qmethod}}.
    \item \emph{Tile Fill Color.}
      Tiles surrounding items are filled by a color identifying the current factor across all plots, as created by \code{\link{q.fcolors}}.
    \item \emph{Tile Fill Transparency.}
      The transparency of the fill color is determined by the loadings-weighted population (!) standard deviation of items \emph{across} flagged Q sorts, as produced by \code{\link{qzscores}}.
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
}

\value{
  Returns a list of \pkg{ggplot2} objects, one for each extracted factor.
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
}

\examples{
  # data(lipset)  # use data included with this package
  # results <- qmethod(lipset[[1]], nfactors=3, rotation="varimax")  # run analysis
  # plots <- q.scoreplot(results = results)  # produces plots list
  # plots[1]  # retrieves and prints plot for first factor
}
