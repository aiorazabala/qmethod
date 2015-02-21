\name{array.viz}
\alias{array.viz}
\title{Q Methodology: creates Q array graph}
\description{
  Visualizes Q Method results, with one Q array graph for each extracted factor.
}
\usage{
    array.viz(
        QmethodRes
        ,f.names = NULL
        ,f.colors = NULL
        ,extreme.labels = c("negative", "positive")
        ,color.scheme = "Set1"
        ,incl.qdc = TRUE
    )
}
\arguments{
    \item{QmethodRes}{
        A results object from \code{\link{qmethod}}.
    }
    \item{f.names}{
        A character vector of desired factor names, in the order in which factors are extracted by \code{\link{qmethod}}.
        Used for naming graphs and legends.
        Defaults to \code{NULL}, in which case factors are named by numbers.
    }
    \item{f.colors}{
        A character vector of factor colors recognizable by R, in the order in which factors are extracted by \code{\link{qmethod}}.
        Used for coloring graphs and distinguishing statements.
        Defaults to \code{NULL}. in which case factors are colored automatically by \pkg{RColorBrewer}.
    }
    \item{extreme.labels}{
        A character vector of length 2 with labels for the negative and positive extreme of the q sort array, ideally as presented to study participants.
        Defaults to \emph{negative} and \emph{positive}.
    }
    \item{color.scheme}{
        A character vector of length 1 from color schemes available in \pkg{RColorBrewer}, suitable for qualitative data (\code{type = "qual"}.)
    }
    \item{incl.qdc}{
        Logical, indicating whether lines for relative position of distinguishing items on other factors should be drawn.
        Defaults to \code{TRUE}.
    }
}
\details{
    During a factor interpretation in Q methodology, it is helpful to consider the \emph{ideal-typical} sort of a factor, as conveniently produced by \code{\link{qmethod}} in \code{zsc}.
    Instead of a tabular view, this function creates a plot similar to the Q sorts in which participants originally expressed their viewpoints, with items stacked according to the (forced) Q distribution.

    The resulting graphs include the following information:
    \enumerate{
        \item \emph{Item Position.}
            The items are sorted as per \code{\link{qmethod}}'s \code{zsc_n}, that is, the standardized average of loadings-weighted, item scores of flagged participants on a given factor, distributed across the original Q distribution.
            Compared to \code{zsc}, which are the z-scores not forced back on the original Q distribution, \code{zsc_n} constitute a \emph{loss} of information.
            Absolute (rather than zscored) positions are chosen here despite that loss to avoid colossal overplotting of labels, as well as to reproduce Q sort data in its original form.
        \item \emph{Item Names.}
            Items are named according to a line-wrapped version of the \code{item.handles} from in \code{\link{import.q.concourse}}, handed on to results by \code{\link{qmethod}}.
        \item \emph{Tile Fill Color.}
            Tiles surrounding items are filled by a color identifying the current factor across all plots.
            The shade of the fill color is determined by the loadings-weighted population (!) standard deviation of items \emph{across} flagged Q sorts, as produced by \code{\link{qzscores}}.
            The shade is scaled to the highest standard deviation of an item within a given factor plot, implying that the scale \emph{cannot} be compared across factor plots and visualizes only \emph{relative} dispersions \emph{within} factors.
            This is a somewhat unconventional statistic in Q methodology, and is meant to give an impression of the \emph{dispersion} of items on across sorts flagged for a given factor.
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
    This visualization currently only works with a forced Q distribution.
}

\seealso{
    \code{\link{import.q.concourse}}
    ,\code{\link{qmethod}}
    ,\code{\link{qzscores}}
    ,\code{\link{qdc}}
}

\examples{
    data(lipset)  # use data included with this package
    results <- qmethod(lipset[[1]], nfactors=3, rotation="varimax")  # run analysis
    plots <- array.viz(QmethodRes = results)  # produces plots list
    plots[1]  # retrieves and prints plot for first factor
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
