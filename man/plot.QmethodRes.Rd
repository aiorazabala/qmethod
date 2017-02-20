\name{plot.QmethodRes}
\alias{plot.QmethodRes}
\title{Q Method: plot for statement z-scores}
\description{Takes an object of class \code{QmethodRes} resulting from \code{\link{qmethod}} and makes a dot-chart with the z-scores for statements and all factors.}
\usage{
\method{plot}{QmethodRes}(x, xlab = 'z-scores', ylab = 'statements', 
     pchlist = NULL, colours = NULL, 
     fnames = NULL, legend = TRUE,
     dist = TRUE, pchlist.fill = NULL,
     leg.pos="bottomright", xlim= NULL, 
     sort.items=T, factors = NULL,
     ...)}
\arguments{
  \item{x}{results object returned by \code{\link{qmethod}}.}
  \item{xlab}{label for x axis. Defaults to 'z-scores'.}
  \item{ylab}{label for y axis. Defaults to 'statements'.}
  \item{pchlist}{array of \code{pch} symbols to be used in plotting the points for each factor. Defaults to a pre-defined set of symbols.}
  \item{colours}{array of colours to be used when plotting the points for each perspective. Defaults to a pre-defined set of colours based on the \code{\link[grDevices]{rainbow}} palette.}
  \item{fnames}{names for factors to be used in the legend. In results where factor names have not been changed (using, e.g. \code{\link{q.fnames}}) it defaults to \code{'Factor 1'}, \code{'Factor 2'}, etc.}
  \item{legend}{logical; if \code{FALSE}, no \code{legend} will be drawn.}
  \item{dist}{Logical. Should distinguishing statements be indicated in the plot dots? If \code{TRUE}, then the z-score values that are distinguishing for a given statement and factor are represented with a different (filled) symbol.}
  \item{pchlist.fill}{List of symbols to indicate distinguishing statements. By default, this is set to \code{NULL}, which provides a set of symbols that coincides with those in \code{pchlist}, but filled.}
  \item{leg.pos}{Position of the legend.}
  \item{xlim}{Limits for the x axis, given as a vector of two numbers. If this is not provided, the limits are calculated from the sample.}
  \item{sort.items}{Whether and how the items are sorted in the vertical axis. Defaults to \code{TRUE}, which sorts the items according to the standard deviation of their z-scores for all factors. A value of \code{FALSE} will not sort the items, which are displayed in the same order as in the raw data. A numerical vector can also be provided in order to sort the statements manually: the vector needs to have the same length as the number of items, and provides the order in which the items are to be ordered.}
  \item{factors}{The factors to plot. Defaults to \code{NULL}, which plots all the factors in the object \code{x} in the order given. To print a subset of these factors or to print them in a different order, provide a numeric vector here with the factors and the order desired, e.g. \code{c(2,1)}}.
  \item{...}{other arguments for \code{\link[graphics]{plot}}.}

}

\author{Aiora Zabala}

\seealso{
\code{\link[graphics]{dotchart}} and \code{\link[graphics]{points}}.
}
\examples{
data(lipset)
results <- qmethod(lipset[[1]], nfactors = 3, rotation = "varimax")
title <- "Q method z-scores, lipset dataset"
subtitle <- paste0("Three factors, PCA, varimax. Printed on ",
                  Sys.Date())
plot(results, main = title, sub = subtitle)

# Order the items in a different way
plot(results, main = title, sub = subtitle, 
     sort.items = c(rev(1:nrow(results$zsc))))
}
\keyword{plot}
