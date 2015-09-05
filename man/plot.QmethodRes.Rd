\name{plot.QmethodRes}
\alias{plot.QmethodRes}
\title{Q Method: plot for statement z-scores}
\description{Takes an object \code{QmethodRes} resulting from \code{\link{qmethod}} and makes a dot-chart with the z-scores for statements and all factors.}
\usage{
\method{plot}{QmethodRes}(x, xlab = 'z-scores', ylab = 'statements', pchlist = NULL,
     colours = NULL, fnames = NULL, legend = TRUE, ...)
                   }
\arguments{
  \item{x}{results object returned by \code{\link{qmethod}}.}
  \item{xlab}{label for x axis. Defaults to 'z-scores'.}
  \item{ylab}{label for y axis. Defaults to 'statements'.}
  \item{pchlist}{array of \code{pch} symbols to be used in plotting the points for each factor. Defaults to a pre-defined set of symbols.}
  \item{colours}{
    array of colours to be used when plotting the points for each perspective.
    Defaults to \code{NULL} in which case colors are read in from results object or automatically generate colors via \code{\link{q.fcolors}}.
    This option may be deprecated in the future.
  }
  \item{fnames}{names for factors to be used in the legend. In results where factor names have not been changed (using, e.g. \code{\link{q.fnames}}) it defaults to \code{'Factor 1'}, \code{'Factor 2'}, etc.}
  \item{legend}{logical; if \code{FALSE}, no \code{legend} will be drawn.}
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
}
\keyword{plot}
