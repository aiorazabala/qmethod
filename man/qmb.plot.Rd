\name{qmb.plot}
\alias{qmb.plot}
\title{Q Methodology: Plot of bootstrap results}
\description{Plots the summary of bootstrap results, either z-scores or factor loadings.}
\usage{qmb.plot(qmbsum, type = c("zsc", "loa"), nfactors, 
         cex = 0.7, cex.leg = 0.8, errbar.col = "black",
         lwd = 1, lty = 1, vertdist = 0.2, limits = NULL, 
         r.names = NA, sort = c("none", "difference", "sd"),
         sbset = NULL, leg.pos = "topleft", 
         bty = "n", plot.std = TRUE, pch= NULL,
         col=NULL, grid.col="gray", ...)}

\arguments{
  \item{qmbsum}{an object with the summary of bootstrap results, as produced by \code{\link{qmb.summary}}.}
  \item{type}{the subject to plot, either z-zcores of statements or factor loadings of Q-sorts.}
  \item{nfactors}{number of factors extracted.}
  \item{cex}{a numerical value giving the amount by which plotting text and symbols should be magnified relative to the default (see \code{\link[graphics]{par}}).)}
  \item{cex.leg}{a numerical value giving the amount by which the legend should be magnified relative to \code{cex}.)}
  \item{errbar.col}{colour used for the error bars. Defaults to \code{"black"}.}
  \item{lwd}{line width (see \code{\link[graphics]{par}}).}
  \item{lty}{line type (see \code{\link[graphics]{par}}).}
  \item{vertdist}{distance between the values for each factor.}
  \item{limits}{axis limits for the numerical values. If set to \code{NULL}, the limits are automatically set as \code{c(-1, 1)} when \code{type = "loa"}, and as the minimum and maximum values of z-scores (including the error bars) when \code{type = "zsc"}}
  \item{r.names}{names of the items to be printed in the axis ticks(either Q-sorts when \code{type = "loa"}, or statements when \code{type = "zsc"}). When the value is \code{NULL}, it defaults to \code{rownames}.}
  \item{sort}{ordering of the items in the axis. If set to \code{"none"}, items are ordered by the default order in the dataset. If set to \code{"difference"}, items are ordered according to the variability in the values across factors. If set to \code{"sd"}, items are ordered according to the sum of the errors obtained in the bootstrap.}
  \item{sbset}{How many items are to be printed? When the value is \code{NULL}, it plots all the items.}
  \item{leg.pos}{Position of the legend.}
  \item{bty}{Legend box (see \code{\link[graphics]{legend}}).}
  \item{plot.std}{logical value. When set to \code{TRUE} (default), it prints the points for values obtained with the standard analysis (non bootstrapped).}
  \item{pch}{plotting symbols. Defaults to \code{NULL}, in which case the symbols are selected automatically. If provided, the vector needs to contain at least as many elements as number of factors. In addition, if argument \code{plot.std == TRUE} (default) the vector needs to contain at least double as many elements as vectors, in order to extract (a) the plotting symbols for bootstrapped values (the first elements) and (b) the plotting symbols for standard values (the next elements).}
  \item{col}{colours for the points. At least as many elements as number of factors have to be provided.}
  \item{grid.col}{colour of the grid.}
  \item{...}{additional arguments to be passed to the functions \code{\link[graphics]{dotchart}}, \code{\link[graphics]{mtext}}, \code{\link[graphics]{segments}}, \code{\link[graphics]{points}}, \code{\link[graphics]{abline}} or \code{\link[graphics]{legend}}.}
}

\author{Aiora Zabala}

\references{Zabala, Pascual (2016) Bootstrapping Q Methodology to Improve the Understanding of Human Perspectives. PLoS ONE 11(2): e0148087.}

\seealso{
\code{\link{qmethod}}, \code{\link{qmboots}}, \code{\link{qmb.summary}}
}
\examples{
data(lipset)
boots <- qmboots(lipset[[1]], nfactors = 3, nsteps = 50,
                load = "auto", rotation = "varimax", 
                indet = "qindet", fsi = TRUE)
                
boots.summary <- qmb.summary(boots)

qmb.plot(boots.summary, 3, type = "loa", sort="difference")
}
\keyword{Q methodology}
\keyword{bootstrapping}
\keyword{plot}
