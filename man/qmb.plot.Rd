\name{qmb.plot}
\alias{qmb.plot}
\title{Q Methodology: Plot of bootstrap results}
\description{Plots the summary of bootstrap results, either z-scores or factor loadings.}
\usage{qmb.plot(qmbsum, type = c("zsc", "loa"), nfactors, 
         cex = 0.7, cex.leg = 0.8, errbar.col = "black",
         lwd = 1, lty = 1, vertdist = 0.2, limits = NA, 
         r.names = NA, sort = F, sbset = NULL,
         leg.pos = "topleft", bty = "n",
         ...)}

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
  \item{sort}{logical value. When set to \code{TRUE}, items in the axis are sorted according to the sum of the errors.}
  \item{sbset}{How many items are to be printed. When the value is \code{NULL}, it plots all the items.}
    \item{leg.pos}{Position of the legend.}
    \item{bty}{Legend box (see \code{\link[graphics]{legend}}).}
  \item{...}{additional arguments to be passed to the functions \code{\link[graphics]{dotchart}}, \code{\link[graphics]{mtext}}, \code{\link[graphics]{segments}}, \code{\link[graphics]{points}}, \code{\link[graphics]{abline}} or \code{\link[graphics]{legend}}.}
}

\author{Aiora Zabala}

\seealso{
\code{\link{qmethod}}, \code{\link{qmboots}}, \code{\link{qmb.summary}}
}
\examples{
data(lipset)
boots <- qmboots(lipset[[1]], nfactors = 3, nsteps = 50,
                load = "auto", rotation = "varimax", 
                indet = "qindet", fsi = TRUE)
                
boots.summary <- qmb.summary(boots)

qmb.plot(boots.summary, 3, type = "loa")
}
\keyword{Q methodology}
\keyword{bootstrapping}
\keyword{plot}