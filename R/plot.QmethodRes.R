plot.QmethodRes <- function(x, 
                            xlab='z-scores', ylab='statements',
                            pchlist = NULL, colours = NULL,
                            fnames = NULL, legend = TRUE, 
                            dist = TRUE, pchlist.fill = NULL, 
                            leg.pos="bottomright", xlim= NULL, 
                            sort.items=T, factors = NULL, ...) {
if (!is.null(factors) & dist) {
  warning("Interpret with care.

Only a subset of all the factors is plotted (argument 'factors'), and filled markers indicate distinguishing statements (argument 'dist = TRUE'). Significant differences are calculated with respect to all the factors in the object of results (not only those factors visible).")
}
    if (is.null(factors)) {
    factors <- c(1:x$brief$nfactors)
  } else if (max(factors) > max(c(1:x$brief$nfactors))) {
    warning("The numbers of factors provided are beyond the number of factors in the object of results. The default factors will be plotted.")
    factors <- c(1:x$brief$nfactors)
  }
  dfr <- x$zsc
  lowlim <- floor(min(dfr[[1]]))
  highlim <- ceiling(max(dfr))
  if (is.null(xlim)) {
    xlimits <- c(lowlim, highlim)
  } else xlimits = xlim
  if (is.null(pchlist)) {
    pchlist <- c(1, 2, 0, 5, 6, 16, 17, 15, 18, 21, 24, 23, 22, 3, 4, 7, 8, 9)
    pchlist.fill <- c(16, 17, 15, 23, 25, 16, 17, 15, 18, 21, 24, 23, 22, 3, 4, 7, 8, 9)
  }
  nfactors <- length(dfr)
  # Sorting of items in y axis
  sta.order <- 1:nrow(dfr)
  if (is.numeric(sort.items)) {
    if (length(sort.items) == nrow(dfr)) sta.order <- sort.items
    if (length(sort.items) != nrow(dfr)) warning("The number of elements in the vector to sort the items ('sort.items') does not equal the number of items. Items will not be sorted in the plot.")
  } else {
    if (sort.items == F) {
      sta.order <- 1:nrow(dfr)
    } else {
      if (sort.items == T) sta.order <- order(apply(dfr, 1, sd))
    }
  }
  dfr <- dfr[sta.order, ]
    # Whether to show distinguishing statements
  if (dist) {
    pts <- qdc.zsc(x)
    pts <- pts[sta.order, ]
  }
  if (is.null(colours)) colours <- rainbow(length(dfr))
  if (is.null(fnames) & names(x$zsc)[1] == "zsc_f1") fnames <- paste0("Factor ", factors)
  if (is.null(fnames) & names(x$zsc)[1] != "zsc_f1") fnames <- names(x$zsc)
  dotchart(dfr[[factors[1]]], lcolor=grey(0.4),
           xlim=xlimits,
           ylab=ylab, xlab=xlab, axis=NULL,
           pch=pchlist[[1]], color=colours[[1]], ...)
  if(length(factors) > 1) {
    for (i in 2:length(factors)){
      points(x=dfr[[factors[i]]], 1:length(dfr[[factors[i]]]), pch = pchlist[i], type = "p", col=colours[[i]], bg=colours[[i]], ...)
    }
  }
  if (dist) {
    for (i in 1:length(factors)){
      points(x=pts[,factors[i]], 1:length(pts[,factors[i]]), pch = pchlist.fill[i], type = "p", col=colours[[i]], bg=colours[[i]], ...)
    }
  }
  axis(side=2, at=1:nrow(dfr), 
       labels=rownames(dfr), 
       las=1, tick=F, line=-0.5, ...)
  abline(v=seq(from=min(xlimits), to=max(xlimits), by=0.5), col=grey(0.6), lty=3)
  if (legend) {
    if (dist) {
      pch.leg = pchlist.fill[1:length(factors)]
      } else pch.leg <- pchlist[1:length(factors)]
    legend(leg.pos, 
           legend=fnames, 
           col=colours[1:length(factors)], 
           pch=pch.leg,
           pt.bg=colours[1:length(factors)],
           bty="n")
  }
}