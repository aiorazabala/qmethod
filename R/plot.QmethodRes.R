plot.QmethodRes <- function(x, 
                            xlab='z-scores', ylab='statements',
                            pchlist = NULL, colours = NULL,
                            fnames = NULL, legend = TRUE, ...) {
  dfr <- x$zsc
  lowlim <- floor(min(dfr[[1]]))
  highlim <- ceiling(max(dfr))
  if (is.null(pchlist)) pchlist <- c(1, 2, 0, 5, 6, 16, 17, 15, 18, 21, 24, 23, 22, 3, 4, 7, 8, 9)
  nfactors <- length(dfr)
  dfr <- dfr[order(apply(dfr, 1, sd)), ]
  if (!is.null(colours)) {  # when there ARE hard-entered colors in THIS FUNCTION CALL
    # this is just in case; for legacy support, we still accept manually specified colors WITHIN this function, which will then ignore other colors, but issue a warning.
    warning ("Your manually defined colors may be inconsistent with other plots. Leave colour blank and run q.fcolors() for consistently colored plots.")
  } else {  # when there are NO hard-entered colors in THIS FUNCTION CALL
    if (!is.null(x$brief$fcolors)) {  # when there ARE colors in object
      colours <- x$brief$fcolors  # just use them
    } else {  # if there NO colors in
      x <- q.fcolors(x)  # make standard fcolors
      warning ("Because no colors were found in results object, they were generated on the fly. Colors are not saved to results object and may be inconsistent between plots. Run q.fcolors() to assign consistent colors.")
      colours <- x$brief$fcolors
    }
  } 
  if (is.null(fnames) & names(x$zsc)[1] == "zsc_f1") fnames <- paste0("Factor ", 1:nfactors)
  if (is.null(fnames) & names(x$zsc)[1] != "zsc_f1") fnames <- names(x$zsc)
  dotchart(dfr[[1]], lcolor=grey(0.4),
           xlim=c(lowlim, highlim),
           ylab=ylab, xlab=xlab, axis=NULL,
           pch=pchlist[[1]], color=colours[[1]], ...)
  for (i in 2:nfactors){
    points(x=dfr[[i]], 1:length(dfr[[i]]), pch = pchlist[i], type = "p", col=colours[[i]], ...)
  }
  axis(side=2, at=1:nrow(dfr), 
       labels=rownames(dfr), 
       las=1, tick=F, line=-0.5, ...)
  abline(v=seq(from=lowlim, to=highlim, by=0.5), col=grey(0.6), lty=3)
  if (legend) {
    legend('bottomright', 
           legend=fnames, 
           col=colours[1:nfactors], 
           pch=pchlist[1:nfactors], 
           bty="n")
  }
}
