q.compplot <- function(results, quietly = FALSE) {
  # Input verification =========================================================
  if (!is.logical(quietly) || !is.vector(quietly) || length(quietly) != 1) {
    stop("The argument set for quietly must be a logical vector of length 1.")
  }
  if (class(results) != "QmethodRes") {  # only accept results object
    stop("The object provided is not of class 'QmethodRes'.")
  }

  loasq <- results$loa^2
  `Q-Sort` <- `Squared Loading` <- Factor <- NULL    # this is a hideous hack to appease CRAN as per http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  loasq.molten <- melt(data = loasq, varnames = c("Q-Sort", "Factor"), value.name = "Squared Loading")
  loasq.molten$Factor <- ordered(loasq.molten$Factor, colnames(results$loa))  # order according to sequence in extraction
  g <- ggplot(data = loasq.molten, mapping = aes(x = `Q-Sort`, y = `Squared Loading`, fill = Factor))
  g <- g + geom_bar(stat = "identity")
  g <- g + coord_flip()  # that's how it is in Brown 1980 234
  g <- g + scale_y_continuous(limits = c(0, 1), expand = c(0,0))  # percentages, up to 100, no boundaries
  #TODO(maxheld83) labels = percent_format(), would be nice in the above, but CRAN doesn't like that
  g <- g + theme(legend.position = "bottom")
  if (is.null(results$brief$fcolors)) {  # if no colors defined, ...
    g <- g + scale_fill_brewer(type = "qual", palette = 7)  # assign own color scheme, different from score plot defaults
    warning("No colours were found in the results. Colors in this plot were assigned at random and are unrelated to the colors in other plots.")
  } else {
    g <- g + scale_fill_manual(values = results$brief$fcolors)
  }
  if (!quietly) {
    print(g)
  }
  return(invisible(g))
}
