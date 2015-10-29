q.scoreplot.num <- function(results, factor = NULL, pointrange = "item.sd", incl.qdc = TRUE, quietly = FALSE) {

  # Input validation =========
  assert_that(class(results) == "QmethodRes")
  if (!is.null(factor)) {
    assert_that(is.scalar(factor))
    if (is.numeric(factor)) {
      assert_that(is.count(factor))
      assert_that(factor <= results$brief$nfactors)
      factor <- colnames(results$loa)[factor]  # to make sure the below always has factor as a name available
    }
    if (is.character(factor)) {
      assert_that(factor %in% colnames(results$loa))  # TODO this gives a slightly uninformative error as per https://github.com/hadley/assertthat/issues/31
    }
  }
  if (!is.null(pointrange)) {
    assert_that(is.string(pointrange))
    av.pointranges <- c("item.sd")  # available sources for pointrange
    assert_that(pointrange %in% av.pointranges)
  }
  assert_that(is.flag(incl.qdc))
  assert_that(is.flag(quietly))

  # Prepare data =======
  handle <- zsc <- sig <- NULL  # stupid hack job to appease R cmd check
  colnames(results$zsc) <- colnames(results$loa) # fix bad names

  max.z <- max(abs(results$zsc))

  factor.scores <- results$zsc
  factor.scores$handle <- rownames(x = factor.scores)  # because melt can't accept rownames
  factor.scores <- melt(data = factor.scores, value.name = "zsc", variable.name = "factor", id.vars = "handle")

  item.sd <- q.item.sd(results = results, standardize = TRUE)
  item.sd$handle <- rownames(item.sd)  # because melt can't accept rownames
  item.sd <- melt(data = item.sd, value.name = "item.sd", variable.name = "factor", id.vars = "handle")

  data <- cbind(factor.scores, item.sd$item.sd)
  colnames(data)[colnames(data) == "item.sd$item.sd"] <- "item.sd"

  if (is.null(results$brief$fcolors)) {  # when there are no colors in object
    fcolors <- q.fcolors(results = results)$brief$fcolors  # calculate them
    warning(
      "Because no colors were found in results object, they were generated on the fly.
      Colors are not saved to results object and may be inconsistent between plots.
      Run q.fcolors() to assign consistent colors."
    )
  } else {  # when there are colors in object
    fcolors <- results$brief$fcolors  # use them
  }
  names(fcolors) <- colnames(results$loa)  # ABSOLUTELY need named fcolors for later consistency of qdc lines


  # Plotting ===========
  g <- NULL
  # notice that because there is no errorbar horizontal, all the x/y are flipped in the below, until coord_flip
  if (!is.null(factor)) {
    position <- "identity"  # no jitter needed for only one factor
    g <- ggplot(data = data[data$factor == factor, ], mapping = aes(x = reorder(handle, zsc), y = zsc, color = factor, group = factor))
  } else {
    g <- ggplot(data = data, mapping = aes(x = handle, y = zsc, color = factor, group = factor))
    position <- "jitter"  # otherwise all errorbars overlap
    data
  }
  if (is.null(pointrange)) {
    g <- g + geom_point(size = 3, position = position)
    g
    g <- g + ylim(-max.z, max.z)
  } else if (pointrange == "item.sd") {
    g <- g + geom_pointrange(mapping = aes(ymin = zsc - item.sd, ymax = zsc + item.sd), position = position)
    #g <- g + ylim(-max.z - max(data$item.sd), max.z + max(data$item.sd))  # this does not work oddly
  }
  if (incl.qdc) {
    # TODO kill the below once results of qdc are available in better format
    qdc.data <- results$zsc
    qdc.data[factor] <- NULL  # these are already plotted
    qdc.data$handle <- rownames(qdc.data)
    qdc.data <- melt(data = qdc.data, variable.name = "factor", value.name = "zsc", id.vars = "handle")
    qdc.data$sig <- NA
    for (item in rownames(results$qdc)) {
      for (other.fac in unique(qdc.data$factor)) {
        exp.strings <- c("sig", factor, other.fac)
        column <- which.max(rowSums((sapply(X = exp.strings, FUN = "grepl", colnames(results$qdc)) == TRUE)))
        # hideous hack job, really must have qdc more readily available
        if (results$qdc[item, column] == "**") {
          qdc.data[qdc.data$handle == item & qdc.data$factor == other.fac, "sig"] <- 0.01
        } else if (results$qdc[item, column] == "*") {
          qdc.data[qdc.data$handle == item & qdc.data$factor == other.fac, "sig"] <- 0.05
        }
      }
    }
    qdc.data[is.na(qdc.data$sig), "zsc"] <- NA  # kill NA values
    qdc.data$sig <- as.character(qdc.data$sig)
    g <- g + geom_point(data = qdc.data, mapping = aes(x = handle, y = zsc, shape = sig, color = factor))
  }
  g <- g + coord_flip()
  g <- g + scale_colour_manual(
      values = fcolors,
      name = "Factor"
  )
  g <- g + scale_shape(solid = FALSE, name = "Signicance Level of Difference")
  g <- g + labs(x = "Item Handle", y = "Standardized Factor Score")
  if(!quietly) {
    plot(g)
  }
  return(g)
}
