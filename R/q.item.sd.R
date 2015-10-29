q.item.sd <- function(results, standardize = FALSE) {
	# calculates the sd of an item on a factor, weighted by loadings

  # Input validation
  if (class(results) != "QmethodRes") {
    stop("The object provided is not of class 'QmethodRes'.")
  }
  assert_that(is.flag(standardize))

  results$dataset <- as.matrix(results$dataset, TRUE)  # because we need to protect against dfs

  if (standardize) {
    # base::scale cannot be used for standardizing dataset,
    #  - because it uses sd, not pop.sd, leading to subtle differences
    #  - because it always standardizes per column
    sd.overall <- pop.sd(x = results$dataset)  # pop.sd because this is not a sample
    # notice that doing an OVERALL sd and mean is harmless if distros are forced, but retains skew/asymmetry if distros are free
    mean.overall <- mean(x = results$dataset)
    # results$dataset <- results$dataset <- mean.overall
    # there is NO centering done here actually, as per https://github.com/aiorazabala/qmethod/issues/325
    results$dataset <- results$dataset/sd.overall
  }

  # Calculate item standard deviation
  wraw_all <- qfwe(  # take all objects from existing qmethod result
    dataset = results$dataset,
    loa = results$loa,
    flagged = results$flagged
  )  # notice that all of the below depends on a SPECIFIC qmethod result, hence the importing
  item_sd <- data.frame(cbind(rep(x = NA, results$brief$nstat)))
    # make empty df
  rownames(item_sd) <- row.names(results$dataset)
  for (i in 1:results$brief$nfactors) {  # loop over the number of factors
    # here comes the sd across all sorts flagged on a factor
    if (all(wraw_all[[i]] == 0)) {
      # in case all scores are zero, which happens when NO person is automatically flagged
      # this creates evil NaN errors
      # this should not happen to begin with, and automatic flagging from within rotations for q.scoreplot.ord should be switched off https://github.com/aiorazabala/qmethod/issues/167
      item_sd[ , i] <- NA
    } else { # if there ARE any scores at all
      wraw_all_flagged <- wraw_all[[i]][ , which(!apply(wraw_all[[i]]==0, 2, all)), drop=FALSE]  # choose only flagged, drop must be FALSE in case there is only one flagged, which causes errors downstream
      item_sd[ , i] <- apply(X = wraw_all_flagged, MARGIN = 1, FUN = pop.sd) # make pop sd
    }
  }
  if (colnames(results$loa)[1] == "f1") { # hack to figure out if results object has been q.fnamed()
    colnames(item_sd) <- paste("item_sd_", c(1:results$brief$nfactors), sep="")  # number if unnambed
  } else {
    colnames(item_sd) <- colnames(results$loa)  # take named colnames otherwise
  }
  return(item_sd)
}
