q.item.sd <- function(results) {
	# calculates the sd of an item on a factor, weighted by loadings

  # Input validation
  if (class(results) != "QmethodRes") {
    stop("The input needs to be an object of class QmethodRes, as produced by qmethod()")
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
    wraw_all_flagged <- wraw_all[[i]][ , which(!apply(wraw_all[[i]]==0, 2, all)), drop=FALSE]  # choose only flagged, drop must be FALSE in case there is only one flagged, which causes errors downstream
    item_sd[ , i] <- apply(X = wraw_all_flagged, MARGIN = 1, FUN = pop.sd) # make pop sd
  }
  if (colnames(results$loa)[1] == "f1") { # hack to figure out if results object has been q.fnamed()
    colnames(item_sd) <- paste("item_sd_", c(1:results$brief$nfactors), sep="")  # number if unnambed
  } else {
    colnames(item_sd) <- colnames(results$loa)  # take named colnames otherwise
  }
  return(item_sd)
}
