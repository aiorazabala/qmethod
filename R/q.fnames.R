q.fnames <- function(results, fnames) {
  # Error checks
  if (class(results) != "QmethodRes") stop("The object provided is not of class 'QmethodRes'.")
  comb <- array(sapply(fnames, function(x) substring(x,1,1)))
  nos <- 0:9
  if (!is.vector(fnames)) {  # correct type?
    stop(
      "The factor names specified are not a vector."
    )
  }
  if(sum(comb %in% nos) > 0) stop("The names should not begin with a number")
  if (length(fnames) != results$brief$nfactors) stop(paste0("The names provided (", length(fnames), ") does not match the number of factors in the results (", results$brief$nfactors, ")"))
  if (max(nchar(fnames)) > 50) stop("The names provided are longer than 50 characters.")

  # are any factors named yet? see #https://github.com/aiorazabala/qmethod/issues/241
  if (any(colnames(results$loa) == c("f1", "f2", "f3"))) {
    warning("At least some of the factors in results were already named. Old names have been overwritten.")
  }

  # Change factor names for meaningful names
  q.objects <- c("loa", "flagged", "zsc", "zsc_n")
  for (i in q.objects) colnames(results[[i]]) <- fnames
  # Factor characteristics
  rownames(results[[7]]$characteristics) <- fnames
  dimnames(results[[7]]$cor_zsc) <- list(fnames, fnames)
  dimnames(results[[7]]$sd_dif)  <- list(fnames, fnames)
  # Factor colors
  if (!is.null(results$brief$fcolors)) {  # when there are factor colors
    names(results$brief$fcolors) <- fnames
  }

  # Rename QDC columns and cells
  qdc <- results$qdc
  for (i in 1:length(fnames)) {
    qdc$dist.and.cons <- gsub(pattern = paste0("f", i), replacement = fnames[i], x = qdc$dist.and.cons)
    colnames(qdc) <- gsub(pattern = paste0("f", i), replacement = fnames[i], x = colnames(qdc))
  }
  results$qdc <- qdc

  # Rename rotmat columns, rows, if it exists
  # must check if exists, because qmethod < 1.4 did not produce it
  if (exists(x = "results$brief$rotmat")) {  #
    colnames(results$brief$rotmat) <- fnames
    rownames(results$brief$rotmat) <- fnames
  }

  return(invisible(results))
}
