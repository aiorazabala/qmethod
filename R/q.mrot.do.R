q.mrot.do <- function(results, rot.mat, quietly = FALSE) {  # this function simply implements some rotation matrix
  # Validation =================================================================
  if (class(results) != "QmethodRes") {  # only accept results object
    stop("The object provided is not of class 'QmethodRes'.")
  }
  if (results$brief$reorder) {
    stop(
      "This function is incompatible with automatically reordered results.
      Please use 'reorder = FALSE' in 'qmethod'."
    )
  }
  if (!is.logical(quietly) || !is.vector(quietly) || length(quietly) != 1) {
    stop("The argument set for quietly must be a logical vector of length 1.")
  }

  # test whether the rotation matrix is actually proper
  if (!is.matrix(rot.mat)) {
    stop("rot.mat is not a matrix.")
  }
  if (!ncol(rot.mat) == nrow(rot.mat)) {
    stop("rot.mat is not a square matrix.")
  }
  if (!results$brief$nfactors == ncol(rot.mat)) {
    stop("The rank of rot.mat must be equal to the number of factors in results.")
  }
  if (!all(round(as.matrix(rot.mat) %*% t(as.matrix(rot.mat)), digits = 10) == diag(results$brief$nfactors))) { # rounding is necessary because of
    stop("The rot.mat is not an orthogonal matrix as is necessary for rotation.")
  }
  if (!results$brief$distro) {
    stop("This function currently only works for forced distributions.")
    # TODO(maxheld83) this is actually needless, easy fix https://github.com/aiorazabala/qmethod/issues/246
  }

  # rotate only, if there IS something to rotate
  if (all(rot.mat == diag(nrow(rot.mat)))) {
    warning("The rot.mat is an identity matrix; there is no rotation to do.")
    results.rot <- results  # no change whatsoever
  } else {
    # apply names ================================================================
    # notice that logically, names should always (if not manually q.fnamed) stem from q.mrot.choose - because the ROTATION defines the names.
    if (is.null(dimnames(rot.mat))) {  # if there are NO dimnames
      colnames(rot.mat) <- paste0("mRC", 1:results$brief$nfactors)  # names must STILL change, because rotations change the context!
      rownames(rot.mat) <- paste0("mRC", 1:results$brief$nfactors)  # names must STILL change, because rotations change the context!
      warning("The rows and columns in 'rot.mat' were unnamed. Rotated factors and all downstream results were renamed automatically. Use 'q.fnames()' to change names. ")
    } else {  # of there ARE dimnames, take them
      if (!all(colnames(rot.mat) == rownames(rot.mat))) {  # test whether they are the same, and in the same ORDER!
        stop("The row and column names of the rot.mat are not the same or are not in the same order.")
      }
    }
    results <- suppressWarnings(q.fnames(results = results, fnames = colnames(rot.mat)))  # rename either way!


    # apply the rotation =========================================================

    # matrix mult
    loa.orig <- as.matrix(results$loa)  # notice that the loa.orig need NOT be unrotated; any non-reordered is fine.
    loa.rot <- loa.orig %*% rot.mat

    # make loa.rot amenable for qmethod
    loa.rot <- as.data.frame(loa.rot)
    names(loa.rot) <- names(results$loa)  # this is taking the OLD names again, for now

    flags.rot <- qflag(loa = loa.rot, nstat = nrow(results$dataset))
    # TODO(maxheld83) use other flagging method here -> https://github.com/aiorazabala/qmethod/issues/167

    results.rot <- qzscores(dataset = results$dataset, nfactors = results$brief$nfactors, forced = results$brief$distro, flagged = flags.rot, loa = loa.rot)
    # TODO(maxheld83) here we need the distribution from somewhere -> https://github.com/aiorazabala/qmethod/issues/246
    # TODO(maxheld83) use other weighting method here -> https://github.com/aiorazabala/qmethod/issues/166

    results.rot$brief$cor.method <- results$brief$cor.method  # qzscores (above) otherwise forgets about cor method, though cor method is NOT unknown here -> https://github.com/aiorazabala/qmethod/issues/278

    results.rot[[8]] <- qdc(dataset = results$dataset, nfactors = results$brief$nfactors, zsc=results.rot[[5]], sed=as.data.frame(results.rot[[7]][[3]]))  # this is just duplicate code from qmethod

    results.rot$brief$flagging <- "automatic"  # until/unless specified otherwise, this is correct
    results.rot$brief$reorder <- FALSE  # must always be false, will otherwise error out (above)

    results.rot$brief$rotation <- "by-hand"  # make sure the results object always "knows" how/if it was rotated

    results.rot$brief$rotmat <- results$brief$rotmat %*% rot.mat  # write the rots into the results
      # notice that old rotmat must be multiplied with new rotmat to keep results up to date

    results.rot$brief <- results.rot$brief[names(results$brief)]  # reorder list elements to make sure they are in the same order
  }

  if (quietly) {
    return(invisible(results.rot))
  } else {
    q.rotplot(results = results.rot, quietly = FALSE)
    return(results.rot)
  }
}
