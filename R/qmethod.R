qmethod <- function(dataset, nfactors, rotation="varimax", forced=TRUE, distribution=NULL, cor.method="pearson", reorder = FALSE, quietly = FALSE, ...) {
  # calculate number of Q sorts and number of statements
  nstat <- nrow(dataset)
  nqsorts <- ncol(dataset)
  #threshold for significant values at p-value=.01 and p-value=.05
  thold.01 <- 2.58/sqrt(nstat)
  thold.05 <- 1.96/sqrt(nstat)
  #check that the input data is correct
  #if (nqsorts!=ncol(dataset)) stop("Q method input: The number of Q sorts introduced does not match with the number of columns of the data frame or matrix") else if (nstat!=nrow(dataset)) stop("Q method input: The number of statements introduced does not match with the number of rows of the data frame or matrix.") else

  # Validation checks
  if (nstat < 2) stop("Q method input: The data frame or matrix entered has less than two statements")
  if (nqsorts < 2) stop("Q method input: The data frame or matrix entered has less than two Q-sorts")
  if (!is.numeric(as.matrix(dataset)) & !is.integer(as.matrix(dataset))) stop("Q method input: The data frame or matrix entered has non-numerical values.")
  if (forced) {
    qscores <- sort(dataset[,1], decreasing=FALSE)
    if (sum(apply(dataset, 2, function(x) sort(x) != qscores)) > 0) stop("Q method input: The argument 'forced' is set as 'TRUE', but your data contains one or more Q-sorts that do not to follow the same distribution.")
  }
  if (!forced) {
    if (is.null(distribution)) stop("Q method input: The argument 'forced' is set as 'FALSE', but no distribution has been provided in the argument 'distribution'.")
    if (length(distribution) != nrow(dataset)) stop("Q method input: The length of the distribution provided does not match the number of statements.")
    if (!is.numeric(distribution) & !is.integer(distribution)) stop("Q method input: The distribution provided contains non-numerical values.")
  }
  if (length(unique(colnames(dataset))) != nqsorts) stop("Q method input: one or more Q-sort names are duplicated. Please change the names of the dataset by using colnames().")
  if (!is.logical(reorder) || !is.vector(reorder) || length(reorder) != 1) {
    stop("The argument set for reorder must be a logical vector of length 1.")
  }
  if (!is.logical(quietly) || !is.vector(quietly) || length(quietly) != 1) {
    stop("The argument set for quietly must be a logical vector of length 1.")
  }

  # Run the analysis ===========================================================
  cor.data <- cor(x = dataset, method=cor.method)
  pca.results <- principal(r = cor.data, nfactors = nfactors, rotate = rotation, n.obs = nrow(dataset), covar = FALSE)
  if (rotation == "none" || nfactors == 1) {  # if there is no rotation or only 1 fac, psych does not return any rot.mat
    rot.mat <- diag(x = 1, nrow = nfactors)  # ... instead, it must be an identity matrix
  } else {  # this is for all other cases
    rot.mat <- pca.results$rot.mat
  }
  if (!reorder & rotation != "none") {  # this only applies for actual rotations
    principal.unrot <- principal(r = cor.data, nfactors = nfactors, rotate = "none", n.obs = nrow(dataset), covar = FALSE)  # must recalculate unrot

    # rotate loadings
    loa.unrot <- unclass(principal.unrot$loa)
    loa <- loa.unrot %*% rot.mat

    # flip loadings
    # still need to make sure that components are not mostly *negative* as per https://github.com/aiorazabala/qmethod/issues/268
    # this is code copied from principal, must sadly here be repeated b/c we cannot take re-ordered results from principal
    # below else clause does not need this, because principal does this automatically
    signed <- sign(colSums((loa)))
    signed[signed == 0] <- 1  # special case of VERY bipolar factors
    loa <- loa %*% diag(signed)  # flips signs where appropriate

    # name loadings
    colnames(loa) <- paste0("f", 1:ncol(loa))  # TODO(maxheld83) giving proper name as per https://github.com/aiorazabala/qmethod/issues/264 would be nice
    colnames(rot.mat) <- colnames(loa)  # name rotmat same as loas
    rownames(rot.mat) <- colnames(loa)  # name rotmat same as loas
    loa <- as.data.frame(loa)  # downstream functions expect dataframe
  } else {  # this is the "old" behavior and/or if rotation is none
    loa <- as.data.frame(unclass(pca.results$loadings)) #PCA from {psych} for factor loadings
    colnames(rot.mat) <- paste0("f", 1:ncol(loa))  # TODO(maxheld83) giving proper name as per https://github.com/aiorazabala/qmethod/issues/264 would be nice
    rownames(rot.mat) <- paste0("f", 1:ncol(loa))  # TODO(maxheld83) giving proper name as per https://github.com/aiorazabala/qmethod/issues/264 would be nice
    # this is to make sure this rotmat is properly understood
    # notice that this rotmat may no longer refer directly to the rotated factors, because these are here RE-ORDERED
    # instead, it refers to the PCs (hence the names) of the UNROTATED results, which are then rotated by this rotmat, and then re-ordered
    # here is one of the MANY reasons to deprecate reorder = TRUE at some point https://github.com/aiorazabala/qmethod/issues/276
    colnames(loa) <- paste0("f", 1:ncol(loa))  # this destroys the reordered names, known, unfixable bug: https://github.com/aiorazabala/qmethod/issues/275
  }
  # The following depends on the qmethod functions: qflag, qzscores, qfcharact, qdc
  flagged <- qflag(loa=loa, nstat=nstat)
  qmethodresults <- qzscores(dataset, nfactors, flagged=flagged, loa=loa, forced=forced, distribution=distribution)
  qmethodresults$brief$rotation <- rotation
  qmethodresults$brief$rotmat <- rot.mat
  qmethodresults$brief$reorder <- reorder  # object must know if it was reordered for compatibility reasons with q.mrot.do etc.
  qmethodresults$brief$flagging <- "automatic"
  qmethodresults$brief$cor.method <- cor.method
  qmethodresults$brief$info <- c("Q-method analysis.",
                                 paste0("Finished on:             ",
                                        qmethodresults$brief$date),
                                 paste0("Original data:           ",
                                        qmethodresults$brief$nstat,
                                        " statements, ",
                                        qmethodresults$brief$nqsorts, " Q-sorts"),
                                 paste0("Forced distribution:     ",
                                        qmethodresults$brief$distro),
                                 paste0("Number of factors:       ",
                                        qmethodresults$brief$nfactors),
                                 paste0("Rotation:                ",
                                        qmethodresults$brief$rotation),
                                 paste0("Flagging:                ",
                                        qmethodresults$brief$flagging),
                                 paste0("Correlation coefficient: ",
                                        qmethodresults$brief$cor.method))
  qmethodresults[[8]] <- qdc(dataset, nfactors, zsc=qmethodresults[[5]], sed=as.data.frame(qmethodresults[[7]][[3]]))
  names(qmethodresults)[8] <- "qdc"
  if (!quietly) {
    cat(qmethodresults$brief$info, sep="\n")
  }
  return(qmethodresults)
}
