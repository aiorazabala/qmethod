qmethod <- function(dataset, nfactors, rotation="varimax", forced=TRUE, distribution=NULL, cor.method="pearson", quietly = FALSE, ...) {
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
  if (!is.logical(quietly) || !is.vector(quietly) || length(quietly) != 1) {
    stop("The argument set for quietly must be a logical vector of length 1.")
  }

  # Run the analysis ===========================================================
  cor.data <- cor(x = dataset, method=cor.method)
  pca.results <- principal(r = cor.data, nfactors = nfactors, rotate = rotation, n.obs = nrow(dataset), covar = FALSE)
  if (rotation == "none") {  # if there is no rotation, psych does not return any rot.mat
    rot.mat <- diag(x = 1, nrow = nfactors)  # ... instead, it must be an identity matrix
  } else {  # this is for all other cases
    rot.mat <- pca.results$rot.mat
  }
  loa <- as.data.frame(unclass(pca.results$loadings)) #PCA from {psych} for factor loadings
  names(loa) <- paste0("f", 1:length(loa))
  colnames(rot.mat) <- names(loa)  # name rotmat same as loas
  rownames(rot.mat) <- names(loa)  # name rotmat same as loas
  # The following depends on the qmethod functions: qflag, qzscores, qfcharact, qdc
  flagged <- qflag(loa=loa, nstat=nstat)
  qmethodresults <- qzscores(dataset, nfactors, flagged=flagged, loa=loa, forced=forced, distribution=distribution)
  qmethodresults$brief$rotation <- rotation
  qmethodresults$brief$rotmat <- rot.mat
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
  # Will this cat() fill the screen when applying bootstrap?
  return(qmethodresults)
}
