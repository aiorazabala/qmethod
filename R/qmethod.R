qmethod <- function(dataset, nfactors, rotation="varimax", forced=TRUE, distribution=NULL, cor.method="pearson",...) {
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

  # Run the analysis ===========================================================

  # find unrotated loadings
  cor.data <- cor(dataset, method=cor.method)
  pca.results <- principal(r = cor.data, nfactors = nfactors, rotate = "none", n.obs = nrow(dataset), covar = FALSE)
  loa.unrot <- unclass(pca.results$loadings)

  # rotate loadings and store rot.mat
  if (rotation == "none") {
    loa <- loa.unrot  # just take the unrotated loadings
    rot.mat <- diag(nfactors)  # if there is no rotation, an identity matrix is the rot.matrix
  } else if (rotation == "varimax") {
  # the following tries to replicate EXACTLY what psych::principal (v.1.5.6) does internally for legacy reasons
  # prior versions of qmethod (< 1.4.0) here only used the rotation from within psych::principal
  # separate rotation procedures were necessary in 1.4.0 to have a rot.mat as per https://github.com/aiorazabala/qmethod/issues/171
  # from psych::principal doc:
  #  > Rotations and transformations are either part of psych (Promax and cluster), of base R (varimax), or of GPArotation (simplimax, quartimax, oblimin).
    varimax.results <- varimax(x = loa.unrot)
    # using base::varimax, because psych::principal does
    # see how in the psych::principal source, stats::varimax is also called with defaults
    # > varimax = {rotated <- stats::varimax(loadings)  #varimax is from stats, the others are from GPArotation
    # > loadings <- rotated$loadings},
    loa <- unclass(varimax.results$loadings)
    rot.mat <- varimax.results$rotmat
  } else if (rotation == "promax") {  # this is done using psych functions as per manual
    promax.results <- Promax(x = loa.unrot)
    loa <- unclass(promax.results$loadings)
    rot.mat <- promax.results$rotmat
    # here too, psych::principal relies on defaults otherwise
    # > cluster = 	 {loadings <- varimax(loadings)$loadings
    # > pro <- target.rot(loadings)
    # > loadings <- pro$loadings
    # > Phi <- pro$Phi},
  } else if (rotation == "cluster") {  # this is done using psych functions as per manual
    cluster.results <- target.rot(x = varimax(x = loa.unrot)$loadings)
    loa <- unclass(cluster.results$loadings)
    rot.mat <- cluster.results$rotmat
    # here too, psych::principal relies on defaults otherwise
    # > cluster = 	 {loadings <- varimax(loadings)$loadings
    # > pro <- target.rot(loadings)
    # > loadings <- pro$loadings
    # > Phi <- pro$Phi},
  # remainder are all GPArotation procedures as per psych manual
  } else if (rotation == "quartimax") {  # this is orthogonal
    GPAresults <- GPForth(A = loa.unrot, method = rotation)
    loa <- unclass(GPAresults$loadings)
    rot.mat <- GPAresults$Th
  } else if (rotation %in% c("simplimax", "oblimin")) {  # these are oblique
    GPAresults <- GPFoblq(A = loa.unrot, method = rotation)
    # notice that these procedures can occassionally NOT converge and fail
    loa <- unclass(GPAresults$loadings)
    rot.mat <- GPAresults$Th
  } else {
    stop("The chosen rotation is not available in qmethod.")
  }

  loa <- as.data.frame(unclass(principal(cor.data, nfactors=nfactors, rotate=rotation, ...)$loadings)) #PCA from {psych} for factor loadings
  names(loa) <- paste0("f", 1:length(loa))
  colnames(rot.mat) <- names(loa)  # name rotmat
  rownames(rot.mat) <- names(loa)  # name rotmat
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
  cat(qmethodresults$brief$info, sep="\n")
  # Will this cat() fill the screen when applying bootstrap?
  return(qmethodresults)
}
