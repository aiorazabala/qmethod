qmethod <- function(dataset, nfactors, extraction="PCA", rotation="varimax", forced=TRUE, distribution=NULL, cor.method="pearson", silent=FALSE, spc= 10^-5, ...) {
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
    if (sum(apply(dataset, 2, function(x) sort(x) != qscores)) > 0) stop("Q method input: The argument 'forced' is set as 'TRUE', but your data contains one or more Q-sorts that do not to follow the same distribution. 
 For details on how to solve this error, see 'help(qmethod)', including Note.")
  }
  if (!forced) {
    if (is.null(distribution)) stop("Q method input: The argument 'forced' is set as 'FALSE', but no distribution has been provided in the argument 'distribution'. 
 For details on how to solve this error or specify the argument 'distribution', see 'help(qmethod)', including Note number 2.")
    if (length(distribution) != nrow(dataset)) stop("Q method input: The length of the distribution provided does not match the number of statements.")
    if (!is.numeric(distribution) & !is.integer(distribution)) stop("Q method input: The distribution provided contains non-numerical values.")
  }
  if (length(unique(colnames(dataset))) != nqsorts) stop("Q method input: one or more Q-sort names are duplicated. Please change the names of the dataset by using colnames().")
  if (rotation != "varimax") warning("Note that the rotation method selected is not standard in Q methodology publications.") # See discussion at https://github.com/aiorazabala/qmethod/issues/95
  uncommon.rotations <- c("quartimax", "bentlerT", "geominT", "targetT", "bifactor", "TargetT", "equamax", "varimin", "specialT", "Promax", "promax", "cluster", "biquartimin", "specialQ", "oblimin", "simplimax")
  # Run the analysis
  cor.data <- cor(dataset, method=cor.method)
  if(extraction == "PCA") {
    loa <- unclass(principal(cor.data, nfactors=nfactors, rotate=rotation, ...)$loadings) #PCA from {psych} for factor loadings
  } 
  if(extraction == "centroid") {
    loa.unr <- unclass(centroid(tmat=cor.data, nfactors=nfactors, spc))
    if(rotation == "none") { 
      loa <- loa.unr
    } else if(rotation == "varimax") { 
      loa <- unclass(varimax(loa.unr[,1:nfactors])$loadings)
    } else if(rotation %in% uncommon.rotations) {
      stop("You have selected a rotation method that is not implemented for 'centroid' extraction within the 'qmethod()' wrapper. To use uncommon rotations with 'centroid' extraction, please run the 'centroid()' function manually. The help page 'help(centroid)' indicates how to run the full analysis step-by-step.")
    }
  }
  colnames(loa) <- paste0("f", 1:ncol(loa))
  # The following depends on the qmethod functions: qflag, qzscores, qfcharact, qdc
  flagged <- qflag(loa=loa, nstat=nstat)
  qmethodresults <- qzscores(dataset, nfactors, flagged=flagged, loa=loa, forced=forced, distribution=distribution)
  if(extraction == "PCA") qmethodresults$brief$extraction <- extraction
  if(extraction == "centroid") qmethodresults$brief$extraction <- paste0(extraction, " (threshold = ", spc, ")")
  qmethodresults$brief$rotation    <- rotation
  qmethodresults$brief$flagging    <- "automatic"
  qmethodresults$brief$cor.method  <- cor.method
  qmethodresults$brief$pkg.version <- packageVersion('qmethod')
  qmethodresults$brief$info        <- c("Q-method analysis.",
                                        paste0("Finished on:               ",
                                               qmethodresults$brief$date),
                                        paste0("'qmethod' package version: ",
                                               qmethodresults$brief$pkg.version),
                                        paste0("Original data:             ",
                                               qmethodresults$brief$nstat,
                                               " statements, ",
                                               qmethodresults$brief$nqsorts, " Q-sorts"),
                                        paste0("Forced distribution:       ",
                                               qmethodresults$brief$distro),
                                        paste0("Number of factors:         ",
                                               qmethodresults$brief$nfactors),
                                        paste0("Extraction:                ",
                                               qmethodresults$brief$extraction),
                                        paste0("Rotation:                  ",
                                               qmethodresults$brief$rotation),
                                        paste0("Flagging:                  ",
                                               qmethodresults$brief$flagging),
                                        paste0("Correlation coefficient:   ",
                                               qmethodresults$brief$cor.method))
  qmethodresults[[8]] <- qdc(dataset, nfactors, zsc=qmethodresults$zsc, 
                             sed=qmethodresults$f_char$sd_dif)
  names(qmethodresults)[8] <- "qdc"
  if (silent== FALSE) cat(qmethodresults$brief$info, sep="\n")
  return(qmethodresults)
}