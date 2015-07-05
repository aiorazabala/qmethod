q.rotate <- function(results, plot = "q.loaplot", quietly = FALSE) {
  # Input validation

  # Set up data structure
  if (results$brief$distro) {  # there is no good storage in brief for this, so must restore
    forced <- TRUE
    distribution <- NULL
  } else {
    forced <- FALSE
    distribution <- results$brief$distro
  }

  combs <- combn(x = colnames(results$loa), m = 2, simplify = TRUE)
  rots <- data.frame(t(combs))
  rots <- cbind(rots, t(apply(X = rots, MARGIN = 1, FUN = function(x) {sapply(X = x, FUN = function(x) {which(colnames(results$loa) == x, arr.ind = TRUE)})})))
  # TODO(maxheld83) there has to be a non-ugly way to do this, stupid!
  rots <- cbind(rots, 0)  # TODO(maxheld83) must read in existing angles here
  colnames(rots) <- c("x.name", "y.name", "x", "y", "angle")

  # Gather inputs
  rots[1, 5] <- -12  # f1 f2
  rots[2, 5] <- 0  # f1 f3
  rots[3, 5] <- 50 # f2 f3

  # rotate
  loa.rot <- as.matrix(results$loa)
  for (i in nrow(rots)) {
    loa.rot <- factor.rotate(f = loa.rot, angle = rots[i, "angle"], col1 = rots[i, "x"], col2 = rots[i, "y"], plot = FALSE)
  }
  loa.rot <- data.frame(loa.rot)  # qmethod does not like matrices
  colnames(loa.rot) <- colnames(results$loa)

  # re-run qmethod
  flags <- qflag(loa = loa.rot, nstat = nrow(results$dataset))  # TODO(maxheld83) use other method here
  results.rot <- qzscores(dataset = results$dataset, nfactors = results$brief$nfactors, forced = forced, distribution = distribution, flagged = flags, loa = loa.rot)  #TODO(maxheld83) use other method here

  # return intermediate plot
  q.loaplot(results.rot)

  # return final object
  results <- results.rot
  #TODO(maxheld) write kind of rotation to results object "by-hand"
  #TODO(maxheld) write angles to results object
  return(invisible(results))

}
