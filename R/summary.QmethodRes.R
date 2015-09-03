summary.QmethodRes <- function(object, ...) {
  cat(object$brief$info, sep="\n")
  cat("\nFactor scores\n")
  print(object$zsc, quote = FALSE)
  fch <- t(object$f_char$characteristics)
  rownames(fch) <- c(
    "Average reliability coefficient",
    "Number of loading Q-sorts",
    "Eigenvalues",
    "Percentage of explained variance",
    "Composite reliability",
    "Standard error of factor scores")
  print(fch, quote = FALSE, digits=2)
}
