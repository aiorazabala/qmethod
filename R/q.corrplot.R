q.corrplot <- function(corr.matrix, quietly = FALSE) {
  # Input validation ===========================================================
  if (!is.logical(quietly) || !is.vector(quietly) || length(quietly) != 1) {
    stop("The argument set for quietly must be a logical vector of length 1.")
  }
  if (!is.matrix(corr.matrix) || ncol(corr.matrix) != nrow(corr.matrix)) {
    stop("The corr.matrix provided is not matrix of equal rank.")
  }

  m <- melt(data = corr.matrix, value.name = "Correlation")
  Var1 <- Var2 <- Correlation <- NULL  # hack job to appease R CMD CHECK as per http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  g <- ggplot(data = m, mapping = aes(x = Var1, y = Var2, fill = Correlation, label = round(x = Correlation, digits = 1)))
  g <- g + geom_tile()
  g <- g + scale_fill_gradient2(low = "red", high = "blue", mid = "white", limits = c(-1, 1))  # make sure whole range of values is covered
  g <- g + geom_text()
  g <- g + theme(axis.title = element_blank())  # kill axis labels
  g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))  # rotate x axis labels
  if (!quietly) {
    print(g)
  }
  return(invisible(g))
}
