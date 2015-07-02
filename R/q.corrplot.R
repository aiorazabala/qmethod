q.corrplot <- function(corr.matrix) {
  m <- melt(data = corr.matrix, value.name = "Correlation")
  Var1 <- Var2 <- Correlation <- NULL  # hack job to appease R CMD CHECK as per http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  g <- ggplot(data = m, mapping = aes(x = Var1, y = Var2, fill = Correlation, label = round(x = Correlation, digits = 1)))
  g <- g + geom_tile()
  g <- g + scale_fill_gradient2(low = "red", high = "blue", mid = "white", limits = c(-1, 1))  # make sure whole range of values is covered
  g <- g + geom_text()
  g <- g + theme(axis.title = element_blank())  # kill axis labels
  return(g)
}
