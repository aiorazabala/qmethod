q.nfactors <- function(dataset, q.matrix = NULL, cutoff = NULL, siglevel = 0.05, quietly = FALSE) {
  # Input verification
  if (!is.logical(quietly) || !is.vector(quietly) || length(quietly) != 1) {
    stop("The argument set for quietly must be a logical vector of length 1.")
  }
  if (!is.numeric(as.matrix(dataset)) & !is.integer(as.matrix(dataset))) {
    stop("Q method input: The data frame or matrix entered has non-numerical values.")
  }  # tests is from qmethod
  if (!is.null(q.matrix)) {
    if (!is.matrix(q.matrix)) {
      stop("The specified q.matrix is not a matrix.")
    }
    if (!is.numeric(q.matrix)) {
      stop("The specified q.matrix is not a matrix.")
    }
    if (ncol(q.matrix) != nrow(q.matrix)) {
      stop("The specified q.matrix is not square.")
    }
  }
  if (!is.null(cutoff)) {
    if (cutoff > ncol(dataset)) {
      stop("The specified cutoff is too large.")
    }
    if (!is.vector(cutoff) & !is.integer(cutoff)) {
      stop("The specified cutoff is not an integer vector.")
    }
    if (length(cutoff) != 1) {
      stop("The specified cutoff is not a vector of length 1.")
    }
  }
  if (!is.null(siglevel)) {
    if (length(siglevel) != 1) {
      stop("The specified siglevel is not a vector of length 1.")
    }
    if (!is.vector(siglevel) & !is.numeric(siglevel)) {
      stop("The specified siglevel is not a numeric vector.")
    }
    if (!(0 < siglevel & siglevel < 1)) {
      stop("The specified siglevel is between 0 and 1.")
    }
  }

  # Compute defaults
  if (is.null(q.matrix)) {  # if not specified ...
    q.matrix <- cor(x = dataset, method = "spearman")  # use spearman
  }
  if (is.null(cutoff)) {  # if not specified ...
    cutoff <- ncol(dataset)/2  # take half
  }

  PC <- Eigenvalue <- Type <- Ncomps <- Communality <- Qsort <- Initial <- NULL  # this is a hideous hack to appease CRAN as per http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when

  howmany <- NULL  # set up empty results object
  # Parallel analysis (includes eigenvalues) ===
  q.paran <- paran(mat = q.matrix,
    iterations = 10000,
    quietly = TRUE,
    status = TRUE,
    all = FALSE,
    centile = 100 - siglevel * 100,
    graph = FALSE,
    n = nrow(dataset)
  )
  howmany$paran <- q.paran  # write out complete results object

  # Simple criteria ===
  q.simple <- c("Magic Number Seven" = 7,  # Brown (1980: 223)
                "6-8 People per Factor" = ncol(dataset)/7,  # Stenner and Watts (2012: 107)
                "Eigenvalue > 1" = sum(q.paran$Ev > 1))  # Kaiser (1960, 1970) and Guttman (1954)
  howmany$simple <- q.simple

  # Screeplot ===
  q.paran.wide <- cbind(q.paran$Ev, q.paran$RndEv, q.paran$AdjEv, deparse.level = 2)  # put lists in dataframe
  q.paran.wide <- q.paran.wide[1:cutoff,]  # drop all below cutoff
  colnames(q.paran.wide) <- c("Unadjusted", "Random", "Adjusted")  # give appropriate names
  q.paran.long <- melt(data = q.paran.wide)  # make long
  colnames(q.paran.long) <- c("PC", "Type", "Eigenvalue")  # give good names
  q.paran.long$PC <- as.ordered(q.paran.long$PC)  # real data type makes plot prettier, easier
  g <- NULL  # just testing, won't hurt
  g <- ggplot(data = q.paran.long, mapping = aes(x = PC, y = Eigenvalue))
  g <- g + geom_line(mapping = aes(group = Type, linetype = Type))  # add the screeplot
  g <- g + geom_point(mapping = aes(shape = Type))  # add points
  g <- g + theme(legend.position = "bottom")  # move legend to the bottom
  if (cutoff >= 7) {
    g <- g + geom_vline(xintercept = q.simple["Magic Number Seven"], colour = "green", show_guide = FALSE)
  }
  g <- g + geom_vline(xintercept = q.simple["6-8 People per Factor"], colour = "blue", show_guide = FALSE)
  g <- g + geom_hline(yintercept = 1, colour = "red", show_guide = FALSE)
  # TODO(maxheld83) the above still need show_guides = TRUE, but that screws up the
  howmany$screeplot <- g

  # Produce communalities and residuals ===
  communalities <- NULL
  communalities <- matrix(data = NA, nrow = cutoff, ncol = ncol(dataset), dimnames = list(1:cutoff, colnames(dataset)))  # make empty matrix
  q.residuals <- array(data = NA, dim = c(ncol(dataset), ncol(dataset), cutoff), dimnames = list(colnames(dataset), colnames(dataset), 1:cutoff))  # make empty array
  q.residuals.plots <- NULL
  for (i in 1:cutoff) { # for all vars until cutoff
    results <- principal(r = q.matrix, nfactors = i, residuals = TRUE, rotate = "none", n.obs = nrow(dataset))
    communalities[i, ] <- results$communality
    q.residuals[,,i] <- results$residual
    q.residuals.plots[[i]] <- q.corrplot(corr.matrix = q.matrix) + ggtitle(paste("Residual Correlations after", i, "Factors"))
  }
  q.residuals.plots
  howmany$communalities <- communalities  # store results
  howmany$residuals <- q.residuals
  howmany$residuals.plots <- q.residuals.plots


  # Bartlett's (et al.) test ===
  q.Bartlett <- nBartlett(x = q.matrix, N = nrow(dataset), alpha = siglevel, cor = TRUE, details = TRUE, correction = TRUE)  # this is Bartlett 1950
  howmany$Bartlett <- q.Bartlett

  # Communalities plot ===
  communalities.long <- melt(communalities)
  colnames(communalities.long) <- c("Ncomps", "Qsort", "Communality")
  communalities.long$Ncomps <- as.ordered(communalities.long$Ncomps)  # proper datatypes make prettier plots
  communalities.long$Initial <- substr(x = communalities.long$Qsort, start = 1, stop = 2)  # assign initial
  p <- ggplot(data = communalities.long, mapping = aes(x = Ncomps, y = Communality, group = Qsort, label = Initial))
  p <- p + geom_line(mapping = aes(colour = Qsort))
  p <- p + geom_point(mapping = aes(colour = Qsort))
  p <- p + theme(legend.position = "bottom")
  p <- p + geom_text(size = 4)
  # p <- p + scale_y_log10()   # maybe that's a bad idea, stacks the deck
  p <- p + geom_vline(xintercept = q.Bartlett$nFactors[1], linetype = "dashed", show_guide = FALSE)
  howmany$commplot <- p

  # Make summary ===
  summary <- data.frame(
    "Criterion" = names(q.simple),
    "NFactors" = as.integer(q.simple),
    "Source" = c("Brown (1980: 223)", "Stenner and Watts (2012: 107)", "Kaiser (1960, 1970) and Guttman (1954)"),
    row.names = c("Magic7", "CommonSense", "KaiserGuttmann"),
    stringsAsFactors = FALSE
  )
  summary <- rbind(summary, list("Parallel Analysis", q.paran$Retained, "Horn (1965)"))
  summary <- rbind(summary, list("No Identity Matrix", howmany$Bartlett$nFactors[1], "Bartlett (1950, 1951)"))
  row.names(summary)[4:5] <- c("Paran", "Bartlett")
  howmany$summary <- summary  # save output

  # Simple correlation matrix
  howmany$corr <- q.corrplot(corr.matrix = q.matrix) + ggtitle(label = "Original Correlation Matrix")

  # Make table of eigenvalues
  q.paran.wide <- as.data.frame(q.paran.wide)  # different units, so should be df
  q.paran.wide$R2 <- q.paran.wide[ , "Unadjusted"] / ncol(dataset)  # divide EV by n of vars, gives explained variance
  howmany$eigenvalues <- q.paran.wide[,c(1,4,2,3)]  # return reordered results

  # Prints ===
  if (!quietly) {
    print(summary)
    grid.arrange(g, p)
  }
  return(invisible(howmany))
}
