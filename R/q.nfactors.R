q.nfactors <- function(dataset, q.matrix = NULL, cutoff = NULL, siglevel = 0.05) {
  # TODO(maxheld83) add input verification

  if (is.null(q.matrix)) {  # if not specified ...
    q.matrix <- cor(x = dataset, method = "spearman")  # use spearman
  }
  if (is.null(cutoff)) {  # if not specified ...
    cutoff <- ncol(dataset)/2  # take half
  }

  q.matrix <- cor$before
  dataset <- q_sorts[,,"before"]
  cutoff <- 6
  siglevel <- 0.05

  howmany <- NULL  # set up empty results object
  # Parallel analysis (includes eigenvalues) ===
  q.paran <- paran(mat = q.matrix,
    iterations = 10000,
    quietly = FALSE,
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
  q.paran.long <- cbind(q.paran$Ev, q.paran$RndEv, q.paran$AdjEv, deparse.level = 2)  # put lists in dataframe
  q.paran.long <- q.paran.long[1:cutoff,]  # drop all below cutoff
  colnames(q.paran.long) <- c("Unadjusted", "Random", "Adjusted")  # give appropriate names
  q.paran.long <- melt(data = q.paran.long)  # make long
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
  # TODO(maxheld83) add source for legend
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
    library(GGally)
    q.residuals.plots[[i]] <- ggcorr(data = as.matrix(q.residuals[,,2]), label = TRUE, geom = "tile") + ggtitle(paste("Residual Correlations after", i, "Factors"))
  }
  howmany$communalities <- communalities  # store results
  howmany$residuals <- q.residuals
  howmany$residuals.plots <- q.residuals.plots

  # Bartlett's (et al.) test ===
  q.Bartlett <- nBartlett(x = q.matrix, N = nrow(dataset), alpha = siglevel, cor = TRUE, details = TRUE, correction = TRUE)
  # TODO(maxheld83) add sources here
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
  summary <- rbind(summary, list("No Identity Matrix", howmany$Bartlett$nFactors[1], "Bartlett"))
  # TODO(maxheld83) add proper bartlett ref
  row.names(summary)[4:5] <- c("Paran", "Bartlett")
  howmany$summary <- summary  # save output

  # Prints ===
  print(summary)
  grid.arrange(g, p)
  return(howmany)
}
