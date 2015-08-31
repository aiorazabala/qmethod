q.rotplot <- function(results, quietly = FALSE, label.scale = 200) {
  # Input verification =========================================================
  if (!is.logical(quietly) || !is.vector(quietly) || length(quietly) != 1) {
    stop("The argument set for quietly must be a logical vector of length 1.")
  }
  if (class(results) != "QmethodRes") {  # only accept results object
    stop("The object provided is not of class 'QmethodRes'.")
  }

  # assign colors according to rotation, if not already assigned
  # Notice that the below leaves out the default color scheme from q.fcolor() ("Set1") so as not to confuse colors with final outputs
  if (is.null(results$brief$fcolors)){
    if (results$brief$rotation == "none" && results$brief$nfactors <= 9) {  # always test for kind of rotation and maximum permissible colors in scheme
      results <- q.fcolors(results = results, color.scheme = "Pastel1")
    }
    else if (results$brief$rotation == "varimax" && results$brief$nfactors <= 12) {
      results <- q.fcolors(results = results, color.scheme = "Set3")
    }
    else if (results$brief$rotation == "quartimax" && results$brief$nfactors <= 9) {
      results <- q.fcolors(results = results, color.scheme = "Set2")
    }
    else if (results$brief$rotation == "promax" && results$brief$nfactors <= 8) {
      results <- q.fcolors(results = results, color.scheme = "Pastel2")
    }
    else if (results$brief$rotation == "oblimin" && results$brief$nfactors <= 8) {
      results <- q.fcolors(results = results, color.scheme = "Dark2")
    }
    else if (results$brief$rotation == "simplimax" && results$brief$nfactors <= 8) {
      results <- q.fcolors(results = results, color.scheme = "Accent")
    }
    else if (results$brief$rotation == "cluster" && results$brief$nfactors <= 8) {
      results <- q.fcolors(results = results, color.scheme = "Accent")  # we're running out of color schemes, hence the same
    }
    else if (results$brief$rotation == "by-hand" && results$brief$nfactors <= 12) {
      results <- q.fcolors(results = results, color.scheme = "Paired")
    }
  }  # notice that if new rotations are added and/or loadings separately supplied, no consistent coloring is applied. That seems safe.

  # find combinations
  combs <- combn(x = colnames(results$loa), m = 2, simplify = FALSE)  # the below couple of lines are unfortunately duplicate code from q.loaplot()

  # make plots
  loaplots <- q.loaplot(results = results, quietly = TRUE)
  compplot <- q.compplot(results = results, quietly = TRUE)
  scoreplots <- q.scoreplot(results = results, incl.qdc = FALSE, quietly = TRUE, label.scale = label.scale)  # Let's not make this more complicated

  rotplots <- loaplots.pairs <- NULL  # allow the object

  # make the pairwise plots
  for (c in combs) { # loop over factor pairs
    name <- paste(c[1], c[2], sep = "-")
    rotplots[["pairs"]][[name]] <- arrangeGrob(scoreplots[[c[2]]], loaplots[[c[1]]][[c[2]]], compplot, scoreplots[[c[1]]], nrow = 2)
    loaplots.pairs[[name]] <- loaplots[[c[1]]][[c[2]]]
  }

  # make all-in-one-plots
  n.total.plots <- length(combs) + results$brief$nfactors + 1
  rotplots[["all"]] <- do.call(what = "arrangeGrob", args = c(loaplots.pairs, scoreplots, list(compplot), nrow = ceiling(sqrt(n.total.plots))))

  if (!quietly) {
    grid.newpage()
    grid.draw(rotplots$all)
  }
  return(invisible(rotplots))
}
