q.fcolors <- function(results, fcolors = NULL, color.scheme = "Set1") {
  # Input verification ===================
  if (class(results) != "QmethodRes") {  # correct class?
    stop("The object provided is not of class 'QmethodRes'.")
  }
  if (!is.null(fcolors)) {  # tests about fcolors
    if (length(fcolors) != results$brief$nfactors) {
      stop(paste0("The colors provided (", length(fcolors), ") does not match the number of factors in the results (", results$brief$nfactors, ")"))
    }
    if (!is.vector(fcolors)) {  # correct type?
      stop(
        "The factor colors specified are not a vector."
      )
    }

    # Make test function for colors (are they valid?)
    # the below is pasted from http://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
    areColors <- function(x) {
      sapply(
        x,
        function(X) {
          tryCatch(
            is.matrix(col2rgb(X)),
            error = function(e) FALSE
          )
        }
      )
    }
    if (any(!(areColors(fcolors)))) {  # if any are false
      print(areColors(fcolors))  # print a named vector of the wrong ones
      stop("At least some of the colors provided are invalid.")
    }
  } else {  # test automatic colors
    try(  # test if the color.scheme is ok, throws early, meaningful error if not.
      brewer.pal(
        n = results$brief$nfactors,
        name = color.scheme
      ),
      silent = FALSE
    )
    # the above tests internal to brewer.pal also error out if nfactors does not work with chosen palette
  }

  # Assign/Create Color Palette
  if (is.null(fcolors)) {  # if there are no colors
    fcolors <- brewer.pal(n = results$brief$nfactors, name = color.scheme)
  }
  results$brief$fcolors <- fcolors
  return(invisible(results))
}
