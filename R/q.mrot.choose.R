q.mrot.choose <- function(results, plot.type = "q.rotplot", plot.all = FALSE, file = "", label.scale = 300) {
  # Input validation ==========================================================
  if (!interactive()) {
    stop("This function can only be used interactively, because it requires user input.")
  }
  if (class(results) != "QmethodRes") {  # only accept results object
    stop("The object provided is not of class 'QmethodRes'.")
  }
  if (results$brief$reorder) {
    stop(
      "This function is incompatible with automatically reordered results.
      Please use 'reorder = FALSE' in 'qmethod'."
    )
  }
  available.plots <- c("q.rotplot", "q.loaplot", "base")
  if (!is.character(plot.type) || !is.vector(plot.type) || length(plot.type) != 1 || !(plot.type %in% available.plots)) {
    stop(paste0(
      "The argument set for 'plot.type' must be a character vector of length 1 from the available plots ",
      available.plots,
      "."
    ))
  }
  if (!is.logical(plot.all) || !is.vector(plot.all) || length(plot.all) != 1) {
    stop("The argument set for 'plot.all' must be a logical vector of length 1.")
  }


  # Set up data structure =====================================================

  # find combinations
  combs <- combn(x = colnames(results$loa), m = 2, simplify = TRUE)  # these are the combinations, duplicate code to other functions
  combs <- t(combs)  # this is the stuff from which to rotate
  combs <- as.data.frame(combs, stringsAsFactors = FALSE)
  rownames(combs) <- apply(X = combs, MARGIN = 1, FUN = paste, collapse = "-")
  combs <- cbind(combs, 1:nrow(combs))  # just for easy access
  colnames(combs) <- c("x-horizontal", "y-vertical", "row number")

  # create rotations matrix
  rot.mat <- diag(x = results$brief$nfactors)
  colnames(rot.mat) <- colnames(results$loa)  # name as they have been named
  rownames(rot.mat) <- colnames(results$loa)


  # Helper functions ==========================================================
  simplify.angle <- function(angle.raw) {  # simplify angles to -180 to 180
      angle.360 <- ((angle.raw*pi/360) %% pi)*360/pi
      if (angle.360 > 180) {
        angle.simple <- angle.360 - 360
      } else if (angle.360 < -180) {
        angle.simple <- angle.360 + 360
      } else {
        angle.simple <- angle.360
      }
      return(invisible(angle.simple))
  }

  # Plotting functions  ========================================================
  plot.rot <- function(results, plot.type, combs, label.scale = label.scale) {
    g <- NULL  # just to be safe that this is empty
    # notice that this, again, computes *all* plots, irrespective of whether they are called; inefficient but makes for cleaner, simpler code
    if (plot.type == "q.loaplot") {
      loaplots <- q.loaplot(results = results, quietly = TRUE)  # run quietly
      for (n in rownames(combs)) {  # retrieve only those that correspond to the combinations in rots
        g[["pairs"]][[n]] <- loaplots[[combs[n,"x-horizontal"]]][[combs[n,"y-vertical"]]]
      }
      g$all <- do.call(what = "arrangeGrob", args = c(g$pairs, ncol = ceiling(sqrt(length(g$pairs)))))  # arrange them together
      # notice that the g$all object CANNOT be plot()ed or print()ed, but requires a grid.draw(), maybe with a grid.newpage() to flush the plot, as per http://stackoverflow.com/questions/31463445/how-do-i-get-rid-of-random-background-grid-from-arrangegrob?noredirect=1#comment50896311_31463445
    } else if (plot.type == "q.rotplot") {
      g <- q.rotplot(results = results, quietly = TRUE, label.scale = label.scale)
    }
    # TODO(maxheld83) add specific functions for other plots here
    return(invisible(g))
  }

  base.plot <- function(loa, pair, combs) {
    plot(loa[, c(combs[pair, c("x-horizontal")], combs[pair, c("y-vertical")])], xlim = c(-1,1), ylim = c(-1,1), asp = 1)
    text(loa[, c(combs[pair, c("x-horizontal")], combs[pair, c("y-vertical")])], labels = rownames(loa))
  }

  plot.wrapper <- function(results, pair, combs, plot.type, plot.all, label.scale = label.scale) { # this just wraps all the plot commands for simplification
    if (plot.all) {pair <- "all"}  # in case user always wants all plots, set pair to all
    loa <- results$loa
    if (plot.type == "base") { # here come the base plots
      if (pair == "all") {  # here is the all-in-one
        par(mfrow = c(ceiling(sqrt(nrow(combs))), ceiling(sqrt(nrow(combs)))))  # set appropriate grid
        for (i in rownames(combs)) {  # again, loop over all combinations, now for combined plot
          # for loop is necessary ONLY for base plots, because other plots use results object
          base.plot(loa = loa, pair = i, combs = combs)
        }
        par(mfrow = c(1,1))  # reset grid
      } else {  # print individually
        grid.newpage()
        base.plot(loa = loa, pair = pair, combs = combs)
      }
    } else {  # this is for all the non-base plots
      if (pair == "all") {  # here is the all-in-one
        grid.newpage()  # clean up plot just in case
        grid.draw(plot.rot(results = results, plot.type = plot.type, combs = combs, label.scale = label.scale)$all)  # draw all
      } else {  # print individually
        grid.newpage()
        plot(plot.rot(results = results, plot.type = plot.type, combs = combs, label.scale = label.scale)$pairs[[pair]])  # plot is the one method that works both for ggplot objects and arrangeGrid objects, both can be the result here.
      }
    }
  }  # returns nothing, just plots things


  # Caching ====================================================================
  # forget(mem.plot.rot)  # these would serve to invalidate the cache, seems unecessary.
  # forget(mem.rotate)  # these would serve to invalidate the cache, seems unecessary.
#   cache.pair <- function(f.pair, results, rots) {
#     cat("Caching integer degree rotations for factor pair", as.character(rots[f.pair, "x"]), "and", as.character(rots[f.pair, "y"]), "...", fill = TRUE)
#     pb <- txtProgressBar(min = -180, max = 180, style = 3, initial = -180)
#     for (i in -180:180) {
#       rots[f.pair, "rotated.angle"] <- i
#       results.rot.cache <- mem.rotate(results = results, rots = rots)
#       g.cache <- mem.plot.rot(results = results.rot.cache, plot.type = plot.type)
#       # tried plotting some of the ggplot2 plots here, slows done caching too much
#       # base plotting is ok, but only if in quartz, so we're leaving that out, too
#       setTxtProgressBar(pb = pb, value = i)
#       # TODO(maxheld83) create video somewhere here -> https://github.com/aiorazabala/qmethod/issues/174
#     }
#     # this function does not return anything, has plenty of side effects though (caching)
#   }


  # Interactive procedure =====================================================

  results.rot <- results  # just so there is something to look at for the first time

  done.all <- FALSE  # we begin by assuming that people are NOT done with all pairs:)
  while (!done.all) {
    repeat { # as long as users have NOT entered a blank f.pair

      # UPDATE on selecting f.pair
      cat("You are in manual rotation, based on a", results.rot$brief$rotation, "rotation.", fill = TRUE)
      plot.wrapper(results = results.rot, pair = "all", plot.type = plot.type, plot.all = plot.all, combs = combs, label.scale = label.scale)

      # SELECT pair
      cat("Which factor pair would you like to rotate?", fill = TRUE)
      print(combs)
      pair <- NULL  # let's assume there is NO factor pair selected
      while (!(any(pair %in% row.names(combs), pair == ""))) {
        pair <- readline(prompt = "Enter row number, blank to complete or 'Esc' to abort: ")
        if (pair %in% combs[, "row number"]) {
          pair <- rownames(combs)[as.numeric(pair)]
        }
      }
      if (pair == "") break

      # ITERATE angle on f.pair
      angle <- 0
      done.pair <- FALSE  # assume it's not done
      while (!done.pair) {
        plot.wrapper(results = results.rot, pair = pair, plot.type = plot.type, plot.all = plot.all, combs = combs, label.scale = label.scale)  # print only current pair, unless overwritten by plot.all

        repeat {  # as long as users have not entered a blank angle

          # UPDATE on selecting angle
          cat("\r", "You are rotating factor pair", pair, "currently at", angle, "degrees.", fill = TRUE)
          flush.console()

          # SELECT angle
          angle.change <- NULL
          while (is.null(angle.change) || !(!suppressWarnings(is.na(as.numeric(angle.change))) || angle.change == "")) {
            angle.change <- readline(prompt = "Enter a change in angle to rotate, blank to complete or 'Esc' to abort: ")
          }
          flush.console()
          if (angle.change == "") break

          # IMPLEMENT angle
          angle <- angle + as.numeric(angle.change)  # readline does not capture numerics
          angle <- simplify.angle(angle.raw = angle)
          radians <- angle * pi / 180 # because rotation matrices are build from radians, not angles
          rot.mat.angle <- diag(results$brief$nfactors)  # make identity matrix
          dimnames(rot.mat.angle) <- dimnames(rot.mat)  # take over dimnames
          rot.mat.angle[combs[pair,"y-vertical"], combs[pair,"y-vertical"]] <- cos(radians)
          rot.mat.angle[combs[pair,"y-vertical"], combs[pair,"x-horizontal"]] <- -sin(radians)
          rot.mat.angle[combs[pair,"x-horizontal"], combs[pair,"y-vertical"]] <- sin(radians)
          rot.mat.angle[combs[pair,"x-horizontal"], combs[pair,"x-horizontal"]] <- cos(radians)

          rot.mat.pair <- rot.mat %*% rot.mat.angle  # take original rot.mat, and multiply by current angle
          # this yields the preliminary rot.mat for the current pair
          # notice that ADDING up the angles between iterating over angles happens in the above angle addition!

          results.rot <- q.mrot.do(results = results, rot.mat = rot.mat.pair, quietly = TRUE)

          # PLOT angle
          plot.wrapper(results = results.rot, pair = pair, plot.type = plot.type, plot.all = plot.all, combs = combs, label.scale = label.scale)
        }

        # CONFIRM done pair?
        done.pair <- NULL
        while (is.null(done.pair) || !(done.pair == "y" || done.pair == "n")) {
          cat("Do you really want to finish rotating factor pair", pair, "?", fill = TRUE)
          done.pair <- readline(prompt = "Enter 'y' to finish and save, 'n' to continue or 'Esc' to abort: ")
        }

        # IMPLEMENT done pair
        if (done.pair == "y") {
          done.pair <- TRUE
          rot.mat <- rot.mat.pair  # NOW we're writing out the current rot.mat.pair
          pair <- NULL  # clean f.pair
        } else if (done.pair == "n") {
          done.pair <- FALSE
        }
      }
    }

    # UPDATE on done all?
    plot.wrapper(results = results.rot, pair = "all", plot.type = plot.type, plot.all = plot.all, combs = combs, label.scale = label.scale)

    # CONFIRM done all?
    done.all <- NULL
    while (is.null(done.all) || !(done.all == "y" || done.all == "n")) {
      cat("Do you really want to finish manual rotation?", fill = TRUE)
      done.all <- readline(prompt = "Enter 'y' to finish and save, 'n' to continue or 'Esc' to abort: ")
    }

    # IMPLEMENT done all
    if (done.all == "y") {
      done.all <- TRUE

      # NAME rotated factors
      # notice that it makes sense to create names on completed rotations; THAT's what's defining the factors
      # these names are then passed on to q.fnames in q.mrot.do.R, IF THERE ARE NAMES
      done.naming <- NULL
      while (is.null(done.naming) || !(done.naming == "y")) {
        names <- NULL
        while (is.null(names) || !(any(names == "") || length(names) == results.rot$brief$nfactors)) {
          cat("Do you want to rename the rotated factors", colnames(results.rot$loa), "(recommended)?", fill = TRUE)
          raw.names <- readline(prompt = "Enter comma-separated factor names in the order given, leave blank for no names, or press 'Esc' to abort: ")
          if (raw.names == "") {
            names <- raw.names
          } else {
            split.names <- unlist(strsplit(x = raw.names, split = ","))  # split string
            names <- trimws(split.names)  # kill whitespace
          }
        }
        if (any(names == "")) {
          cat("Are you sure you want to leave the rotated factors unnamed (not recommended)?")
        } else {
          colnames(rot.mat) <- names
          rownames(rot.mat) <- names
          results.rot <- q.mrot.do(results = results, rot.mat = rot.mat, quietly = TRUE)  # implement WITH names
          plot(q.rotplot(results = results.rot, quietly = TRUE)$all)
          cat("Are the rotated factors named correctly?")
        }
        done.naming <- readline(prompt = "Enter 'y' to accept, 'n' to change names or 'Esc' to abort.")
      }
      if (any(names == "")) {
        dimnames(rot.mat) <- NULL  # just to be sure and to clean up
      }

      write.csv(x = rot.mat, file = file, row.names = TRUE)
      cat("You have completed a manual rotation. A rotation matrix has been returned.", fill = TRUE)
      cat("To change your results object, pass the rotation matrix to 'q.mrot.do'. Remember to save the rotation matrix.", fill = TRUE)
    } else if (done.all == "n") {
      done.all <- FALSE
    }
  }
  return(rot.mat)
}
