q.rotate <- function(results, plot.type = "q.rotplot", plot.all = TRUE, cache = FALSE) {
  # Input validation ==========================================================
  if (class(results) != "QmethodRes") {  # only accept results object
    stop("The object provided is not of class 'QmethodRes'.")
  }
  available.plots <- c("q.rotplot", "q.loaplot", "base")
  if (!is.character(plot.type) || !is.vector(plot.type) || length(plot.type) != 1 || !(plot.type %in% available.plots)) {
    stop(paste0(
      "The argument set for plot.type must be a character vector of length 1 from the available plots ",
      available.plots,
      "."
    ))
  }
  if (!is.logical(plot.all) || !is.vector(plot.all) || length(plot.all) != 1) {
    stop("The argument set for plot.all must be a logical vector of length 1.")
  }
  if (!is.logical(cache) || !is.vector(cache) || length(cache) != 1) {
    stop("The argument set for cache must be a logical vector of length 1.")
  }

  # Set up data structure =====================================================
  # retrieve arguments used to produce results, so it can be reproduced later
  if (results$brief$distro) {  # there is no good storage in brief for this, so must restore this to rerun qmethod()
    forced <- TRUE
    distribution <- NULL
  } else {
    forced <- FALSE
    distribution <- results$brief$distro
  }

  # find combinations
  combs <- combn(x = colnames(results$loa), m = 2, simplify = TRUE)  # these are the combinations, duplicate code to other functions

  # create rotations dataframe
  rots <- data.frame(t(combs), stringsAsFactors = FALSE)
  rots <- cbind(rots, 0, 0)
  # TODO(maxheld83) must read in existing angles here -> https://github.com/aiorazabala/qmethod/issues/178
  colnames(rots) <- c("x", "y", "original.angle", "rotated.angle")
  rots$name <- paste(rots[,"x"], rots[,"y"], sep = "-")  # assign names


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


  # Rotation functions =========================================================
  rotate <- function(results, rots){
    # this function always starts again with the original results loadings
    # meaning: for better reproducibility, it does NOT operate in any cumulative way

    loa.rot <- results$loa  # always start with original object
    for (i in 1:nrow(rots)) {  # loop over the factor pairs
      # yes, this is redundant in its later use, because it ALSO loops over factors that may not have changed
      # still it's easier to handle, and very robust, speed penalty is small
      loa.rot <- as.matrix(loa.rot)  # psych package rotation does not like dataframes
      loa.rot <- factor.rotate(f = loa.rot, angle = rots[i, "rotated.angle"], col1 = which(colnames(loa.rot) == rots[i, "x"]), col2 = which(colnames(loa.rot) == rots[i, "y"]), plot = FALSE)  # rotate the current factor pair
      colnames(loa.rot) <- colnames(results$loa)  # factor.rotate looses colnames, oddly
      loa.rot <- data.frame(loa.rot)  # qmethod does not like matrices
    }

    flags.rot <- qflag(loa = loa.rot, nstat = nrow(results$dataset))
    # TODO(maxheld83) use other flagging method here -> https://github.com/aiorazabala/qmethod/issues/167

    results.rot <- qzscores(dataset = results$dataset, nfactors = results$brief$nfactors, forced = forced, distribution = distribution, flagged = flags.rot, loa = loa.rot)
      # TODO(maxheld83) use other weighting method here -> https://github.com/aiorazabala/qmethod/issues/166
      # TODO(maxheld83) write rots to results object here for reproduceability

    results.rot$brief$rotation <- "by-hand"  # make sure the results object always "knows" how/if it was rotated
    results.rot$brief$rotation.angles <- rots  # write the rots into the results

    return(invisible(results.rot))
  }
  mem.rotate <- memoise(rotate)  # make cached version out of function


  # Plotting functions  ========================================================
  plot.rot <- function(results, plot.type) {
    rots <- results$brief$rotation.angles  # retrieve rots from results.rot
    g <- NULL  # just to be safe that this is empty
    # notice that this, again, computes *all* plots, irrespective of whether they are called; inefficient but makes for cleaner, simpler code
    if (plot.type == "q.loaplot") {
      loaplots <- q.loaplot(results = results, quietly = TRUE)  # run quietly
      for (n in 1:nrow(rots)) {  # retrieve only those that correspond to the combinations in rots
        g[["pairs"]][[rots[n, "name"]]] <- loaplots[[rots[n, "x"]]][[rots[n, "y"]]]
      }
      g$all <- do.call(what = "arrangeGrob", args = c(g$pairs, ncol = ceiling(sqrt(length(g$pairs)))))  # arrange them together
      # notice that the g$all object CANNOT be plot()ed or print()ed, but requires a grid.draw(), maybe with a grid.newpage() to flush the plot, as per http://stackoverflow.com/questions/31463445/how-do-i-get-rid-of-random-background-grid-from-arrangegrob?noredirect=1#comment50896311_31463445
    } else if (plot.type == "q.rotplot") {
      g <- q.rotplot(results = results, quietly = TRUE)
    }
    # TODO(maxheld83) add specific functions for other plots here
    return(invisible(g))
  }
  mem.plot.rot <- memoise(plot.rot)  # make cachable version out of function

  base.plot <- function(loa, f.pair) {
    plot(loa[, c(rots[f.pair, c("x")], rots[f.pair, c("y")])], xlim = c(-1,1), ylim = c(-1,1), asp = 1)
    text(loa[, c(rots[f.pair, c("x")], rots[f.pair, c("y")])], labels = rownames(loa))
  }

  plot.wrapper <- function(results, pair, plot.type, plot.all) { # this just wraps all the plot commands for simplification
    if (plot.all) {pair <- "all"}  # in case user always wants all plots, set pair to all
    rots <- results$brief$rotation.angles
    loa <- results$loa
    if (plot.type == "base") { # here come the base plots
      if (pair == "all") {  # here is the all-in-one
        par(mfrow = c(ceiling(sqrt(nrow(rots))), ceiling(sqrt(nrow(rots)))))  # set appropriate grid
        for (i in 1:nrow(rots)) {  # again, loop over all combinations, now for combined plot
          # for loop is necessary ONLY for base plots, because other plots use results object
          base.plot(loa = loa, f.pair = i)
        }
        par(mfrow = c(1,1))  # reset grid
      } else {  # print individually
        grid.newpage()
        base.plot(loa = loa, f.pair = pair)
      }
    } else {  # this is for all the non-base plots
      if (pair == "all") {  # here is the all-in-one
        grid.newpage()  # clean up plot just in case
        grid.draw(mem.plot.rot(results = results, plot.type = plot.type)$all)  # draw all
      } else {  # print individually
        grid.newpage()
        plot(mem.plot.rot(results = results, plot.type = plot.type)$pairs[[rots[pair, "name"]]])  # plot is the one method that works both for ggplot objects and arrangeGrid objects, both can be the result here.
      }
    }
  }  # returns nothing, just plots things


  # Caching ====================================================================
  # forget(mem.plot.rot)  # these would serve to invalidate the cache, seems unecessary.
  # forget(mem.rotate)  # these would serve to invalidate the cache, seems unecessary.
  cache.pair <- function(f.pair, results, rots) {
    cat("Caching integer degree rotations for factor pair", as.character(rots[f.pair, "x"]), "and", as.character(rots[f.pair, "y"]), "...", fill = TRUE)
    pb <- txtProgressBar(min = -180, max = 180, style = 3, initial = -180)
    for (i in -180:180) {
      rots[f.pair, "rotated.angle"] <- i
      results.rot.cache <- mem.rotate(results = results, rots = rots)
      # g.cache <- mem.plot.rot(results = results.rot.cache, plot.type = plot.type)
      # tried plotting some of the ggplot2 plots here, slows done caching too much
      # base plotting is ok, but only if in quartz, so we're leaving that out, too
      setTxtProgressBar(pb = pb, value = i)
      # TODO(maxheld83) create video somewhere here -> https://github.com/aiorazabala/qmethod/issues/174
    }
    # this function does not return anything, has plenty of side effects though (caching)
  }


  # Interactive procedure =====================================================

  results.rot <- results
  if (!("rotation.angles" %in% names(results.rot$brief))) {  # if there are no rotation angles defined
    rots$rotated.angle <- 0
    results.rot$brief$rotation.angles <- rots  # assign them to 0
  }

  done.all <- FALSE  # we begin by assuming that people are NOT done with all pairs:)
  while (!done.all) {
    repeat { # as long as users have NOT entered a blank f.pair

      # UPDATE on selecting f.pair
      cat("You are in manual rotation, based on a", results$brief$rotation, "rotation.", fill = TRUE)
      print(results.rot$brief$rotation.angles)  # print current angles
      plot.wrapper(results = results.rot, pair = "all", plot.type = plot.type, plot.all = plot.all)

      # SELECT f.pair
      f.pair <- NULL  # let's assume there is NO factor pair selected
      cat("Which factor pair would you like to rotate?", fill = TRUE)
      while (is.null(f.pair) || !(any(f.pair == row.names(rots)) || f.pair == "")) {
        f.pair <- readline(prompt = "Enter row number, blank to complete or 'Esc' to abort: ")
      }
      if (f.pair == "") break

      # CACHING depending on f.pair
      if (plot.type != "base" && cache) {
        cache.pair(f.pair = f.pair, results = results, rots = results.rot$brief$rotation.angles)
      }

      # ITERATE angle on f.pair
      rots.it <- results.rot$brief$rotation.angles  # let's assign current round angles for starters
      done.pair <- FALSE  # assume it's not done
      while (!done.pair) {
        plot.wrapper(results = results.rot, pair = f.pair, plot.type = plot.type, plot.all = plot.all)  # print only current pair, unless overwritten by plot.all

        repeat {  # as long as users have not entered a blank angle

          # UPDATE on selecting angle
          cat("\r", "You are rotating factor pair", rots.it[f.pair, "name"], "currently at", rots.it[f.pair, "rotated.angle"], "degrees.")
          flush.console()

          # SELECT angle
          angle <- NULL
          while (is.null(angle) || !(!suppressWarnings(is.na(as.numeric(angle))) || angle == "")) {
            angle <- readline(prompt = "Enter a change in angle to rotate, blank to complete or 'Esc' to abort: ")
          }
          flush.console()
          if (angle == "") break

          # IMPLEMENT angle
          angle <- as.numeric(angle)  # readline does not capture numerics
          rots.it[f.pair, "rotated.angle"] <- rots.it[f.pair, "rotated.angle"] + angle  # add the angle
          rots.it[f.pair, "rotated.angle"] <- simplify.angle(rots.it[f.pair, "rotated.angle"])
          results.rot.it <- mem.rotate(results = results, rots = rots.it)  # rotate based on original results again

          # PLOT angle
          plot.wrapper(results = results.rot.it, pair = f.pair, plot.type = plot.type, plot.all = plot.all)
        }

        # CONFIRM done pair?
        done.pair <- NULL
        while (is.null(done.pair) || !(done.pair == "y" || done.pair == "n")) {
          cat("Do you really want to finish rotating factor pair", rots[f.pair, "name"], "?", fill = TRUE)
          done.pair <- readline(prompt = "Enter 'y' to finish and save, 'n' to continue or 'Esc' to abort: ")
        }

        # IMPLEMENT done pair
        if (done.pair == "y") {
          done.pair <- TRUE
          results.rot <- results.rot.it  # write out the rotation results
          f.pair <- NULL  # clean f.pair
        } else if (done.pair == "n") {
          done.pair <- FALSE
        }
      }
    }

    # UPDATE on done all?
    print(rots)
    plot.wrapper(results = results.rot, pair = "all", plot.type = plot.type, plot.all = plot.all)

    # CONFIRM done all?
    done.all <- NULL
    while (is.null(done.all) || !(done.all == "y" || done.all == "n")) {
      cat("Do you really want to finish manual rotation?", fill = TRUE)
      done.all <- readline(prompt = "Enter 'y' to finish and save, 'n' to continue or 'Esc' to abort: ")
    }

    # IMPLEMENT done all
    if (done.all == "y") {
      done.all <- TRUE
      if (any(results$loa != results.rot$loa)) {  # only write out results if results are actually different
        cat("You have completed a manual rotation. A rotated result has been returned.", fill = TRUE)
        results <- results.rot
      } else {  # otherwise,
        warning("You have not changed any of the loadings. An unchanged results object will be returned.")
      }
    } else if (done.all == "n") {
      done.all <- FALSE
    }
  }
  #TODO check here if there was actually any rotation happening!
  return(invisible(results))
}
