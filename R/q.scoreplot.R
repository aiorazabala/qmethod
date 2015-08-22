q.scoreplot <- function(results, extreme.labels = c("negative", "positive"), incl.qdc = TRUE, quietly = FALSE, hyph.pattern = "en", hyphenate = TRUE, split.string = "[[:punct:]]", label.scale=500) {
  # Input verification ===================
  if (!is.logical(quietly) || !is.vector(quietly) || length(quietly) != 1) {
    stop("The argument set for quietly must be a logical vector of length 1.")
  }
  if (class(results) != "QmethodRes") {  # only accept results object
    stop("The object provided is not of class 'QmethodRes'.")
  }
  if (!is.vector(extreme.labels) | length(extreme.labels) != 2) {  # correct length of extreme labels?
    stop(
      "The extreme labels specified are not a vector of length 2."
    )
  }
  if (!is.logical(incl.qdc)) {  # type correct?
    stop(
      "The argument set for incl.qdc needs to be TRUE or FALSE."
    )
  }
  if (!hyph.pattern %in% c("de", "de.old", "en", "es", "fr", "it", "en.us", "ru")) {  # type correct?
    stop(
      "The argument set for hyph.pattern is not one of the allowed options."
    )
  }
  if (!is.logical(hyphenate)) {  # type correct?
    stop(
      "The argument set for hyphenate needs to be TRUE or FALSE."
    )
  }
  if (!is.vector(split.string) | length(split.string) != 1) {  # correct length of extreme labels?
    stop(
      "The split.string specified must be a vector of length 1."
    )
  }
  if (!is.numeric(label.scale) || !is.vector(label.scale) || length(label.scale) != 1) {
    stop("The argument set for label.scale must be an numeric vector of length 1.")
  }


  # Preparation ===============================================================
  if (is.null(results$brief$fcolors)) {  # when there are no colors in object
    fcolors <- q.fcolors(results = results)$brief$fcolors  # calculate them
    warning ("Because no colors were found in results object, they were generated on the fly. Colors are not saved to results object and may be inconsistent between plots. Run q.fcolors() to assign consistent colors.")
  } else {  # when there are colors in object
    fcolors <- results$brief$fcolors  # use them
  }
  names(fcolors) <- colnames(results$loa)  # ABSOLUTELY need named fcolors for later consistency of qdc lines
  factors <- seq(results$brief$nfactors)  # set vector with length of factors
  names(factors) <- colnames(results$loa)  # take names from loa whatever they may be
  g.list <- as.list(factors)  # set up empty list
  if (results$brief$distro) {
    q.distribution <- round(as.data.frame(table(as.matrix(results$dataset))/ncol(results$dataset))[,2])  # take average for precautionary reasons
  } else {
    q.distribution <- NULL
  }

  # we want to know the distros for all subplots in advance, so we can scale all plots similarly (ylim especially in the below)
  distros <- apply(X = results$zsc_n, MARGIN = 2, FUN = count)  # gather the distribution for all
  distros <- join_all(dfs = distros, by = "x")  # join 'em
  rownames(distros) <- distros[,1]  # name 'em
  colnames(distros) <- names(factors)
  distros <- distros[,-1]  # simplify them

  # Calculate some stuff for text size scaling =================================

  # vertical space
  v.space <- 1 / max(distros)
  # now let's give the max chars after which to break the line for below str_wrap
  max.lines.per.cell <- min(round(v.space * 45), 4)
  wrap.length <- max(round(max(nchar(rownames(results$dataset)))/max.lines.per.cell), 8)

  # horizontal space
  splitstrings <- strsplit(x = rownames(results$dataset), split = "-")
  splitstrings <- unlist(splitstrings)
  longest.word <- max(nchar(splitstrings))
  h.space <- 1 / sum(abs(range(results$dataset))) / wrap.length

  # wrap handles
  wrapped.handles <- rownames(results$dataset)
  wrapped.handles <- strsplit(x = wrapped.handles, split = split.string)
  names(wrapped.handles) <- rownames(results$dataset)

  # put stuff together again if there are too many parts
  for (i in names(wrapped.handles)) {
    if(length(wrapped.handles[[i]]) > max.lines.per.cell) {
      n <- 1
      while (length(wrapped.handles[[i]]) > max.lines.per.cell) {
        wrapped.handles[[i]][n + 1] <- paste(wrapped.handles[[i]][c(n, n+1)], collapse = " ")
        wrapped.handles[[i]] <- wrapped.handles[[i]][-n]
        wrapped.handles[[i]]
        n <- n + 1
      }
    }
  }

  # hyphenate
  if(hyphenate) {
    for (handle in names(wrapped.handles)) {
      if(length(wrapped.handles[[handle]]) < max.lines.per.cell) {
        linebreaks <- length(wrapped.handles[[handle]])
        for (handle.part in 1:length(wrapped.handles[[handle]])) {
          if(nchar(wrapped.handles[[handle]][handle.part]) > wrap.length & linebreaks < max.lines.per.cell) {
            hyphenated <- hyphen(words = wrapped.handles[[handle]][handle.part], quiet = TRUE, cache = TRUE, hyph.pattern = hyph.pattern)
            hyphenated <- slot(object = hyphenated, name = "hyphen")$word
            hyphenated <- strsplit(x = hyphenated, split = "-")
            hyphenated <- unlist(hyphenated)
            split.point <- round(length(hyphenated)/2)
            hyphenated <- paste(c(paste0(c(hyphenated[1:split.point], "-"), collapse = ""), paste0(hyphenated[(split.point+1):length(hyphenated)], collapse = "")), sep = "-")
            wrapped.handles[[handle]] <- append(x = wrapped.handles[[handle]], values = hyphenated, after = handle.part)
            wrapped.handles[[handle]] <- wrapped.handles[[handle]][-handle.part]
            linebreaks <- linebreaks + 1  # add to counter
          }
        }
      }
    }
  }

  # add linebreaks
  wrapped.handles <- lapply(X = wrapped.handles, FUN = paste, collapse = "\n")
  wrapped.handles <- cbind(names(wrapped.handles), unlist(wrapped.handles))
  wrapped.handles <- wrapped.handles[, -1]

  # Loop over extracted factors ================================================
  for (current.fac in factors) {
    array.viz.data <- cbind(  # read in data
      results$zsc_n[current.fac]  # the x position, rounded (zscore would be nicer, but is hard to visualize because of overlapping item labels etc.)
      , results$zsc[current.fac]  # also add zscore, maybe for future use
      , q.item.sd(results = results)[current.fac]  # get sd per item, call other function
    )
    colnames(array.viz.data)[1] <- "fsc"  # add generalized name
    colnames(array.viz.data)[2] <- "zsc"  # add generalized name
    colnames(array.viz.data)[3] <- "item.sd"  # add generalized name
    array.viz.data$item.sd[is.na(array.viz.data$item.sd)] <- 0  # set SDs to 0 if they are NA, which appropriately happens when only 1 person loads, since sd for n=1 is undefined. Bit of a hack job, but should not be much needed in real life, as factors with only 1 loader are crap anyway.
    array.viz.data$item.wrapped <- wrapped.handles[c(row.names(array.viz.data))]
    array.viz.data <- array.viz.data[order(array.viz.data[,"fsc"],array.viz.data[,"item.sd"]),]  # ordering, needed for y variable
    array.viz.data$ycoord <- sequence(distros[, current.fac])  # add meaningless y variable for plotting

    # actual plotting ==========================================================
    fsc <- ycoord <- item.sd <- difference <- significance <- item.wrapped <- NULL  # this is a hideous, nonsensical piece of code to avoid a spurious R CMD Check note. It is recommended by the author of the offending package on StackExchange at http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
    g <- ggplot(
      data = array.viz.data,
      aes(
        x = fsc  # factor scores, always same variable bc dataframe is constructed for every factor array by above loop
        , y = ycoord  # just the random ycoord for viz
        , ymax = max(ycoord)
        , ymin = 0
        #,label = item.wrapped  # this for some reason causes an error
      )
    )
    g <- g + geom_tile(  # add background tiles
      aes(
        fill = item.sd
      )
    )
    g <- g + scale_fill_gradient(
      low = fcolors[current.fac]
      , high="white"
      , limits = c(0, max(q.item.sd(results), na.rm = TRUE))
      , name = "Item Standard Deviation Across Flagged Q-Sorts\n"
      , guide = "colorbar"
    )

    # include QDC information on distinguishing and consensus factors ==========
    if (incl.qdc == TRUE) {
      array.viz.qdc.diff <- array.viz.data[,c("fsc", "ycoord")]  # duplicate baseline dataset
      array.viz.qdc.sig <- array.viz.data[,c("fsc", "ycoord")]  # duplicate baseline dataset
      for (other.fac in factors[-current.fac]) {

        sig2other <- as.matrix(results$qdc[rownames(array.viz.qdc.diff), grepl(names(factors)[current.fac], colnames(results$qdc)) & grepl(names(factors)[other.fac], colnames(results$qdc)) & grepl("sig", colnames(results$qdc))])
        colnames(sig2other) <- names(factors)[other.fac]
        sig2other[sig2other==""] <- NA
        sig2other[sig2other=="**"] <- ".01"
        sig2other[sig2other=="*"] <- ".05"
        array.viz.qdc.sig <- cbind(array.viz.qdc.sig, sig2other)

        diff2other <- as.matrix(results$qdc[rownames(array.viz.qdc.diff), grepl(names(factors)[current.fac], colnames(results$qdc)) & grepl(names(factors)[other.fac], colnames(results$qdc)) & !grepl("sig", colnames(results$qdc))])
        colnames(diff2other) <- names(factors)[other.fac]
        diff2other[is.na(sig2other)] <- NA  # kill non-significant diffs
        array.viz.qdc.diff <- cbind(array.viz.qdc.diff, diff2other)

      }

      array.viz.qdc.diff$item <- rownames(array.viz.qdc.diff)  # add manual item names for orientation (gets killed in melt)
      array.viz.qdc.diff <- melt(
        data = array.viz.qdc.diff
        , id.vars = c("fsc", "ycoord", "item")
        , variable.name="factor"
        , value.name="difference"
        , na.rm=TRUE
      )
      array.viz.qdc.sig$item <- rownames(array.viz.qdc.sig)  # add manual item names for orientation (gets killed in melt)
      array.viz.qdc.sig <- melt(
        data = array.viz.qdc.sig
        , id.vars = c("fsc", "ycoord", "item")
        , variable.name="factor"
        , value.name="significance"
        , na.rm=TRUE
      )
      array.viz.qdc <- merge(array.viz.qdc.diff, array.viz.qdc.sig)  # merge sig and diff
      g <- g + geom_segment(
        data = array.viz.qdc
        , mapping = aes(
          x = fsc + (difference/max(abs(range(difference)))) * 0.45  # scale to highest existing difference
          , y = ycoord - 0.5
          , xend = fsc + (difference/max(abs(range(difference)))) * 0.45  # multiply by .45 to make sure it's within half of the tile
          , yend = ycoord + 0.5
          , colour = factor(factor)
          , linetype = significance
        )
        , position = "dodge"
      )
      g <- g + labs(linetype = "Item Difference Significance")
      # g <- g + scale_colour_manual(values = f.colors[- current.fac])  # this, oddly, won't work, probably because of this: https://groups.google.com/forum/#!msg/ggplot2/mQskLl-TNQk/UxxE1FQcVH8J hence the need for the below shenanigan
      g <- g + scale_colour_manual(
        values = fcolors
        , name = "Item Position on Distinguishing Factor"
      )
    }
    # Add item labels ======================================================
    g <- g + geom_text(  # this cannot happen earlier; mind the levels
      data = array.viz.data,  # must call data again, because of possible qdc above
      mapping = aes(
        label = item.wrapped
        , x = fsc
        , y = ycoord
      )
      , size = rel(h.space * label.scale)
      , lineheight = rel(0.8)
    )
    # Visual Polishing ======================================================
    g <- g + coord_cartesian(  # shorten axes
      xlim = c(
        min(results$dataset)-.5  # subtract .5 from minimum for 1 unit tile size
        , max(results$dataset)+.5
      )
      , ylim = c(
        0.5
        , max(distros)+.5)
    )
    g <- g + labs(  # add meaningful x axis title (replaces need for title, too)
      x=paste("Factor Score:", names(factors)[current.fac], sep=" ")
    )
    x.tick.labels <- seq(min(results$dataset), max(results$dataset))  # add numeric tick labels
    x.tick.labels[1] <- paste(x.tick.labels[1], extreme.labels[1], sep = "\n")  # add lable for negative extreme
    x.tick.labels[length(x.tick.labels)] <- paste(x.tick.labels[length(x.tick.labels)], extreme.labels[2], sep = "\n")  # add lable for extreme positive
    g <- g + scale_x_continuous(breaks= seq(min(results$dataset),max(results$dataset)), labels=x.tick.labels)  # break only at full scores
    g <- g + theme_bw()  # choose simpler theme
    g <- g + theme(  # get rid of fluff
      axis.title.y = element_blank()  # no y labels, because meaningless
      , axis.ticks.y = element_blank()  # no y ticks, because meaningless
      , axis.text.y = element_blank()  # no y text, because meaningless
      , legend.position = "bottom"  # manually adjust legend
      , legend.direction = "horizontal"
      , panel.grid = element_blank()
      , legend.box = "horizontal"
    )
    # Put into list ============================================================
    g.list[[current.fac]] <- g  # add current graph to list
  }
  if (!quietly) {
    do.call(what = "grid.arrange", args = c(g.list, ncol = round(sqrt(length(results$brief$nfactors)))))
  }
  return(invisible(g.list))
}
