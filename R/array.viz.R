array.viz <- function(results, extreme.labels = c("negative", "positive"), incl.qdc = TRUE) {
  # Input verification ===================
  if (class(results) != "QmethodRes") {  # only accept results object
    stop("The object provided is not of class 'QmethodRes'.")
  }
  if (!is.vector(extreme.labels) | length(extreme.labels) != 2) {  # correct length of extreme labels?
    stop(
      "The extreme labels specified are not a vector of length 2."
    )
  }
  if (!is.logical(incl.qdc)) {  # type correct?
    "The argument set for incl.qdc needs to be TRUE or FALSE."
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
  q.distribution <- count(df = results$dataset, 1)[,2]  # infer distribution from first column in dataset
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
    array.viz.data$item.wrapped <- str_wrap(gsub("-", " ", rownames(array.viz.data)), 10)  # make shorter, wrapped item handles
    array.viz.data <- array.viz.data[order(array.viz.data[,"fsc"],array.viz.data[,"item.sd"]),]  # ordering, needed for y variable
    array.viz.data$ycoord <- sequence(q.distribution)  # add meaningless y variable for plotting

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
      , limits = c(0, max(array.viz.data$item.sd))
      , name = "Item Standard Deviation Across Flagged Q-Sorts\n"
      , guide = "colorbar"
    )
    
    # include QDC information on distinguishing and consensus factors ==========
    if (incl.qdc == TRUE) {
      array.viz.qdc.diff <- array.viz.data[,c("fsc", "ycoord")]  # duplicate baseline dataset
      array.viz.qdc.sig <- array.viz.data[,c("fsc", "ycoord")]  # duplicate baseline dataset
      for (other.fac in factors[-current.fac]) {  # go over the other factors
        sed <- results$f_char$sd_dif[current.fac, other.fac]  # this and below unfortunately reproduces analysis from qdc.R, because qdc results are hard to read in
        for (it in rownames(array.viz.data)) {  # go over all items
          diff <- results$zsc[it, other.fac] - results$zsc[it, current.fac]  # set up difference in zscores
          if (sed * 2.58 < abs(diff)) {  # if sig at .05 ...
            array.viz.qdc.diff[it, names(factors)[other.fac]] <- diff  # ... assign difference
            array.viz.qdc.sig[it, names(factors)[other.fac]] <- ".01"  # ... assign sig
          } else if (sed * 1.96 < abs(diff)) {  # if sig at .01
            array.viz.qdc.diff[it, names(factors)[other.fac]] <- diff  # ... assign difference
            array.viz.qdc.sig[it, names(factors)[other.fac]] <- ".05"  # ... assign sig
          } else {  # if non-sig ...
            array.viz.qdc.diff[it, names(factors)[other.fac]] <- NA  # ... assign NA
            array.viz.qdc.sig[it, names(factors)[other.fac]] <- NA  # ... assign NA
          }
        }
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
    )
    # Visual Polishing ======================================================
    g <- g + coord_cartesian(  # shorten axes
      xlim = c(
        min(results$dataset)-.5  # subtract .5 from minimum for 1 unit tile size
        , max(results$dataset)+.5
      )
      , ylim = c(
        0.5
        , max(array.viz.data$ycoord)+.5)
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
      , legend.position = c(0,1)  # manually adjust legend
      , legend.justification = c(0,1)
      , legend.direction = "horizontal"
      , panel.grid = element_blank()
    )
    # Put into list ============================================================
    g.list[[current.fac]] <- g  # add current graph to list
  }
  return(g.list)
}
