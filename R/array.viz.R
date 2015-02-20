array.viz <- function(QmethodRes, f.names = NULL, f.colors = NULL, extreme.labels = c("very much disagree", "very much agree"), color.scheme = "Set1", incl.qdc = TRUE) {
  # Input verification ===================
#   QmethodRes <- keyneson$before
#   f.names <- c("resentment","critical","moderate")
#   f.colors <- NULL
#   extreme.labels <- c("very much disagree","very much agree")
#   color.scheme <- "Set1"
#   incl.qdc <- FALSE
#   
#   library("RColorBrewer")
#   library("reshape2")
#   library("stringr")
#   library("ggplot2")
#   
#   current.fac <- 1
  # Preparation ===============================================================
  factors <- seq(QmethodRes$brief$nfactors)  # set vector with length of factors
  if (!(is.null(f.names))) {  # if factor names are specified ...
    names(factors) <- f.names  # ... name vector accordingly
  }
  if (is.null(f.names)) {  # if there are no factor names ...
    f.names <- factors  # ... name them just by numbers
  }
  if (is.null(f.colors)) {  # if there are no colors
    f.colors <- brewer.pal(n = length(factors), name = color.scheme)
  }
  g.list <- as.list(f.names, all.names=FALSE)  # set up empty list
  names(g.list) <- f.names  # name list items appropriately

  # Loop over extracted factors ================================================
  for (current.fac in factors) {
    array.viz.data <- cbind(  # read in data
      QmethodRes$zsc_n[current.fac]  # the x position, rounded (zscore would be nicer, but is hard to visualize because of overlapping item labels etc.)
      ,QmethodRes$zsc[current.fac]  # also add zscore, maybe for future use
      ,QmethodRes$item_sd[current.fac]  # standard deviation per item
    )
    head(array.viz.data)
    colnames(array.viz.data)[1] <- "fsc"  # add generalized name
    colnames(array.viz.data)[2] <- "zsc"  # add generalized name
    colnames(array.viz.data)[3] <- "item.sd"  # add generalized name
    array.viz.data$item.wrapped <- str_wrap(gsub("-", " ", rownames(array.viz.data)), 10)  # make shorter, wrapped item handles
    array.viz.data <- array.viz.data[order(array.viz.data[,"fsc"],array.viz.data[,"item.sd"]),]  # ordering, needed for y variable
    array.viz.data$ycoord <- sequence(q.distribution)  # add meaningless y variable for plotting

    # actual plotting ==========================================================
    g <- ggplot(
      data = array.viz.data
      ,aes(
        x = fsc  # factor scores, always same variable bc dataframe is constructed for every factor array by above loop
        ,y = ycoord  # just the random ycoord for viz
        ,ymax = max(ycoord)
        ,ymin = 0
        ,label = item.wrapped  # for identification, separate variable with wrapped strings
      )
    )
    g <- g + geom_tile(  # add background tiles
      aes(
        fill = item.sd
      )
    )
    g <- g + scale_fill_gradient(
      low=f.colors[current.fac]
      ,high="white"
      ,limits = c(0,max(array.viz.data$item.sd))
      ,name = "Item Standard Deviation Across Flagged Q-Sorts\n"
      ,guide= "colorbar"
    )
    
    # include QDC information on distinguishing and consensus factors ==========
    if (incl.qdc == TRUE) {
      array.viz.qdc <- array.viz.data  # duplicate baseline dataset
      array.viz.qdc$zsc <- NULL  # not needed, only coordinates remain
      array.viz.qdc$item_sd <- NULL  # not needed, only coordinates remain
      array.viz.qdc$item.handles <- NULL  # not needed, only coordinates remain
      n <- 1  # just a counter for navigating columns later on
      for (other.fac in factors[-current.fac]) {
        for (it in rownames(array.viz.qdc)) { # go over all items
          if (QmethodRes$f_char$sd_dif[current.fac,other.fac] * 1.96 < abs(QmethodRes$zsc[it,other.fac] - QmethodRes$zsc[it,current.fac])) { # same formula as in qdc, duplicate here because qdc table results are hard to read in
            array.viz.qdc[it,2+n] <- QmethodRes$zsc_n[it,other.fac] - QmethodRes$zsc_n[it,current.fac]  # add difference in absolutes / 4+n is used to navigate to next empty column to the right
          } else {
            array.viz.qdc[it,2+n] <- NA  # add null if non-significant
          }
        }
        colnames(array.viz.qdc)[2+n] <- f.names[other.fac]  # give real colname
        n <- n + 1 # advance the counter one for the current factor
      }
      array.viz.qdc$item <- rownames(array.viz.qdc)
      array.viz.qdc <- melt(data = array.viz.qdc, id.vars = c("fsc","ycoord", "item"), variable.name="distinguishing.factor", value.name="difference", na.rm=TRUE)
      array.viz.qdc["sign"] <- lapply(X = array.viz.qdc["difference"], FUN = sign)
      g <- g + geom_segment(
        data = array.viz.qdc
        ,mapping = aes(
          x = fsc+(difference/28),
          y = ycoord-0.5,
          xend = fsc+(difference/28),
          yend = ycoord+0.5,
          colour = distinguishing.factor
          #,size = difference
        ),
        position = "dodge"
      )
      g <- g + scale_color_manual(values=f.colors[-current.fac])
    }
    # Add item labels ======================================================
    g <- g + geom_text(  # this cannot happen earlier; mind the levels
      data = array.viz.data,  # must call data again, because of possible qdc above
      mapping = aes(
        label = item.wrapped
        ,x = fsc
        ,y = ycoord
      )
    )
    # Visual Polishing ======================================================
    g <- g + coord_cartesian(  # shorten axes
      xlim = c(
        min(QmethodRes$dataset)-.5  # subtract .5 from minimum for 1 unit tile size
        ,max(QmethodRes$dataset)+.5
      )
      ,ylim = c(
        0.5
        ,max(array.viz.data$ycoord)+.5)
    )
    g <- g + labs(  # add meaningful x axis title (replaces need for title, too)
      x=paste("Factor Score:", f.names[current.fac], sep=" ")
    )
    x.tick.labels <- seq(min(QmethodRes$dataset),max(QmethodRes$dataset))  # add numeric tick labels
    x.tick.labels[1] <- paste(x.tick.labels[1], extreme.labels[1], sep = "\n")  # add lable for negative extreme
    x.tick.labels[length(x.tick.labels)] <- paste(x.tick.labels[length(x.tick.labels)], extreme.labels[2], sep = "\n")  # add lable for extreme positive
    g <- g + scale_x_continuous(breaks= seq(-7,7), labels=x.tick.labels)  # break only at full scores
    g <- g + theme_bw()  # choose simpler theme
    g <- g + theme(  # get rid of fluff
      axis.title.y = element_blank()  # no y labels, because meaningless
      ,axis.ticks.y = element_blank()  # no y ticks, because meaningless
      ,axis.text.y = element_blank()  # no y text, because meaningless
      ,legend.position = c(0,1)  # manually adjust legend
      ,legend.justification = c(0,1)
      ,legend.direction = "horizontal"
      ,panel.grid = element_blank()
    )
    # Put into list ============================================================
    g.list[[current.fac]] <- g  # add current graph to list
  }
  return(g.list)
}
