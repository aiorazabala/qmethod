q.eigenpair <- function (data, cormatrix = NULL, nlabels = 3) {
  # TODO(maxheld83) explain what this does
  # Input validation =============
    # TODO(maxheld83) data validation
  # Testing setup =====
#   data <- q_sorts[,,"before"]
#   cormatrix <- NULL
#   pairs <- NULL
#   nlabels <- 3
  # Setting up vars ==========
  if (is.null(cormatrix)) {
    cormatrix <- cor(x = data , method="spearman")  # calculate correlation matrix with spearman as default
  }
  # Make pairs
  pairs <- vector("list", 4)  # create empty list
  names(pairs) <- c("positive", "negative", "lowest", "identity")
  pairs$positive <- rownames(which(cormatrix == max(cormatrix[cormatrix < 1]), arr.ind = TRUE))  # find the highest positive correlation pair
  pairs$negative <- rownames(which(cormatrix == min(cormatrix[cormatrix < 1]), arr.ind = TRUE))  # find the highest negative correlation pair
  pairs$lowest <- rownames(which(cormatrix == min(abs(cormatrix[cormatrix < 1])), arr.ind = TRUE))  # find the lowest correlation
  pairs$identity <- c(rep(x = pairs$lowest[2], times = 2))  # identity is just one with him/herself
  
  # Sample item labels (using all would hopelessly overlopt)
  labels <- row.names(data)  # make labels vector
  labels[-sample(x = 1:length(row.names(q_sorts)), size = nlabels, replace = FALSE )] <- ""  # NA#s would be cleaner but shout later on
  
  eigenpairs <- list(scatterplots = NA)  # just to be sure, in case we want to return other objects later
  eigenpairs$scatterplots <- pairs  # just assign these for the names
  n <- 1  # stupid counter to go along pairs
  for (i in pairs) {
    # this is the part as documented on http://stats.stackexchange.com/q/153564/60119
    i[1]
    g <- NULL
    g <- ggplot(data = as.data.frame(data), mapping = aes_string(x = i[1], y = i[2]))
    g <- g + geom_point(alpha = 1/3)  # alpha b/c of overplotting
    g <- g + geom_smooth(method = "lm")  # just for comparsion
    g <- g + coord_fixed()  # otherwise, the angles of vectors are off
    if (i[1] != i[2]) {  # ellipse is only defined if not equal, makes sense!
      g <- g + stat_ellipse(type = "norm") 
      # add ellipse, though I am not sure which is the adequate type
      # as per https://github.com/hadley/ggplot2/blob/master/R/stat-ellipse.R
    }
    eigen <- eigen(cormatrix[i,i])   # calculate eigenvectors and values
    eigen$slopes[1] <- eigen$vectors[1,1]/eigen$vectors[2,1]  # calc slopes as ratios
    eigen$slopes[2] <- eigen$vectors[1,1]/eigen$vectors[1,2]  # calc slopes as ratios
    g <- g + geom_abline(intercept = 0, slope = eigen$slopes[1], colour = "green")  # plot pc1
    if (eigen$values[2] > 0) {  # second axis sense only if there is, in fact, a value
      g <- g + geom_abline(intercept = 0, slope = eigen$slopes[2], colour = "red")  # plot pc2
    }
    g <- g + geom_segment(x = 0, y = 0, xend = eigen$values[1], yend = eigen$slopes[1] * eigen$values[1], colour = "green", arrow = arrow(length = unit(0.2, "cm")))  # add arrow for pc1
    g <- g + geom_segment(x = 0, y = 0, xend = eigen$values[2], yend = eigen$slopes[2] * eigen$values[2], colour = "red", arrow = arrow(length = unit(0.2, "cm")))  # add arrow for pc2
    # Here come the perpendiculars, from StackExchange answer http://stackoverflow.com/questions/30398908/how-to-drop-a-perpendicular-line-from-each-point-in-a-scatterplot-to-an-eigenv ===
    perp.segment.coord <- function(x0, y0, a=0,b=1){
      #finds endpoint for a perpendicular segment from the point (x0,y0) to the line
      # defined by lm.mod as y=a+b*x
      x1 <- (x0+b*y0-a*b)/(1+b^2)
      y1 <- a + b*x1
      list(x0=x0, y0=y0, x1=x1, y1=y1)
    }
    ss <- perp.segment.coord(data[,i[1]], data[,i[2]], 0, eigen$slopes[1])
    g <- g + geom_segment(data=as.data.frame(ss), aes(x = x0, y = y0, xend = x1, yend = y1), colour = "green", linetype = "dotted")
    g <- g + ggtitle(names(pairs[n]))  # add titles
    g <- g + geom_text(mapping = aes(label = labels), hjust = 0, vjust = 0)  # ad sampled labels
    eigenpairs$scatterplots[[n]] <- g  # assign output
    n <- n+1  # advance counter, not elegant, but works
  }
  return(eigenpairs)
}
