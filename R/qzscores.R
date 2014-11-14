#calculates final z-scores and factor scores, and extracts main results for Q method
qzscores <- function(dataset, nfactors, loa=loa, flagged=flagged, forced=TRUE, distribution=NA) {    
  # calculate number of Q sorts and number of statements
  nstat <- nrow(dataset)
  nqsorts <- ncol(dataset)
  #A. select FLAGGED Q sorts
  floa <- flagged*loa #as.data.frame(loa); floa[which(!flagged, arr.ind=T)] <- 0 # the latter does not work in old versions of R
  #B. calculate FACTOR WEIGHTS for each Q sort, in a new matrix -needs to be a data.frame to perform variable calculations
  fwe <- as.data.frame(apply(floa, 2, function(x) x/(1-x^2)))
  #C. calculate Z-SCORES for each sentence and factor 
  #-- new matrix for wsubm*ssubmn (original matrix * q sort factor weight), and transpose
  wraw_all <- list()
  n <- 1
  for (i in fwe) {
    wraw_all[[n]] <- t(t(dataset)*i)
    names(wraw_all[[n]]) <- paste("wraw_",n,sep="")
    wraw_all[[n]] <- as.data.frame(wraw_all[[n]])
    n <- n+1
  }
  #-- sums, average and stdev for each statement
  zsc_sum <- data.frame(cbind(1:nstat))
  zsc_mea <- data.frame(cbind(1:nstat))
  zsc_std <- data.frame(cbind(1:nstat))
  row.names(zsc_sum) <- row.names(dataset)
  row.names(zsc_mea) <- row.names(dataset)
  row.names(zsc_std) <- row.names(dataset)
  n <- 1
  while (n <= length(floa)) {
    zsc_sum[,n] <-      rowSums(wraw_all[[n]])
    zsc_mea[,n] <- mean(rowSums(wraw_all[[n]]))
    zsc_std[,n] <-   sd(rowSums(wraw_all[[n]]))
    n <- n+1
  }
  colnames(zsc_sum) <- paste("z_sum_",c(1:length(floa)),sep="")
  colnames(zsc_mea) <- paste("z_mea_",c(1:length(floa)),sep="")
  colnames(zsc_std) <- paste("z_std_",c(1:length(floa)),sep="")
  #-- z-scores for each statement
  zsc <- data.frame(cbind(1:nstat))
  row.names(zsc) <- row.names(dataset)
  n <- 1
  while (n <= length(floa)) {
    zsc[,n] <- (zsc_sum[,n]-zsc_mea[,n])/zsc_std[,n]
    n <- n+1
  }
  colnames(zsc) <- paste("zsc_f",c(1:length(floa)),sep="")
  #D. FACTOR SCORES: rounded z-scores
  if (forced) {
    qscores <- sort(dataset[,1], decreasing=FALSE)
  } else {
    qscores <- sort(distribution, decreasing=FALSE)
    if (length(distribution) != nrow(dataset) | (class(distribution)[1] != "numeric" & class(distribution) != "integer")) stop("Q method input: The distribution of items was set as non-forced and the distribution provided is not suitable (it is the wrong length or it is non numerical)")
  }
  zsc_n <- as.data.frame(zsc)
  f <- 1
  while (f <= length(floa)) {
    if (length(unique(zsc[,f])) == length(zsc[,f])) {
      zsc_n[,f] <- qscores[rank(zsc[,f])]
    } else {
      zsc_n[,f] <- qscores[rank(zsc[,f])]
      # statements with identical z-score
      izsc <- which(round(rank(zsc[,f])) != rank(zsc[,f]))
      uizsc <- unique(zsc[izsc,f])
      for (g in uizsc) {
        izscn <- which(zsc[,f] == g)
        zsc_n[izscn,f] <- min(zsc_n[izscn,f])
      }
    }
    f <- f+1
  }
  colnames(zsc_n) <- paste("fsc_f",c(1:length(floa)),sep="")
  #E. FACTOR CHARACTERISTICS
  f_char <- qfcharact(loa, flagged, zsc, nfactors, floa)
  #F. FINAL OUTPUTS
  brief <- list()
  brief$date <- date()
  brief$nstat <- nstat
  brief$nqsorts <- nqsorts
  brief$nfactors <- nfactors
  brief$rotation <- "unknown"
  brief$cor.method <- "unknown"
  brief$info <- c("Q-method z-scores.",
                  paste0("Finished on:             ", brief$date), 
                  paste0("Original data:           ", brief$nstat, " statements, ", brief$nqsorts, " Q-sorts"),
                  paste0("Number of factors:       ", brief$nfactors),
                  paste0("Rotation:                ", brief$rotation),
                  paste0("Flagging:                unknown"),
                  paste0("Correlation coefficient: ", brief$cor.method))
  # brief <- paste0("z-scores calculated on ", date(), ". Original data: ", nstat, " statements, ", nqsorts, " Q-sorts. Number of factors: ",nfactors,".")
  qmethodresults <- list()
  qmethodresults[[1]] <- brief
  qmethodresults[[2]] <- dataset
  qmethodresults[[3]] <- loa
  qmethodresults[[4]] <- flagged
  qmethodresults[[5]] <- zsc
  qmethodresults[[6]] <- zsc_n
  qmethodresults[[7]] <- f_char
  names(qmethodresults) <- c("brief", "dataset", "loa", "flagged", "zsc", "zsc_n", "f_char")
  class(qmethodresults) <- "QmethodRes"
  return(qmethodresults)
}