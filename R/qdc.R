qdc <- function(dataset, nfactors, zsc, sed) {
  if (nfactors==1) stop("Only one factor selected. No distinguishing and consensus statements can be calculated.")
  # Distinguishing and consensus statements
  # create data frame
  comparisons <- combn(nfactors, 2, simplify=F)
  comp <- vector()
  for (i in 1:length(comparisons)) {
    comp <- append(comp, paste("f", comparisons[[i]], collapse="_", sep=""), 
                   after = length(comp))
  }
  # Differences in z-scores (for statistical testing)
  qdc.diff <- data.frame(matrix(data=as.numeric(NA), 
                           ncol=length(comp), 
                           nrow=nrow(dataset), 
                           dimnames=list(row.names(dataset), comp)))
  for (n in 1:length(comp)) {
    first <-  names(zsc)[grep(paste0("f", comparisons[[n]][1]), 
                              names(zsc))]
    second <- names(zsc)[grep(paste0("f", comparisons[[n]][2]), 
                              names(zsc))]
    qdc.diff[n] <- zsc[first] - zsc[second]
  }
  qdc <- qdc.diff
  # significant differences
  sed <- data.frame(sed)
  for (n in 1:length(comp)) {
    # find the threshold for the pair of factors
    first <-  names(sed)[grep(paste0("f", comparisons[[n]][1]), 
                              names(sed))]
    second <- names(sed)[grep(paste0("f", comparisons[[n]][2]), 
                              names(sed))]
    # differences are significant when > 2.58*SED for p < .01, 
    # or the same value rounded upwards (Brown, 1980, pp.245)
    sedth.01 <- sed[first, second]*2.58
    sedth.05 <- sed[first, second]*1.96 
    qdc[which(abs(qdc.diff[[n]]) <= sedth.05), n] <- ""
    qdc[which(abs(qdc.diff[[n]]) >  sedth.05), n] <- "*"
    qdc[which(abs(qdc.diff[[n]]) >  sedth.01), n] <- "**"
  }
  names(qdc) <- paste0("sig_",names(qdc))
  
  # Indicate which ones are distinguishing and which are consensus
  
  # Find statements that have no stars and label them as 'Consensus'
  qdc$dist.and.cons <- as.character(apply(qdc, 1, function(x) sum(x!="")==0))
  qdc[which(qdc$dist.and.cons == T), "dist.and.cons"] <- "Consensus"
  
  # If there are only two factors, the rest are 'Distinguishing'
  if (nfactors == 2) {
    qdc[which(qdc$dist.and.cons != "Consensus"), "dist.and.cons"] <- "Distinguishing"
  }
  
  # If there are more than two factors, need to look one by one
  if (nfactors > 2) {
    qdc[which(qdc$dist.and.cons != "Consensus"), "dist.and.cons"] <- ""
    for (i in 1:nfactors) {
      varsin  <- names(qdc)[grep(i, names(qdc))]
      varsout <- names(qdc)[-grep(i, names(qdc))]
      varsout <- varsout[-which(varsout=="dist.and.cons")]
      for (s in 1:nrow(qdc)) {
        if (sum(qdc[s, varsin] != "") == length(varsin) & sum(qdc[s, varsout] != "") == 0) qdc[s, "dist.and.cons"] <- paste0("Distinguishes f",i, " only") else if (sum(qdc[s, c(varsin, varsout)] != "") == nfactors) qdc[s, "dist.and.cons"] <- "Distinguishes all" else if (sum(qdc[s, varsin] != "") == length(varsin) & sum(qdc[s, varsout] != "") != 0 & sum(qdc[s, c(varsin, varsout)] != "") != length(qdc)) qdc[s, "dist.and.cons"] <- paste0(qdc[s, "dist.and.cons"], "Distinguishes f",i, " ", collapse="")
      }
      #The above loop assigns these values in the column dist.and.cons, according to the following rules:
      # -- "Distinguishes f* only" when the differences of f* with all other factors are significant, AND all other differences are not.
      # -- "Distinguishes all" when all differences are significant.
      # -- "Distinguishes f*" when the differences of f* and all other factors are significant, AND some (but not all) of the other differences are significant.
      # -- "" leaves empty those which do not fullfil any of the above conditions, i.e. are not consensus neither are clearly distinguishing any factor.
    }
  }
  #Bind all together
  qdc.res <- cbind(qdc.diff, qdc)
  ord <- rep(1:nfactors, each=2)
  ord[which(1:(nfactors*2) %% 2 == 0)] <- ord[which(1:(nfactors*2) %% 2 == 0)] + nfactors
  qdc.res <- qdc.res[c(length(qdc.res), ord)]
  return(qdc.res)
}