# formerly internal to qzscores()
qfwe <- function(dataset, loa, flagged) {
  floa <- flagged*loa
  #B. calculate FACTOR WEIGHTS for each Q sort, in a new matrix -needs to be a data.frame to perform variable calculations  
  fwe <- as.data.frame(apply(floa, 2, function(x) x/(1-x^2))) # this the formula from from Brown 1980: 242
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
  return(wraw_all)
}
