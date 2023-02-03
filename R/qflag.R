#flags Q sorts automatically according to the given loadings matrix
qflag <- function(loa=loa, nstat) {
  # calculate number of Q sorts and number of statements
  nqsorts <- nrow(loa)
  #FLAGGING CRITERIA: 
  # -- 1) qsorts which factor loading is higher than the threshold for pval >0.95, and 
  # -- 2) qsorts which square loading is higher than the sum of square loadings of the same q-sort in all other factors
  thold.05 <- 1.96/sqrt(nstat)
  loa_sq <- loa^2
  flagged <- matrix(data=F, nrow=nqsorts, ncol=ncol(loa))
  f <- 1
  while (f <= ncol(loa)) {
    n <- 1
    while (n <= nqsorts) {
      flagged[n,f] <- loa_sq[n,f] > (rowSums(loa_sq)[[n]]-loa_sq[n,f]) & abs(loa[n,f]) > thold.05
      n <- n+1
    }
    f <- f+1
  }
  flagged <- as.matrix(flagged)
  colnames(flagged) <- paste("flag_f",1:ncol(loa), sep="")
  row.names(flagged) <- row.names(loa)
  # Checks to recommend manual inspection
  # Negative loading and flagged
  if (sum(as.matrix(loa)[which(flagged)] < 0) > 0) warning("One or more Q-sorts with negative loadings are flagged through the automatic pre-flagging. This is not necessarily an issue, but double check the flags manually, e.g. using the function 'loa.and.flags()'.")
  # A Q-sort flagged in more than one
  if (sum(apply(flagged, 1, sum) > 1) > 0) warning("One or more Q-sorts is flagged for two or more factors through the automatic pre-flagging. This is not necessarily an issue, but double check the flags manually, e.g. using the function 'loa.and.flags()'.")
  return(flagged)
}