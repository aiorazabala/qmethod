#Frans Hermans, January 2021
#based on Brown 1980: Political Subjectivity, pages 208-224.


centroid <- function (tmat, nfactors = 7, spc = 10^-5) 
  #tmat is a correlation matrix
  #nfactors is number of components to extract. Warning: extracting more components than respondents is allowed!
  #spc is the threshold to accept factor results (in Brown this is set to 0.02)
{
  if (isSymmetric(tmat) == F)  
    stop("Input matrix should be a symmetrical correlation matrix.")
  if (nfactors > nrow(tmat)) warning("The number of components to extract is larger than the number of respondents.")
  compmat <- matrix(data = NA, nrow = nrow(tmat), ncol = nfactors, dimnames = list (rownames(tmat), LETTERS[1:nfactors]))
  for (i in 1:nfactors)
  {
    diag(tmat) <- 0
    refvec <- NULL
    while(all(colSums(tmat) >0) ==F) #maximize positive manifold
    {
      oo <-which(min(colSums(tmat)) == colSums(tmat))
      vec <- tmat[oo,] *-1
      tmat[oo,] <-vec
      tmat[,oo] <-vec
      refvec <- append(refvec, oo)
    }
    rmean <-colSums(tmat) / (nrow(tmat)-1)
    t1 <- rmean + colSums(tmat)
    f1  <- t1/sqrt(sum(t1))
    while(all(abs(rmean-f1^2)<spc)==F) #Factor extraction
    {
      rmean <- f1^2
      t2 <- colSums(tmat)+f1^2 
      f2 <- t2/sqrt(sum(t2))
      f1 <- f2
    }
    tfac <- f1
    for (n in 1:length(refvec)) #reflecting the factor back 
    {
      tfac [refvec[n]] <- tfac[refvec[n]]*-1
    }
    compmat[,i] <-tfac
    residual_mat <- tmat*0
    for (n in 1:nrow(tmat)) 
    {
      residual_mat[n,]<-tmat[n,] -f1[n]*f1
    }
    revvec <-rev(refvec)
    for (n in 1:length(revvec)) # reflecting the residuals back
    {
      residual_mat[revvec[n],] <- residual_mat[revvec[n],]*-1
      residual_mat[,revvec[n]] <- residual_mat[,revvec[n]]*-1      
    }
    tmat <-residual_mat
  }
  return(compmat)
}


