#Procrustes rotation for each bootstrap step, uses procrustes() function from MCMCpack
qpcrustes <- function(loa, target, nfactors) {
  if (!requireNamespace("MCMCpack", quietly = TRUE)) {
    stop("Package \"MCMCpack\" needed for this function to work. Please install it.", call. = FALSE)
  }
    prox <- as.matrix(loa)
    prores <- MCMCpack::procrustes(prox, target)
    loarot <- as.data.frame(prores[1])
    warning("The procrustes rotation is not working currently due to an issue in package dependency from 'MCMCpack' and 'graph'. If you'd like to try this: 1) see the code for the function 'qpcrustes' and 2) uncomment lines 4 and 5 and comment line 6. You will need to install and load the package 'MCMCpack' to run this function")
    loarot <- as.data.frame(prox)
    colnames(loarot) <- paste("loarot_f", 1:nfactors, sep="")
    return(loarot)
}
