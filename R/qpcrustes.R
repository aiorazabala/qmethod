#Procrustes rotation for each bootstrap step, uses procrustes() function from MCMCpack
qpcrustes <- function(loa, target, nfactors) {
    prox <- as.matrix(loa)
    prores <- procrustes(prox, target)
    loarot <- as.data.frame(prores[1])
    colnames(loarot) <- paste("loarot_f", 1:nfactors, sep="")
    return(loarot)
}