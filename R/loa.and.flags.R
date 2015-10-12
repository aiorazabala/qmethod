loa.and.flags <- function(results, nload=F,...){
  loa <- round(results$loa, digits=2)
  fla <- results$flagged
  names(fla) <- paste0("fg", 1:ncol(fla))
  for (i in 1:length(fla)) fla[[i]] <- as.character(fla[[i]])
  for (i in 1:length(fla)) fla[which(fla[[i]]=="FALSE"),i ] <- ""
  for (i in 1:length(fla)) fla[which(fla[[i]]=="TRUE"),i ] <- "*"
  flagqs <- cbind(loa, fla)
  pos <- vector()
  for(i in 1:length(fla)) pos <- c(pos, i+length(fla), i)
  flagqs <- flagqs[pos]
  if (nload) {
    cat("\nNumber of Q-sorts flagged for each factor:\n")
    print(results$f_char$characteristics["nload"])
  }
  cat("\n")
  return(flagqs)
}