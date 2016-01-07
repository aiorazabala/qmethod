qbstep <- function(subdata, subtarget, indet, nfactors, nqsorts, nstat, 
                   qmts=qmts, qmts_log=qmts_log, rotation="unknown", 
                   flagged=flagged, ...) {
  #--------------------------------------------------------------------
  # 1. Generate matrix of factor loadings
  cor.data <- cor(subdata, method="pearson")
  if (rotation=="unknown") rotation <- "none"
  loa <- as.data.frame(unclass(principal(cor.data, nfactors=nfactors, rotate=rotation, ...)$loadings))
  
  # Note (2015.12.17): the original line run 'principal()' directly: 
  # loa <- as.data.frame(unclass(principal(subdata, rotate="none", nfactors=nfactors)$loa))
  # However (funny enough!) principal() blocks the console when the data introduced are a square matrix (e.g. 30 observations and 30 statements); producing the correlation table first avoids that bug.
  #--------------------------------------------------------------------
  # 2. Apply solutions for indeterminacy issue of PCA bootstrap  
  if (indet == "none") {
    #loa <- as.data.frame(PCA(subdata, graph=FALSE)$var$coord[,c(1:nfactors)])
    loa <- as.matrix(unclass(varimax(as.matrix(loa))[[1]]))
  }
  if (indet == "procrustes") {
    #loa <- as.data.frame(PCA(subdata, graph=FALSE)$var$coord[,c(1:nfactors)])
    #caution: selecting rotation ="varimax" here implies that both varimax and Procrustes are used one on top of the other, and probably just one or the other should be used. For the qindtest though, the selected rotation is used
    procrustes <- qpcrustes(loa=loa, target=subtarget, nfactors=nfactors)
    loa <- procrustes
  }
  if (indet == "qindtest" | indet == "both") {
    loa <- as.matrix(unclass(varimax(as.matrix(loa))[[1]]))
    qindeterminacy <- qindtest(loa=loa, target=subtarget, 
                               nfactors=nfactors)
    loa <- qindeterminacy[[1]]
    if (indet == "both") {
      loa <- qpcrustes(loa=loa, target=subtarget, nfactors=nfactors)
    }
  }
  #--------------------------------------------------------------------
  # 3. Calculate z-scores and factor scores with the indeterminacy corrected factor loadings 'loa'
  flagged <- qflag(nstat=nstat, loa=loa)
  qstep <- qzscores(subdata, nfactors=nfactors,  
                    flagged=flagged, loa=loa, ...)
  #--------------------------------------------------------------------
  # 4. Export necessary results
  step_res <- list()
  step_res[[1]] <- list()
  step_res[[2]] <- list()
  step_res[[3]] <- list()
  
  qstep[[4]] <- as.data.frame(qstep[[4]])
  qstep[[3]] <- as.data.frame(qstep[[3]])
  
  n <- 1
  while (n <= nfactors) {
    #flagged q sorts
    step_res[[1]][n] <- qstep[[4]][n] #to append in qmbr[[n]][[1]]
    #z-scores
    step_res[[2]][n] <- qstep[[5]][n] #to append in qmbr[[n]][[2]]
    #factor loadings
    step_res[[3]][n] <- qstep[[3]][n] #to append in qmbr[[n]][[3]]
    n <- n + 1
  }
  if (indet == "qindtest" | indet == "both") {
    qindt_log <- qindeterminacy[[2]]
    qindt <- qindeterminacy[[3]]
    #test results (logical)
    step_res[4] <- qindt[1] #to append in qmts[1]
    step_res[5] <- qindt[2] #to append in qmts[2]
    #reports of solution implementation
    step_res[6] <- qindt_log[1] #to append in qmts_log[1]
    step_res[7] <- qindt_log[2] #to append in qmts_log[2]
    names(step_res) <- c("flagged", "z_scores", "loadings", "torder_res_log", "tsign_res_log", "torder_res_report", "tsign_res_report")
  } else {names(step_res) <- c("flagged", "z_scores", "loadings")}
  return(step_res)
}