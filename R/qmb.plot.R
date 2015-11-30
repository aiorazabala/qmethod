###############################################################
# Code adapted from original plots in /home/aiora/Copy/PhD-LandEcon/REBISE/5_qmethodology/3_analysis/ALL_R-Scripts/lipset/p1plot_lipset_bts_plot_loading_SE.R (and zsc)
# Transformed into function, valid for both zsc and loa, based on the summary object of a boxplot

qmb.plot <- function(qmbsum, type=c("zsc", "loa"), nfactors, cex = 0.7, cex.leg=0.8, errbar.col= "black", lwd=1, lty=1, vertdist = 0.2, limits=NA, r.names=NA, sort=F, sbset=NULL, leg.pos="topleft", bty = "n", ...) {
  if(type == "loa") {
    boloa <- qmbsum[[1]]
    db <- boloa[ ,c(grep("loa", names(boloa)), grep("SE", names(boloa)), grep("std", names(boloa)))]
    item <- "Q-sort"
    values <- "Factor loading"
    if(is.na(limits)) limits <- c(-1.0,1.0)
  }
  
  if(type == "zsc") {
    boloa <- qmbsum[[2]]
    db <- boloa[ ,c(grep("zsc.bts", names(boloa)), grep("SE", names(boloa)),   grep("std", names(boloa)))]
    item <- "Statement"
    values <- "z-score"
    if(is.na(limits)) {
      zscs <- grep("zsc.bts", names(db))
      SEs <- grep("SE", names(db))
      lms.down <- db[,zscs] - db[,SEs]
      lms.up <- db[,zscs] + db[,SEs]
      limits <- c(floor(min(lms.down)), ceiling(max(lms.up)))
    }
  }
  if(is.numeric(sbset)) db <- db[c(1:min(nrow(db), sbset)), ]
  nitems <- nrow(db)
  if(length(r.names) == nrow(db)) rownames(db) <- r.names
  
  if(sort) {
    sds <- apply(db[,(1+nfactors):(2*nfactors)], 1, sum)
    db <- db[order(sds), ]
  }
  
  #plotting parameters
  db$position <- c(1:nitems)
  colegend=c(rep("black", nfactors), "white", "white", "white")
  pich=array(c(20, 15, 17, 18))
  pitx=array(c(21, 22, 24, 23))
  
  i=1
  dotchart(db[,i], labels = rownames(db), pch=pich[i], 
           xlim=limits,  
           xlab=values, lcolor="white",
           lwd = lwd, cex=cex, ...)
  mtext(item, side=2, line=1.5, cex=cex, ...)
  points(x=db[,(2*nfactors)+i], db[,"position"], pch = pitx[i], type = "p", lwd = lwd, cex=cex, ...)
  segments(x0=db[,i], y0=db[,"position"], 
           x1=db[,i]+db[,nfactors+i], 
           y1=db[,"position"], lwd = lwd, lty = lty, col = errbar.col, cex=cex, ...)
  segments(x0=db[,i], y0=db[,"position"], 
           x1=db[,i]-db[,nfactors+i], 
           y1=db[,"position"], lwd = lwd, lty = lty, col = errbar.col, cex=cex, ...)
  for (i in 2:nfactors) {
    points(x=db[,i], db[,"position"]+(vertdist*(i-1)), 
           pch = pich[i], type = "p", lwd = lwd, cex=cex, ...)
    points(x=db[,(2*nfactors)+i], db[,"position"]+(vertdist*(i-1)), 
           pch = pitx[i], type = "p", lwd = lwd, cex=cex, ...)
    segments(x0=db[,i], y0=db[,"position"]+(vertdist*(i-1)), 
             x1=db[,i]+db[,nfactors+i], 
             y1=db[,"position"]+(vertdist*(i-1)), lwd = lwd, 
             lty = lty, col = errbar.col, cex=cex, ...)
    segments(x0=db[,i], y0=db[,"position"]+(vertdist*(i-1)), 
             x1=db[,i]-db[,nfactors+i], 
             y1=db[,"position"]+(vertdist*(i-1)), lwd = lwd, 
             lty = lty, col = errbar.col, cex=cex, ...)
  }
  abline(v=seq(limits[1], limits[2], 0.5), col=gray(.5), lty="dotted", lwd = lwd, ...)
  abline(h=c(0.7:(nitems+0.7)), col="black", lty="dotted", lwd = lwd, ...)
  legend(leg.pos, legend=c(paste0("Factor ", 1:nfactors), "Empty symbol: standard", "Filled symbol: bootstrap"), pch=c(pich[1:nfactors], 0, 0), cex=cex.leg*cex, pt.cex=cex, col=colegend, bg=NA, bty=bty, ...)
}