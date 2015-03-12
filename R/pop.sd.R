pop.sd <- function(x, ...){
	return(sd(x, ...) * sqrt((length(x)-1)/length(x)))
}