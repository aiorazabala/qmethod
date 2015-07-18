pop.sd <- function(x, ...){
  # this is just a simple helper function that returns the POPULATION standard deviation
  
  # No input validation is required, in addition to what sd() already does
  
	return(sd(x, ...) * sqrt((length(x)-1)/length(x)))
}

