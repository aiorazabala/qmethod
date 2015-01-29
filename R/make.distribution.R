make.distribution <- function(nstat, max.bin = 5) {

	# Input validation
	if (!is.vector(nstat)) {
		stop("nstat is not a vector.")
	}
	if (!is.vector(max.bin)) {
		stop("max.bin is not a vector.")
	}
	if (!(mode(nstat) == "numeric" && nstat > 0 && nstat %% 1 == 0)) {
		stop("The nstat specified is not a positive integer.")
	}
	if (!(mode(max.bin) == "numeric" && max.bin > 0 && max.bin %% 1 == 0)) {
		stop("The max.bin specified is not a positive integer.")
	}

	nbins <- (2 * max.bin) + 1 # make sure that nbins is uneven
 	# this +1 ensures that there will always be a 0 bin.
	item.pos <- qnorm( # to the left of which value lies...
		1:(nstat) / (nstat+1)# ... the n-statement?
		# using nstat + 1 because we want midpoints, not cutoffs for later
	)
	bins <- cut(
		x = item.pos,
		breaks = nbins,
		ordered_result = TRUE,
		include.lowest = FALSE,
		right = FALSE,
		labels = c(-max.bin:max.bin)
	)
	bins
	distribution <- summary(bins)
	distribution <- as.numeric(distribution)
	return(distribution)
}