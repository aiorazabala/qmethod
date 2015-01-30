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
	range <- qnorm(1/nstat) # what's the cutoff for 1/n?
	distribution <- dnorm(seq((-range),range, length = nbins))/sum(dnorm(seq(-range,range, length = nbins)))*nstat
	distribution <- round(distribution)
	if (sum(distribution) != nstat) {
		warning(
			paste(
				"Could not fit",
				nstat,
				"items neatly under a standard normal distribution.",
				"You need",
				sum(distribution),
				"items for good fit instead.",
				"Live with that or try again with a different max.bin."
			)
		)
	}
	return(distribution)
}