build.q.set <- function(q.concourse, q.sample, q.distribution) {
  q.sample <- as.character(q.sample) #  just to be safe
  # Test q.sample =============================================================
  if (length(q.sample) != sum(q.distribution)) { #  test if sums are equal
    stop(paste("There are", length(q.sample), "items in your q-sample, but", sum(q.distribution), "entries expected in the q-distribution", sep=" "))
  }

  # Set up data structure =================================================
  q.set <- q.concourse[q.sample,]  # only add sampled rows from concourse

  message(paste("Build a q.set of", nrow(q.set), "items."))
	return(q.set)
}
