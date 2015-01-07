ImportQSample <- function(dir.concourse, translations, file.ids, q.distribution, file.sample) {
	# Imports full items per translation (full.trans) from a directory with items as /trans/*.tex files, where * is the item short handle (short) and returns them in a dataframe with item identifiers (id), with items as rows.
	#
	# Args:
	#   dir.concourse: a directory of folders per translation, with concourse of items as individual *.tex files inside each translation folder, with file names as (short) item handles and same across all translations.
	#   translations: a vector of languages, same as folders in dir.concourse.
	#   file.ids: *a* csv with 'short', and 'id' as columns
	#   file.sampling.structure: csv with sampled 'short' as column can be found; other columns will also be imported.
	#   q.distribution: vector of the q distribution
	#
	# Returns:
	#   a dataframe q.sample with items as rows, short, id, hash, full.[lang] as columns, as well as other metadata from file.sampling.structure


	# Preambs =====================================================================
	require(digest) #   for hashing, maybe later


	# Set up data structure =================================================

	q.sample <- read.csv(file.sample) #  read in sampling structure
	q.ids <- read.csv(file.ids) #  TODO this should be replaced by hashing!
	q.sample <- merge(q.ids, q.sample) #  merge the two

	if (nrow(q.sample) != sum(q.distribution)) { #  test if sums are equal
		stop(paste("There are", nrow(q.items), "items in your q-sample, but", sum(q.distribution), "entries expected in the q-distribution", sep=" "))
	}


	# Reading in all full.translation ========================

	for (lang in translations) {
		for (n in 1:nrow(q.sample)) { #  for every row in the dataframe
			short <- q.sample$short[n] #  assign the short handle
			full.file <- paste(dir.concourse, "/", lang, "/", short, ".tex", sep="") #  asign the path
			full <- readChar(full.file, file.info(full.file)$size) # assign the full text
			q.sample[n, paste("full.", lang, sep="")] <- full
		}
	}
	return(q.sample)
}

