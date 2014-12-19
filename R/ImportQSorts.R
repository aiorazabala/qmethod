ImportQSorts <- function(dir.sorts, q.sample, q.distribution) {
	# Turn raw q-sorts (from csv) into clean, verified qsorts array.
	#
	# Args:
	#   dir.sorts: a relative path to a directory structure where:
	#     - folders are conditions (such as before or after)
	#     - filenames of csvs are participant names (ideally anonymized)
	#     - csvs contain raw qsorts, easily entered with a spreadsheet editor
	#     - cells are qsample items by their id 'id
	#   sample: a dataframe that includes as columnames
	#     - id some random string for identification of items
	#     - hash an automatized hash to serve for identification in future
	#     - full.lang the full item in various translations
	#     - short some meaningful short string to describe the item
	#     PS: why the effort with the random ids? so that participants don't get unintended cues from meaningful short strings.
	# Returns:
	#   a three-dimensional array 'q.sorts' with
	#     - 'rows' as q.items (also in dimnames)
	#     - 'columms' as participants (also in dimnames), read from filenames
	#     - third-dim ('levels') as treatment conditions (also in dimnames), read from folder names
	#   throws errors when there's something fishy with the imported csvs


	# Preamble ===================================================================
	require(tools)


	# Set up data structure =======================================================
	participants <- list.files( #  find *all* participants over all  conditions
		dir.sorts, #  this should become a variable later on
		all.files = FALSE, #  don't look at invisibles
		recursive = TRUE, #  do look at before/after folders
		full.names = TRUE, #  don't include relative path, just filename
		pattern = "\\.csv$" #  look only at csvs
	)
	participants <- basename(participants) #  kill paths
	participants <- file_path_sans_ext(participants) #  kill extensions
	participants <- unique(participants) #  kill duplicates

	conditions <- list.dirs( #   Find treatment conditions
		dir.sorts,
		full.names = FALSE,
		recursive = FALSE #  they have to be at toplevel
	)
	conditions <- factor(conditions) #  because that's what they are...


	# Set up empty array ==========================================================

	q.sorts <- array(
		#  participants, conditions, items makes 3 dimensions, all of which integer
		data = , #  no such thing yet, so all NAs (that should be the baseline!)
		dim = c(
			nrow(q.sample), #  number of items
			length(participants), #  number of conditions
			length(conditions) #  before and after
		),
		dimnames = list( #  dims should be called accordingly ...
			q.sample$short, #  those are the items by meaningful short names
			participants, #  those are the participants
			conditions #  those are the conditions
		)
	)


	# Import Loops ==============================================================

	for (c in conditions) {  # loop over the conditions (such as before, after)
		for (p in participants) {  # loop over participants
			path <- paste(dir.sorts, "/", c, "/", p, ".csv", sep = "")  # establish path
			if (!file.exists(path)) {  # there may be missing cases for some conditions
				warning(paste(  # it's not a deal-breaker just a warning
					"There is no file for",
					p,
					"under condition",
					c,
					". NAs remains in array.",
					sep = " "
				))
			} else {
				participant <- read.csv(path,
					# *participant* (singular) is the single participant in current loop
					header = FALSE, #  colnames will do
					stringsAsFactors = FALSE, #  would only add confusion
					nrows = max(q.distribution), # stuff below is ignored (item feedback, scores etc.)
					na.strings = "" #  empty cells become NAs
				)
				participant <- as.matrix(participant) #  because read.csv makes dataframe


				# Test content of csv for consistency ==================================

				if (!all(colSums(!is.na(participant)) == q.distribution)) {  # distr. ok?
					stop(paste(
						"The qsort in",
						path,
						"does not conform to the set distribution.",
						"Offending columns:",
						colSums(!is.na(participant)) == q.distribution,
						sep = " "
					))
				}
				participant_ids <- c(as.character(participant))  # preparation
				participant_ids <- na.omit(participant_ids)  # NAs confuse the comparisons
				sample_ids <- c(as.character(q.sample$id))  # preparation
				if (!all(sample_ids %in% participant_ids)) {  # all sampled items in sort?
					stop(paste(  # must error out b/c of inconsistency
						"Some items from the sample cannot be found in",
						path,
						".",
						"Missing IDs:",
						q.sample$id[!(sample_ids %in% participant_ids)],
						"Missing Items:",
						q.sample$short[!(sample_ids %in% participant_ids)],
						sep = " "
					))
				}
				if (!all(participant_ids %in% sample_ids)) { # all part items in qsample?
					stop(paste(  # must error out b/c of inconsistency
						"Some items from",
						path,
						"cannot be found in the sample",
						".",
						"Missing IDs:",
						q.sample$id[!(participant_ids %in% sample_ids)],
						"Missing Items:",
						q.sample$short[!(participant_ids %in% sample_ids)],
						sep = " "
					))
				}
				# Transposing and aggreation of individual qsort =========================
				for (id in q.sample$id) {  # loops over items
					# Replaces item_id with item_short within participant qsort
					participant[participant==id] <- as.character(q.sample$short[which(q.sample$id == id)])
				}
				for (short in q.sample$short) {  # loops over items
					q.sorts[short,p,c] <- which( #  write in qsort content of
						participant == short,  # participant cell of curr. looped item_short
						arr.ind = TRUE,  # return complete index (row, column)
						useNames = TRUE
						)[2] # return only column
					q.sorts[short,p,c] <- as.integer(q.sorts[short,p,c]-((length(q.distribution)+1)/2))
					# make it into integers, revert to original score
				}
			}
		}
	}
	return(q.sorts)
}
