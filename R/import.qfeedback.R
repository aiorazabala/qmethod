import.qfeedback <- function(dir.feedback, dir.sorts, q.sample, q.distribution) {
	# Import item feedback into an array, same structure as q.sorts
	#
	# Args:
	#   dir.sorts: a relative path to a directory structure where:
	#     - folders are conditions (such as before or after)
	#     - filenames of csvs are participant names (ideally anonymized)
	#     - csvs contain items as rows, with feedback per item (if given)
	#   sample: a dataframe that includes as columnames
	#     - id some random string for identification of items
	#     - hash an automatized hash to serve for identification in future
	#     - full.lang the full item in various translations
	#     - short some meaningful short string to describe the item
	#     PS: why the effort with the random ids? so that participants don't get unintended cues from meaningful short strings.
	# Returns:
	#   a three-dimensional array 'q.items' with
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

	q.feedback <- array(
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
			path <- paste(dir.feedback, "/", c, "/", p, ".csv", sep = "")  # establish path
			if (file.exists(path)) {  # not everyone comments
				participant <- read.csv(path,
					# *participant* (singular) is the single participant in current loop
					header = TRUE, #  these do have names
					stringsAsFactors = FALSE, #  would only add confusion
					nrows = max(q.distribution), # stuff below is ignored (item feedback, scores etc.)
					row.names = 1,
					colClasses = c("character","character","logical"),
					na.strings = "" #  empty cells become NAs
				)
				participant <- as.matrix(participant) #  because read.csv makes dataframe

				if (!all(rownames(participant) %in% q.sample$id)) { #  all feedback on sampled items?
					warning(paste( #  warning will do, not fatal just FYI
						"Some feedback from",
						path,
						"did not pertain to the sample and was ignored."
						))
				}
				# Gathering data into array ==============================================
				commented.ids <- row.names(participant)
				for (id in commented.ids) {
					comment <- participant[id,1]
					short <- as.character(q.sample$short[which(q.sample$id==id)])
					if (is.na(participant[id,2])) {
						stop(paste(
							"Feedback from",
							path,
							"does not specify whether it is a correction."
							))
					}
					if (participant[id,2]==FALSE) {
						q.feedback[short,p,c] <- comment
					}
				}
			}
		}
	}
	return(q.feedback)
}
