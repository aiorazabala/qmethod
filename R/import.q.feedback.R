import.q.feedback <- function(q.feedback.dir, q.sorts, q.set, manual.lookup=NULL) {

	# Set up empty array ==========================================================
	q.feedback <- q.sorts # copy data structure from q.sorts
  q.feedback[ ] <- NA  # make NA for now

  # Take care of one-condition case
  if (length(dim(q.feedback))==2) {  # if only one-condition
    conditions <- "only.one"  # assign this dummy
  } else {  # if more than one
    conditions <- unlist(dimnames(q.feedback)[3])  # assign dimname
  }
  participants <- unlist(dimnames(q.feedback)[2])  # extract participants

	# Create lookup table =======================================================
	if (is.null(manual.lookup)) {  # automatic hashing, same as in make cards
	  lookup.table <- apply(  # replace every language field with its hash
	    X = q.set,
	    MARGIN = c(1,2),
	    digest,
	    algo = "crc32",
	    serialize = FALSE
	  )
	} else {  # manually entered lookup table
	  lookup.table <- manual.lookup
	}
	if (any(duplicated(lookup.table))) {  # test lookup table
	  stop ("There are duplicate IDs in the lookup table.")
	}

  # Import Loops ==============================================================
	for (cond in conditions) {  # loop over the conditions (such as before, after)
		for (part in participants) {  # loop over participants
			path <- paste( # establish path
        q.feedback.dir,
        if (length(conditions) > 1) {  #if more than 1 cond
          paste(
            cond,  # add condition to path
            "/",
            sep = ""
          )
        },  # otherwise, keep path simple
        part,
        ".csv",
        sep = ""
      )
			if (file.exists(path)) {  # not everyone comments
				current.feedback <- read.csv(
          path,
					header = TRUE, #  these do have names
					stringsAsFactors = FALSE, #  would only add confusion
					row.names = 1,
					colClasses = c("character","character","logical"),
					na.strings = "" #  empty cells become NAs
				)
				current.feedback <- current.feedback[!(current.feedback[,2]),]  # drop corrections
        for (id in rownames(current.feedback)) {  # loops over ids
				  if (any(lookup.table == id)) {  # do we know the id in the current feedback?
				    row <- which(lookup.table == id, arr.ind=TRUE)[,1]  # where is it in the table?
				    handle <- rownames(lookup.table)[row]  # what is the short handle?
				    row.names(current.feedback[id,]) <- handle  # reassign it
            # Gathering data into array
				    q.feedback[handle,part,cond] <- current.feedback[id,1]
				  } else {
				    warning(
				      paste(
				        "Feedback in",
				        path,
				        "under id",
				        id,
				        "is not defined in the lookup table and was ignored."
				      )
				    )
				  }
			  }
		  }
		}
	}
  if (length(conditions)>1) {  # if more than one condition
	  return(q.feedback)
  } else {  # if only one condition
    return(q.feedback[,,1])
  }
}
