make.cards <- function(q.set, study.language, paper.format, output.pdf = TRUE, manual.lookup = NULL) {
  # Read in items =============================================================
  q.set.print <- as.data.frame( #  read in complete q.set, all translations
    x = q.set,
    optional = TRUE
  )
  # Create lookup table (same as in import.q.feedback and import.q.sorts!)=====
  if (is.null(manual.lookup)) {  # in case there is no manual lookup
    lookup.table <- apply(  # replace every language field with its hash
      X = q.set,
      MARGIN = c(1,2),
      digest,
      algo = "crc32",
      serialize = FALSE
    )
  } else {  # in case of manually entered lookup table
    lookup.table <- manual.lookup  # just assign it
  }
  if (any(duplicated(lookup.table))) {  # test lookup table
    stop ("There are duplicate IDs in the lookup table.")
  }
  
  # Add ids to q.set.print ====================================================
  q.set.print$id <- NA  # set up empty id
  for (handle in rownames(q.set.print)) {  # loop over all ids in q.set
    if (is.null(manual.lookup)) {  # for automatic hashing
      q.set.print[handle,"id"] <- lookup.table[handle,study.language]
    } else {
      q.set.print[handle,"id"] <- lookup.table[handle]  # plug in id as row
    }

  }
  path <- paste(  # assign path to template
    path.package("qmethod"),  # where is the package?
    # remember, "inst" is not in path, because stuff from inst get put in root of package!
    "/cardtemplates/",
    paper.format,  # hopefully will have more templates in the future
    ".Rnw",
    sep = ""
  )
  if (output.pdf == TRUE) {
    return(
      knit2pdf(path)
    ) 
  } else {
    return(
      knit(path)
    )
  }
}
