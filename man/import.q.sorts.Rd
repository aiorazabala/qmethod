\name{import.q.sorts}
\alias{import.q.sorts}
\title{Q methodology: imports q sorts from CSV}
\description{
	Turns raw Q sorts (from .CSV) into clean, verified q.sorts array or matrix.
}
\usage{
	import.q.sorts(
		q.sorts.dir,
		q.set,
		q.distribution,
		conditions=NULL,
		manual.lookup=NULL
	)
}

\arguments{
  \item{q.sorts.dir}{
		A relative path to a directory structure where:
		\itemize{
			\item folders are \code{conditions} (such as \emph{before} or \emph{after}).
			If there are no \code{conditions} (defaults to \code{NULL}), there should be no folders.
			\item filenames of .CSVs are participant names (might be given pseudonyms).
			\item .CSVs contain \emph{raw} qsorts, beginning with all lines of items, such as \code{,,item-12,item-64,,}.
			There is no need to include the values of the x-axis (say, \code{-4} to \code{+4}) in these files.
			If they are included, they should be the last row.
			\item cells contain \emph{not} meaningful short handles, but some string (ID) to identify them as per the \code{manual.lookup} option.
			If there is no \code{manual.lookup} (defaults to \code{NULL}), this function expects automatically created hashes as created by \code{\link{make.cards}}.
		}
	}
	\item{q.set}{
		A matrix with short handles as rownames, languages (if applicable) in columns, as produced by \code{\link{build.q.set}}.
	}
	\item{q.distribution}{
		The chosen Q distribution as a vector of integers, such as \code{c(1,3,1)}.
	}
	\item{conditions}{
		A character vector of study conditions, such as \code{c("before","after")}, same as folders under \code{q.sorts.dir}.
		Defaults to \code{NULL} in which case there is only condition, and CSVs are expected directly under \code{q.sorts.dir}.
	}
	\item{manual.lookup}{
		A matrix with meaningful short handles (same as in \code{\link{build.q.set}}, \code{\link{import.q.concourse}}) as rownames, and arbitary, unique identifying strings (ID) in any of the columns as printed on the q-cards by \code{\link{make.cards}}.
		Defaults to \code{NULL} in which case items are automatically identified by full item hashes, as also used by \code{\link{make.cards}}.
	}
}

\details{
	Q sorts in their raw form, as created by participants, have columns as ranks (from, say, \code{-6} to \code{+6}) with cards (items) sorted in rows.
	The vertical dimension of Q sorts is \emph{meaningless}.
	For reproduceable research, Q sorts, as all data, should be maintained in their rawest form and any processing should be well-documented.

	That is what this function offers, by reading in Q sorts from the form in which they were entered by participants.

	Q sorts are best entered not by typing up the full form of an item, but some unique string (ID) printed on the card.
	So as \emph{not} to influence participants, these strings should be meaningless to humans.
	This function offers either a set of arbitrary strings hard entered by a researcher for identification in \code{manual.lookup}, or expects a cryptographic hash of the complete item wording, as conveniently also created by \code{\link{make.cards}}.
	These hash values change whenever \emph{anything} in the full item wordings is changed, and allow a precise idenfication of different versions of an item.

	Q sorts are conveniently entered as *.CSV (comma separated values) files in popular spreadsheet editors.
	This function will ignore any rows in the files below the maximum height of columns expected from the \code{q.distribution}.

	This function includes a number of tests to verify the integrity of entered Q sorts:
	\enumerate{
		\item \code{manual.lookup} tables provided are tested for duplicate identifiers.
		\item Function returns a warning if some participants do not have Q sort files under all conditions (applies only if there are more than one \code{conditions}).
		\item Function errors out if there are IDs in a Q sort not defined in the (automatic or manual) lookup table.
		\item Function errors out if the distribution in any given Q sort does not conform to the defined \code{q.distribution}.
		\item Function errors out if there are items in the sample \code{q.set} that cannot be found in any given Q sort.
		\item Function errors out if there are items in any given Q sort that cannot be found in the sample \code{q.set}.
	}

	More details on the files and directory structure expected by this function can be found in the \link{qmethod-package} documentation, and on the package \href{https://github.com/aiorazabala/qmethod/wiki/Data-management}{wiki}.
}

\value{
	Returns a matrix or array (of more than one condition) with short handles as rownames, people as colnames, conditions (if more than one) as 3rd dimension and ranks in cells, as expected for analysis by \code{\link{qmethod}}.
}

\note{
	This function currently works only with forced distributions.
	If you want it to work with free distributions, check out and/or chip in at \url{https://github.com/aiorazabala/qmethod/issues/33}.

	Hashed identification has not been widely tested in Q studies and should be used with great care and only for extra convenience.
	When using hash identification, researchers should be careful to record the precise item wordings at the time of hashing for the printed Q cards, preferably with a version control system.
	Researchers should also record the complete Q sorts of participants in an \emph{unhashed} form, such as a picture of the completed sort in full wordings, in case problems with the hashing arise.

	The one test this function \emph{cannot} do is whether Q sorts were entered correctly into the .CSVs.
	It is recommended to enter any given Q sort more than once and have a spreadsheet editor compare several entry attempts for consistency.
	This function conveniently ignores any entries in CSVs below the highest row expected by the \code{q.distribution}.
}

\author{Maximilian Held}

\seealso{
	\code{\link{import.q.concourse}},
	\code{\link{import.q.feedback}},
	\code{\link{build.q.set}},
	\code{\link{make.cards}},
	\code{\link{qmethod}}
}

\examples{
  ## Import a Q sample from a directory of CSVs
  data(importexample)
  path.sorts <- paste(  # this part is only for the example!
    path.package("qmethod"),  # just to make sure, use absolute path
    # import example files are in root/extdata of package
    "/extdata/importexample/qsorts/",  # location of sorts
    sep = ""
  )
  q.sorts <- import.q.sorts(  # now import the sorts
    q.sorts.dir = path.sorts,  # add your path here
    q.set = importexample$q.set,  # as produced by build.q.set
    q.distribution = c(1,2,1), # very simple distribution
    conditions = c("before","after"),  # enter your conditions here, same as in path
    manual.lookup = matrix(  # ideally empty for automatic hashing, or read in from csv
      c("i01","i02","i03","i04"),
      ncol = 1,
      nrow = 4,
      dimnames = list(c("r-dominance","q-uprising","small-village","life-with-q"),"ID")
    )
  )
}
