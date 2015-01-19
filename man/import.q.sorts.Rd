\name{import.q.sorts}
\alias{import.q.sorts}
\title{Q methodology: imports q sorts from csvs}
\description{
	Turns raw q-sorts (from csv) into clean, veriefied q.sorts array or matrix.
}
\usage{
	import.q.sorts(
		q.sorts.dir,
		q.set,
		q.distribution, conditions=NULL,
		manual.lookup=NULL
	)
}

\arguments{
  \item{q.sorts.dir}{
		A relative path to a directory structure where:
		\itemize{
			\item folders are \code{conditions} (such as before or after), if there are more than one conditions.
			\item filenames of \code{*.csv}s are participant names (might be given pseudneums)
			\item csvs contain raw qsorts, beginning with all lines of items (not the values or), such as \code{,,item-12,item-64,,}
			\item cells contain \emph{not} actual meaningful short handles, but some string to identify them as per \code{manual.lookup} option.
		}
	}
	\item{q.set}{
		A matrix with short handles as rownames, languages in columns, as produced by \code{\link{build.q.set}}.
	}
	\item{q.distribution}{
		The chosen q distribution as a vector of integers, such as \code{c(1,3,1)}.
	}
	\item{conditions}{
		A character vector of study conditions, such as \code{c("before","after")}, same as folders under \code{q.sorts.dir}.
		Defaults to NULL in which case there are is only condition, and \code{*.csv}s are directly under \code{q.sorts.dir}.
	}
	\item{manual.lookup}{
		A matrix with meaningful short handles (same as in \code{\link{build.q.set}}, \code{\link{import.q.concourse}}) as rownames, and arbitary, unique identifying strings in any of the columns as printed on the q-cards by \code{\link{make.cards}}.
		Defaults to NULL in which case items are automatically identified by full item hashes, as also used by \code{\link{make.cards}}.
	}
}

\details{
Q-sorts in their raw form, as created by participants, have columns as ranks (from, say, -6 to +6) with cards (items) sorted in rows.
The vertical dimension of q-sorts is \emph{meaningless}.
For reproduceable research, q-sorts, as all data, should be maintained in their rawest form and any processing should be well-documented.

That is what this function offers, by reading in q-sorts from the form in which they were entered by participants.

Q-sorts are best entered not by typing up the full form of an item, but some unique string printed on the card.
So as \emph{not} to influence participants, these strings should be meaningless to humans.
This function offers either a set of arbitrary strings hard entered by a researcher for identification in \code{manual.lookup}, or expects a cryptographic hash of the complete item wording, as conveniently also created by \code{\link{make.cards}}.
These hash values change whenever \emph{anything} in the full item wordings is changed, and allow a precise idenfication of different versions of an item.

Q-sorts are conveniently entered as \code{*.csv} (comma separated values) in popular spreadsheet editors.
This function will ignore any rows in the files below the maximum height of columns expected from the q-distribution.

This function includes a number of tests to verify the integrity of entered q-sorts:
\enumerate{
	\item \code{manual.lookup} tables provided are tested for duplicate identifiers.
	\item Function returns a warning if some participants do not have q-sort-files under all conditions (applies only if there are more than one conditions).
	\item Function errors out if there are identifiers in a q-sort not defined in the (automatic or manual) lookup table.
	\item Function errors out if the distribution in a q-sort does not conform to the defined \code{q.distribution}.
	\item Function errors out if there are items in the sample \code{q.set} that cannot be found in a q-sort.
	\item Function errors out if there are items in a q-sort that cannot be found in the sample \code{q.set}.
}
}

\value{
	Returns a matrix or array (of more than one condition) with short handles as rownames, people as colnames, conditions (if more than one) as 3rd dimname and ranks in cells, as expected for analysis by \code{\link{qmethod}}.
}

\note{
	This function currently works only with forced distributions.
	If you want it to work with free distributions, check out and/or chip in at \url{https://github.com/aiorazabala/qmethod/issues/33}.

	Hashed identification has not been widely tested in q studies and should be used with great care and only for extra convenience.
	When using hash identification, researchers should be careful to record the precise item wordings at the time of hashing for the printed q-cards, preferably with a version control system.
	Researchers should also record the complete q-sorts of participants in an \emph{unhashed} form, such as a picture of the completed sort in full wordings, in case problems with the hashing arise.

	The one test this function cannot do is whether q-sorts were entered correctly into the \code{*.csv}s.
	It is recommended to enter any given q-sort more than once and have a spreadsheet editor compare several entries for consistency.
	This function conveniently ignores any entries in \code{*.csv}s below the highest row expected by the q-distribution.
}

\author{Maximilian Held}

\seealso{
	\code{\link{import.q.concourse}},
	\code{\link{import.q.feedback}},
	\code{\link{build.q.set}},
	\code{\link{make.cards}},
	\code{\link{qmethod}}
}