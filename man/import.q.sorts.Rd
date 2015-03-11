\name{import.q.sorts}
\alias{import.q.sorts}
\title{Q methodology: imports q sorts from CSV}
\description{
	Turns raw Q sorts (from .CSV) into clean, verified q.sorts array (when there are > 2 conditions) or matrix (with single condition).
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
			\item filenames of .CSV files are participant names (might be given pseudonyms).
			\item .CSV files contain \emph{raw} qsorts, beginning with all lines of items, such as \code{,,item-12,item-64,,}.
			All lines below the maximum Q Sort height will be ignored and can have arbitrary input.
			For example in a study with a highest column of 8 cards, everything below the 8th line th file will be ignored.
			There is no need to include the values of the x-axis (say, \code{-4} to \code{+4}) in these files.
			If they are included, they should be the last row.
			\item cells contain a string ID to identify.
			Meaningful short handles should \emph{not} be used here.
			If a \code{manual.lookup} is specified, strings from that matrix (see below) are expected.
			Otherwise, hash values as produced by \code{\link{make.cards}} are expected for identification.
		}
	}
	\item{q.set}{
		A matrix with (meaningful, short) handles (\code{''q-is-great''}, for example) as rownames, languages (if applicable) in columns, as produced by \code{\link{build.q.set}}.
	}
	\item{q.distribution}{
		The chosen Q distribution as a vector of integers, such as \code{c(1,3,1)}.
	}
	\item{conditions}{
		A character vector of study conditions, such as \code{c("before","after")}, same as folders under \code{q.sorts.dir}.
		Defaults to \code{NULL} in which case there is only condition, and CSVs are expected directly under \code{q.sorts.dir}.
	}
	\item{manual.lookup}{
		A matrix with (meaningful, short) handles (such as \code{''q-is-great''}, same as in \code{\link{build.q.set}}, \code{\link{import.q.concourse}}) as rownames, and arbitary, unique identifying strings (ID, such as \code{''it212''}) in any of the columns as printed on the q-cards by \code{\link{make.cards}}.
		Defaults to \code{NULL} in which case items are automatically identified by full item hashes, as also used by \code{\link{make.cards}}.
	}
}

\details{
	This function imports Q sorts from their raw format stored in CSV files, in the form in which they were sorted by participants.

	Q sorts in their raw form have columns as ranks (from, say, \code{-6} to \code{+6}) with cards (items) sorted in rows.
	The vertical dimension of Q sorts is \emph{meaningless}.
	It is recommended that Q sort data are kept in their rawest form (with clear documentation of any processing applied on this data).
	This is good practice for reproducible research.

	Q sorts are best entered not by typing up the full form of an item, but some unique string (ID) printed on the card.
	So as \emph{not} to influence participants, these strings should be meaningless to humans.

	This function offers two ways to build such unique strings (IDs), paralleled by equivalent functions in \code{\link{make.cards}}.
	\enumerate{
		\item Using the \code{manual.lookup} argument, you can specify your own, \emph{manually} created IDs, such as \code{sta102}.
		See above argument \code{manual.lookup} for how to add your own IDs.
		\item By leaving the \code{manual.lookup} argument to \code{NULL} (as is default behavior), you can use \emph{automatically} created IDs, that are computed by ``summarizing'' the \emph{complete} item wordings (\code{''Q Method is used by crazy, but charming community of ...''}) into a hexadecimal string (\code{ae128fs}), a process known as cryptographic \emph{hashing}.
		These hash values change whenever \emph{anything} in the full item wordings is changed, and allow a precise identification of different versions of an item.
		This function never exposes users to the hash values; the encrypting and decrypting are done under the hood by this function and \code{\link{make.cards}}.
		See the package \href{https://github.com/aiorazabala/qmethod/wiki/Data-management}{wiki} for more information.
	}

	Q sorts are conveniently entered as *.CSV (comma separated values) files in standard spreadsheet editors.
	This function ignores any rows in the files below the maximum height of columns expected from the \code{q.distribution}.

	This function includes a number of tests to verify the integrity of entered Q sorts:
	\enumerate{
		\item \code{manual.lookup} tables provided are tested for duplicate identifiers.
		\item Function returns a warning if some participants do not have Q sort files under all conditions (applies only if there are more than one \code{conditions}).
		\item Function errors out if there are IDs in a Q sort not defined in the (automatic or manual) lookup table.
		\item Function errors out if the distribution in a given Q sort does not conform to the defined \code{q.distribution}.
		\item Function errors out if there are items in the sample \code{q.set} that cannot be found in any given Q sort.
		\item Function errors out if there are items in a given Q sort that cannot be found in the sample \code{q.set}.
	}

	More details on the files and directory structure expected by this function can be found in the \link{qmethod-package} documentation, and on the package \href{https://github.com/aiorazabala/qmethod/wiki/Data-management}{wiki}.
}

\value{
	Returns a matrix (when there is a single condition) or array (with two or more conditions) with (meaningful, short) handles as row names, people as column names, conditions (if more than one) as 3rd dimension and Q sort ranks in cells, as expected for analysis by \code{\link{qmethod}}.
	Notice that \code{\link{qmethod}} expects a matrix (with two dimensions).
	If you have several conditions, and therefore an array of data, you must pass them to \code{\link{qmethod}} in individual ``slices'' of conditions, using subsetting.
}

\note{
	This function currently works only with forced distributions.
	If you want it to work with free distributions, check out and/or chip in at \url{https://github.com/aiorazabala/qmethod/issues/33}.

	Hashed identification has not been widely tested in Q studies and should be used with great care and only for extra convenience.
	When using hash identification, researchers should be careful to record the precise item wordings at the time of hashing for the printed Q cards, preferably with a version control system.
	Researchers should also record the complete Q sorts of participants in an \emph{unhashed} form, such as a picture of the completed sort in full wordings, in case problems with the hashing arise.

	This function does \emph{not} test whether Q sorts were entered correctly into the CSV files.
	It is recommended to enter any given Q sort more than once and have a spreadsheet editor compare several entry attempts for consistency.
	This function ignores any entries in CSV files below the highest row expected by the \code{q.distribution}.
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
