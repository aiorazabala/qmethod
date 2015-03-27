\name{import.q.sorts}
\alias{import.q.sorts}
\title{Q methodology: import Q-sorts from CSV}
\description{
	Turns raw Q-sorts (from *.CSV) into a Q-sorts array (when there are > 2 conditions) or matrix (with single condition).
}
\usage{
import.q.sorts(q.sorts.dir,	q.set, q.distribution, 
               conditions = NULL, manual.lookup = NULL)
}

\arguments{
  \item{q.sorts.dir}{
		A relative path to a directory structure where:
		\itemize{
			\item (optional) folders are (within-subjects) \code{conditions} (such as \emph{before} or \emph{after}), if there is more than one condition as per the \code{conditions} argument.
			If there are no \code{conditions} (defaults to \code{NULL}), there should be no folders.
			\item file names of *.CSV files are participant names (might be given pseudonyms).
			\item *.CSV files contain \emph{raw} Q-sorts, where each line contains item IDs, such as \samp{,,sta12,sta64,,}.
			All lines below the maximum Q-sort height will be ignored and can have arbitrary input.
			For example, in a study with a highest column of 8 cards, everything below the 8th line in the file will be ignored.
			There is no need to include the values of the x-axis (say, \code{-4} to \code{+4}) in these files.
			If they are included, they should be the last row.
			\item cells contains either all manual or all automatic item IDs (such as \kbd{"sta12"}), both as produced by \code{\link{make.cards}}.
		}
	}
	\item{q.set}{
		A matrix with handles as row names, languages (if applicable) in columns, as read in by \code{\link{build.q.set}}.
	}
	\item{q.distribution}{
		The chosen Q distribution as a vector of integers, such as \code{c(1,3,1)}.
	}
	\item{conditions}{
		A character vector of (optional) study (within-subjects) conditions, such as 
    
    \code{c("before","after")}, same as folders under \code{q.sorts.dir}.
		Defaults to \code{NULL} in which case there is only one condition, and *.CSV files are expected directly under \code{q.sorts.dir}.
	}
	\item{manual.lookup}{
		A matrix with handles (such as \kbd{q-is-great}, same as in \code{\link{build.q.set}}, 
    
    \code{\link{import.q.concourse}}) as row names, and arbitrary strings (item IDs, such as \code{"it212"}) in any of the columns as printed on the Q-cards by \code{\link{make.cards}}.
		Defaults to \code{NULL} in which case items are automatically identified by automatic hash IDs, as also produced by \code{\link{make.cards}}.
	}
}

\details{

	This function imports Q-sorts from their raw format stored in *.CSV files, in the form in which they were sorted by participants (applicable to Q-sorts with forced distributions only).

    Q-sorts in their raw form have columns as ranks (from, say, \code{-6} to \code{+6}) with cards (items) sorted in rows.
	The vertical dimension of Q-sorts is \emph{meaningless}.


Q-sorts are conveniently entered as *.CSV (comma separated values) files in standard spreadsheet editors.
  This function ignores any rows in the files below the maximum height of columns expected from \code{q.distribution}.

    It is recommended that Q-sort data are kept in their rawest form, with clear documentation of any processing applied to this data.
	This is also good practice for reproducible research.

	Q-sorts are best entered not by typing up the full form of an item, but some unique string (ID) printed on the card.
	This function, and, analogously, \code{\link{make.cards}} and \code{\link{import.q.feedback}} offer a manual and automatic way to create these IDs, which are then expected as input (see \link{qmethod-package} for details).
  
  The automatic summary of full item wordings, technically known as \emph{hashing}, proceeds internally by passing the full item wording to the \code{\link[digest]{digest}} function of the package \pkg{digest} (with arguments set to \code{algo = crc32, serialize = FALSE}.)

	Q-sorts are conveniently entered as *.CSV (comma separated values) files in standard spreadsheet editors.

	This function includes a number of tests to verify the integrity of entered Q-sorts:
	\enumerate{
		\item \code{manual.lookup} tables provided are tested for duplicate identifiers.
		\item Function returns a warning if some participants do not have Q-sort files under all conditions (applies only if there are more than one \code{conditions}).
		\item Function errors out if there are item IDs in a Q-sort not matched by any manually or automatically specified ID, respectively (see \code{\link{qmethod-package}} for details).
		\item Function errors out if the distribution in a given Q-sort does not conform to the defined \code{q.distribution}.
		\item Function errors out if there are items in the sample \code{q.set} that cannot be found in any given Q-sort.
		\item Function errors out if there are items in a given Q-sort that cannot be found in the sample \code{q.set}.
	}

If you are not familiar with the terminology of item \emph{handle}, \emph{ID} and \emph{wording} or the file structure expected for import functions, please read the respective sections in the documentation for \code{\link{qmethod-package}} first or consider the package \href{https://github.com/aiorazabala/qmethod/wiki/Data-management}{wiki}

}

\value{
	Returns a matrix (when there is a single condition) or array (with two or more conditions) with handles as row names, people as column names, conditions (if more than one) as 3rd dimension and Q-sort ranks in cells, as expected for analysis by \code{\link{qmethod}}.

  Notice that \code{\link{qmethod}} expects a matrix (with two dimensions).
	If you have several conditions, and therefore an array of data, you must pass them to \code{\link{qmethod}} in individual 'slices' of conditions, using subsetting.
}

\note{
	This function currently works only with forced distributions.
	If you would like it to work with free distributions, check out and/or chip in at \url{https://github.com/aiorazabala/qmethod/issues/33}.

 When argument \code{manual.lookup} is set to \code{NULL}, IDs are computed by "summarising" the complete item wordings (\code{"Q Method is used by a crazy, but charming community of ..."}) into a hexadecimal number (\code{"ae128fs"}), a process known as cryptographic hashing. These hash values change whenever anything in the full item wordings is changed, and allow a precise identification of different versions of an item. This function never exposes users to the hash values; the encrypting and decrypting are done under the hood by the respective functions. Automatic, hashed IDs are generally recommended and easier to use, but some caveats apply.

	Hashed identification has not been widely tested in Q studies and should be used with great care and only for extra convenience.
	When using hash identification, researchers should be careful to record the precise item wordings at the time of hashing for the printed Q-cards, preferably with a version control system.
	Researchers should also record the complete Q-sorts of participants in an \emph{unhashed} form, such as a picture of the completed sort in full wordings, in case problems with the hashing arise.

	This function does \emph{not} test whether Q-sorts were entered correctly into the *.CSV files.
	It is recommended to enter any given Q-sort more than once and have a spreadsheet editor compare several entry attempts for consistency.
	This function ignores any entries in *.CSV files below the highest row expected by the \code{q.distribution}.
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
## Import a Q sample from a directory of *.CSV files
data(importexample)
path.sorts <- paste(                 # this part is only for the example!
  path.package("qmethod"),           # just to make sure, use absolute path
  # import example files are in root/extdata of package:
  "/extdata/importexample/qsorts/",  # location of sorts
  sep = ""
  )
q.sorts <- import.q.sorts(           # now import the sorts
  q.sorts.dir = path.sorts,          # add your path here
  q.set = importexample$q.set,       # as produced by build.q.set
  q.distribution = c(1,2,1),         # very simple distribution
  conditions = c("before","after"),  # enter your conditions here, same as in path
  manual.lookup = matrix(  # ideally empty for automatic hashing, or read in from *.CSV file
    c("i01","i02","i03","i04"),
    ncol = 1,
    nrow = 4,
    dimnames = list(c("r-dominance","q-uprising","small-village","life-with-q"),"ID")
    )
  )
}
