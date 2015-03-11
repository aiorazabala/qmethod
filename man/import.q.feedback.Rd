\name{import.q.feedback}
\alias{import.q.feedback}
\title{Q methodology: imports feedback on Q items}
\description{
	Turns raw item feedback (in *.CSV files) into a  verified array or matrix.
}
\usage{
	import.q.feedback(
		q.feedback.dir,
		q.sorts,
		q.set,
		manual.lookup=NULL
	)
}

\arguments{
  \item{q.feedback.dir}{
		A relative path to a directory structure where:
		\itemize{
			\item folders are \code{conditions} (such as before or after), if there are more than one conditions.
			\item filenames of *.CSV are participant names (might be given pseudonyms)
			\item *.CSV files within folders contain raw feedback, beginning with an arbitrary header line (ignored), and the following columns, starting from the left:
			\enumerate{
				\item An ID, either as an automatic hash from \code{\link{make.cards}} or manually specified per the \code{manual.lookup} options.
				Each ID only occurs once.
				\item The full feedback.
				\item Optionally, a logical indicator whether current line should be ignored (in which case it should be set to \code{TRUE}).
				If there is no such column, all feedback will be imported.
			}
		}
	}
	\item{q.sorts}{
		A matrix or array with handles as row names, participant as column names, conditions as 3rd dimension and cells as Q-sort ranks, as produced by \code{\link{import.q.sorts}}.
	}
	\item{q.set}{
		A matrix with handles as row names, languages (if applicable) in columns, as produced by \code{\link{build.q.set}}.
	}
	\item{manual.lookup}{
		A matrix with handles (same as in \code{\link{build.q.set}}, \code{\link{import.q.concourse}}) as row names, and arbitary, unique identifying strings (\emph{IDs}) (such as \code{'sta121'}) in any of the columns as printed on the Q-cards by \code{\link{make.cards}}.
		Defaults to \code{NULL} in which case items are automatically identified by hashes of full item wordings, as also used by \code{\link{make.cards}}.
	}
}

\details{
	Participants in Q studies are often invited to provide open-ended feedback on items, giving Q researchers a additional information on participants' viewpoints.
	This feedback is conveniently entered in a spreadsheet editor (2nd column), where each line of feedback corresponds to an item ID (1st column), which can take the form of either manually specified strings (such as \code{'sta121'}) as per the \code{manual.lookup} option, or be automatically generated strings, as produced by \code{\link{make.cards}}.
	An additional, optional (3rd) column indicates whether the current line should be ignored (TRUE), as may be the case for privacy reasons or when the feedback is merely a correction of a typographic error.
	If no such 3rd column is included, all feedback will be imported.

	After an (arbitrary) header line, a *.CSV file may look like this:
	\code{sta001,"This q-item sounds like r-research to me!",FALSE}, indicating that it should \emph{not} be ignored (\code{FALSE}).

	In order to allow commas in the feedback, this should be enclosed with quotes.

	More details on the files and directory structure expected by this function can be found in the \link{qmethod-package} documentation, and on the package \href{https://github.com/aiorazabala/qmethod/wiki/Data-management}{wiki}.
}

\value{
	Returns a matrix or array (of more than one condition) with handles as row names, people as column names, conditions (if more than one) as 3rd dimension name and item feedback in cells.
	The return parallels the output from \code{\link{import.q.sorts}}, only with feedback as array cells, not Q-sort ranks.
}

\author{Maximilian Held}

\seealso{
	\code{\link{import.q.concourse}},
	\code{\link{import.q.sorts}},
	\code{\link{build.q.set}},
	\code{\link{make.cards}},
	\code{\link{qmethod}}
}

\examples{
  data(importexample)
  path.feedback <- paste(  # this part is only for the example!
    path.package("qmethod"),  # just to make sure, use absolute path
    # import example files are in root/extdata of package
    "/extdata/importexample/feedback/",  # location of sorts
    sep = ""
  )
  q.feedback <- import.q.feedback(  # now import the feedback
    q.feedback.dir = path.feedback,  # add your path here
    q.sorts = importexample$q.sorts,
    q.set = importexample$q.set,  # as produced by build.q.set
    manual.lookup = matrix(  # ideally empty for automatic hashing, or read in from *.CSV
      c("i01","i02","i03","i04"),
      ncol = 1,
      nrow = 4,
      dimnames = list(c("r-dominance","q-uprising","small-village","life-with-q"),"ID")
    )
  )
}
