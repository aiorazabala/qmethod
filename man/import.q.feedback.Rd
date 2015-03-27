\name{import.q.feedback}
\alias{import.q.feedback}
\title{Q methodology: imports feedback on Q items}
\description{
	Turns raw item feedback (in *.CSV files) into a  verified array or matrix.
}
\usage{
import.q.feedback(q.feedback.dir,	q.sorts, q.set,	manual.lookup = NULL)
}

\arguments{
  \item{q.feedback.dir}{
		A relative path to a directory structure where:
		\itemize{
			\item (optional) folders are \code{conditions} (such as \kbd{before} and \kbd{after}), if there is more than one condition.
			Conditions are inferred from the specified \code{q.sorts}.
			If there are no conditions, there should be no folders.
			\item filenames of *.CSV are participant names (might be given pseudonyms).
			\item *.CSV files within folders contain raw feedback, beginning with an arbitrary header line (ignored), and the following columns, starting from the left:
			\enumerate{
				\item An ID, either as an automatic hash or manually specified (see \code{\link{qmethod-package}}), as specified per the \code{manual.lookup} option of \code{\link{make.cards}}.
				Each ID only occurs once.
				\item The full feedback in plain text, enclosed in quotes.
				\item Optionally, a logical indicator whether current line should be ignored (in which case it should be set to \code{TRUE}).
				If there is no such column, all feedback will be imported.
			}
		}
	}
	\item{q.sorts}{
		A matrix or array with handles as row names, participant as column names, (optional) conditions as 3rd dimension and cells as Q-sort ranks, as produced by \code{\link{import.q.sorts}}.
	}
	\item{q.set}{
		A matrix with handles as row names, languages (if applicable) in columns, as produced by \code{\link{build.q.set}}.
	}
	\item{manual.lookup}{
		A matrix with handles as row names, and IDs (such as \kbd{"sta121"}, as printed on the Q-cards by \code{\link{make.cards}}) in any of the columns.
		Defaults to \code{NULL} in which case items IDs are expected to be item wording hashes, as produced by \code{\link{make.cards}}.
	}
}

\details{

	Participants in Q studies are often invited to provide open-ended feedback on items, giving researchers additional information on participants' viewpoints.
	This feedback is conveniently entered in a spreadsheet editor (2nd column), where each line of feedback corresponds to an item ID (1st column)
	An additional, optional (3rd) column indicates whether the current line should be ignored (\code{TRUE}), as may be the case for privacy reasons or when the feedback is merely a correction of a typographic error.
	If no such 3rd column is included, all feedback will be imported.

	The automatic summary of full item wordings, technically known as \emph{hashing}, proceeds internally by passing the full item wording to the \code{\link[digest]{digest}} function of the package \pkg{digest} (with arguments set to \code{algo = crc32, serialize = FALSE}.)

	After an (arbitrary) header line, a *.CSV file may look like this:\cr
  \samp{sta001,"This q-item sounds like r-research to me!",FALSE}, indicating that it should \emph{not} be ignored (\kbd{FALSE}).
  
  If you are not familiar with the terminology of item \emph{handle}, \emph{ID} and \emph{wording} or the file structure expected for import functions, please read the respective sections in the documentation for \link{qmethod-package} first or consider the package \href{https://github.com/aiorazabala/qmethod/wiki/Data-management}{wiki}
}

\value{
	Returns a matrix or array (if there is more than one condition) with handles as row names, people as column names, (optional) conditions as 3rd dimension name and item feedback in cells.
	The return parallels the output from \code{\link{import.q.sorts}}, but with feedback as array cells, rather than Q-sort ranks.
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
path.feedback <- paste(            # this part is only for the example!
  path.package("qmethod"),         # just to make sure, use absolute path
  # import example files are in root/extdata of package:
  "/extdata/importexample/feedback/",  # location of sorts
  sep = ""
  )
q.feedback <- import.q.feedback(   # now import the feedback
  q.feedback.dir = path.feedback,  # add your path here
  q.sorts = importexample$q.sorts,
  q.set = importexample$q.set,     # as produced by build.q.set
  manual.lookup = matrix(  # ideally empty for automatic hashing, or read in from *.CSV
    c("i01","i02","i03","i04"),
    ncol = 1,
    nrow = 4,
    dimnames = list(c("r-dominance","q-uprising","small-village","life-with-q"),"ID")
    )
  )
}
