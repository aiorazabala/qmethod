\name{import.q.feedback}
\alias{import.q.feedback}
\title{Q methodology: imports feedback on q items}
\description{
	Turns raw item feedback (in csvs) into a clean, verified array or matrix.
}
\usage{
	import.q.feedback(q.feedback.dir, q.sorts, q.set, manual.lookup)
}

\arguments{
  \item{q.feedback.dir}{
		A relative path to a directory structure where:
		\itemize{
			\item folders are \code{conditions} (such as before or after), if there are more than one conditions.
			\item filenames of \code{*.csv}s are participant names (might be given pseudneums)
			\item csvs containing raw feedback, beginning with an arbitary header line (ignored), and the following columns, starting from the left:
			\enumerate{
				\item An automatic hash from \code{\link{make.cards}} or a \code{manual.lookup} id.
				Each id only occurs once.
				\item The full feedback.
				\item Logical indicator whether current line should be ignored (TRUE).
			}
		}
	}
	\item{q.sorts}{
		A matrix or array with handles as rownames, participant as colnames, conditions as 3rd dimension and cells as ranks, as produced by \code{\link{import.q.sorts}}.
	}
	\item{q.set}{
		A matrix with short handles as rownames, languages in columns, as produced by \code{\link{build.q.set}}.
	}
	\item{manual.lookup}{
		A matrix with meaningful short handles (same as in \code{\link{build.q.set}}, \code{\link{import.q.concourse}}) as rownames, and arbitary, unique identifying strings in any of the columns as printed on the q-cards by \code{\link{make.cards}}.
		Defaults to NULL in which case items are automatically identified by full item hashes, as also used by \code{\link{make.cards}}.
	}
}

\details{
	Participants in q studies are often invited to provide open-ended feedback on items, given q-researchers a additional information on participants' viewpoints.
	This feedback is conveniently entered in a spreadsheet editor (2nd column), where each line of feedback corresponds to an item id (1st column), either a \code{manual.lookup}, or an automatic hash from \code{\link{make.cards}}.
	An additional (3rd) column indicates whether the current line should be ignored (TRUE), as may be the case for privacy reasons or when the feedback is merely a correction.

	After an (arbitrary) header line, a csv may look like this:
	\code{c12,"This q-item sounds like r-research to me!",FALSE}, indicating that it should \emph{not} be ignored (FALSE).

	Item feedback is best enclosed in quotes, allowing comas inside cells.
}

\value{
	Returns a matrix or array (of more than one condition) with short handles as rownames, people as colnames, conditions (if more than one) as 3rd dimname and ranks in cells.
	The return parallels the output from \code{\link{import.q.sorts}}, only with feedback as array cells, not ranks.
}

\author{Maximilian Held}

\seealso{
	\code{\link{import.q.concourse}},
	\code{\link{import.q.sorts}},
	\code{\link{build.q.set}},
	\code{\link{make.cards}},
	\code{\link{qmethod}}
}