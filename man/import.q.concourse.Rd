\name{import.q.concourse}
\alias{import.q.concourse}
\title{Q methodology: import concourse of Q items}
\description{
  Imports full set of items (concourse) from a directory of .TEX files, including possible translations.
}
\usage{
  import.q.concourse(q.concourse.dir, languages=c("english"))
}

\arguments{
  \item{q.concourse.dir}{
    a directory of folders per language, with all full Q items as individual .TEX files in each language folder, with files named as the same meaningful item short handle (e.g. \code{happy-feeling.tex}) across all languages.
    Directories end with a trailing slash, such as \code{study/q-sample/q-concourse/}.
   }
  \item{languages}{
  	a character vector of languages, same as folders below \code{q.concourse.dir}.
  	If your concourse is monolingual, just specify your language as a vector with length one, such as \code{c("esperanto")}.
  	Defaults to \code{c("english")}.
  }
}
\details{
	Q studies are conducted by asking participants (or a p-set) to rank order a \emph{sample} (or q-set) of items, drawn from a universe (or concourse) of items, based on some sampling strategy.
	A concourse is, simply put, \emph{the sum of all things people could say about a subject matter}.

	It is helpful to keep the \emph{entire} concourse readily available, so as to draw samples from it.

	For some studies, it is necessary to have the complete items available in several languages.

	This function simply imports all full item wordings and assigns a \emph{meaningful item short handle} based on the file name.
	These handles should be meaningful to the researcher.

	Individual items as .TEX files should include minimal markup, and no trailing whitespace or empty newlines.
	If you do not need any additional formatting, you can just save plain text files (.TXT) with the extension .TEX.
	There is no need to know \href{http://www.latex-project.org}{LaTeX}.

	Throws error if items are not available in all translations.
	Must have at least one language (as all studies will).
}
\value{
	Returns character matrix with meaningful item short handles (from file names) as rownames, languages as columns and full wording per language in cells.
}

\author{Maximilian Held}

\seealso{
	\code{\link{build.q.set}},
	\code{\link{import.q.feedback}},
	\code{\link{import.q.sorts}},
	\code{\link{make.cards}}
}

\examples{
  ## Import a full q concourse from importexample dataset
  path.concourse <- paste(  # this part is only for the example!
    path.package("qmethod"),  # just to make sure, use absolute path
    # import example files are in root/extdata of package
    "/extdata/importexample/sample/concourse/",  # location of concourse
    sep = ""
  )
  q.concourse <- import.q.concourse(  # import concourse
    q.concourse.dir = path.concourse, # insert your applicable path here
    languages = c("english","german")  # choose your languages from path here
)

}
