\name{import.q.concourse}
\alias{import.q.concourse}
\title{Q methodology: import concourse of Q items}
\description{
  Imports a full set of items (statements in a concourse) from a directory of *.TEX files (one file per item), including possible translations in separate folders.
}
\usage{
import.q.concourse(q.concourse.dir, languages = NULL)
}

\arguments{
  \item{q.concourse.dir}{
    A directory of \emph{individual} item wordings in *.TEX files with handles as filenames (e.g. \kbd{happy-feeling.tex}).
    If \code{languages} are specified, the directory should contain one folder per language, with all full item wordings as individual *.TEX files in \emph{each} language folder.
    Items should have the \emph{same} file name across all languages (e.g. \kbd{happy-feeling.tex}).
    Directories end with a trailing slash, such as \code{study/q-sample/q-concourse/}.
   }
  \item{languages}{
  	A character vector of languages, same as folders within \code{q.concourse.dir}.
  	If the concourse is monolingual, leave empty. Defaults to \code{NULL}.
  }
}
\details{
	Q studies are conducted by asking participants (or a P set) to rank order a \emph{sample} (or Q Set) of items, drawn from a universe (or concourse) of items, based on some sampling strategy.
	A concourse is, simply put, \emph{the sum of all things people could say about a subject matter}.

	It is helpful to keep the \emph{entire} concourse readily available, so as to draw samples from it.

	For some studies, it is necessary to have the complete items available in several languages.

	This function simply imports all full item wordings and assigns a \emph{handle} for the item, based on the filename (see \link{qmethod-package}).
	These filenames should be short and meaningful to the researcher.

	Individual items as *.TEX files should include minimal markup, and no trailing whitespace or empty newlines.
	If you do not need any additional formatting, you can just save plain text files (*.TXT) with the extension *.TEX.
	There is no need to know \href{http://www.latex-project.org}{LaTeX}.

	Returns error if items are not available in all translations.

  Defaults to monolingual variant.
  
  If you are not familiar with the terminology of Q item \emph{handle}, \emph{ID} and \emph{wording} or the file structure expected for import functions, please read the respective sections in the documentation for \link{qmethod-package} first or consider the package \href{https://github.com/aiorazabala/qmethod/wiki/Data-management}{wiki}.
}
\value{
	Returns a character matrix with handles as row names, languages (if applicable) as columns and full item wording per language in cells.
}

\author{Maximilian Held}

\seealso{
	\code{\link{build.q.set}},
	\code{\link{import.q.feedback}},
	\code{\link{import.q.sorts}},
	\code{\link{make.cards}}
}

\examples{
## Import a full q concourse from 'importexample' dataset
path.concourse <- paste(            # this part is only for the example!
  path.package("qmethod"),          # just to make sure, use absolute path
  # import example files are in root/extdata of package
  "/extdata/importexample/sample/concourse/",  # location of concourse
  sep = ""
  )
q.concourse <- import.q.concourse(  # import concourse
  q.concourse.dir = path.concourse, # insert your applicable path here
  languages = c("english","german") # choose your languages from path here
  )

}
