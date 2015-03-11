\name{import.q.concourse}
\alias{import.q.concourse}
\title{Q methodology: import concourse of Q items}
\description{
  Imports full set of items (concourse) from a directory of .TEX files (one file per item), including possible translations in separate folders.
}
\usage{
  import.q.concourse(q.concourse.dir, languages=NULL)
}

\arguments{
  \item{q.concourse.dir}{
    A directory of individual .TEX files named as (meaningful, short) handles for items (e.g. \code{happy-feeling.tex}).
    If \code{languages} are specified, the directory should contain one folder per language, with all full Q items as individual individual TEX files in each language folder.
    The files within each language folder should be named as the \emph{same} (meaningful, short) item handle (e.g. \code{happy-feeling}) across all languages.
    Directories end with a trailing slash, such as \code{study/q-sample/q-concourse/}.
   }
  \item{languages}{
  	A character vector of languages, same as folders below \code{q.concourse.dir}.
  	If your concourse is monolingual, leave empty, defaults to \code{NULL}.
  }
}
\details{
	Q studies are conducted by asking participants (or a P set) to rank order a \emph{sample} (or Q Set) of items, drawn from a universe (or concourse) of items, based on some sampling strategy.
	A concourse is, simply put, \emph{the sum of all things people could say about a subject matter}.

	It is helpful to keep the \emph{entire} concourse readily available, so as to draw samples from it.

	For some studies, it is necessary to have the complete items available in several languages.

	This function simply imports all full item wordings and assigns a (meaningful, short) \emph{handle} for the item, based on the file name.
	These handles should be meaningful to the researcher.

	Individual items as TEX files should include minimal markup, and no trailing whitespace or empty newlines.
	If you do not need any additional formatting, you can just save plain text files (TXT) with the extension TEX.
	There is no need to know \href{http://www.latex-project.org}{LaTeX}.

	Returns error if items are not available in all translations.

  Defaults to monolingual variant.

  More details on the files and directory structure expected by this function can be found in the \link{qmethod-package} documentation, and on the package \href{https://github.com/aiorazabala/qmethod/wiki/Data-management}{wiki}.
}
\value{
	Returns character matrix with (meaningful, short) handles (from file names, such as \code{''q-is-great''}, for example) as rownames, languages (if applicable) as columns and full wording per language in cells.
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
