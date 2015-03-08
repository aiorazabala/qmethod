\name{make.cards}
\alias{make.cards}
\title{Q methodology: produce printable cards for Q study with ID and full item wording}
\description{
  Creates production quality cards for administering a Q study.
  Full item wordings are printed on the front of business cards, item identifiers (ID) on the back.
}
\usage{
  make.cards(
  	q.set,
  	study.language=NULL,
  	paper.format="AveryZweckformC32010.Rnw",
  	output.pdf=TRUE,
  	manual.lookup=NULL,
    wording.font.size = NULL,
    file.name = "QCards",
    babel.language = NULL
    )
}

\arguments{
  \item{q.set}{
    A matrix with short handles as rownames, languages (if applicable) in columns, as produced by \code{\link{build.q.set}}.
  }
  \item{study.language}{
  	A character vector of length one, choosing a language from \code{q.concourse.dir} and same as in \code{\link{import.q.concourse}} to print items in.
  	Defaults to \code{NULL}, in which case the first column from \code{q.concourse} is selected.
  }
  \item{paper.format}{
  	A character vector of length one, choosing among available templates of business card sheets.
  	Defaults to the only currently available \code{"AveryZweckformC32010.Rnw"}.
    Must include file extension of template.
  }
  \item{output.pdf}{
  	Logical.
  	If \code{TRUE}, function invokes \code{knit2pdf} to create a PDF in the workspace.
  	If \code{FALSE}, function invokes \code{knit} to return only a .TEX in the workspace, may be preferable if no \href{http://www.latex-project.org}{LaTeX} installation is available on the used computer.
  	Defaults to \code{TRUE}.
  }
  \item{manual.lookup}{
  	A matrix with meaningful short handles (same as in \code{\link{build.q.set}}, \code{\link{import.q.concourse}}) as rownames, and arbitary, unique identifying strings in any of the columns as also expected in \code{\link{import.q.sorts}} and \code{\link{import.q.feedback}}.
		Defaults to \code{NULL} in which case items are automatically identified by full item hashes, as also detected by \code{\link{import.q.sorts}} and \code{\link{import.q.feedback}}.
  }
  \item{wording.font.size}{
    A character vector of length 1 to set the font size of the full item wording on the cards.
    Defaults to \code{NULL} in which the default font size 12pt is used.
    Only \href{http://en.wikibooks.org/wiki/LaTeX/Fonts#Sizing_text}{standard LaTeX font sizes} are allowed, from \code{\\tiny} to \code{\\Huge}.
  }
  \item{file.name}{
    A character vector of length 1 to set the filename \emph{without file extension}.
    Defaults to \code{QCards}.
  }
  \item{babel.language}{
    A character vector of length 1 to set the babel language for appropriate hyphenation, special letters and other international support as provided by the babel LaTeX package.
    Only available babel options are permissible.
    Defaults to \code{NULL}, in which case babel is never called.
    Changing \code{babel.language} between function class can occassionally leave inconsistent LaTeX temp files, which may trip up compilation.
    Please re-run the function once again or clean up temp files in that case.
  }
}
\details{
	Preparing cards with full items and identifiers (IDs) quickly becomes cumbersome if a study is done several times or if items change frequently.
	Participants require well-printed, well-designed cards for their sorting task, ideally on heavier paper.
	Cards should include shorthand, unique identifiers to simply later data entry.

	This function prepares a production-quality PDF (or .TEX source), where items are printed on readily-available A4 (or other) business card templates, from which individual cards can be easily broken out.

	The function prints the full item wording on the \emph{right} column of any page, and the identifier (ID) on the \emph{left} column.
	If templates are duplex printed with the same page on the front and back, and in proper orientation, the front of each card includes the full wording, and the back its unique identifier (ID).

	Identifiers (ID) entered manually or automatically hashed from full wordings are also expected in the import functions \code{\link{import.q.sorts}} and \code{\link{import.q.feedback}}.

	Identifiers (ID) in this function \emph{should not be mistaken} for the \emph{meaningful short handles}, also used in several of these functions.
	Meaningful short handles are meant for researchers only, to easily navigate a larger concourse or sample of items.
	They should \emph{not} be exposed to participants, because these short handles may be interpreted by participants as unintended, additional stimulus.
	Instead, this function expects or creates identifiers (ID) that are not intelligble to participants.

	The function proceeds internally by preparing a dataframe with full item wordings and identifiers (ID), and then invokes a prepared \code{*.Rnw} template included with this package, which in turn includes a \pkg{knitr} chunk, which in turn calls \pkg{xtable} to return a neatly layed out multi-page table.

  More details on the files and directory structure expected by this function can be found in the \link{qmethod-package} documentation, and on the package \href{https://github.com/aiorazabala/qmethod/wiki/Data-management}{wiki}.
}

\value{
	Writes a PDF or its source TEX to the working directory ready for printout.
}

\note{
	Hashed identification has not been widely tested in Q studies and should be used with great care and only for extra convenience.
	When using hash identification, researchers should be careful to record the precise item wordings at the time of hashing for the printed Q cards, preferably with a version control system.
	Researchers should also record the complete Q sorts of participants in an \emph{unhashed} form, such as a picture of the completed sort in full wordings, in case problems with the hashing arise.

  When \code{output.pdf = TRUE}, the function will sometimes fail with the error message \code{Running 'texi2dvi' on ... failed}.
  This is not a bug with the function, but simply indicates that the path to pdflatex is not available in the current R environment.
  To fix this issue, compile the resulting TEX manually, use RStudio or try \href{http://www.r-bloggers.com/building-r-packages-missing-path-to-pdflatex/}{this fix}.

  This function does \emph{not} automatically scale the font size to fit the given card size.
  Instead, users will have to proceed by trial and error, using a \code{wording.font.size} that works for their longest item.
  The default value should work for most Q items.

	This function currently only works for Avery Zweckform C32010 templates, designed in \code{/cardtemplates/AveryZweckformC32010.Rnw}.
	If you would like support for other templates, check out / chip in here: \url{https://github.com/aiorazabala/qmethod/issues/34}.
}

\author{Maximilian Held}

\seealso{
	\code{\link{build.q.set}},
	\code{\link{import.q.feedback}},
	\code{\link{import.q.sorts}},
	\code{\link{import.q.concourse}}
}

\examples{
 ## Make cards from importexample
 data(importexample)
 make.cards(importexample$q.set)
}
