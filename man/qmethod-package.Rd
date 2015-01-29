\name{qmethod-package}
\alias{qmethod-package}
\alias{qmethod-package}
\docType{package}
\title{Package for Q methodology analysis}
\description{
	Q is a methodology to study the distinct perspectives existing within a group on a topic of interest.
	It is used across social, health, and environmental studies.
	See the references below for more details about the methodology.

	Overview of the \code{qmethod} package.
	This package performs the analysis of Q methodology data using principal components analysis, varimax rotation (replaceable by other rotations allowed in \code{\link[psych]{principal}}), and automatic flagging.

	The following steps of the analysis correspond to separate functions: factor loadings for Q-sorts, automatic flagging of Q-sorts (manual flagging is optional), z-scores and rounded scores for statements, distinguishing and consensus statements, and general characteristics of the factors.
	The function \code{\link{qmethod}} wraps them all.

	The functions for each step may be used separately for advanced analysis, for example, for manual flagging (see details in \code{\link{qzscores}}).

	The package includes a function to import data from PQMethod software, to export plain text data for interpretation in two flavours, and to print and plot Q method results.
	Use \code{help(package="qmethod")} for a list of all the functions.

	The package also features several functions to strengthen reproduceable research, including functions to import Q sorts \code{\link{import.q.sorts}} and participant item feedback (\code{\link{import.q.feedback}}) from flat \code{*.csv} files, as well as complete concourses (\code{\link{import.q.concourse}}) and samples (\code{\link{build.q.set}}).

	The package also includes functions for the administration of a Q study, including \code{\link{make.cards}}, which produces a production-quality PDF with full item wordings and codes on business cards ready for printout as well as \code{\link{make.distribution}} to create Q distributions.

  The import functions \code{\link{import.q.sorts}}, \code{\link{import.q.concourse}}, \code{\link{build.q.set}}, \code{\link{import.q.feedback}} and the print function \code{\link{make.cards}} imply three distinct ways to refer to \emph{items}:
  \enumerate{
    \item \emph{Full item wording}, is, obviously, the complete item, such as \code{One small community of indomitable Q-methodologists.} (...) from \code{\link{importexample}}.
      The full wording is read in from individual TEX files.
    \item The item \emph{handle} is a shorthand way of referring to an item, that should be meaningful to the researcher.
      Item handles are \emph{researcher-facing}.
      It is used as rownames in imported data, and expected as filenames for the full wording TEX files.
    \item The item \emph{ID} is another shorthand way of referring to an item, that should not be intelligble to humans, such as an arbitrary string of characters.
    Depending on the used arguments, these IDs can be either hard-entered by setting \code{manual.lookup}, or automatically generated as hashes from the full item wording (default behavior for all functions).
    Item IDs are \emph{participant-facing}.
    Item IDs are used to quickly identify items in any given Q sort, as well as to identify item feedback by participants.
    Item IDs need to be non-intelligible so as to not provide unintended, additional stimulus to participants.
  }

  The import functions \code{\link{import.q.sorts}}, \code{\link{import.q.concourse}}, \code{\link{build.q.set}}, \code{\link{import.q.feedback}} and the print function \code{\link{make.cards}} also imply a nested directory structure under the root of \code{/study/} as can be found in \code{/extdata/importexample}.
  The subdirectories for \emph{conditions} and \emph{languages} are optional depending on passed arguments.
  It is highly recommended to follow this directory structure.
  For larger, collaborative and systematic Q projects, it is also recommended to put these directories under \emph{independent} version control (such as \code{git submodule}), as explained on the package \href{https://github.com/aiorazabala/qmethod/wiki/Data-management}{wiki}.
}
\author{
  Aiora Zabala\cr
  \url{http://www.landecon.cam.ac.uk/directory/aiora-zabala}

Maintainer: Aiora Zabala \email{aiora.zabala@gmail.com}
}
\references{Brown, S. R., 1980 \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press.\cr Available from: \url{http://qmethod.org/papers/Brown-1980-PoliticalSubjectivity.pdf}.

Van Exel, J., and G. de Graaf, 2005. \emph{Q Methodology: A Sneak Preview}\cr Available from: \url{http://qmethod.org/articles/vanExel.pdf}.

Watts, S., and and P. Stenner, 2012. \emph{Doing Q Methodological Research: Theory, Method & Interpretation}, London: Sage Publications Ltd.


\url{http://qmethod.org/}\cr
The official Q Website of the \emph{International Society for the Scientific Study of Subjectivity}.

\url{http://schmolck.org/qmethod}\cr
Peter Schmolck's Q Method Page.
}
\examples{
data(lipset)
results <- qmethod(lipset[[1]], nfactors=3, rotation="varimax")
summary(results)
results
}
