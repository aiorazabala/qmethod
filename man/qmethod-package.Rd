\name{qmethod-package}
\alias{qmethod-package}
\alias{qmethod-package}
\docType{package}
\title{Package for Q methodology analysis}
\description{
	Q is a methodology to study the distinct perspectives existing within a group on a topic of interest.
	It is used across social, health, and environmental studies.
	See the references below for more details about the methodology.

  \subsection{Overview}{
    This package performs the analysis of Q methodology data using principal components analysis, varimax rotation (replaceable by other rotations allowed in \code{\link[psych]{principal}}), and automatic flagging.

    The following steps of the analysis correspond to separate functions: factor loadings for Q-sorts, automatic flagging of Q-sorts (manual flagging is optional), z-scores and rounded scores for statements, distinguishing and consensus statements, and general characteristics of the factors.
    The function \code{\link{qmethod}} wraps them all.

    The functions for each step may be used separately for advanced analysis, for example, for manual flagging (see details in \code{\link{qzscores}}).

    The package includes a function to import data from PQMethod software, to export plain text data for interpretation in two flavours, and to print and plot Q method results.
    Use \code{help(package="qmethod")} for a list of all the functions.

    The package also features several functions to strengthen reproduceable research, including functions to import Q-sorts (\code{\link{import.q.sorts}}) and participant item feedback (\code{\link{import.q.feedback}}) from flat *.CSV files, as well as complete concourses (\code{\link{import.q.concourse}}) and samples (\code{\link{build.q.set}}).

    The package also includes a function for the administration of a Q study (\code{\link{make.cards}}), which produces a PDF with full item wordings and codes on business cards ready for printout, easily broken into individual Q-cards.
  }

  \subsection{Terminology}{
    The import functions \code{\link{import.q.sorts}}, \code{\link{import.q.concourse}}, \code{\link{build.q.set}}, \code{\link{import.q.feedback}} and the print function \code{\link{make.cards}} refer to items in three distinct ways:
    \enumerate{
      \item Item full \strong{wording}, is, obviously, the complete item, such as "\samp{One small community of indomitable Q-methodologists ...}" from \code{\link{importexample}}.
        This item wording is read in from individual *.TEX files in \code{\link{import.q.concourse}}.
        The wording is not passed on to any other function, but can be readily retrieved from the object returned from \code{\link{import.q.concourse}}.
      \item The item \strong{handle} is a shorthand way of referring to an item, that should be \emph{meaningful} to the researcher.
        Item handles are \emph{researcher-facing} and are used to refer to items during data \emph{analysis}.
        Item handles are read in from the \emph{filenames} of individual *.TEX in \code{\link{import.q.concourse}}.
        Handles are used to identify items in many other functions and their respective results objects.
        For example, the resulting array or matrix from \code{\link{import.q.sorts}} carries these handles as row names.
      \item The item \strong{ID} is another shorthand way of referring to an item, that should be \emph{meaningless} to humans, such as an arbitrary string of characters.
      Item IDs are \emph{participant-facing} and are used to identify items during data \emph{entry}.
      (They should be meaningless so as not to influence the participants in unintended ways.)
      The item ID can take two forms, depending on function arguments specified by the user:
      \enumerate{
        \item A set of \strong{manual IDs} (such as \kbd{sta12}, \kbd{sta13}) can be hard-entered (or read in) by the user using the respective \code{manual.lookup} arguments in \code{\link{make.cards}}, \code{\link{import.q.sorts}} and \code{\link{import.q.feedback}}.
        The manual IDs are related to the (above) handles using values passed to the \code{manual.lookup} arguments in the above function.
        See the documentation of these functions for details.
        \item A set of hexadecimal \strong{hashed IDs} (such as \kbd{ae128fs}) can be automatically generated and expected by the functions \code{\link{make.cards}}, \code{\link{import.q.sorts}} and \code{\link{import.q.feedback}} if the \code{manual.lookup} argument remains empty and defaults to \code{NULL}.
        In that case, IDs are computed by ``summarizing'' the \emph{complete} item wordings ("\samp{Q Method is used by a crazy, but charming community of ...}") into a hexadecimal number ("\kbd{ae128fs}"), a process known as cryptographic \emph{hashing} (for more details see the \pkg{digest} function).
        These hash values change whenever \emph{anything} in the full item wordings is changed, and allow a precise identification of different versions of an item.
        This function never exposes users to the hash values; the encrypting and decrypting are done under the hood by the respective functions.
        Automatic, hashed IDs are generally recommended and easier to use, but some caveats apply (see \code{\link{make.cards}}).
      }
    }
  For more information on this terminology and the rationale behind it, consider the suggested best practices on the package \href{https://github.com/aiorazabala/qmethod/wiki/Data-management}{wiki}.
  }

  \subsection{File Structure}{
    The import functions \code{\link{import.q.sorts}}, \code{\link{import.q.concourse}}, \code{\link{build.q.set}}, \code{\link{import.q.feedback}} and the print function \code{\link{make.cards}} also imply a nested directory structure under the root of \code{/study/} as can be found in \code{/extdata/importexample}.
    The subdirectories for (within-subjects) \emph{conditions} and \emph{languages} are optional, and can be used only if there are more than one condition and language, respectively.
    Arguments \code{conditions} and \code{languages} for the above import functions must be specified accordingly.
    It is highly recommended to follow this directory structure, preferably together with \emph{independent} version control (such as \code{git submodule}).

    For more information on the file structure and the rationale behind it, consider the suggested best practices on the package \href{https://github.com/aiorazabala/qmethod/wiki/Data-management}{wiki}.
  }
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
