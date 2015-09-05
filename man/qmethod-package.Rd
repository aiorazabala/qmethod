\name{qmethod-package}
\alias{qmethod-package}
\alias{qmethod-package}
\docType{package}
\title{Package for Q methodology analysis}
\description{
	Q is a methodology to study distinct perspectives existing within a group on a topic of interest.
	It is used across social, health, and environmental studies.
	See the references below for more details about the methodology.

  This package performs the analysis of Q methodology data using principal components analysis, varimax rotation (replaceable by other rotations allowed in \code{\link[psych]{principal}}), and automatic flagging (manual flagging is optional).

  The following steps of the analysis correspond to separate functions:
  factor loadings for Q-sorts, automatic flagging of Q-sorts (\code{\link{qflag}}), z-scores and factor scores for statements (\code{\link{qzscores}}), distinguishing and consensus statements (\code{\link{qdc}}), and general characteristics of the factors (\code{\link{qfcharact}}).
  The function \code{\link{qmethod}} wraps them all.

  The functions for each step may be used separately for advanced analysis, for example, for manual flagging (see details in \code{\link{qzscores}}).

  The package also includes several additional functions for the following:

  \itemize{
    \item \emph{Administration} of a Q study, including:
    \itemize{
      \item generating printable cards for a Q study (\code{\link{make.cards}})
      \item calculating an appropriate Q-distribution
      %TODO(maxheld83) add here: (\code{\link{make.distribution}})
    }
    \item \emph{Importing} data from other software and raw files, including:
    \itemize{
      \item Data from PQMethod software (\code{\link{import.pqmethod}})
      \item Data from online Q-sorting software HTMLQ and FlashQ (\code{\link{import.htmlq}})
      \item Q-sorts in their \emph{raw form} from *.CSV files into Q-sort arrays to ingest into \code{\link{qmethod}} (\code{\link{import.q.sorts}})
      \item Participant item feedback from raw *.CSV files (\code{\link{import.q.feedback}})
      \item Complete concourses from raw *.TEX files (\code{\link{import.q.concourse}})
      \item Sampling items (\code{\link{build.q.set}})
    }
    \item Advising \emph{factor retention} decisions (\code{\link{q.nfactors}})
    \item \emph{Manual rotation} (\code{\link{q.mrot.choose}}, \code{\link{q.mrot.do}})
    \item Informative \emph{plots} specific to Q Methodology, including:
    \itemize{
      \item Factor score plots (\code{\link{q.scoreplot}})
      \item Factor composition plots (\code{\link{q.compplot}})
      \item Correlation matrix heatmaps (\code{\link{q.corrplot}})
      \item Loadings plots (\code{\link{q.loaplot}})
    }
    \item Calculating the dispersion of items \emph{within} factors.
    \item Naming (\code{\link{q.fnames}}) and coloring (\code{\link{q.fcolors}}) factors in results and plots for easier interpretation.
    \item Export a plain-text report of the analysis for interpretation in two flavours (\code{\link{export.qm}}).
    \item Generic methods to \code{\link{print.QmethodRes}} and \code{\link{plot.QmethodRes}} Q method results.
    %TODO(maxheld83) it's completely unclear to me what a consistent / appropriate punctuation would be in this case
  }
  Use \code{help(package="qmethod")} for a list of all the functions.

\subsection{Terminology}{
    The functions for analysis use the terms standard in Q methodology.

    In addition, the optional functions to import raw data from separate *.CSV files (\code{\link{import.q.sorts}}, \code{\link{import.q.concourse}}, \code{\link{build.q.set}}, \code{\link{import.q.feedback}}) and the card printing function (\code{\link{make.cards}}) refer to items in three distinct ways:
    \enumerate{
      \item Item \strong{full wording}, is the complete item, such as:

      "\samp{One small community of indomitable Q-methodologists ...}".
        This item can be read in from individual *.TEX files by using \code{\link{import.q.concourse}}.
        The wording is not passed on to any other function, but can be readily retrieved from the object returned from \code{\link{import.q.concourse}}.
      \item The item \strong{handle} is a shorthand way of referring to an item, which should be \emph{meaningful} to the researcher (e.g. \code{"life-with-q"}).
        Item handles are \emph{researcher-facing} and can be used to refer to items during data \emph{analysis}.
        They are read in from the \emph{filenames} of individual *.TEX files when using \code{\link{import.q.concourse}}.
        Handles can be used to identify items in other functions and their outputs.
        For example, the resulting array or matrix from \code{\link{import.q.sorts}} carries these handles as row names.
      \item The item \strong{ID} is another shorthand way of referring to an item, that should be \emph{meaningless} to humans (so as not to influence the participants in unintended ways), such as an arbitrary string of characters.
      Item IDs are \emph{participant-facing} and are used to identify items during data \emph{entry}.
      The item ID can take two forms, depending on function arguments specified by the user:

      \enumerate{
        \item Standard \strong{IDs} (such as \kbd{sta12}, \kbd{sta13}) can be provided by the user using the respective \code{manual.lookup} arguments in \code{\link{make.cards}}, \code{\link{import.q.sorts}} and \code{\link{import.q.feedback}}.
        See the documentation of these functions for details.
        \item A set of hexadecimal \strong{hashed IDs} (such as \kbd{ae128fs}) can be automatically generated and expected by the functions \code{\link{make.cards}}, \code{\link{import.q.sorts}} and \code{\link{import.q.feedback}} if the argument \code{manual.lookup} remains empty and defaults to \code{NULL}.
        In that case, IDs are computed by 'summarising' the full item wordings (e.g. "\samp{Q Method is used by a crazy, but charming community ...}") into a hexadecimal number (e.g. "\kbd{ae128fs}"), a process known as cryptographic \emph{hashing} (for more details see \code{\link[digest]{digest}})).
        These hash values change whenever \emph{anything} in the full item wordings is changed, and allow a precise identification of different versions of an item.
        This function never exposes users to the hash values.
        Automatic, hashed IDs are generally recommended and easier to use, but some caveats apply (see \code{\link{make.cards}}).
      }
    }
  For more information on this terminology and the rationale behind it, consider the best practices suggested by Maximilian Held on the \href{https://github.com/aiorazabala/qmethod/wiki/Data-management}{data management} page.
  }

  \subsection{Suggested File Structure}{
    For studies in which each Q-sort and item are kept in separate *.CSV files, the import functions \code{\link{import.q.sorts}}, \code{\link{import.q.concourse}}, \code{\link{build.q.set}}, \code{\link{import.q.feedback}} and the print function \code{\link{make.cards}} require a nested directory structure in the study folder. An example of such structure can be found in \code{../qmethod/extdata/importexample}.
    Although recommended for complex studies, this structure is not necessary for using the data analysis functions or for exploring and exporting results.

    If the suggested file structure is followed, the subdirectories for (within-subjects) \emph{conditions} and \emph{languages} are optional, and need to be used only if there are more than one condition and language, respectively.
    In such case, the arguments \code{conditions} and \code{languages} for the above import functions must be specified accordingly.

    For more information on the file structure and the rationale behind it, consider the best practices suggested by Maximilian Held on the \href{https://github.com/aiorazabala/qmethod/wiki/Data-management}{data management} page.
  }
}
\author{
  Aiora Zabala\cr
  Creator, author and maintainer\cr
  \url{http://www.landecon.cam.ac.uk/directory/aiora-zabala}\cr
  \email{aiora.zabala@gmail.com}

  Maximilian Held\cr
  Author and co-maintainer\cr
  \url{http://www.maxheld.de/}\cr
  \email{info@maxheld.de}

}
\references{
\itemize{
  \item Zabala, A., 2014. qmethod: A Package to Explore Human Perspectives Using Q Methodology. \emph{The R Journal}, 6(2):163-173.\cr Available from: \url{http://journal.r-project.org/archive/2014-2/zabala.pdf}.
  \item Watts, S., and and P. Stenner, 2012. \emph{Doing Q Methodological Research: Theory, Method & Interpretation}, London: Sage Publications Ltd.
  \item Van Exel, J., and G. de Graaf, 2005. \emph{Q Methodology: A Sneak Preview}\cr Available from: \url{http://qmethod.org/articles/vanExel.pdf}.
  \item Brown, S. R., 1980. \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press.\cr Available from: \url{http://qmethod.org/papers/Brown-1980-PoliticalSubjectivity.pdf}.
  \item \url{http://qmethod.org/}\cr
The website of the \emph{International Society for the Scientific Study of Subjectivity}.
  \item \url{http://schmolck.org/qmethod}\cr
Peter Schmolck's Q Method Page, with further references, datasets and the PQMethod software.
  }
}
\examples{
data(lipset)
results <- qmethod(lipset[[1]], nfactors = 3, rotation = "varimax")
summary(results)
results
}
