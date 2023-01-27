\name{qmethod-package}
\alias{qmethod-package}
\alias{qmethod-package}
\docType{package}
\title{Package for Q methodology analysis}
\description{
	Q is a methodology to study distinct perspectives existing within a group on a topic of interest.
	It is used across social, health, and environmental studies.
	See the references below for more details about the methodology.

  This package performs the analysis of Q methodology data (both forced and non-forced distributions). The user can choose the extraction method (principal components analysis or centroid factor extraction) and the rotation method (none, varimax or other rotations, uncommon in Q but implemented via \code{\link[psych]{principal}}). The default analysis conducts automatic flagging, and manual flagging is optional.

  The following steps of the analysis correspond to separate functions: automatic flagging of Q-sorts (\code{\link{qflag}}), z-scores and factor scores for statements (\code{\link{qzscores}}), distinguishing and consensus statements (\code{\link{qdc}}), and general characteristics of the factors (\code{\link{qfcharact}}).
  The function \code{\link{qmethod}} wraps them all.

  The functions for each step may be used separately for advanced analysis, for example, for manual flagging (see details in \code{\link{qzscores}}).

  The package also includes additional functions for the following:

  \itemize{
    \item An interactive Graphical User Interface with the basic functionality (\code{\link{runInterface}})
    \item Import data from \href{http://schmolck.org/qmethod/}{PQMethod software} (\code{\link{import.pqmethod}}), both \href{https://github.com/aproxima/htmlq/}{HTMLQ} and FlashQ (\code{\link{import.htmlq}}), and \href{https://github.com/shawnbanasick/easy-htmlq/}{easy-htmlq} (\code{\link{import.easyhtmlq}}) tools for online data collection of Q-sorts.
    \item Export a plain-text report of the analysis for interpretation in two flavours (\code{\link{export.qm}}).
    \item Generic methods to \code{\link{print.QmethodRes}} and \code{\link{plot.QmethodRes}} Q method results. The specific dotchart visualisation of Q results in \code{\link{plot.QmethodRes}} was first developed and introduced in this package, in preparation for the study in \href{https://www.sciencedirect.com/science/article/abs/pii/S0921800916302683}{Zabala et al. (2017)}.  
    \item Functions to explore the analysis and facilitate interpretation:
    \itemize{
       \item {explore automatic pre-flagging,  (\code{\link{loa.and.flags}})} 
       \item {to rename the factors in the results, with short, meaningful names (\code{\link{q.fnames}}).}
    }
    \item Generate printable cards for the administration of a Q study. The function \code{\link{make.cards}} produces a PDF with full item wording and codes, ready for printout on business card templates that can be easily broken into individual Q-cards.
    \item Several functions to aid reproducible research, by importing the following from raw, separate *.CSV or *.TEX files for each respondent or item:
    \itemize{
       \item Q-sorts (\code{\link{import.q.sorts}})
       \item Participant item feedback (\code{\link{import.q.feedback}})
       \item Complete concourses (\code{\link{import.q.concourse}})
       \item Item samples (\code{\link{build.q.set}})
    }
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
        \item Standard \strong{IDs} (such as \kbd{sta12}, \kbd{sta13}) which are generated automatically in \code{\link{qmethod}} or can be provided by the user using the respective \code{manual.lookup} arguments in \code{\link{make.cards}}, \code{\link{import.q.sorts}} and \code{\link{import.q.feedback}}.
        See the documentation of these functions for details.
        \item A set of hexadecimal \strong{hashed IDs} (such as \kbd{ae128fs}) can be automatically generated and expected by the functions \code{\link{make.cards}}, \code{\link{import.q.sorts}} and \code{\link{import.q.feedback}} if the argument \code{manual.lookup} remains empty and defaults to \code{NULL}.
        In that case, IDs are computed by 'summarising' the full item wordings (e.g. 
        
        "\samp{Q Method is used by a crazy, but charming community ...}") into a hexadecimal number (e.g. "\kbd{ae128fs}"), a process known as cryptographic \emph{hashing} (for more details see \code{\link[digest]{digest}})).
        These hash values change whenever \emph{anything} in the full item wordings is changed, and allow a precise identification of different versions of an item.
        This function never exposes users to the hash values.
        Automatic, hashed IDs are generally recommended and easier to use, but some caveats apply (see \code{\link{make.cards}}).
      }
    }
  For more information on this terminology and the rationale behind it, consider the best practices suggested by Maximilian Held on the \href{https://github.com/aiorazabala/qmethod/wiki/Data-management/}{data management} page.
  }

  \subsection{Suggested File Structure}{
    For studies in which each Q-sort and item are kept in separate *.CSV files, the import functions \code{\link{import.q.sorts}}, \code{\link{import.q.concourse}}, \code{\link{build.q.set}}, \code{\link{import.q.feedback}} and the print function \code{\link{make.cards}} require a nested directory structure in the study folder. An example of such structure can be found in \code{../qmethod/extdata/importexample}.
    Although recommended for complex studies, this structure is not necessary for using the data analysis functions or for exploring and exporting results.

    If the suggested file structure is followed, the subdirectories for (within-subjects) \emph{conditions} and \emph{languages} are optional, and need to be used only if there are more than one condition and language, respectively.
    In such case, the arguments \code{conditions} and \code{languages} for the above import functions must be specified accordingly.

    For more information on the file structure and the rationale behind it, consider the best practices suggested by Maximilian Held on the \href{https://github.com/aiorazabala/qmethod/wiki/Data-management}{data management} page.
  }
  \subsection{Bootstrapping}{
    A set of functions are available to perform bootstrapping with Q data (see \href{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0148087}{Zabala and Pascual, 2016, Bootstrapping Q Methodology to Improve the Understanding of Human Perspectives. PLoS ONE} for details). The main bootstrap functions are as follows:
    \itemize{
      \item \code{\link{qmboots}} performs a full bootstrap. It calls internally the functions \code{\link{qbstep}} (for each bootstrap step), and to either \code{\link{qindtest}} or \code{\link{qpcrustes}} in order to correct the \strong{alignment problem}.
      \item \code{\link{qmb.summary}} summarises the object resulting from \code{\link{qmboots}} into two tables: (1) summary of \strong{factor loadings} (standard factor loadings, bootstrapped factor loadings, and flagging frequency) and (2) summary of \strong{statement scores} (bootstrapped and standard z-scores, bootstrapped and standard factor scores, standard error of bootstrapped z-scores, and differences between standard and bootstrapped values).
      \item \code{\link{qmb.plot}} plots an object resulting from \code{\link{qmb.summary}} in a figure designed for bootstrapped Q data (either the factor loadings or the statement z-scores).
    }
  }
}
\author{
  Aiora Zabala\cr
  Main author and maintainer\cr
  \url{https://aiorazabala.net/}\cr
  \email{aiora.zabala@gmail.com}
  
  Maximilian Held\cr 
  Author of data management functions: \code{\link{import.q.sorts}}, \code{\link{import.q.concourse}}, \code{\link{build.q.set}}, \code{\link{import.q.feedback}} and \code{\link{make.cards}}\cr
  \url{https://maxheld.de/}

  Frans Hermans\cr 
  Author of centroid function: \code{\link{centroid}}\cr
  \url{https://www.researchgate.net/profile/Frans-Hermans-3}
}
\references{
\itemize{
  \item Zabala, A., 2014. qmethod: A Package to Explore Human Perspectives Using Q Methodology. \emph{The R Journal}, 6(2):163-173.\cr Available from: \url{https://journal.r-project.org/archive/2014-2/zabala.pdf}.
  \item Zabala A. and Pascual, U., 2016. Bootstrapping Q Methodology to Improve the Understanding of Human Perspectives. \emph{PLoS ONE}.\cr Available from: \doi{10.1371/journal.pone.0148087}.
  \item Watts, S., and and P. Stenner, 2012. \emph{Doing Q Methodological Research: Theory, Method & Interpretation}, London: Sage Publications Ltd.
  \item Van Exel, J., and G. de Graaf, 2005. \emph{Q Methodology: A Sneak Preview}\cr \url{https://qmethodblog.files.wordpress.com/2016/01/qmethodologyasneakpreviewreferenceupdate.pdf}{Available at this link}.
  \item Brown, S. R., 1980. \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press.\cr \url{https://qmethodblog.files.wordpress.com/2016/01/brown-1980-politicalsubjectivity.pdf}{Available at this link}.
  \item \url{https://qmethod.org/}\cr
The website of the \emph{International Society for the Scientific Study of Subjectivity}.
  \item \url{http://schmolck.org/qmethod/}\cr
Peter Schmolck's Q Method Page, with further references, datasets and the PQMethod software.
  }
}
\examples{
data(lipset)
results <- qmethod(lipset[[1]], nfactors = 3, rotation = "varimax")
summary(results)
results
plot(results)
}
