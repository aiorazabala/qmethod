\name{qmethod-package}
\alias{qmethod-package}
\alias{qmethod-package}
\docType{package}
\title{Package for Q methodology analysis}
\description{Q is a methodology to study the distinct perspectives existing within a group on a topic of interest. It is used across social, health, and environmental studies. See the references below for more details about the methodology.

Overview of the \code{qmethod} package. This package performs the analysis of Q methodology data using principal components analysis, varimax rotation (replaceable by other rotations allowed in \code{\link[psych]{principal}}), and automatic flagging.

The following steps of the analysis correspond to separate functions: factor loadings for Q-sorts, automatic flagging of Q-sorts (manual flagging is optional), z-scores and rounded scores for statements, distinguishing and consensus statements, and general characteristics of the factors. The function \code{\link{qmethod}} wraps them all.

The functions for each step may be used separately for advanced analysis, for example, for manual flagging (see details in \code{\link{qzscores}}).

The package includes a function to import data from PQMethod software, to export plain text data for interpretation in two flavours, and to print and plot Q method results. Use \code{help(package="qmethod")} for a list of all the functions.}
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