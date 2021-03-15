\name{runInterface}
\alias{runInterface}
\title{Q methodology: Graphical User Interface (GUI)}
\description{Launch an interactive interface to run Q methodology analysis using the basic features.
The interface is also [available online](https://azabala.shinyapps.io/qmethod-gui/).}

\usage{runInterface()}

\details{
This GUI allows the user to conduct a full Q methodology analysis, choosing:
	\itemize{
	  \item {either PCA or centroid extraction method}
	  \item {varimax or no rotation method (for PCA and centroid) and other uncommon rotation methods (for PCA)}
	  \item {selecting from 2 to 7 factors/components.}
	  }
The GUI conducts analysis with forced distribution and automatic flagging. See Note.

The GUI shows the full results from the analysis, and also:
\itemize{
    \item{Plot of z-scores}
    \item{Automatically flagged Q-sorts}
    \item{Information to explore how many factors to extract (including a screeplot)}
    \item{Plot of z-scores}
}
	 }
	 
\note{
This GUI has limited functionality in comparison to that through the command-line. For full functionality (such as specifying non-forced analysis, manual flagging, and much more), use the command-line directly in the R console. See, for example, a tutorial for \href{http://aiorazabala.github.io/qmethod/Advanced-analysis}{manual manipulation of Q-sort loadings and/or manual flagging}.

To run this same analysis directly in R, see the code generated in the GUI in \emph{Run the analysis directly in R}. 
}

\examples{
## Only run this example in interactive R sessions
if (interactive()) {
  runInterface()
}
}