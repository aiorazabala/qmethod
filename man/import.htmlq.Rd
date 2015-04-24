\name{import.htmlq}
\alias{import.htmlq}
\title{Q methodology: import data from HTMLQ and FlashQ}
\description{Imports data from *.CSV files created with HTMLQ or FlashQ softwares for Q-sort administration.}
\usage{import.htmlq(filename, ...)}

\arguments{
  \item{filename}{a file with extension *.CSV (see full description of the file below in References).}
  \item{...}{further arguments to be passed to \code{\link[utils]{read.csv2}}.}
}
\details{
Extracts the raw data of a Q methodology study from the native format saved in both \emph{FlashQ} and \emph{HTMLQ}. Returns a list with two objects.

The first object contains a data frame with items as rows and Q-sorts as columns, ready to be used in \code{\link{qmethod}}. It sets the Q-sort names to the values in the column 'uid' or else in 'sid'.

The second object contains the additional data collected. Columns \code{npos}, \code{nneu} and \code{nneg} have the number of items allocated to the groups of 'positive', 'neutral', and 'negative' respectively. Columns which name start with \code{comment*} and \code{form*} contain further information introduced by the respondent. Columns which name start with \code{dur*} contain the time that the respondent spent in each screen. Column \code{datetime} contains the data stamp when the Q-sort was responded.
}

\author{Aiora Zabala}

\references{Hackert, Christian and Braehler, Gernot, 2007. \emph{FlashQ}, Available at: \url{http://www.hackert.biz/flashq}

Oschlies, Johannes and Killing, Marvin, 2015. \emph{HTMLQ}, Available at: \url{https://github.com/aproxima/htmlq}
}