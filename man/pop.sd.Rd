\name{pop.sd}
\alias{pop.sd}
\title{Q methodology: calculate population standard deviation}
\description{
	Calculates the \emph{population} standard deviation.
	Wraps base R's \code{\link[stats]{sd}}.
}
\usage{
	pop.sd(x, ...)
}

\arguments{
  \item{x}{
  	a numeric vector or an R object which is coercible to one by \code{as.vector(x, "numeric")}.
  }
  \item{...}{
  	other arguments to pass to \code{\link[stats]{sd}}.
  }
}
\details{
	Base R provides only a \emph{sample} standard deviation, as documented for \code{\link[stats]{sd}}.
	Q methodology requires a \emph{population} standard deviation in several places.
	This function calculates a population standard deviation from a sample standard deviation as per this fix: \url{http://stackoverflow.com/questions/6457755/standard-deviation-in-r-seems-to-be-returning-the-wrong-answer-am-i-doing-some}.
	This helper function is exposed for advanced users who may wish to calculate population standard deviations for custom analyses.
}

\author{Maximilian Held}

\examples{
  pop.sd(c(1,2))
  sd(c(1,2))  # notice the difference
}
