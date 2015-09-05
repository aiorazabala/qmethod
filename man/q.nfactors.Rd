\name{q.nfactors}
\alias{q.nfactors}
\title{Q methodology: number of factors to be extracted}
\description{
  Produces statistics and plots to advise the number of factors to be extracted in a Q methodological analysis.
}
\usage{
  q.nfactors(
    dataset,
    q.matrix = NULL,
    cutoff = NULL,
    siglevel = 0.05,
    quietly = FALSE,
    cor.method = "pearson"
  )
}

\arguments{
  \item{dataset}{
    a matrix or a data frame containing original data, with statements as rows, Q-sorts as columns, and the column scores in the distribution in each cell.
    The matrix or data frame should not contain character strings.
    The results keep row names and column names if set in the \code{dataset} (see 'Details').
    Same as for \code{\link{qmethod}}.
  }
  \item{q.matrix}{
    a correlation matrix as produced by \code{\link[stats]{cor}}.
    Defaults to \code{NULL} in which case a correlation matrix is computed from the provided \code{dataset}, using \emph{Pearson's} correlation coefficient.
    For comparable results, the correlation matrix provided here should be based on the same \code{cor.method} as for \code{qmethod}.
  }
  \item{cutoff}{
    an integer vector of length one to specify the maximum feasible number of factors to be \emph{considered} for extraction.
    Must be \emph{less} than the number of Q sorts.
    Considering \emph{less} than the technically possible number of factors improves plots and makes results more manageable.
    Defaults to \code{NULL}, in which case \emph{half the number of Q sorts} are used as a cutoff.
  }
  \item{siglevel}{
    The significance level passed on to Horn's (1965) parallel analysis in \code{\link[paran]{paran}} and Bartlett's (1950, 1951) test in \code{\link[nFactors]{nBartlett}}.
    Defaults to \code{0.05}.
  }
  \item{quietly}{
    Logical, indicating whether a summary and plot should be printed.
    Defaults to \code{FALSE}.
  }
  \item{cor.method}{
    If \code{q.matrix} is unspecified (\code{NULL}), character string indicating which correlation coefficient is to be computed, to be passed on to the function \code{\link[stats]{cor}}: \code{"pearson"} (default), \code{"kendall"}, or \code{"spearman"}.
    Should be the same as the one later used in \code{\link{qmethod}}.
  }
}

\details{
  Deciding \emph{how many} factors, or shared viewpoints, to extract from a Q study is both hard and consequential.
  This decision is required as an input to \code{\link{qmethod}}.
  In Q (Brown 1980: 233) and R (Thompson 2004: 31), there is no single, conclusive statistical criterion to make this decision: judgment is required.
  This function conveniently wraps some existing packages for applicable statistical tests and provides plots to inform a researcher's judgment.

  The function returns a brief summary table of alternative criteria and two relevant plots.
}

\value{
  Returns a list of eleven objects:
  \item{paran}{
    Results from Horn's (1965) parallel analysis, improved by Glorfeld (1995) as implemented in \code{\link[paran]{paran}}.
    Random data can include spurious correlations and spurious factors by mere chance.
    This problem might affect Q studies, too: people might produce similar Q sorts not because they share viewpoints, but just out of random chance.
    Parallel analysis tests this assertion by extracting (spurious) principal components from many sets of random data, similarly parametrized as the provided \code{dataset}.
    The analysis then calculates what eigenvalue would have to be observed for any given principal component to be certain at the specified \code{siglevel} that the component is \emph{not} spurious.
    In summary, parallel analysis suggests the number of factors in the data that are unlikely to be products of random chance at the specified \code{siglevel}.
    \code{\link[paran]{paran}} is being run with 10.000 \code{iterations}.
    The analysis can take some time and gives a progress indicator.
    For more details, consider \code{\link[paran]{paran}}.
  }
  \item{simple}{
    A named vector with three simple criteria: Brown's static number seven (1980: 223), Watt's and Stenner's common sense of 6-8 people per factor, and the number of factors with an eigenvalue greater than one (Kaiser 1960; Guttman 1954).
  }
  \item{screeplot}{
    A screeplot as list of class \code{ggplot}, plotting unadjusted, adjusted and random eigenvalues as well as the above-mentioned \code{simple} criteria.
    Catell suggested that on inspection of a screeplot, only factors to the left of an imagined elbow should be retained (1966b).

    Also includes lines for the above \code{simple} criteria.
  }
  \item{communalities}{
    A matrix of communalities, with number of extracted factors as rows and Q sorts as columns.
    Communalities are not usually considered to decide on a number of factors, but may be instructive in the context of Q methodology.
    Communalities, as the sum of squared loadings of a Q sort \emph{across} all extracted factors measures the share of variance of a Q sort captured by any given factor model.
    As more factors are extracted, it increases to one.

    In Q methodology, participant Q sorters (as the \emph{variables} in the factor analysis) are often not randomly sampled.
    Eigenvalue-based criteria (but also communalities) may therefore be ``relatively meaningless'' (Brown 1980: 233) because they depend on the vagaries of the p-set.

    A careful investigation of communalities \emph{across various factor models} may still inform a researcher's judgment \emph{at the margin} between two factor models (say, retaining two or three factors).
    For example, a Q researcher may be inclined to retain an additional factor, if it precipitously \emph{increases} the communalities of a subset of Q sorts the researcher suspects to be similar in viewpoint (based on interviews, or socio-demographic data of respondents, for instance).
    Researchers should note that \emph{depending on the rotation}, a parallel increase of several Q sorts in communalities as an additional factor is retained \emph{need not precipitate in loadings on that factor}.
    A supposed similarity in shared viewpoints of any subset of Q sorts can only be ascertained after rotation procedures are completed.

    A more rigorous criterion for this approach may be to look at changes in \emph{residual} correlations (\code{residuals}) between any two factor models, and associated significance tests (\code{Bartlett}) (such as Bartlett 1950, 1951).

    Communalities are calculated calling \code{\link[psych]{principal}}.
  }
  \item{commplot}{
    A plot of communalities as list of class \code{ggplot}.
    Tracks the communality all Q sorts across all factor models, with lines labeled by the initials of Q sorts, taken from the column names in \code{dataset}.
    The plot also includes a vertical line for Bartlett's (1950, 1951) test (see \code{Bartlett}).

    May be inspected for parallel, sharp increases of Q sorts suspected to share viewpoints.
  }
  \item{residuals}{
    An array of residual correlation matrices, with rows and columns as Q sorts, and third dimension as the number of factors extracted.

    Communalities are calculated calling \code{\link[psych]{principal}}.
  }
  \item{residuals.plots}{
    A list of lists of class \code{ggplot} as heatmap plots for the residual correlations.

    May be inspected for residual correlations suspected to express shared viewpoints or changes in residual correlations.
  }
  \item{Bartlett}{
    The results object from \code{\link[nFactors]{nBartlett}}, including Bartlett's (1950, 1951) and similar tests.
    Tests for each residual correlation matrix, whether the null hypothesis of an identity matrix (no remaining correlations) can be rejected at the specified \code{siglevel}.
    Reports number of factors for which this null hypothesis can be rejected.

    Uses the (more conservative) variant of \code{\link[nFactors]{nBartlett}} for Bartlett's test, with \code{correction = TRUE}.
  }
  \item{corr}{
    A list of class \code{ggplot} as a plot for the correlation matrix.
  }
  \item{eigenvalues}{
    A dataframe, conveniently wrapping the unadjusted, random and adjusted eigenvalues, as well as the contribution to R2 by that component, calculated by dividing the unadjusted eigenvalue by the number of people-variables in the dataset.
  }
  \item{summary}{
    A dataframe with criterion, number of retained factors and source as columns.
  }
}
\references{
  \itemize{
    \item Bartlett, M. S. (1950): \emph{Tests of significance in factor analysis}, British Journal of Psychology, 3, 77-85.
    \item Bartlett, M. S. (1951): \emph{A further note on tests of significance}, British Journal of Psychology, 4, 1-2.
    \item Brown, S. R., 1980: \emph{Political subjectivity: Applications of Q methodology in political science}, New Haven, CT: Yale University Press.
    \item Catell, Raymond B. (1966): \emph{The Scree Test for the Number of Factors}, Multivariate Behavioral Research (1, 2).
    \item Fruchter, Benjamin (1954): \emph{Introduction to Factor Analysis}, Princeton, NJ: Van Nostrand.
    \item Glorfeld, L. W. 1995: \emph{An Improvement on Horn-s Parallel Analysis Methodology for Selecting the Correct Number of Factors to Retain}, Educational and Psychological Measurement. 55(3): 377-393.
    \item Gutmann, L. (1954): \emph{An Outline of Some New Methodology for Social Research}, The Public Opinion Quarterly (18: 4).
    \item Horn, J. L. (1965): \emph{A rationale and a test for the number of factors in factor analysis}, Psychometrika. 30: 179-185.
    \item Kaiser, H.F. (1960). \emph{The application of electronic computers to factor analysis.} Educational and Psychological Measurement, 20, 141-151.
    \item Thompson, Bruce (2004): \emph{Exploratory and Confirmatory Factor Analysis --- Understanding Concepts and Applications}, Washington, D.C.: American Psychological.
    \item Watts, Simon and Stenner, Paul (2012): \emph{Doing Q Methodological Research: Theory, Method & Interpretation}, Oaks, CA: Sage Publications; 2012.
  }
}

\note{
  This function is currently based on principal components analysis (PCA) as a ``factor'' extraction technique.
  Thompson (2004: 30ff) and others seem to suggest that such PCA-based criteria can be used as rough indications for how many factors to extract with other exploratory techniques.
  However, some of the results presented here are meaningful \emph{only} in a PCA-context, and dependent functions are sometimes called with PCA-related options.
  If you would like to add support for other extraction techniques, comment or contribute \href{https://github.com/aiorazabala/qmethod/issues/156}{on the development repository}.

  This function foregoes some criteria for the number of factors discussed in the context of Q methodology, including Humphrey's rule (Fruchter 1954: 79-80 as cited in Brown 1980: 223), and Brown's rule of more than two significant factor loadings (Watts & Stenner 2012: 107).
  These criteria are not included because they appear to be based on \emph{loadings}, which themselves are dependent on the downstream rotation procedure.
  If you disagree or have an idea how to add these criteria, \href{https://github.com/aiorazabala/qmethod/issues/154}{comment on this issue}.
}

\author{Maximilian Held}

\seealso{
  \code{\link[psych]{principal}} in package \pkg{psych},
  \code{\link[nFactors]{nBartlett}} in package \pkg{nFactors} and
  \code{\link[paran]{paran}} in package \pkg{nFactors}
}
\examples{
  data(lipset)
  nresults <- q.nfactors(dataset = lipset$ldata)
}
