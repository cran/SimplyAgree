% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agree_test.R
\name{agree_test}
\alias{agree_test}
\title{Tests for Absolute Agreement}
\usage{
agree_test(
  x,
  y,
  delta,
  conf.level = 0.95,
  agree.level = 0.95,
  TOST = TRUE,
  prop_bias = FALSE
)
}
\arguments{
\item{x}{Vector with first measurement}

\item{y}{Vector with second measurement}

\item{delta}{The threshold below which methods agree/can be considered equivalent, can be in any units. Often referred to as the "Equivalence Bound for Agreement" or "Maximal Allowable Difference".}

\item{conf.level}{the confidence level required. Default is 95\%.}

\item{agree.level}{the agreement level required. Default is 95\%. The proportion of data that should lie between the thresholds, for 95\% limits of agreement this should be 0.95.}

\item{TOST}{Logical indicator (TRUE/FALSE) of whether to use two one-tailed tests for the limits of agreement. Default is TRUE.}

\item{prop_bias}{Logical indicator (TRUE/FALSE) of whether proportional bias should be considered for the limits of agreement calculations.}
}
\value{
Returns single list with the results of the agreement analysis.
\itemize{
\item \code{shieh_test}: The TOST hypothesis test as described by Shieh.
\item \code{ccc.xy}: Lin's concordance correlation coefficient and confidence intervals.
\item \code{s.shift}: Scale shift from x to y.
\item \code{l.shift}: Location shift from x to y.
\item \code{bias}: a bias correction factor that measures how far the best-fit line deviates from a line at 45 degrees. No deviation from the 45 degree line occurs when bias = 1. See Lin 1989, page 258.
\item \code{loa}: Data frame containing the limits of agreement calculations
\item \code{h0_test}: Decision from hypothesis test.
\item \code{call}: the matched call
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}}

Development on \code{agree_test()} is complete, and for new code we recommend
switching to \code{agreement_limit()}, which is easier to use, has more features,
and still under active development.

The agree_test function calculates a variety of agreement statistics. The hypothesis test of agreement is calculated by the method described by Shieh (2019). Bland-Altman limits of agreement, and confidence intervals, are also provided (Bland & Altman 1999; Bland & Altman 1986).
In addition, the concordance correlation coefficient (CCC; Lin 1989) is additional part of the output.
}
\examples{
data('reps')
agree_test(x=reps$x, y=reps$y, delta = 2)

}
\references{
Shieh (2019). Assessing Agreement Between Two Methods of Quantitative Measurements: Exact Test Procedure and Sample Size Calculation,
Statistics in Biopharmaceutical Research,
\doi{10.1080/19466315.2019.1677495}

Bland, J. M., & Altman, D. G. (1999). Measuring agreement in method comparison studies. Statistical methods in medical research, 8(2), 135-160.

Bland, J. M., & Altman, D. (1986). Statistical methods for assessing agreement between two methods of clinical measurement. The lancet, 327(8476), 307-310.

Lawrence, I., & Lin, K. (1989). A concordance correlation coefficient to evaluate reproducibility. Biometrics, 255-268.
}
