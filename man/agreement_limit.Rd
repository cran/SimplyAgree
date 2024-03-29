% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agreement_limit.R
\name{agreement_limit}
\alias{agreement_limit}
\title{Limits of Agreement}
\usage{
agreement_limit(
  x,
  y,
  id = NULL,
  data,
  data_type = c("simple", "nest", "reps"),
  loa_calc = c("mover", "blandaltman"),
  agree.level = 0.95,
  alpha = 0.05,
  prop_bias = FALSE,
  log_tf = FALSE
)
}
\arguments{
\item{x}{Name of column with first measurement}

\item{y}{Name of other column with the other measurement to compare to the first.}

\item{id}{Column with subject identifier. Default is "id" if no entry is provided.}

\item{data}{Data frame with all data.}

\item{data_type}{The type of data structure. Options include "simple" (all independent data points), "nest" (nested data) and "reps" (replicated data points).}

\item{loa_calc}{The method by which the limits of agreement confidence intervals are calculated. Options are "mover" (Methods of Recovering Variances method) or "blandlatman" (Bland-Altman method).}

\item{agree.level}{the agreement level required. Default is 95\%. The proportion of data that should lie between the thresholds, for 95\% limits of agreement this should be 0.95.}

\item{alpha}{The alpha-level for confidence levels.}

\item{prop_bias}{Logical indicator (TRUE/FALSE) of whether proportional bias should be considered for the limits of agreement calculations.}

\item{log_tf}{Calculate limits of agreement using log-transformed data.}
}
\value{
Returns single loa class object with the results of the agreement analysis.
\itemize{
\item \code{loa}: A data frame containing the Limits of Agreement.
\item \code{call}:The matched call.
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#maturing}{\figure{lifecycle-maturing.svg}{options: alt='[Maturing]'}}}{\strong{[Maturing]}}

A function for calculating for Bland-Altman limits of agreement based on the difference between two measurements (difference = x-y).
Please note that the package developer recommends reporting/using tolerance limits (see \code{"tolerance_limit"} function).
}
\details{
The limits of agreement (LoA) are calculated in this function are based on the method originally detailed by Bland & Atlman (1986 & 1999).
The \code{loa_calc} allow users to specify the calculative method for the LoA which can be based on Bland-Altman (1999) (\code{loa_calc = "blandaltman"}),
or by the more accurate MOVER method of Zou (2013) and Donner & Zou (2012) (\code{loa_calc = "mover"}).
}
\examples{
data('reps')

# Simple
agreement_limit(x = "x", y ="y", data = reps)

# Replicates
agreement_limit(x = "x", y ="y", data = reps, id = "id", data_type = "rep")

# Nested
agreement_limit(x = "x", y ="y", data = reps, id = "id", data_type = "nest")

}
\references{
MOVER methods:

Zou, G. Y. (2013). Confidence interval estimation for the Bland–Altman limits of agreement with multiple observations per individual. Statistical methods in medical research, 22(6), 630-642.

Donner, A., & Zou, G. Y. (2012). Closed-form confidence intervals for functions of the normal mean and standard deviation. Statistical Methods in Medical Research, 21(4), 347-359.

Bland & Altman methods:

Bland, J. M., & Altman, D. (1986). Statistical methods for assessing agreement between two methods of clinical measurement. The Lancet, 327(8476), 307-310.

Bland, J. M., & Altman, D. (1999). Measuring agreement in method comparison studies. Statistical methods in medical research, 8(2), 135-160.

Bland, J. M., & Altman, D. G. (1996). Statistics notes: measurement error proportional to the mean. BMJ, 313(7049), 106.
}
