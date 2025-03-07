% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.tolerance_delta.R
\name{tolerance_delta-methods}
\alias{tolerance_delta-methods}
\alias{print.tolerance_delta}
\alias{plot.tolerance_delta}
\alias{check.tolerance_delta}
\title{Methods for tolerance_delta objects}
\usage{
\method{print}{tolerance_delta}(x, digits = 4, ...)

\method{plot}{tolerance_delta}(
  x,
  geom = c("geom_point", "geom_bin2d", "geom_density_2d", "geom_density_2d_filled",
    "stat_density_2d"),
  delta = NULL,
  ...
)

\method{check}{tolerance_delta}(x)
}
\arguments{
\item{x}{object of class \code{tolerance_delta} as returned from a agreement_limit function.}

\item{digits}{The number of digits to print.}

\item{...}{further arguments passed through, see description of return value
for details.
\code{\link{tolerance_limit}}.}

\item{geom}{String naming the type of geometry to display the data points. Default is "geom_point". Other options include: "geom_bin2d", "geom_density_2d", "geom_density_2d_filled", and "stat_density_2d".}

\item{delta}{The maximal allowable difference.}
}
\value{
\describe{
\item{\code{print}}{Prints short summary of the tolerance limits.}
\item{\code{plot}}{Returns a plot of the tolerance limits.}
\item{\code{check}}{Returns plots testing the assumptions of the model. P-values for the normality and heteroskedasticity tests are provided as captions to the plot.}
}
}
\description{
Methods defined for objects returned from the tolerance_delta function(s).
}
