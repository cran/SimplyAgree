% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jmvagree.h.R
\name{jmvagree}
\alias{jmvagree}
\title{Simple Agreement Analysis}
\usage{
jmvagree(
  data,
  method1,
  method2,
  ciWidth = 95,
  agreeWidth = 95,
  testValue = 2,
  CCC = TRUE,
  plotbland = TRUE,
  plotcon = FALSE,
  plotcheck = FALSE,
  prop_bias = FALSE,
  xlabel = "Average of Both Methods",
  ylabel = "Difference between Methods"
)
}
\arguments{
\item{data}{Data}

\item{method1}{Name of column containing 1st Vector of data}

\item{method2}{Name of column containing Vector of data}

\item{ciWidth}{a number between 50 and 99.9 (default: 95), the width of
confidence intervals}

\item{agreeWidth}{a number between 50 and 99.9 (default: 95), the width of
agreement limits}

\item{testValue}{a number specifying the limit of agreement}

\item{CCC}{\code{TRUE} or \code{FALSE} (default), produce CCC table}

\item{plotbland}{\code{TRUE} or \code{FALSE} (default), for Bland-Altman
plot}

\item{plotcon}{\code{TRUE} or \code{FALSE} (default), for Bland-Altman plot}

\item{plotcheck}{\code{TRUE} or \code{FALSE} (default), assumptions plots}

\item{prop_bias}{\code{TRUE} or \code{FALSE}}

\item{xlabel}{The label for the x-axis on the BA plot}

\item{ylabel}{The label for the y-axis on the BA plot}
}
\value{
A results object containing:
\tabular{llllll}{
\code{results$text} \tab \tab \tab \tab \tab a html \cr
\code{results$blandtab} \tab \tab \tab \tab \tab a table \cr
\code{results$ccctab} \tab \tab \tab \tab \tab a table \cr
\code{results$plotba} \tab \tab \tab \tab \tab an image \cr
\code{results$plotcon} \tab \tab \tab \tab \tab an image \cr
\code{results$plotcheck} \tab \tab \tab \tab \tab an image \cr
}

Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:

\code{results$blandtab$asDF}

\code{as.data.frame(results$blandtab)}
}
\description{
Simple Agreement Analysis
}
