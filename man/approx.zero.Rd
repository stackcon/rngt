% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numeric.R
\name{approx.zero}
\alias{approx.zero}
\title{Check if values are approximately zero}
\usage{
approx.zero(x, tolerance = sqrt(.Machine$double.eps))
}
\arguments{
\item{x}{vector of values to check whether near-zero}

\item{tolerance}{machine tolerance. related to numerical precision of the machine}
}
\value{

}
\description{
Based on [this](https://www.rdocumentation.org/packages/bazar/versions/1.0.11/topics/almost.zero) implementation
}
