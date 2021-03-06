% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{bro_top_n}
\alias{bro_top_n}
\title{Filter top n rows by value}
\usage{
bro_top_n(df, n, ...)
}
\arguments{
\item{df}{data frame to filter}

\item{n}{number of rows to return}

\item{...}{names of columns to sort by}
}
\description{
This function sorts a data frame by one or more columns and returns the top n rows.
In contrast to \code{dplyr::top_n}, this function alows to sort by multiple columns and returns
exactly n rows even if ties are present in the columns used for sorting.
}
