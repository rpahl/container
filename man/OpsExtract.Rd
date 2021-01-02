% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ops-extract.R
\name{OpsExtract}
\alias{OpsExtract}
\alias{[[.Dict}
\alias{[.Dict}
\alias{[[.dict.table}
\alias{$.dict.table}
\title{Extract Operators for Containers}
\usage{
\method{[[}{Dict}(x, key, default = NULL)

\method{[}{Dict}(x, key, default = NULL)

\method{[[}{dict.table}(x, j, default = NULL)

\method{$}{dict.table}(x, key)
}
\arguments{
\item{x}{\code{Container} or \code{dict.table} object from which to extract
elements or columns.}

\item{key}{\code{character} vector with key(s).}

\item{default}{A suitable default value.}

\item{j}{\code{numeric} or \code{character} column index.}
}
\value{
For \code{Dict}, \code{[[} returns the element found at key. If not found, an
error is signaled, unless \code{default} was specified, which then would be
returned instead.

For \code{Dict}, \code{[} returns a new dict containing the extracted elements
found at the keys. If one or more keys are not found, an error is signaled,
unless \code{default} was specified, which then would be put in place for all
missing keys.

For \code{dict.table}, \code{[[} returns the selected column. If the
column does not exist, an error is signaled, unless \code{default} was specified,
which then would be returned instead.

For \code{dict.table}, \code{$} returns the selected column. If the
column does not exist, an error is signaled.
}
\description{
Extract parts of Container and dict.table objects.
}