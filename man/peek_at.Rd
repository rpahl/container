% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peek_at.R
\name{peek_at}
\alias{peek_at}
\alias{peek_at.Container}
\alias{peek_at.dict.table}
\title{Peek at Indices}
\usage{
peek_at(.x, ...)

\method{peek_at}{Container}(.x, ..., .default = NULL)

\method{peek_at}{dict.table}(.x, ..., .default = NULL)
}
\arguments{
\item{.x}{an \code{R} object of the respective class.}

\item{...}{indices of elements to be extracted}

\item{.default}{value to be returned if peeked value does not exist.}
}
\value{
For \code{Container}, returns the value at the given indices or (if not
found) the given default value.

For \code{dict.table}, returns the columns at the given indices or (if not
found) columns with the given default value.
}
\description{
Try to access elements and return default values if not found.
In contrast to \verb{[at()]}, this function provides a less stricter element
access, that is, it remains valid even if elements don't exist.
}
\details{
\code{peek_at} tries to access specific values.
}
\examples{

# Container
co = container(a = 1, 2, b = 3, 4)
peek_at(co, 1)
peek_at(co, "a")
peek_at(co, "x")
peek_at(co, "x", .default = 0)
peek_at(co, "a", "x", 2, 9, .default = -1)

# Dict
d = dict(a = 1, b = 1:3)
peek_at(d, "b")
peek_at(d, "x")
peek_at(d, "x", .default = 4:7)

# dict.table
dit = dict.table(a = 1:3, b = 4:6)
peek_at(dit, "a")
peek_at(dit, 1)
peek_at(dit, 3)
peek_at(dit, "x")
peek_at(dit, "x", .default = 0)
peek_at(dit, "a", "x", .default = 0)
}
\seealso{
\code{\link[=at]{at()}} for strict element extraction
}
