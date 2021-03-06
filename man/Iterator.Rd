% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iterator.R
\docType{class}
\name{Iterator}
\alias{Iterator}
\alias{iter}
\alias{is.iterator}
\alias{itbegin}
\alias{itget}
\alias{itget_next}
\alias{itpos}
\alias{ithas_next}
\alias{itnext}
\title{Iterator}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
Iterator

iter(x)

is.iterator(x)

itbegin(it)

itget(it)

itget_next(it)

itpos(it)

ithas_next(it)

itnext(it)
}
\arguments{
\item{x}{iterable object, e.g., \code{\link[base]{list}},
\code{\link[base]{vector}}, \code{\link[container]{Container}}}

\item{it}{\code{\link[container]{Iterator}} object}
}
\description{
An \code{\link[container]{Iterator}} is an object that allows
to iterate over sequences. It implements \code{_next} and \code{get} to
iterate and retrieve the value of the sequence it is associated with.
}
\section{Constructor}{

\code{Iterator$new(x)}
}

\section{Iterator interface}{

\describe{
 \item{\code{begin()}}{Reset iterator position to 1.}
 \item{\code{get()}}{Get value at current iterator position.}
 \item{\code{get_next()}}{Get value after incrementing by one.}
 \item{\code{pos()}}{Return current iterator position.}
 \item{\code{has_next()}}{Return TRUE if there is a next element.}
 \item{\code{next()}}{Increment iterator to point at next element.}
}
}

\section{S3 method interface}{

\describe{
 \item{\code{itbegin(it)}}{Reset iterator position to 1.}
 \item{\code{itget(it)}}{Get value at current iterator position.}
 \item{\code{itget_next()}}{Get value after incrementing by one.}
 \item{\code{itpos()}}{Return current iterator position.}
 \item{\code{ithas_next(it)}}{Return TRUE if there is a next element.}
 \item{\code{itnext(it)}}{Increment iterator to point at next element.}
}
}

\examples{
# Iterator on primitive list
it <- Iterator$new(list("A", 1, 2))
while(it$has_next()) {
print(it$get_next())
}
it$has_next()   # FALSE
print(it)       # <Iterator> at position 3
it$begin()
print(it)       # <Iterator> at position 0

# Iterator from Container object
d <- deque(1:3)
it <- iter(d)
sum <- 0
while(it$has_next()) {
sum <- sum + it$get_next()
}
print(sum)

# S3 method interface
it <- iter(list("A", 1, 2))
while(ithas_next(it)) {
print(itget_next(it))
}
ithas_next(it)   # FALSE
print(it)       # <Iterator> at position 3
itbegin(it)
print(it)       # <Iterator> at position 0
}
\seealso{
\code{\link[container]{Iterable}},
\code{\link[container]{Container}}, \code{\link[container]{container}}
}
\author{
Roman Pahl
}
\keyword{datasets}
