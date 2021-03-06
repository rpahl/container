% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/container-pkg.R
\docType{package}
\name{container.pkg}
\alias{container.pkg}
\alias{container.pkg-package}
\title{Container, Deque, Set, and Dict (aka Map) - R6 based container classes
with iterators and reference semantics.}
\description{
Implements a general Container class with typical member functions to insert,
delete and access objects from the container. The Container class serves as
the base class for the Deque, Set and Dict classes (resembling 'Python's
dict type). Supports iterators and, being R6 classes, reference semantics.
The focus of implementation was not on speed but to define consistent class
interfaces based on a meaningful class hierarchy.
}
\author{
Roman Pahl, \email{roman.pahl@gmail.com}
}
