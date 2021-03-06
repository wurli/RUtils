% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replace-package-functions.R
\name{loud}
\alias{loud}
\title{Create a loud version of a function}
\usage{
loud(f, f_name = deparse(substitute(f)))
}
\arguments{
\item{f}{Function to wrap}

\item{f_name}{The function name}
}
\value{
A function
}
\description{
Can be used to wrap a function so that it
prints its input(s) and output when it runs.
Toy function for testing \code{with_package_bindings}
}
\examples{
loud(nchar)("Hi!")

}
