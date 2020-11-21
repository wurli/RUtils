# These are test functions that demonstrate a way of temporarily wrapping
# functions for the duration of a call - even package functions that aren't
# explicitly referenced.

# Test function
thing_1 <- function(a = 1) {
  a + 10
}

# Test function
thing_2 <- function(a = 2, b = 3) {
  a + b + 1
}

#' Toy function for testing
#'
#' @export
do_things <- function(a = -1, b = -3) {
  thing_1(a) %>%
    thing_2(b)
}

#' Create a loud version of a function
#'
#' Can be used to wrap a function so that it
#' prints its input(s) and output when it runs.
#' Toy function for testing `with_package_bindings`
#'
#' @param f Function to wrap
#' @param f_name The function name
#'
#' @return A function
#' @export
#'
#' @examples
#' loud(nchar)("Hi!")
#'
loud <- function(f, f_name = deparse(substitute(f))) {
  .f <- f
  .f_name <- f_name
  function(...) {
    print(sprintf("`%s` input(s):", .f_name))
    print(paste(..., sep = "; "))
    out <- .f(...)
    print(sprintf("`%s` output:", .f_name))
    print(out)
    out
  }
}

#' Temporarily change bindings in this package
#'
#' Thin wrapper for `rlang::local_bindings`
#'
#' @param expr An expression that makes use of the function in `functions`
#' @param ... Pairs of names and values of functions to replace
#'
#' @return The result of the evaluated expression
#' @export
#'
#' @examples
#' with_package_bindings(do_things(1), thing_1 = loud(thing_1))
with_package_bindings <- function(expr, ...) {
  rlang::with_bindings(
    eval(rlang::enexpr(expr)),
    ...,
    .env = topenv(environment())
  )
}
