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
#' @param a,b, Toy args
#'
#' @export
do_things <- function(a = -1, b = -3) {
  thing_1(a) %>%
    thing_2(b)
}

#' Get the inputs to a function
#'
#' This gets all inputs to a function `f`, including
#' default arguments, when the arguments in `...` are
#' passed. All returned arguments are named, except those
#' which would be in a `...` argument, if `f` has one.
#'
#' @param f A function
#' @param ... Arguments to pass to `f`
#'
#' @return A named list
#' @export
#'
#' @examples get_args(paste, "hi", "there!")
get_args <- function(f, ...) {

  if ("..." %in% rlang::fn_fmls_names(f)) {
    return_args <- function() c(as.list(environment()), list(...))
  } else {
    return_args <- function() as.list(environment())
  }

  formals(return_args) <- formals(f)

  return_args(...)

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
#' @examples loud(nchar)("Hi!")
loud <- function(f, f_name = deparse(substitute(f))) {
  .f <- f
  .f_name <- f_name
  function(...) {

    args <- get_args(.f, ...)
    named <- args[names(args) != ""]
    named <- paste(names(named), "=", named, collapse = ", ")
    unnamed <- args[names(args) == ""]
    unnamed <- paste0(unnamed, collapse = ", ")

    message(sprintf("`%s` input(s):\n  `%s, %s`",
                    .f_name, named, unnamed))
    out <- .f(...)
    message(sprintf("`%s` output:\n  `%s`",
                    .f_name, out))
    out
  }
}

#' Temporarily change bindings in this package
#'
#' @param expr An expression that makes use of the function in `functions`
#' @param ... Pairs of names and values of functions to replace
#'
#' @return The result of the evaluated expression
#' @export
#'
#' @examples
#' with_package_bindings(do_things(1), do_things = loud(do_things))
with_package_bindings <- function(expr, ...) {

  fns <- list(...)
  fn_names <- names(fns)

  old <- lapply(fn_names, utils::getFromNamespace, "RUtils") %>%
    rlang::set_names(fn_names)

  on.exit(
    for (n in fn_names) {
      utils::assignInMyNamespace(n, old[[n]])
    }
  )

  for (n in fn_names) {
    utils::assignInMyNamespace(n, fns[[n]])
  }

  eval(rlang::enexpr(expr))

}

