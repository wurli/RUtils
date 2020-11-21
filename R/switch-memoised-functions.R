#' Evaluate the memoised version of a function
#'
#' This function is intended to be called within other
#' functions. When called within a function `foo` it searches
#' for another function called `foo_mem` and uses that instead.
#' `foo` should have an argument `use_cached_result`, which
#' controls whether or not this function gets called.
#'
#' @return The result of calling a memoised function
#' @export
#'
#' @examples runif2 <- function(n, use_cached_result = FALSE) {
#'
#' if (use_cached_result) { return(cached_result()) }
#'
#' runif(n)
#'
#' }
#'
#' runif2_mem <- memoise::memoise(runif2)
#'
#' # These give different results
#' runif2(2)
#' runif2(2)
#'
#' # These give the same results
#' runif2(2, TRUE)
#' runif2(2, TRUE)s
cached_result <- function() {

  # Get the call that called this function
  call <- as.list(match.call(
    sys.function(sys.parent()),
    sys.call(sys.parent()),
    TRUE,
    parent.frame(2L)
  ))

  # Get the name of the caller function
  function_name <- deparse(call[[1]])

  # Get the name of the memoised function
  function_name_mem <- paste0(function_name, "_mem")

  # Search for a memoised version
  memoised_version <- tryCatch(
    get(function_name_mem),
    error = function(e) {
      warning(sprintf("Couldn't find function`%s`", function_name_mem))
      get(function_name)
    }
  )

  # Warn if the memoised version isn't the same as the non-memoised version
  if (!identical(environment(memoised_version)$`_f`, sys.function(1))) {
    warning(sprintf("`%s` is not a memoised version of `%s`.
                    Have you changed `%s` since last running `%s <- memoise(%s)`?",
                    function_name_mem, function_name, function_name,
                    function_name_mem, function_name))
  }

  # Get the caller args and evaluate them in the environment of the caller function
  arg_names <- call[-1]
  args <- lapply(arg_names, eval, parent.frame(1))

  # Make sure the call to the memoised function doesn't search for the cached
  # version to avoid accidental recursion
  args$use_cached_result <- FALSE

  # Call the memoised function
  do.call(memoised_version, args)
}

