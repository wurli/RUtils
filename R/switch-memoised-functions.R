#' Switch to the memoised version of a function
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
#' @examples
#' runif2 <- function(n, use_cached_result = FALSE) {
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
#' runif2(2, TRUE)
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

#' The uniform distribution
#'
#' Imports stats::runif for demoing memoise_package_fns
#' @export
runif <- stats::runif

#' Replace package functions with their memoised versions
#'
#' You can only memoise functions which are part of
#' the package!
#'
#' @param groups The groups of functions to memoise
#' @param functions The names of the functions in the groups
#' @param unmemoise Whether to unmemoise instead of memoise
#'
#' @return Quietly returns NULL
#' @export
#' @examples
#' # First make sure `runif` is unmemoised
#' memoise_package_fns(functions = list(other = "runif"), unmemoise = TRUE)
#'
#' # Gives different values as expected
#' RUtils::runif(2)
#' RUtils::runif(2)
#'
#' # Now memoise `runif`
#' memoise_package_fns(functions = list(other = "runif"), unmemoise = FALSE)
#'
#' # Gives the same value due to memoisation
#' RUtils::runif(2)
#' RUtils::runif(2)
memoise_package_fns <- function(groups = c("read_fns", "slow_fns", "other"),
                                functions = list(read_fns = c(),
                                                 slow_fns = c(),
                                                 other = "runif"),
                                unmemoise = FALSE) {

  # Get the functions to memoise/unmemoise
  functions <- unlist(functions[groups], use.names = FALSE)

  # Use this to unmemoise a function
  unmemoise_ <- function(f) {
    if (!memoise::is.memoised(f)) {
      f
    } else {
      environment(f)$`_f`
    }
  }

  # Functional to alter specified functions
  memoise_fn <- if (unmemoise) unmemoise_ else memoise::memoise

  # Apply functional to specified package functions
  for (f_name in functions) {
    f <- utils::getFromNamespace(f_name, "RUtils")
    f <- memoise_fn(f)
    utils::assignInMyNamespace(f_name, f)
  }

  # Return NULL
  invisible(NULL)

}


