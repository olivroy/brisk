assert_function <- function(fun, name) {
  if (!is.function(fun)) {
    rlang::abort(
      paste0("\"fun\" must be a function for ", name, "."),
      class = "brisk"
    )
  }
}

assert_chr <- function(x) {
  if (!is.character(x)) {
    name <- deparse(substitute(x))
    rlang::abort(paste0("\"", name, "\" must be a string."), class = "brisk")
  }
}

assert_num <- function(x) {
  if (!is.numeric(x)) {
    name <- deparse(substitute(x))
    rlang::abort(paste0("\"", name, "\" must be numeric."), class = "brisk")
  }
}

assert_brs <- function(x, mcda) {
  all_names <- vapply(x, function(xx) xx$name, character(1))
  dups <- unique(all_names[duplicated(all_names)])
  if (length(dups) > 0) {
    rlang::abort(
      paste0(
        "Benefit/risk names cannot be repeated: ",
        paste(dups, collapse = ", ")
      ),
      class = "brisk"
    )
  }
  assert_weights(x, mcda)
}

assert_groups <- function(groups, brs) {
  purrr::map(groups, assert_group, brs = brs)
  mcmc_lengths <- purrr::map_int(groups, assert_mcmc)
  u_mcmc <- unique(mcmc_lengths)
  if (length(u_mcmc) > 1) {
    rlang::abort(
      "posterior sample lengths are not consistent across groups.",
      class = "brisk"
    )
  }
}

assert_group <- function(group, brs) {
  nmes <- names(group)
  br_names <- vapply(brs, function(xx) xx$name, character(1))
  missing_names <- unique(br_names[!(br_names %in% nmes)])
  label <- attr(group, "label")
  if (length(missing_names) > 0) {
    rlang::abort(
      paste0(
        "\"", label, "\" is missing posterior samples for ",
        paste(missing_names, collapse = ", "),
        "."
      ),
      class = "brisk"
    )
  }
  extra_names <- nmes[!(nmes %in% br_names)]
  if (length(extra_names) > 0) {
    rlang::warn(
      paste0(
        "\"", label, "\" has unused posterior samples for ",
        paste(extra_names, collapse = ", "),
        "."
      ),
      class = "brisk"
    )
  }
}

assert_mcmc <- function(group) {
  mcmc_lengths <- purrr::map_int(group, length)
  u_mcmc <- unique(mcmc_lengths)
  label <- attr(group, "label")
  if (length(u_mcmc) > 1) {
    rlang::abort(
      paste0("posterior sample lengths are not equal for \"", label, "\"."),
      class = "brisk"
    )
  }
  u_mcmc
}

assert_weights <- function(x, mcda) {
  if (!mcda) return()
  w_sum <- vapply(x, function(xx) xx$weight, numeric(1)) %>%
    sum()
  if (!isTRUE(all.equal(w_sum, 1))) {
    rlang::abort("Weights must sum to 1.", class = "brisk")
  }
}
