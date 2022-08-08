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

assert_brs <- function(x) {
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
  assert_weight_lengths(brs, u_mcmc)
}

assert_weight_lengths <- function(brs, n_mcmc) {
  w_len <- vapply(brs, get_weight_length, integer(1))
  if (!all(w_len == 1L | w_len == n_mcmc)) {
    msg <- paste0(
      "\"weights\" must be length 1 or equal to length of posterior samples: ",
      n_mcmc,
      "."
    )
    rlang::abort(msg, class = "brisk")
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

assert_weights <- function(x) {
  w_sum <- vapply(x, function(xx) xx$weight, numeric(1)) %>%
    sum()
  if (!isTRUE(all.equal(w_sum, 1))) {
    rlang::abort("Weights must sum to 1.", class = "brisk")
  }
}

assert_utility_range <- function(x) {
  mcda_problems <- x %>%
    dplyr::summarize(
      across(ends_with("_utility"), ~ any(.x < 0 | .x > 1))
    ) %>%
    tidyr::pivot_longer(everything()) %>%
    dplyr::filter(.data$value)
  if (nrow(mcda_problems) > 0) {
    names <- gsub("_utility$", "", mcda_problems$name)
    msg <- paste0(
      "The following utility functions have values outside of [0, 1]: ",
      paste(names, collapse = ", "),
      "."
    )
    rlang::abort(msg, class = "brisk")
  }
}

assert_no_extra_args <- function(args, brs, groups) {
  if (length(args) > (length(brs) + length(groups))) {
    rlang::abort(
      "All arguments must be calls to benefit(), risk(), or br_group().",
      class = "brisk"
    )
  }
}

assert_reference <- function(x, reference) {
  if (is.null(reference)) return()
  if (length(reference) > 1) {
    rlang::abort(
      "\"reference\" must have length 1 or be NULL.",
      class = "brisk"
    )
  }
  labels <- unique(x$label)
  if (!(reference %in% labels)) {
    msg <- paste0(
      "\"reference\" must be one of: ",
      paste(labels, collapse = ", "),
      "."
    )
    rlang::abort(msg, class = "brisk")
  }
}

assert_p <- function(p) {
  if (any(p < 0 | p > 1)) {
    rlang::abort("\"p\" must be between 0 and 1.", class = "brisk")
  }
}
