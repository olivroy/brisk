#' @rdname br
#' @param name a string indicating the name of the benefit or risk.
#' @param fun a utility function which maps a parameter value to a utility
#'   value.
#' @param weight the weight of the benefit/risk.
#' @export
benefit <- function(name, fun, weight) {
  assert_chr(name)
  assert_function(fun, name)
  assert_num(weight)
  out <- list(name = name, fun = fun, weight = weight)
  class(out) <- c("brisk_benefit", "brisk_br")
  return(out)
}

#' @rdname br
#' @export
risk <- function(name, fun, weight) {
  assert_chr(name)
  assert_function(fun, name)
  assert_num(weight)
  out <- list(name = name, fun = fun, weight = weight)
  class(out) <- c("brisk_risk", "brisk_br")
  return(out)
}

#' Bayesian Benefit Risk
#' @param ... calls to `benefit()`, `risk()`, and `br_group()` to define the
#'   utility functions and treatment groups.
#' @param probs a vector of probabilities used to obtain quantiles of
#'   the posterior of the weighted utilities for each group.
#' @details The `br()` function allows the user to define an arbitrary number
#'   of "benefits" and "risks".  Each benefit/risk requires a utility
#'   function (`fun`) and a weight.  The utility function maps the benefit/risk
#'   parameter to a utility score.  The `br_group()` function supplies samples
#'   from the posterior distribution for each benefit risk for a specific
#'   group (e.g. treatment arm).
#'
#'   The `br()` function then calculates the posterior distribution of the
#'   overall utility for each group.  The overall utility is a weighted sum of
#'   the utilities for each benefit/risk.
#'
#'   The `mcda()` function is the same as `br()`, but has extra checks to
#'   ensure that the total weight of all benefits and risks is 1, and that the
#'   utility functions produce values between 0 and 1 for all posterior
#'   samples.
#' @return A named list with posterior summaries of utility for each group and
#'   the raw posterior utility scores.
#' @example man/examples/ex-mcda.R
#' @export
br <- function(..., probs = c(.025, .975)) {
  args <- list(...)
  brs <- get_brs(args)
  groups <- get_groups(args)
  assert_no_extra_args(args, brs, groups)
  assert_brs(brs)
  assert_groups(groups, brs)
  scores <- purrr::map_dfr(groups, get_group_utility, brs = brs)
  total <- rowSums(dplyr::select(scores, ends_with("_score")))
  scores <- scores %>%
    dplyr::mutate(total = !!total) %>%
    dplyr::as_tibble()
  sumry <- scores %>%
    dplyr::group_by(.data$label) %>%
    dplyr::summarize(
      mean = mean(.data$total),
      qtiles = stats::quantile(.data$total, prob = !!probs, names = FALSE)
    ) %>%
    dplyr::mutate(qtile_label = sprintf("%.2f%%", 100 * !!probs)) %>%
    tidyr::pivot_wider(
      names_from = "qtile_label",
      values_from = "qtiles"
    ) %>%
    dplyr::ungroup()
  out <- list(summary = sumry, scores = scores)
  w <- purrr::map(brs, get_weight)
  w <- do.call("c", w)
  attr(out, "weights") <- w
  class(out) <- c("brisk_br")
  return(out)
}

#' @rdname br
#' @export
mcda <- function(..., probs = c(.025, .975)) {
  args <- list(...)
  brs <- get_brs(args)
  assert_weights(brs)
  out <- br(..., probs = probs)
  assert_utility_range(out$scores)
  class(out) <- c("brisk_mcda", class(out))
  return(out)
}

get_brs <- function(x) {
  ind <- vapply(x, inherits, logical(1), what = "brisk_br")
  x[ind]
}

get_groups <- function(x) {
  ind <- vapply(x, inherits, logical(1), what = "brisk_group")
  x[ind]
}

#' Posterior Samples for a Benefit/Risk Group
#' @param label a string indicating the name of the group.
#' @param ... named arguments which correspond to the names of the
#'   benefits/risks specified by `benefit()` and `risk()` in a call to `br()`.
#' @details This function is intended to be used as an input argument to
#'   the `br()` function.
#' @return A named list with the posterior samples and an assigned S3 class.
#' @example man/examples/ex-br.R
#' @export
br_group <- function(label, ...) {
  samps <- list(...)
  attr(samps, "label") <- label
  class(samps) <- "brisk_group"
  return(samps)
}

get_weight <- function(x) {
  out <- x$weight
  names(out) <- x$name
  out
}

get_group_utility <- function(br_group, brs) {
  purrr::map_dfc(brs, get_utility, br_group = br_group) %>%
    dplyr::mutate(
      label = attr(br_group, "label"),
      iter = 1:n()
    )
}

get_utility <- function(x, br_group) {
  samples <- br_group[[x$name]]
  out <- data.frame(
    samps = samples,
    weight = x$weight,
    utility =  x$f(samples)
  ) %>%
    dplyr::mutate(score = .data$weight * .data$utility)
  colnames(out) <- c(x$name, paste0(x$name, c("_weight", "_utility", "_score")))
  out
}
