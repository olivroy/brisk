#' @export
benefit <- function(name, fun, weight, samples) {
  out <- list(name = name, fun = fun, weight = weight, samples = samples)
  class(out) <- "brisk_benefit"
  return(out)
}

#' @export
risk <- function(name, fun, weight, samples) {
  out <- list(name = name, fun = fun, weight = weight, samples = samples)
  class(out) <- "brisk_risk"
  return(out)
}

#' @export
br <- function(...) {
  brs <- list(...)
  scores <- do.call(tidyr::bind_rows, brs)
  sumry <- scores %>%
    dplyr::group_by(label) %>%
    summarize(
      mean = mean(total),
      lb = quantile(total, prob = .025),
      ub = quantile(total, prob = .975),
    )
}

#' @export
br_group <- function(label, ...) {
  brs <- list(...)
  utilities <- purrr::map_dfc(brs, get_utility) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      total = sum(c_across(everything()))
    ) %>%
    ungroup() %>%
    dplyr::mutate(label = !!label)
  w <- purrr::map(brs, get_weight)
  w <- do.call("c", w)
  attr(utilities, "weights") <- w
  return(utilities)
}

get_weight <- function(x) {
  out <- x$weight
  names(out) <- x$name
  out
}

get_utility <- function(x) UseMethod("get_utility")

#' @export
get_utility.brisk_benefit <- function(x) {
  out <- data.frame(
    y = x$samples,
    x = x$weight * x$f(x$samples)
  )
  colnames(out) <- c(x$name, paste0(x$name, "_utility"))
  out
}

#' @export
get_utility.brisk_risk <- function(x) {
  out <- data.frame(
    y = x$samples,
    x = - x$weight * x$f(x$samples)
  )
  colnames(out) <- c(x$name, paste0(x$name, "_utility"))
  out
}
