benefit <- function(name, fun, weight, samples) {
  out <- list(name = name, fun = fun, weight = weight, samples = samples)
  class(out) <- "brisk_benefit"
  return(out)
}

risk <- function(name, fun, weight, samples) {
  out <- list(name = name, fun = fun, weight = weight, samples = samples)
  class(out) <- "brisk_risk"
  return(out)
}

br <- function(...) {
  brs <- list(...)
  utilities <- purrr::map_dfc(brs, get_utility) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      total = sum(c_across(everything()))
    ) %>%
    ungroup()
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
