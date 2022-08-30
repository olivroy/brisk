#' Simulate Normalized Weights
#' @param n number of weights to simulate.
#' @param ... vectors of length 2 indicating the lower
#'   and upper bound (respectively) of the un-normalized weights.  At least one
#'   set of bounds must be equal to each other (e.g. c(1, 1)) and be the largest
#'   set of bounds in the set specified.
#' @details The weights are normalized relative to a set of bounds which
#'   are equal to each other (e.g. c(1, 1)), and also are the largest set of
#'   bounds in the set specified.  See Example.
#' @return A tibble with weights for each argument supplied to `...`.  Each
#'   column represents the weights, and each row (total of `n` rows) is a
#'   set of random weights across groups. Column names are obtained from the
#'   argument names of `...`, if supplied.
#' @example man/examples/ex-weights.R
#' @export
sim_weights <- function(n, ...) {
  ranges <- list(...)
  k <- length(ranges)
  max_vals <- get_max_val(ranges)
  max <- max_vals$max
  ind <- max_vals$ind[1]
  const <- NULL
  for (i in setdiff(1:k, ind)) {
    const <- hitandrun::mergeConstraints(
      const,
      hitandrun::lowerRatioConstraint(k, ind, i, max / ranges[[i]][2]),
      hitandrun::upperRatioConstraint(k, ind, i, max / ranges[[i]][1])
    )
  }
  const <- hitandrun::mergeConstraints(const, hitandrun::simplexConstraints(k))
  w <- hitandrun::hitandrun(const, n.samples = n)
  colnames(w) <- names(ranges)
  w <- dplyr::as_tibble(w, .name_repair = "minimal")
  return(w)
}

get_max_val <- function(ranges) {
  len2 <- vapply(ranges, length, numeric(1)) == 2
  if (!all(len2))
    rlang::abort("All ... must be length 2.", class = "brisk")

  vals <- do.call(rbind, ranges)
  colnames(vals) <- c("lb", "ub")
  vals <- vals %>%
    dplyr::as_tibble(rownames = "brs") %>%
    dplyr::mutate(
      direction = .data$lb <= .data$ub,
      equal = .data$lb == .data$ub,
      max = max(.data$ub) == .data$ub)

  if (!all(vals$direction)) {
    msg <- paste0(
      "Upper bounds must be >= lower bounds: ",
      paste0(vals$brs[!vals$direction], collapse = ", ")
    )
    rlang::abort(msg, class = "brisk")
  }

  if (sum(vals$equal & vals$max) < 1) {
    msg <- paste0(
      "At least one benefit/risk must have ",
      "lower and upper bounds equal and is largest."
    )
    rlang::abort(msg, class = "brisk")
  }
  list(max = max(vals$ub), ind = which(vals$equal & vals$max))
}
