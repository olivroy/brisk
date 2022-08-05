#' Plot Benefit/Risk Posterior Scores
#' @param x output from a call to `br()` or `mcda()`.
#' @param reference a string indicating which group is the reference group which
#'   is used to subtract scores from other groups.
#' @param ... additional arguments throw an error.
#' @return A ggplot object plotting the posterior densities of the weighted
#'   utility scores.
#' @export
plot.brisk_br <- function(x, reference = NULL, ...) {
  ellipsis::check_dots_empty()
  scores <- adjust_scores(x, reference)
  title <- adjust_title("Benefit-Risk Score Distribution", reference)
  p <- ggplot(
    scores,
    aes(
      .data$total,
      group = .data$label,
      fill = .data$label,
      color = .data$label
    )
  ) +
    geom_density(alpha = 0.5) +
    labs(
      x = "Benefit-Risk Score",
      y = "Density",
      fill = "Label",
      color = "Label"
    ) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5))
  return(p)
}

#' Plot Posterior Mean Utility Scores
#' @inheritParams plot.brisk_br
#' @param stacked logical indicating if a stacked version of the barplot should
#'   be produced.
#' @return A ggplot barplot of the posterior mean utiltity scores.
#' @example man/examples/ex-mcda.R
#' @export
plot_utility <- function(x, reference = NULL, stacked = FALSE) {
  scores <- adjust_scores(x, reference)
  post_mean <- scores %>%
    dplyr::group_by(.data$label) %>%
    dplyr::summarize(
      across(ends_with("_utility"), mean),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      - .data$label,
      names_to = "Outcome",
      values_to = "Utility"
    ) %>%
    dplyr::mutate(Outcome = sub("_utility$", "", .data$Outcome))
  title <- adjust_title("Posterior Mean Utility", reference)
  if (!stacked)  {
    p <- ggplot(post_mean, aes(.data$Utility, .data$Outcome)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ .data$label) +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    data_label <- post_mean %>%
      dplyr::group_by(.data$label) %>%
      dplyr::summarise(Utility = sum(.data$Utility))
    p <- ggplot(
      post_mean,
      aes(
        .data$label,
        .data$Utility,
        color = .data$Outcome,
        fill = .data$Outcome
      )
    ) +
      geom_bar(stat = "identity") +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(
        aes(
          label = round(.data$Utility, 2),
          color = NULL,
          fill = NULL,
          vjust = - .25
        ),
        data = data_label
      )
  }
  return(p)
}

adjust_scores <- function(x, reference) {
  assert_reference(x, reference)
  if (is.null(reference)) return(x$scores)
  scores <- x$scores
  scores_ref <- dplyr::filter(scores, .data$label == !!reference)
  scores <- dplyr::filter(scores, .data$label != !!reference)
  scores <- scores %>%
    dplyr::left_join(scores_ref, by = "iter", suffix = c("", "_ref")) %>%
    dplyr::mutate(
      across(
        ends_with("_utility") | ends_with("_score") | .data$total,
        ~ .x - cur_data()[[paste0(cur_column(), "_ref")]]
      )
    ) %>%
    dplyr::select(- ends_with("_ref")) %>%
    dplyr::mutate(reference = !!reference)
}

adjust_title <- function(title, reference) {
  if (is.null(reference)) return(title)
  title <- paste0(title, "\n", "(Adjusted Relative to ", reference, ")")
}
