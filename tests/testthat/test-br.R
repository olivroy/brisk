test_that("br runs", {
  res <- br(
    benefit("CV", function(x) x, weight = 0.25),
    risk("DVT", function(x) 1.3 * x, weight = 0.75),
    br_group(
      label = "PBO",
      CV = 1:2,
      DVT = 3:4
    ),
    br_group(
      label = "TRT",
      CV = 5:6,
      DVT = 7:8
    )
  )

  exp_scores <- tibble::tibble(
    CV = c(1, 2, 5, 6),
    DVT = c(3, 4, 7, 8),
    label = c("PBO", "PBO", "TRT", "TRT"),
    iter = rep(1:2, 2)
  ) %>%
    dplyr::mutate(
      CV_utility = .25 * CV,
      DVT_utility = - .75 * 1.3 * DVT,
      total = CV_utility + DVT_utility
    ) %>%
    dplyr::select(all_of(colnames(res$scores)))

  exp_summary <- exp_scores %>%
    dplyr::group_by(label) %>%
    dplyr::summarize(
      mean = mean(total),
      lb = stats::quantile(total, .025),
      ub = stats::quantile(total, .975)
    )

  expect_type(res, "list")
  expect_equal(res$scores, exp_scores)
  expect_equal(res$summary, exp_summary)

  exp_weights <- c(.25, .75)
  names(exp_weights) <- c("CV", "DVT")
  expect_equal(attr(res, "weights"), exp_weights)
})
