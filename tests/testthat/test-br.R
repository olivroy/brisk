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
      CV_weight = 0.25,
      CV_utility = CV,
      CV_score = .25 * CV,
      DVT_weight = 0.75,
      DVT_utility = 1.3 * DVT,
      DVT_score = .75 * 1.3 * DVT,
      total = CV_score + DVT_score
    ) %>%
    dplyr::select(all_of(colnames(res)))
  class(exp_scores) <- c("brisk_br", class(exp_scores))

  exp_summary <- exp_scores %>%
    dplyr::group_by(label) %>%
    dplyr::summarize(
      mean = mean(total),
      `2.50%` = stats::quantile(total, .025, names = FALSE),
      `97.50%` = stats::quantile(total, .975, names = FALSE)
    ) %>%
    dplyr::ungroup()

  expect_equal(res, exp_scores)
})

test_that("stochastic br()", {
  w1 <- c(.2, .933)
  w2 <- c(.1311634, .33331)
  res <- br(
    benefit("CV", function(x) x, weight = w1),
    risk("DVT", function(x) 1.3 * x, weight = w2),
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
      CV_weight = rep(w1, 2),
      CV_utility = CV,
      CV_score = w1 * CV,
      DVT_weight = rep(w2, 2),
      DVT_utility = 1.3 * DVT,
      DVT_score = w2 * 1.3 * DVT,
      total = CV_score + DVT_score
    ) %>%
    dplyr::select(all_of(colnames(res)))
  class(exp_scores) <- c("brisk_br", class(exp_scores))

  exp_summary <- exp_scores %>%
    dplyr::group_by(label) %>%
    dplyr::summarize(
      mean = mean(total),
      `2.50%` = stats::quantile(total, .025, names = FALSE),
      `97.50%` = stats::quantile(total, .975, names = FALSE)
    ) %>%
    dplyr::ungroup()

  expect_equal(res, exp_scores)

  expect_error(
    br(
      benefit("CV", function(x) x, weight = 1:3),
      risk("DVT", function(x) 1.3 * x, weight = .25),
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
    ),
    class = "brisk"
  )
})

test_that("br() no extra args", {
  expect_error(
    br(
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
      ),
      foo = 1
    ),
    class = "brisk"
  )
})
