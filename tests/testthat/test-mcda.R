test_that("mcda", {
  ilogit <- function(x) 1 / (1 + exp(- x))
  res <- mcda(
    benefit("CV", function(x) ilogit(x), weight = 0.25),
    risk("DVT", function(x) ilogit(1.3 * x), weight = 0.75),
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
      CV_utility = ilogit(CV),
      CV_score = .25 * ilogit(CV),
      DVT_weight = 0.75,
      DVT_utility = ilogit(1.3 * DVT),
      DVT_score = .75 * ilogit(1.3 * DVT),
      total = CV_score + DVT_score
    ) %>%
    dplyr::select(all_of(colnames(res)))
  class(exp_scores) <- c("brisk_mcda", "brisk_br", class(exp_scores))

  expect_equal(res, exp_scores)
})

test_that("mcda checks utility range between 0 and 1", {
  expect_error(
    mcda(
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
    ),
    class = "brisk"
  )
})

test_that("mcda checks weights sum to 1", {
  expect_error(
    mcda(
      benefit("CV", function(x) x, weight = 0.1),
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
    ),
    class = "brisk"
  )
})

test_that("stochastic mcda()", {
  w <- c(.2, .933)
  ilogit <- function(x) 1 / (1 + exp(- x))
  res <- mcda(
    benefit("CV", function(x) ilogit(x), weight = w),
    risk("DVT", function(x) ilogit(1.3 * x), weight = 1 - w),
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
      CV_weight = rep(w, 2),
      CV_utility = ilogit(CV),
      CV_score = w * ilogit(CV),
      DVT_weight = rep(1 - w, 2),
      DVT_utility = ilogit(1.3 * DVT),
      DVT_score = (1 - w) * ilogit(1.3 * DVT),
      total = CV_score + DVT_score
    ) %>%
    dplyr::select(all_of(colnames(res)))
  class(exp_scores) <- c("brisk_mcda", "brisk_br", class(exp_scores))

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
    mcda(
      benefit("CV", function(x) ilogit(x), weight = c(.25, .25, .25)),
      risk("DVT", function(x) ilogit(1.3 * x), weight = 0.75),
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
