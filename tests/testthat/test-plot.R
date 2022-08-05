gg_save <- function(plot, filename, ...) {
  fs::dir_create("figs")
  filename <- fs::path("figs", filename)
  ggplot2::ggsave(filename, plot, width = 7, height = 7, ...)
  filename
}

set.seed(1132)
ilogit <- function(x) 1 / (1 + exp(- x))
out <- mcda(
  benefit("CV", function(x) ilogit(x), weight = .75),
  risk("DVT", function(x) ilogit(- .5 * x), weight = .25),
  br_group(
    label = "PBO",
    CV = rnorm(1e4, .1),
    DVT = rnorm(1e4, .1)
  ),
  br_group(
    label = "TRT",
    CV = rnorm(1e4, 2),
    DVT = rnorm(1e4, 1)
  )
)

test_that("plot", {
  skip_on_cran()
  skip_on_ci()
  p <- plot(out)
  expect_snapshot(gg_save(p, "plot.png"))
})

test_that("plot with ref", {
  skip_on_cran()
  skip_on_ci()
  p <- plot(out, reference = "PBO")
  expect_snapshot(gg_save(p, "plot-ref.png"))
})

test_that("plot utility", {
  skip_on_cran()
  skip_on_ci()
  p <- plot_utility(out)
  expect_snapshot(gg_save(p, "plot-utility.png"))
})

test_that("plot utility with reference", {
  skip_on_cran()
  skip_on_ci()
  p <- plot_utility(out, reference = "PBO")
  expect_snapshot(gg_save(p, "plot-utility-ref.png"))
})

test_that("plot utility stacked", {
  skip_on_cran()
  skip_on_ci()
  p <- plot_utility(out, stacked = TRUE)
  expect_snapshot(gg_save(p, "plot-utility-stacked.png"))
})

test_that("plot utility stacked with reference", {
  skip_on_cran()
  skip_on_ci()
  p <- plot_utility(out, reference = "PBO", stacked = TRUE)
  expect_snapshot(gg_save(p, "plot-utility-stacked-ref.png"))
})
