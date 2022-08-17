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
  p <- plot(out)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  skip_on_ci()
  expect_snapshot_file(gg_save(p, "plot.png"))
})

test_that("plot with ref", {
  p <- plot(out, reference = "PBO")
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  skip_on_ci()
  expect_snapshot_file(gg_save(p, "plot-ref.png"))
})

test_that("plot utility", {
  p <- plot_utility(out)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  skip_on_ci()
  expect_snapshot_file(gg_save(p, "plot-utility.png"))
})

test_that("plot utility with reference", {
  p <- plot_utility(out, reference = "PBO")
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  skip_on_ci()
  expect_snapshot_file(gg_save(p, "plot-utility-ref.png"))
})

test_that("plot utility stacked", {
  p <- plot_utility(out, stacked = TRUE)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  skip_on_ci()
  expect_snapshot_file(gg_save(p, "plot-utility-stacked.png"))
})

test_that("plot utility stacked with reference", {
  p <- plot_utility(out, reference = "PBO", stacked = TRUE)
  expect_s3_class(p, "ggplot")
  skip_on_cran()
  skip_on_ci()
  expect_snapshot_file(gg_save(p, "plot-utility-stacked-ref.png"))
})
