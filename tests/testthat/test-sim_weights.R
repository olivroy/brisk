test_that("sim_weights works", {
  w <- sim_weights(1e4, a = c(.4, .45), b = c(1, 1), d = c(.2, .5))
  expect_true(all(0.4 <= w[, 1] / w[, 2] & w[, 1] / w[, 2] <= .45))
  expect_true(all(0.2 <= w[, 3] / w[, 2] & w[, 3] / w[, 2] <= .5))
  expect_error(sim_weights(10, a = c(1:3)), class = "brisk")
  expect_error(sim_weights(10, a = c(2, 1)), class = "brisk")
  expect_error(sim_weights(10, a = c(.5, 1)), class = "brisk")
})
