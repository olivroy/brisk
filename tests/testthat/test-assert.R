test_that("assertions", {
  expect_error(assert_function(1, "foo"), class = "brisk")
  expect_error(assert_chr(1), class = "brisk")
  expect_error(assert_num("a"), class = "brisk")
  expect_error(
    assert_mcmc(br_group("pbo", CV = 1:5, CVT = 1:4)),
    class = "brisk"
  )
  expect_error(assert_reference(reference = c("a", "b")), class = "brisk")
  expect_error(
    assert_reference(list(scores = list(label = c("a", "b"))), "c"),
    class = "brisk"
  )
})

test_that("assert_br, assert_group, & assert_groups", {
  brs <- list(
    benefit("CV", function(x) x, weight = 0.25),
    risk("CV", function(x) 2 * x, weight = 0.75),
    benefit("CVT", function(x) x, weight = 0.25),
    risk("CVT", function(x) 2 * x, weight = 0.75)
  )
  expect_error(assert_brs(brs), class = "brisk")

  brs_good <- list(
    benefit("CV", function(x) x, weight = 0.25),
    risk("CVT", function(x) 2 * x, weight = 0.75)
  )
  groups <- list(
    br_group("pbo", CV = 1:5, CVT = 1:5),
    br_group("trt", CV = 1:4, CVT = 1:4)
  )
  expect_error(assert_groups(groups, brs_good), class = "brisk")
  expect_error(
    assert_group(br_group("pbo", CV = 1:5), brs_good),
    class = "brisk"
  )
  expect_warning(
    assert_group(br_group("pbo", CV = 1:5, CVT = 1:5, DVT = 1:5), brs_good),
    class = "brisk"
  )
})
