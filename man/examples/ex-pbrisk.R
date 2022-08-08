set.seed(1132)
out <- br(
  benefit("CV", function(x) x, weight = 1),
  risk("DVT", function(x) - .5 * x, weight = 1),

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

pbrisk(out, q = c(.03, .04))
pbrisk(out, q = c(.03, .04), direction = "upper")
pbrisk(out, q = c(.03, .04), reference = "PBO")

qbrisk(out, p = c(.025, .975))
qbrisk(out, p = c(.025, .975), reference = "PBO")
