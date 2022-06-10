br(
  benefit("CV", function(x) x, weight = 0.5),
  risk("DVT", function(x) 2 * x, weight = 0.5),
  br_group(
    label = "PBO",
    CV = rnorm(100),
    DVT = rnorm(100)
  ),
  br_group(
    label = "TRT",
    CV = rnorm(100),
    DVT = rnorm(100)
  )
)

