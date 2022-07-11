br(
  benefit("CV", function(x) x, weight = 1),
  risk("DVT", function(x) 2 * x, weight = 1),

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
