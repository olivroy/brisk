# we shouldn't have to define multiple things...

res <- br(
  br_group(
    label = "PBO",
    benefit("CV", function(x) x, weight = 0.5, samples = rnorm(100)),
    risk("DVT", function(x) 2 * x, weight = 0.5, samples = rnorm(100))
  ),
  br_group(
    label = "TRT",
    benefit("CV", function(x) x, weight = 0.5, samples = rnorm(100)),
    risk("DVT", function(x) 2 * x, weight = 0.5, samples = rnorm(100))
  )
)
