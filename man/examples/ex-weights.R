w <- sim_weights(1e4, a = c(1, 1), b = c(.4, .6), c = c(.2, .3))
# ratio of b to a is between c(.4, .6) / c(1, 1)
summary(w$b / w$a)
# ratio of c to a is between c(.2, .3) / c(1, 1)
summary(w$c / w$a)

# Weights can be used to add uncertainty to the benefit/risk analysis
set.seed(1132)
ilogit <- function(x) 1 / (1 + exp(-x))
out <- mcda(
  benefit("CV", function(x) ilogit(x), weight = w$a),
  risk("DVT", function(x) ilogit(- .5 * x), weight = w$b),
  risk("MI", function(x) ilogit(- .5 * x), weight = w$c),
  br_group(
    label = "PBO",
    CV = rnorm(1e4, .1),
    DVT = rnorm(1e4, .1),
    MI = rnorm(1e4, .1)
  ),
  br_group(
    label = "TRT",
    CV = rnorm(1e4, 2),
    DVT = rnorm(1e4, 1),
    MI = rnorm(1e4, 0.5)
  )
)
