set.seed(1132)
ilogit <- function(x) 1 / (1 + exp(-x))
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

out

summary(out, probs = c(.025, .5, .975))
summary(out, reference = "PBO")

plot(out)
plot(out, reference = "PBO")

plot_utility(out)
plot_utility(out, reference = "PBO")
plot_utility(out, stacked = TRUE)
