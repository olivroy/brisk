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

out

summary(out, probs = c(.025, .5, .975))
summary(out, reference = "PBO")

plot(out)
# adjusted relative to PBO
plot(out, reference = "PBO")

plot_utility(out)
plot_utility(out, reference = "PBO")
plot_utility(out, stacked = TRUE)
