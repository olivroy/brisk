w <- sim_weights(10, a = c(1, 1), b = c(.4, .6), c = c(.2, .3))
# ratio of b to a is between c(.4, .6) / c(1, 1)
summary(w$b / w$a)
# ratio of c to a is between c(.2, .3) / c(1, 1)
summary(w$c / w$a)

