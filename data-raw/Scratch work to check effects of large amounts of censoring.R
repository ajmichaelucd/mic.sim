aba <- tibble(left_bound = c(rep(NA, 90), rep(16, 10), 4),
              right_bound = c(rep(0.5, 90), rep(Inf, 10), 8),
              weights = c(rep(1, 90), rep(0.01, 11)))
aba2 = aba[1:90,]
summary(survreg(Surv(left_bound, right_bound, type = "interval2") ~ 1, weights = weights,  data = aba, dist = "lognormal"))

1.046/log(2)

-79.61/log(2)

#check for censoring

test1 = with(
  aba,
  all(
    (left_bound %in% c(NA, -Inf)) |
      (right_bound %in% c(NA, Inf))
    ) )

if(test1)
{
  stop("all the data is left or right censored; we can't fit a model to this data")
}




