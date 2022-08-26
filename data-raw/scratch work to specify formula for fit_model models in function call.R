#formula = Surv(time = left_bound,
#               time2 = right_bound,
#               type = "interval2") ~ 0 + c + strata(c) + t:c)
#
df <- tibble(left_bound = c(4, 2, 7, 5, 6), right_bound = c(5, 3, 8, 6, 7), c = c(1, 2, 1, 2, 2), t = c(1, 2, 3, 4, 5), dogs = c(0, 0, 1, 1, 1))



aaa <- function(data = df, lb = "left_bound", rb = "right_bound", type = "interval2", c = c, covariates = c("t", "dogs")){
  variables  = case_when(is.null(covariates) ~ "1",
                         TRUE ~ covariates)
lb = data %>% pull(lb)
rb = data %>% pull(rb)

  outcome <- Surv(time = lb,
                  time2 = rb,
                  type = "interval2")
 f <- as.formula(
    paste("outcome",
          paste(variables, collapse = " + "),
          sep = " ~ "))
 survreg(f, data = data, dist = "gaussian")
}




aaa(df, covariates = c("t", "dogs"))


