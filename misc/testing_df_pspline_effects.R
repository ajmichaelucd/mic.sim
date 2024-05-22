
check1 = function(a,b){
m = df %>%
  survival::survreg(Surv(time = left_bound,
                         time2 = right_bound,
                         type = "interval2") ~ pspline(t, df = a, nterms = b), weights = weights, data = .,
                    dist = "gaussian")
return(
  list(
    a,
    b,
    m$df[2],
    m$scale,
    m$penalty[2]
    )
  )
}
checkcheck = safely(check1)

d = expand.grid(a = c(1:20), b = c(1:20)) %>% tibble()
out = map2(d$a, d$b, ~checkcheck(.x, .y))
compress = function(input){
  tibble(df = input$result[[1]], nterms = input$result[[2]], df_out = input$result[[3]], scale = input$result[[4]], penalty = input$result[[5]]) %>% return()
}

results = out %>% map(., ~compress(.x)) %>% rbindlist() %>% tibble()
results
surv_fit %>% summary()
mgcv_fit$family$getTheta()
