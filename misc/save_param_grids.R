

`E[X|T,C]` = list(function(t, c){
  case_when(
    c == "1" ~ -3.0 + 0.2 * t,
    c == "2" ~ -1 + (15 * sqrt((t ^ 0.7) * 0.02)),
    TRUE ~ NaN
  )
}
)

sd_vals = list(
  0.1,
  0.2,
  0.3,
  0.4
)

pi_vals = list(
  function(t) {
    z <- 0 + (0.002 * t)
    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
    tibble("1" = 1 - z, "2" = z)
  },
  function(t) {
    z <- 0.1 + (0.002 * t)
    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
    tibble("1" = 1 - z, "2" = z)
  },
  function(t) {
    z <- 0.2 + (0.002 * t)
    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
    tibble("1" = 1 - z, "2" = z)
  }
)

sd_vector = list(c("1" = 1, "2" = 1))

param_grid_1 = tidyr::expand_grid(sd_vals = sd_vals, pi_vals = pi_vals, `E[X|T,C]` = `E[X|T,C]`, sd_vector = sd_vector)

`E[X|T,C]` = list(function(t, c) {
  case_when(c == "1" ~ -3.0 + 0.05 * t,
            c == "2" ~ 2.5,
            TRUE ~ NaN)
},
function(t, c) {
  case_when(c == "1" ~ -3.0 + 0.05 * t,
            c == "2" ~ 3,
            TRUE ~ NaN)
},
function(t, c) {
  case_when(c == "1" ~ -3.0 + 0.05 * t,
            c == "2" ~ 3.25,
            TRUE ~ NaN)
},
function(t, c) {
  case_when(c == "1" ~ -3.0 + 0.05 * t,
            c == "2" ~ 3.5,
            TRUE ~ NaN)
},
function(t, c) {
  case_when(c == "1" ~ -3.0 + 0.05 * t,
            c == "2" ~ 3.75,
            TRUE ~ NaN)
},
function(t, c) {
  case_when(c == "1" ~ -3.0 + 0.05 * t,
            c == "2" ~ 4,
            TRUE ~ NaN)
})

sd_vector = list(c("1" = 1, "2" = 1),
                 c("1" = 1, "2" = 0.75))

sd_vals = list(
  0.2
)

pi_vals = list(
  function(t) {
    z <- 0.3
    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
    tibble("1" = 1 - z, "2" = z)
  }
)

param_grid_2 = tidyr::expand_grid(sd_vals = sd_vals, pi_vals = pi_vals, `E[X|T,C]` = `E[X|T,C]`, sd_vector = sd_vector)


`E[X|T,C]` = list(function(t, c) {
  case_when(c == "1" ~ -1.0,
            c == "2" ~ 0.05 * (t - 8)^2 + 1,
            TRUE ~ NaN)
},
function(t, c) {
  case_when(c == "1" ~ 0,
            c == "2" ~ 0.05 * (t - 8)^2 + 2,
            TRUE ~ NaN)
})

sd_vector = list(c("1" = 1, "2" = 0.75),
                 c("1" = 1, "2" = 1),
                 c("1" = 1, "2" = 1.25))

sd_vals = list(
  0.2
)

pi_vals = list(
  function(t) {
    z <- 0.3
    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
    tibble("1" = 1 - z, "2" = z)
  },
  function(t) {
    z <- 0.5
    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
    tibble("1" = 1 - z, "2" = z)
  }
)

param_grid_3 = tidyr::expand_grid(sd_vals = sd_vals, pi_vals = pi_vals, `E[X|T,C]` = `E[X|T,C]`, sd_vector = sd_vector)


`E[X|T,C]` = list(
  function(t, c) {
    case_when(c == "1" ~ -0.2,
              c == "2" ~ 3.2,
              TRUE ~ NaN)
  })

sd_vector = list(
  c("1" = 1, "2" = 0.75),
  c("1" = 1, "2" = 0.875),
  c("1" = 1, "2" = 1),
  c("1" = 1, "2" = 1.125),
  c("1" = 1, "2" = 1.25),
  c("1" = 1, "2" = 1.375),
  c("1" = 1, "2" = 1.5)
)

sd_vals = list(
  0.2
)

pi_vals = list(
  function(t) {
    z <- 0.3
    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
    tibble("1" = 1 - z, "2" = z)
  },
  function(t) {
    z <- 0.5
    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
    tibble("1" = 1 - z, "2" = z)
  }
)

param_grid_4 = tidyr::expand_grid(sd_vals = sd_vals, pi_vals = pi_vals, `E[X|T,C]` = `E[X|T,C]`, sd_vector = sd_vector)


`E[X|T,C]` = list(
  function(t, c) {
    case_when(c == "1" ~ -0.2,
              c == "2" ~ 3.2,
              TRUE ~ NaN)
  })

sd_vector = list(
  c("1" = 1, "2" = 0.75),
  c("1" = 1, "2" = 1),
  c("1" = 1, "2" = 1.25),
  c("1" = 1, "2" = 1.5)
)

sd_vals = list(
  0.2, 0.3, 0.4
)

pi_vals = list(
  #  function(t) {
  #    z <- 0.3
  #    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
  #    tibble("1" = 1 - z, "2" = z)
  #  },
  function(t) {
    z <- 0.5
    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
    tibble("1" = 1 - z, "2" = z)
  }
)

param_grid_5 = tidyr::expand_grid(sd_vals = sd_vals, pi_vals = pi_vals, `E[X|T,C]` = `E[X|T,C]`, sd_vector = sd_vector)


`E[X|T,C]` = list(
  function(t, c) {
    case_when(c == "1" ~ -0.2,
              c == "2" ~ 3.2,
              TRUE ~ NaN)
  })

sd_vector = list(
  c("1" = 1, "2" = 0.75),
  c("1" = 1, "2" = 1),
  c("1" = 1, "2" = 1.25),
  c("1" = 1, "2" = 1.5)
)

sd_vals = list(
  0.2, 0.3, 0.4
)

pi_vals = list(
  function(t) {
    z <- 0.3
    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
    tibble("1" = 1 - z, "2" = z)
  }#,
  #  function(t) {
  #    z <- 0.5
  #    #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
  #    tibble("1" = 1 - z, "2" = z)
  #  }
)

param_grid_6 = tidyr::expand_grid(sd_vals = sd_vals, pi_vals = pi_vals, `E[X|T,C]` = `E[X|T,C]`, sd_vector = sd_vector)

grid_list =
list(
  param_grid_1 = param_grid_1,
  param_grid_2 = param_grid_2,
  param_grid_3 = param_grid_3,
  param_grid_4 = param_grid_4,
  param_grid_5 = param_grid_5,
  param_grid_6 = param_grid_6
)

save(grid_list,
  file = "~/Desktop/Dissertation Project/Chapter 1/simulation_scripts/param_grid_list.Rdata"
)






