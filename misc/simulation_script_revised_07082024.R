Sys.setlocale (locale = "en_US.UTF-8")
print(sort(c("10", "1:")))

args <- as.numeric(commandArgs(trailingOnly = TRUE))

###EDIT PARAMETERS HERE-------------------------------
scenario = 1
number_per_batch = 10
total_runs_per_parameter_set = 500

n = 300
pi = c(function(t) {
  z <- 0.17 + 0.025 * t - 0.00045 * t^2
  #z <- (1+ exp(-m))^-1 #if exp(m) gets large, it won't add the 1 so we write like this
  tibble("1" = 1 - z, "2" = z)
})
`E[X|T,C]` = c(function(t, c)
{
  case_when(
    c == "1" ~ -4.0 + (0.24 * t) - (0.0055 * t^2),
    c == "2" ~ 3 + 0.001 * t,
    TRUE ~ NaN
  )
})
low_con = -3
high_con = 6
sd_vector = list(c("1" = 1, "2" = 1.05))
model = "pspline"
approach = "full"
pi_formula = c(c == "2" ~ s(t))

##BELOW THIS LINE WILL BE SAME FOR ALL RUNS IN THIS SCENARIO------------

ncomp = 2
t_dist = function(n){runif(n, min = 0, max = 15)}
attr(t_dist, "min") = 0
attr(t_dist, "max") = 15
scale = "log"
pre_set_degrees = NULL
max_degree = 8
degree_sets = "matched"
nfolds = 10
non_linear_term = "t"
covariates = NULL
fixed_side = NULL
extra_row = FALSE
max_it = 3000
ncomp = 2
tol_ll = 1e-06
pi_link = "logit"
verbose = 3
model_coefficient_tolerance = 1e-05
maxiter_survreg = 30
initial_weighting = 3
sd_initial = 0.2
scale = NULL
reruns_allowed = 3


###EDIT ABOVE THIS LINE ONLY--------------------------------------------






row = ceiling(args / (total_runs_per_parameter_set/number_per_batch))
param_table = tidyr::expand_grid(n = n, pi_vals = pi, mu_vals = `E[X|T,C]`, lc = low_con, hc = high_con, sd_vals = sd_vector, model = model, approach = approach, pi_formula = pi_formula)
params = param_table %>%
  mutate(id = row_number()) %>%
  filter(id == row)

n = params$n
pi = params$pi_vals[[1]]
`E[X|T,C]` = params$mu_vals[[1]]
low_con = params$lc
high_con = params$hc
sd_vector = params$sd_vals[[1]]
model = params$model
approach = params$approach
pi_formula = params$pi_formula[[1]]


fit_EM_safe = safely(fit_EM)
simulation_run = function(i, ...){
  set.seed(i)
  simulated_data =
    simulate_mics(
      n = n,
      t_dist = t_dist,
      pi = pi,
      `E[X|T,C]` = `E[X|T,C]`,
      sd_vector = sd_vector,
      covariate_list = covariate_list,
      covariate_effect_vector = covariate_effect_vector,
      conc_limits_table = conc_limits_table,
      low_con = low_con,
      high_con = high_con,
      scale = scale
    )
  iteration_output = fit_EM_safe(
    model = model,
    approach = approach,
    pre_set_degrees = pre_set_degrees,
    max_degree = max_degree,
    degree_sets = degree_sets,
    visible_data = simulated_data,
    nfolds = nfolds,
    non_linear_term = non_linear_term,
    covariates = covariates,
    pi_formula = pi_formula,
    fixed_side = fixed_side,
    extra_row = extra_row,
    max_it = max_it,
    ncomp = ncomp,
    tol_ll = tol_ll,
    pi_link = pi_link,
    verbose = verbose,
    model_coefficient_tolerance = model_coefficient_tolerance,
    maxiter_survreg = maxiter_survreg,
    initial_weighting = initial_weighting,
    sd_initial = sd_initial,
    scale = scale,
    reruns_allowed = reruns_allowed
  )

  iteration_output$iteration_number = i
  return(iteration_output)
}




iteration_numbers = c((((args - 1) * number_per_batch) + 1): (args * number_per_batch))

run_name = paste0("scenario_", scenario,"_row_", ceiling(args/(total_runs_per_parameter_set/number_per_batch)), "_12212023_run")
file_name <- paste0(run_name,"_", args)
path <- paste0(file_name, ".Rdata")


output = map(iteration_numbers, ~simulation_run(i = .x,
                                                n = n,
                                                t_dist = t_dist,
                                                pi = pi,
                                                `E[X|T,C]` = `E[X|T,C]`,
                                                sd_vector = sd_vector,
                                                covariate_list = covariate_list,
                                                covariate_effect_vector = covariate_effect_vector,
                                                conc_limits_table = conc_limits_table,
                                                low_con = low_con,
                                                high_con = high_con,
                                                scale = scale,
                                                model = model,
                                                approach = approach,
                                                pre_set_degrees = pre_set_degrees,
                                                max_degree = max_degree,
                                                degree_sets = degree_sets,
                                                visible_data = simulated_data,
                                                nfolds = nfolds,
                                                non_linear_term = non_linear_term,
                                                covariates = covariates,
                                                pi_formula = pi_formula,
                                                fixed_side = fixed_side,
                                                extra_row = extra_row,
                                                max_it = max_it,
                                                ncomp = ncomp,
                                                tol_ll = tol_ll,
                                                pi_link = pi_link,
                                                verbose = verbose,
                                                model_coefficient_tolerance = model_coefficient_tolerance,
                                                maxiter_survreg = maxiter_survreg,
                                                initial_weighting = initial_weighting,
                                                sd_initial = sd_initial,
                                                scale = scale,
                                                reruns_allowed = reruns_allowed))

batch_result = list(
  output = output,
  settings = list(
    n = n,
    t_dist = t_dist,
    pi = pi,
    `E[X|T,C]` = `E[X|T,C]`,
    sd_vector = sd_vector,
    covariate_list = covariate_list,
    covariate_effect_vector = covariate_effect_vector,
    conc_limits_table = conc_limits_table,
    low_con = low_con,
    high_con = high_con,
    scale = scale,
    model = model,
    approach = approach,
    pre_set_degrees = pre_set_degrees,
    max_degree = max_degree,
    degree_sets = degree_sets,
    visible_data = simulated_data,
    nfolds = nfolds,
    non_linear_term = non_linear_term,
    covariates = covariates,
    pi_formula = pi_formula,
    fixed_side = fixed_side,
    extra_row = extra_row,
    max_it = max_it,
    ncomp = ncomp,
    tol_ll = tol_ll,
    pi_link = pi_link,
    verbose = verbose,
    model_coefficient_tolerance = model_coefficient_tolerance,
    maxiter_survreg = maxiter_survreg,
    initial_weighting = initial_weighting,
    sd_initial = sd_initial,
    scale = scale,
    reruns_allowed = reruns_allowed
),
batch = args,
row = row,
param_table = param_table)

save(batch_result, file = path)







