



##Parameters--------------
iterations = 1000
covariate_effect_vector = c(0)
covariate_list = NULL
covariate_names = NULL
n = 100
t_dist = function(n){runif(n, min = 0, max = 1)}
pi = function(t) {z <- 1 + 0* t
c("1" = z)}
`E[X|T,C]` = function(t, c)
{
  case_when(
    c == "1" ~ 0 + 0.05 * t,
    TRUE ~ NaN
  )
}
sd_vector =  c("1" = 1)
low_con = 2^-4
high_con = 2^4
type_list = c("loglogistic", "lognormal", "weibull", "gaussian", "exponential", "logistic")
time = "t"
MIC_breakpoint = 1
stderr = FALSE
name = "output_0.05_1comp_01"


##TEST before running full batch-----------
single_aft_run_new_data(
  covariate_effect_vector,
  covariate_list,
  covariate_names,
  n,
  t_dist,
  pi,
  `E[X|T,C]`,
  sd_vector,
  low_con,
  high_con,
  type_list,
  time = "t",
  MIC_breakpoint,
  iteration = "test",
  stderr
)

##FULL BATCH RUN------------
#batch_aft_runs_new_data(
#  iterations,
#  covariate_effect_vector,
#  covariate_list,
#  covariate_names,
#  n,
#  t_dist,
#  pi,
#  `E[X|T,C]`,
#  sd_vector,
#  low_con,
#  high_con,
#  type_list,
#  time = "t",
#  MIC_breakpoint,
#  stderr
#) %>%
#  dplyr::group_by(coef_name) %>%
#  dplyr::summarise(
#    loglogistic = mean(loglogistic / log(2)),
#    lognormal = mean(lognormal / log(2)),
#    logistic = mean(logistic),
#    gaussian = mean(gaussian),
#    weibull = mean(weibull),
#    exponential = mean(exponential)
#  )
##need to transform loglogistic and lognornal with division by log(2), some other transformation needed for weibull and exponential (not sure what at the moment)
    ##works for std error too!

batch_aft_runs_new_data(
  iterations,
  covariate_effect_vector,
  covariate_list,
  covariate_names,
  n,
  t_dist,
  pi,
  `E[X|T,C]`,
  sd_vector,
  low_con,
  high_con,
  type_list,
  time = "t",
  MIC_breakpoint,
  stderr = TRUE
) -> temp_df

assign(paste(name), temp_df)
#output_null_1comp_01
#output_10_1comp_01
#output_0.05_1comp_01







