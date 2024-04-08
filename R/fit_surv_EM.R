#' Title
#'
#' @param random_seeds_vector
#' @param visible_data
#' @param mu_formula
#' @param pi_formula
#' @param max_it
#' @param ncomp
#' @param tol_ll
#' @param pi_link
#' @param verbose
#' @param initial_weighting
#' @param model_coefficient_tolerance
#' @param maxiter_survreg
#' @param sd_initial
#' @param randomize
#'
#' @return
#' @export
#'
#' @examples
fit_surv_EM =
  function(random_seeds_vector,
           visible_data,
           n_models = length(random_seeds_vector),
           mu_formula = Surv(time = left_bound,
                             time2 = right_bound,
                             type = "interval2") ~ pspline(t, df = 0, caic = TRUE),
           pi_formula = c == "2" ~ s(t),
           max_it = 500,
           ncomp = 2,
           tol_ll = 1e-6,
           pi_link = "logit",
           verbose = 1,
           initial_weighting = 7,
           model_coefficient_tolerance = 0.00001,
           maxiter_survreg = 30,
           sd_initial = 0.2,
           randomize = "all") {

    if(initial_weighting != 7){
      message("initial weighting ", initial_weighting, " does not contain a random start, so only 1 model will be run. To use the random start option, use initial_weighting value of 7")
      EM_algorithm(
        visible_data = visible_data,
        model = "surv",
        mu_formula = mu_formula,
        pi_formula = pi_formula,
        max_it,
        ncomp,
        tol_ll,
        browse_at_end = FALSE,
        browse_each_step = FALSE,
        plot_visuals = FALSE,
        prior_step_plot = FALSE,
        pause_on_likelihood_drop = FALSE,
        pi_link = pi_link,
        verbose = verbose,
        model_coefficient_tolerance = model_coefficient_tolerance,
        maxiter_survreg = maxiter_survreg,
        initial_weighting = initial_weighting,
        sd_initial = sd_initial
      ) %>% return()
    }

    set_random_seeds_vector(random_seeds_vector, n_models)


    grid_output = purrr::map2(
      random_seeds_vector,
      1:length(random_seeds_vector),
      ~EM_fm_surv_single_run(
        .x,
        .y,
        visible_data,
        mu_formula,
        pi_formula,
        max_it,
        ncomp,
        tol_ll,
        pi_link,
        verbose,
        initial_weighting,
        model_coefficient_tolerance,
        maxiter_survreg,
        sd_initial,
        randomize,
        n_models
      ),
      progress = TRUE
    )

    report = sort_solutions(grid_output)

    list(
      top_output = select_top_surv_likelihood(grid_output, report),
      sort_solutions(grid_output),
      grid_output = grid_output,
      surv_run_convergence_report = surv_run_convergence_report(report)
    ) %>% return()

  }

select_top_surv_likelihood = function(grid_output, report){
  grid_output[[report%>% head(1) %>% pull(iter)]] %>% return()
}


set_random_seeds_vector = function(random_seeds_vector, n_models){
  if(is.null(random_seeds_vector)){
    sample(1:10000000, n_models) %>% return()
  }else{
    random_seeds_vector %>% return()
  }
}

get_like = function(grid_output){
  grid_output$final_like %>% return()
}
sort_solutions = function(grid_output){
  map(grid_output, get_like) %>% data.table::rbindlist(.) %>% tibble %>% arrange(desc(loglikelihood)) %>% return()
}

surv_run_convergence_report = function(surv_run_summary){
  surv_run_summary %>% summarize(.by = comp_conv, n = n()) %>% return()
}
