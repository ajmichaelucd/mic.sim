#' Title
#'
#' @param setup_number
#' @param files_folder
#' @param run_name
#' @param n_runs_per_setup
#' @param date
#'
#' @return
#' @export
#'
#' @examples
analysis_one_setup = function(setup_number, files_folder, run_name, n_runs_per_setup, date){
  if(files_folder != "" && str_sub(files_folder, - 1, -1) != "/"){
    files_folder = paste0(files_folder, "/")
  }

  file_names = paste0(files_folder, run_name, "_row_", rep(setup_number, each = n_runs_per_setup), "_", date,"_run_", ((n_runs_per_setup * (setup_number - 1)) + 1):(n_runs_per_setup * (setup_number)), ".Rdata")

  map(file_names, ~analysis_one_run(.x, setup_number))
  #map over the files
}

analysis_one_data_set = function(batch, setup_number, parameters){

print(batch$set_mic_seed)

  conv_summary = map(batch$set_output, get_like) %>% data.table::rbindlist(.) %>% tibble %>% arrange(desc(likelihood))
  #print(conv_summary)

  options = tibble(comp_conv = c("both", "comp 1", "comp 2", "neither"))

  convergence_table =
    conv_summary %>%
    summarize(.by = comp_conv, n = n()) %>%
    left_join(options, ., by = "comp_conv") %>%
    mutate(n  = case_when(
      is.na(n) ~ 0,
      TRUE ~ n),
      setup_number = setup_number,
      mic.seed = batch$set_mic_seed,
      sd_initial = parameters$sd_initial
    )


  sigma_summary = map(batch$set_output, ~get_sigmas(.x, sd_init = parameters$sd_initial)) %>% data.table::rbindlist(.) %>% tibble %>% arrange(desc(likelihood))
  #print(sigma_summary)

  sigma_summary_table =
    sigma_summary %>%
    mutate(setup_number = setup_number,
           mic.seed = batch$set_mic_seed)

  if(nrow(sigma_summary_table %>% filter(!is.na(likelihood))) > 2){
    models_to_plot = rbind(
      sigma_summary_table %>% filter(!is.na(likelihood)) %>% head(1) %>% mutate(rank = "high"),
      sigma_summary_table %>% head((sigma_summary_table %>% filter(!is.na(likelihood)) %>% nrow)/2 %>% ceiling) %>% tail(1) %>% mutate(rank = "med"),
      sigma_summary_table %>% filter(!is.na(likelihood)) %>% tail(1) %>% mutate(rank = "low")
    )
  }else if(nrow(sigma_summary_table %>% filter(!is.na(likelihood))) == 2){
    models_to_plot = sigma_summary_table %>% filter(!is.na(likelihood)) %>% mutate(rank = c("high", "low"))
  }else if(nrow(sigma_summary_table %>% filter(!is.na(likelihood))) == 1){
    models_to_plot = sigma_summary_table %>% filter(!is.na(likelihood)) %>% mutate(rank = c("med"))
  }else{
    models_to_plot = sigma_summary_table %>% filter(!is.na(likelihood)) %>% mutate(rank = NA)
  }

  addinf = function(it, df, setup_number){
    m = df[[it]]
    c(m, setup = setup_number) %>% return()
  }
  models = map(models_to_plot$iter, ~addinf(.x, batch$set_output, setup_number = setup_number))


  return(list(convergence_table = convergence_table,
         sigma_summary_table = sigma_summary_table,
         models_to_plot = models_to_plot,
         models = models))
}
analysis_one_run = function(file, setup_number){
  #file = file_names[1]

  #outer function starts here: inputs are file names (we map over this), setup_number
  results = file %>% loadRData()
  param = results$batch_parameters

  n_data_sets = results$batch_output %>% length
  if(n_data_sets == 1){
    analysis_one_data_set(batch = results$batch_output, setup_number, parameters = results$batch_parameters) %>% return()
  }else{
    map(results$batch_output, ~analysis_one_data_set(batch = .x, setup_number, parameters = results$batch_parameters)) %>% return()
  }

}

get_like = function(grid_output){
  grid_output$final_like %>% return()
}

get_sigmas = function(grid_output, sd_init = NULL){
  if(length(grid_output$output) == 1 && grid_output$output == "Error"){
    tibble(
    c1_scale_init = NaN,
    c2_scale_init = NaN,
    c1_scale_final = NaN,
    c2_scale_final = NaN,
    grid_output$final_like,
    sd_initial = ifelse(is.null(sd_init), NaN, sd_init)) %>% return()
  }else{

  tibble(c1_scale_init = grid_output$output$possible_data %>% filter(c == "1") %>% pull(sigma_initial) %>% unique,
         c2_scale_init = grid_output$output$possible_data %>% filter(c == "2") %>% pull(sigma_initial) %>% unique,
         c1_scale_final = grid_output$output$possible_data %>% filter(c == "1") %>% pull(`sd[Y|t,c]`) %>% unique,
         c2_scale_final = grid_output$output$possible_data %>% filter(c == "2") %>% pull(`sd[Y|t,c]`) %>% unique,
         grid_output$final_like,
         sd_initial = grid_output$output$sd_initial) %>% return()
    }
}
