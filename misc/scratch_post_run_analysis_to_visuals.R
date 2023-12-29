

set = loadRData("~/Desktop/Dissertation Project/Chapter 1/simulation_scripts/setup_2_analysis_set_1.Rdata")
#set ## list of 50, 1 for each run with this setup, each run has 2 mic seeds, and each mic seed has 100 models (originally)
#set[[1]] ##list of 2, 1 for each mic seed?
#set[[1]][[1]]$convergence_table
#set[[1]][[1]]$sigma_summary_table
#set[[1]][[1]]$models_to_plot
#set[[1]][[1]]$models #list of length 1 to 3, contains the models listed in 'models to plot'


plot_high = ggplot() + xlim(0, 16)
plot_med = ggplot() + xlim(0, 16)
plot_low = ggplot() + xlim(0, 16)

#loop over all 50, within that loop over 2
#ex_list = set[[1]][[1]]
high_df = tibble(comp_1 = c(NA), comp_2 = c(NA), mu_2 = c(NA), mu_1 = c(NA), run = c(NA))

high_comp_1 = list(NULL)
high_comp_1_i = list(NULL)
high_comp_2 = list(NULL)
high_comp_2_i = list(NULL)






function(t, i, j, item, set){
  current_set = set[[i]][[j]]

  set_num =  (2 * i) - (2 - j)

  if(length(current_set$models) == 3){
    high = current_set$models[[1]]
    med = current_set$models[[2]]
    low = current_set$models[[3]]
  }else if(length(current_set$models) == 2){
    high = current_set$models[[1]]
    med = NULL
    low = current_set$models[[2]]
  }else if(length(current_set$models) == 1){
    high = current_set$models[[1]]
    med = NULL
    low = NULL
  }else{
    errorCondition("Should only grab 3 example models, not more")
  }



}
















for(i in 1:length(set)){
  current_run = set[[i]]
  for(j in 1:length(current_run)){

    current_set = current_run[[j]]

   set_num =  (2 * i) - (2 - j)

    if(length(current_set$models) == 3){
      high = current_set$models[[1]]
      med = current_set$models[[2]]
      low = current_set$models[[3]]
    }else if(length(current_set$models) == 2){
      high = current_set$models[[1]]
      med = NULL
      low = current_set$models[[2]]
    }else if(length(current_set$models) == 1){
      high = current_set$models[[1]]
      med = NULL
      low = NULL
    }else{
      errorCondition("Should only grab 3 example models, not more")
    }

   if(!is.null(high)) {
     if (!is.na(high$output$mu_model[[1]]$scale) &
         !is.na(predict(high$output$mu_model[[1]], newdata = data.frame(t = 2)))) {
    high_comp_1 = append(high_comp_1,
      function(t) {
             predict(high$output$mu_model[[1]], newdata = data.frame(t = t))
           }
    )
    high_comp_1_i = append(high_comp_1_i, set_num)
     }
     if (!is.na(high$output$mu_model[[2]]$scale) &
         !is.na(predict(high$output$mu_model[[2]], newdata = data.frame(t = 2)))) {
       high_comp_2 = append(high_comp_2,
      function(t) {
             predict(high$output$mu_model[[2]], newdata = data.frame(t = t))
           }
       )
       high_comp_2_i = append(high_comp_2_i, set_num)
     }
   }

  }
}












if(!is.null(high)){
  if (!is.na(high$output$mu_model[[1]]$scale) &
      !is.na(predict(high$output$mu_model[[1]], newdata = data.frame(t = 2)))) {
    high_df = tibble( comp_1 = c(high_df$comp_1, function(t) {
      + predict(high$output$mu_model[[1]], newdata = data.frame(t = 2))}))
  }
}








    if(!is.null(high)) {
      if (!is.na(high$output$mu_model[[1]]$scale) &
          !is.na(predict(high$output$mu_model[[1]], newdata = data.frame(t = 2)))) {
        plot_high = plot_high +
          geom_function(
            fun = function(t) {
              predict(high$output$mu_model[[1]], newdata = data.frame(t = 2))
            }, aes(color = paste0("set_", set_num))
          )
      }
      if (!is.na(high$output$mu_model[[2]]$scale) &
          !is.na(predict(high$output$mu_model[[2]], newdata = data.frame(t = 2)))) {
        plot_high = plot_high +
          geom_function(
            fun = function(t) {
              predict(high$output$mu_model[[2]], newdata = data.frame(t = 2))
            }, aes(color = paste0("set_", set_num))
          )
      }
    }





    if(!is.null(med)) {
      if (!is.na(med$output$mu_model[[1]]$scale) &
          !is.na(predict(med$output$mu_model[[1]], newdata = data.frame(t = 2)))) {
        plot_med = plot_med +
          geom_function(
            fun = function(t) {
              predict(med$output$mu_model[[1]], newdata = data.frame(t = 2))
            }, aes(color = paste0("set_", set_num))
          )
      }
      if (!is.na(med$output$mu_model[[2]]$scale) &
          !is.na(predict(med$output$mu_model[[2]], newdata = data.frame(t = 2)))) {
        plot_med = plot_med +
          geom_function(
            fun = function(t) {
              predict(med$output$mu_model[[2]], newdata = data.frame(t = 2))
            }, aes(color = paste0("set_", set_num))
          )
      }
    }

    if(!is.null(low)) {
      if (!is.na(low$output$mu_model[[1]]$scale) &
          !is.na(predict(low$output$mu_model[[1]], newdata = data.frame(t = 2)))) {
        plot_low = plot_low +
          geom_function(
            fun = function(t) {
              predict(low$output$mu_model[[1]], newdata = data.frame(t = 2))
            }, aes(color = paste0("set_", set_num))
          )
      }
      if (!is.na(low$output$mu_model[[2]]$scale) &
          !is.na(predict(low$output$mu_model[[2]], newdata = data.frame(t = 2)))) {
        plot_low = plot_low +
          geom_function(
            fun = function(t) {
              predict(low$output$mu_model[[2]], newdata = data.frame(t = 2))
            }, aes(color = paste0("set_", set_num))
          )
      }
    }


  }
}
plot_high

