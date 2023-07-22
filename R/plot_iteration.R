#' Title
#'
#' @param iteration
#' @param number_per_batch
#' @param analysis
#' @param location
#' @param format
#' @param array_name
#' @param date
#'
#' @return
#' @export
#'
#' @import patchwork
#'
#' @examples
plot_iteration <-
  function(iteration,
           number_per_batch,
           analysis = NULL,
           location,
           format,
           array_name,
           date) {
    file  <-
      gen_path_sim(
        location = location,
        format = format,
        array_name = array_name,
        date = date,
        i = ceiling(iteration / number_per_batch)
      )
    batch_results <- loadRData(file)
    iteration - number_per_batch * (ceiling(iteration / number_per_batch) - 1) -> i
    batch_results$settings -> settings
    batch_results$model_results[[i]] -> results

    if (is.null(analysis)) {
      analysis <- case_when(
        length(results$single_model_output$single_model_output_fms) > 1 ~ "fms",
        length(results$single_model_output$single_model_output_fms) == 1 &
          length(results$single_model_output$single_model_output_fm) > 1 ~ "fm",
        TRUE ~ "nothing"
      )
    }

    if (analysis == "fm") {
      single_model_output <-
        results$single_model_output$single_model_output_fm
    } else if (analysis == "fms") {
      single_model_output <-
        results$single_model_output$single_model_output_fms
    } else{
      single_model_output <- "nothing"
    }
    if (analysis %in% c("fm", "fms")) {
      pi_plot <- plot_pi(settings, single_model_output)


      if (analysis == "fm") {
        mu_plot <- plot_mu_fm(results, settings)
      } else{
        mu_plot <- plot_mu_fms(results, settings)
      }

      print(mu_plot / pi_plot)

    } else{
      print(paste0(
        "no results to show, no model convergence achieved for iteration ",
        iteration
      ))
    }

  }



mu.se <-
  function(t, c, z, results) {
    predict(results$single_model_output$single_model_output_fm$newmodel[[c]],
            data.frame(t = t)) + z * predict(
              results$single_model_output$single_model_output_fm$newmodel[[c]],
              data.frame(t = t),
              se = TRUE
            )$se.fit
  }
mu.se.fms <-
  function(t, z, results) {
    predict(results$single_model_output$single_model_output_fms$newmodel,
            data.frame(t = t)) + z * predict(
              results$single_model_output$single_model_output_fms$newmodel,
              data.frame(t = t),
              se = TRUE
            )$se.fit
  }
plot_pi <- function(settings, single_model_output) {
  single_model_output$possible_data %>% filter(comp == c) %>%
    mutate(pi_hat = predict(single_model_output$binom_model, data.frame(t = t), type = "response")) %>%
    rowwise %>%
    mutate(pi_dgm = settings$pi(t) %>% pull(2)) %>%
    ungroup %>%
    mutate(false_resid = pi_dgm - pi_hat,
           resid = (c == "2") * 1 - pi_hat) %>%
    ggplot() +
    #  geom_point(aes(x = t, y = pi_hat, color = "Fitted Model")) +
    geom_function(
      fun = function(t) {
        predict(single_model_output$binom_model, data.frame(t), type = "response")
      },
      aes(color = "Fitted Model")
    ) +
    #   geom_point(aes(x = t, y = pi_dgm, color = "Data Generating Mechanism")) +
    ggplot2::geom_function(
      fun = function(t) {
        settings$pi(t) %>% pull(2)
      },
      aes(color = "Data Generating Mechanism")
    ) +
    geom_smooth(aes(x = t, y = (c == "2") * 1), formula = y ~ x, method = "loess")
}
plot_mu_fms <-
  function(results, settings) {
    ##change later once i update fms to save length 2 list
    results$single_model_output$single_model_output_fms$possible_data %>% #filter(c == "2") %>%
      mutate(mu_dgm = settings$`E[X|T,C]`(t = t, c = c)) %>%
      mutate(
        mu_hat = case_when(
          c == "1" ~ predict(
            results$single_model_output$single_model_output$single_model_output_fms$newmodel,
            data.frame(t = t)
          ),
          c == "2" ~ NaN,
          TRUE ~ NaN
        ),
        mu_hat_se = case_when(
          c == "1" ~ predict(
            results$single_model_output$single_model_output$single_model_output_fms$newmodel,
            data.frame(t = t),
            se = TRUE
          )$se.fit,
          c == "2" ~ NaN,
          TRUE ~ NaN
        )
      ) %>%
      mutate(resid = observed_value - mu_hat,
             false_resid = mu_dgm - mu_hat) %>%
      mutate(
        predicted_comp = case_when(
          c == "2" & `P(C=c|y,t)` > 0.5 ~ "2",
          c == "2" & `P(C=c|y,t)` < 0.5 ~ "1",
          c == "1" & `P(C=c|y,t)` > 0.5 ~ "1",
          c == "1" & `P(C=c|y,t)` < 0.5 ~ "2",
          TRUE ~ "both"
        ) #rename observed data to underlying values
      ) %>% ###add a variable for misclassified observations maybe?
      filter(c == "2") %>%
      ggplot() +
      #geom_point(aes(x = t, y = mu_dgm, color = c)) + #make as line: geom_function
      geom_function(
        fun = function(t) {
          settings$`E[X|T,C]`(t, c = 1)
        },
        aes(color = "Component 1 Mu", linetype = "Data Generating Mechanism"),
        size = 0.9
      ) +
      geom_function(
        fun = function(t) {
          settings$`E[X|T,C]`(t, c = 2)
        },
        aes(color = "Component 2 Mu", linetype = "Data Generating Mechanism"),
        size = 0.9
      ) +
      geom_function(
        fun = function(t) {
          predict(
            results$single_model_output$single_model_output_fms$newmodel,
            data.frame(t = t)
          )
        },
        aes(color = "Component 1 Mu", linetype = "Fitted Model"),
        size = 0.9
      ) +
      #geom_function(fun = function(t){predict(results$single_model_output$newmodel[[2]], data.frame(t = t))}, aes(color = "Component 2 Mu", linetype = "Fitted Model"), size = 0.9) +
      # geom_point(aes(x = t, y = mu_hat, color = c), fill = "black", shape = 21) + #geom_function
      geom_smooth(
        aes(x = t, y = observed_value),
        size = 0.3,
        alpha = 0.8,
        data = . %>% filter(comp == "1"),
        se = FALSE,
        span = 0.5,
        color = "darkgreen",
        formula = y ~ x,
        method = "loess"
      ) +  ###fix later
      geom_smooth(
        aes(x = t, y = observed_value),
        size = 0.3,
        alpha = 0.8,
        data = . %>% filter(comp == "2"),
        se = FALSE,
        span = 0.5,
        color = "darkgreen",
        formula = y ~ x,
        method = "loess"
      ) + ###fix later
      geom_hline(yintercept = settings$low_con %>% unlist) +
      geom_hline(yintercept = settings$high_con %>% unlist) +
      geom_function(
        fun = function(t) {
          mu.se.fms(t, z = 1.96, results = results)
        },
        aes(color = "Component 1 Mu", linetype = "Fitted Model SE"),
        size = 0.6,
        alpha = 0.6
      ) +
      geom_function(
        fun = function(t) {
          mu.se.fms(t, z = -1.96, results = results)
        },
        aes(color = "Component 1 Mu", linetype = "Fitted Model SE"),
        size = 0.6,
        alpha = 0.6
      ) +
      #geom_function(fun = function(t){mu.se(t, c = 2, z = 1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
      #geom_function(fun = function(t){mu.se(t, c = 2, z = -1.96)}, aes(color = "Component 2 Mu", linetype = "Fitted Model SE"), size = 0.6, alpha = 0.6) +
      ggnewscale::new_scale_color() +
      geom_point(aes(
        x = t,
        y = observed_value,
        color = `P(C=c|y,t)`,
        shape = comp
      ),
      alpha = 0.3) +
      ggplot2::scale_colour_gradientn(colours = c("purple", "orange"))
  }
plot_mu_fm <- function(results, settings) {
  results$single_model_output$single_model_output_fm$possible_data %>%
    mutate(mu_dgm = settings$`E[X|T,C]`(t = t, c = c)) %>%
    mutate(
      mu_hat = case_when(
        c == "1" ~ predict(
          results$single_model_output$single_model_output_fm$newmodel[[1]],
          data.frame(t = t)
        ),
        c == "2" ~ predict(
          results$single_model_output$single_model_output_fm$newmodel[[2]],
          data.frame(t = t)
        ),
        TRUE ~ NaN
      ),
      mu_hat_se = case_when(
        c == "1" ~ predict(
          results$single_model_output$single_model_output_fm$newmodel[[1]],
          data.frame(t = t),
          se = TRUE
        )$se.fit,
        c == "2" ~ predict(
          results$single_model_output$single_model_output_fm$newmodel[[2]],
          data.frame(t = t),
          se = TRUE
        )$se.fit,
        TRUE ~ NaN
      )
    ) %>%
    mutate(resid = observed_value - mu_hat,
           false_resid = mu_dgm - mu_hat) %>%
    mutate(
      predicted_comp = case_when(
        c == "2" & `P(C=c|y,t)` > 0.5 ~ "2",
        c == "2" & `P(C=c|y,t)` < 0.5 ~ "1",
        c == "1" & `P(C=c|y,t)` > 0.5 ~ "1",
        c == "1" & `P(C=c|y,t)` < 0.5 ~ "2",
        TRUE ~ "both"
      ) #rename observed data to underlying values
    ) %>%
    filter(c == "2") %>%
    ggplot() +
    #geom_point(aes(x = t, y = mu_dgm, color = c)) + #make as line: geom_function
    geom_function(
      fun = function(t) {
        settings$`E[X|T,C]`(t, c = 1)
      },
      aes(color = "Component 1 Mu", linetype = "Data Generating Mechanism"),
      size = 0.9
    ) +
    geom_function(
      fun = function(t) {
        settings$`E[X|T,C]`(t, c = 2)
      },
      aes(color = "Component 2 Mu", linetype = "Data Generating Mechanism"),
      size = 0.9
    ) +
    geom_function(
      fun = function(t) {
        predict(
          results$single_model_output$single_model_output_fm$newmodel[[1]],
          data.frame(t = t)
        )
      },
      aes(color = "Component 1 Mu", linetype = "Fitted Model"),
      size = 0.9
    ) +
    geom_function(
      fun = function(t) {
        predict(
          results$single_model_output$single_model_output_fm$newmodel[[2]],
          data.frame(t = t)
        )
      },
      aes(color = "Component 2 Mu", linetype = "Fitted Model"),
      size = 0.9
    ) +
    # geom_point(aes(x = t, y = mu_hat, color = c), fill = "black", shape = 21) + #geom_function
    geom_smooth(
      aes(x = t, y = observed_value),
      size = 0.3,
      alpha = 0.8,
      data = . %>% filter(comp == "1"),
      se = FALSE,
      span = 0.5,
      color = "darkgreen",
      formula = y ~ x,
      method = "loess"
    ) +  ###fix later
    geom_smooth(
      aes(x = t, y = observed_value),
      size = 0.3,
      alpha = 0.8,
      data = . %>% filter(comp == "2"),
      se = FALSE,
      span = 0.5,
      color = "darkgreen",
      formula = y ~ x,
      method = "loess"
    ) + ###fix later
    geom_hline(yintercept = settings$low_con %>% unlist) +
    geom_hline(yintercept = settings$high_con %>% unlist) +
    geom_function(
      fun = function(t) {
        mu.se(t,
              c = 1,
              z = 1.96,
              results = results)
      },
      aes(color = "Component 1 Mu", linetype = "Fitted Model SE"),
      size = 0.6,
      alpha = 0.6
    ) +
    geom_function(
      fun = function(t) {
        mu.se(t,
              c = 1,
              z = -1.96,
              results = results)
      },
      aes(color = "Component 1 Mu", linetype = "Fitted Model SE"),
      size = 0.6,
      alpha = 0.6
    ) +
    geom_function(
      fun = function(t) {
        mu.se(t,
              c = 2,
              z = 1.96,
              results = results)
      },
      aes(color = "Component 2 Mu", linetype = "Fitted Model SE"),
      size = 0.6,
      alpha = 0.6
    ) +
    geom_function(
      fun = function(t) {
        mu.se(t,
              c = 2,
              z = -1.96,
              results = results)
      },
      aes(color = "Component 2 Mu", linetype = "Fitted Model SE"),
      size = 0.6,
      alpha = 0.6
    ) +
    ggnewscale::new_scale_color() +
    geom_point(aes(
      x = t,
      y = observed_value,
      color = `P(C=c|y,t)`,
      shape = comp
    ),
    alpha = 0.3) +
    ggplot2::scale_colour_gradientn(colours = c("purple", "orange"))
}
