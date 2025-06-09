#' Plot Data
#'
#' Produces a basic plot of data generated from simulate_mics or import_mics_with_metadata
#'
#' @param data Tibble or data frame from simulate_mics or import_mics_with_metadata
#' @param title Title of plot
#' @param y_min Minimum value for plot, some extra space will be added below so the lowest tested concentration is a reasonable value
#' @param y_max Maximum value for plot, some extra space will be added below so the highest tested concentration is a reasonable value
#'
#' @importFrom ggnewscale new_scale_color
#'
#' @return
#' @export
#'
#' @examples
preview_data = function(data, title = "", y_min = NULL, y_max = NULL, ECOFF = NULL){

  if(is.null(y_max)){
    y_max = case_when(max(data$right_bound) == Inf ~ max(data$left_bound))
  }

  if(is.null(y_min)){
    y_min = case_when(min(data$left_bound) == -Inf ~ min(data$right_bound))
  }

  plot = data %>%
    ggplot() +
    geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound), color = "black", data = (. %>% filter(left_bound != -Inf & right_bound != Inf)), alpha = 0.2) +
    geom_segment(aes(x = t, xend = t, y = right_bound, yend = left_bound), color = "black", data = (. %>% filter(left_bound == -Inf) %>% mutate(left_bound = low_con - 2)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
    geom_segment(aes(x = t, xend = t, y = left_bound, yend = right_bound), color = "black", data = (. %>% filter(right_bound == Inf) %>% mutate(right_bound = high_con + 2)), arrow = arrow(length = unit(0.03, "npc")), alpha = 0.2) +
    geom_point(aes(x = t, y = left_bound), color = "black", data = . %>% filter(left_bound != -Inf), alpha = 0.2) +
    geom_point(aes(x = t, y = right_bound), color = "black", data = . %>% filter(right_bound != Inf), alpha = 0.2) +
    ylim(y_min - 2, y_max + 2) %>% suppressWarnings() +
    ggtitle(title) + ylab(bquote(log[2]~ MIC))

  if(!is.null(ECOFF)){
    plot = plot +
      ggnewscale::new_scale_color() +
      geom_hline(aes(yintercept = ECOFF, color = "ECOFF")) +
      scale_color_manual(values = c("ECOFF" = "darkorange"), name = NULL)
  }

  plot %>% return()
}
