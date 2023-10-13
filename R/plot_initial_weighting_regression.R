#' Title
#'
#' @param possible_data
#'
#' @return
#' @export
#'
#' @examples
plot_initial_weighting_regression = function(possible_data) {
  data.plot = possible_data %>% mutate(
    cens =
      case_when(left_bound == -Inf ~ "lc",
                right_bound == Inf ~ "rc",
                TRUE ~ "int"),
    mid =
      case_when(
        left_bound == -Inf ~ right_bound - 0.5,
        right_bound == Inf ~ left_bound + 0.5,
        TRUE ~ (left_bound + right_bound) / 2
      )
  )


  #mean <-
  data.plot %>% ggplot() +
    ylim(low_con - (2 * 0.5 * (high_con - low_con)) , high_con + (2 * 0.5 *
                                                                    (high_con - low_con))) +
    #geom_bar(aes(x = mid, fill = cens)) +
    # geom_point(aes(x = t, y = mid, color = `P(C=c|y,t)`), data = data.plot %>% filter(c == "2"), alpha = 0) +
    geom_ribbon(
      aes(
        ymin = low_con - 1.96 * 0.3 * (high_con - low_con),
        ymax = low_con + 1.96 * 0.3 * (high_con - low_con),
        x = t,
        fill = "Component 1 Mu"
      ),
      data = ci_data,
      alpha = 0.25
    ) +
    geom_ribbon(
      aes(
        ymin = high_con - 1.96 * 0.3 * (high_con - low_con),
        ymax = high_con + 1.96 * 0.3 * (high_con - low_con),
        x = t,
        fill = "Component 2 Mu"
      ),
      data = ci_data,
      alpha = 0.25
    ) +
    geom_hline(aes(yintercept = high_con, color = "Component 2 Mean")) +
    geom_hline(aes(yintercept = low_con, color = "Component 1 Mean")) +
    ggnewscale::new_scale_color() +
    # scale_color_gradient2(low = "purple", high = "darkorange", mid = "green", midpoint = 0.5) +
    scale_color_gradient2(
      low = "red",
      high = "blue",
      mid = "green",
      midpoint = 0.5
    ) +
    geom_segment(
      aes(
        x = t,
        xend = t,
        y = left_bound,
        yend = right_bound,
        color = `P(C=c|y,t)`
      ),
      data = (data.plot %>% filter(cens == "int" &
                                     c == "2")),
      alpha = 0.3
    ) +
    geom_segment(
      aes(
        x = t,
        xend = t,
        y = right_bound,
        yend = left_bound,
        color = `P(C=c|y,t)`
      ),
      data = (data.plot %>% filter(cens == "lc" &
                                     c == "2") %>% mutate(plot_min)),
      arrow = arrow(length = unit(0.03, "npc")),
      alpha = 0.3
    ) +
    geom_segment(
      aes(
        x = t,
        xend = t,
        y = left_bound,
        yend = right_bound,
        color = `P(C=c|y,t)`
      ),
      data = (data.plot %>% filter(cens == "rc" &
                                     c == "2") %>% mutate(plot_max)),
      arrow = arrow(length = unit(0.03, "npc")),
      alpha = 0.3
    ) +
    geom_point(
      aes(x = t, y = left_bound,  color = `P(C=c|y,t)`),
      data = data.plot %>% filter(left_bound != -Inf &
                                    c == "2"),
      alpha = 0.3
    ) +
    geom_point(
      aes(x = t, y = right_bound,  color = `P(C=c|y,t)`),
      data = data.plot %>% filter(right_bound != Inf &
                                    c == "2"),
      alpha = 0.3
    ) +
    #scale_colour_gradientn(colours = c("purple", "darkorange")) +
    #ylim(plot_min - 0.5, plot_max + 0.5) +
    ggtitle(paste0("Initial Condition")) +
    xlab("Time") +
    ylab("MIC") %>%
    suppressMessages()
}
