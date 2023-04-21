#' plot_mixed_normal_density
#'
#' @param mean
#' @param sigma
#' @param pi
#' @param min
#' @param max
#' @param low_con
#' @param high_con
#'
#' @return
#' @export
#'
#' @examples
plot_mixed_normal_density <- function(mean = c(-2, 1), sigma = c(1, 0.8), pi = c(0.4, 0.6), min = -5, max = 5, low_con = -3, high_con = 3){


  tibble(x = c(min, max)) %>%
    ggplot(aes(x = x)) +
    stat_function(fun = dnorm_half, args = list(mean = mean[1], sd = sigma[1], pi = pi[1]), color = "blue") +
    stat_function(fun = dnorm_half, args = list(mean = mean[2], sd = sigma[2], pi = pi[2]), color = "tomato3") +
    theme_classic() +
    geom_vline(xintercept = min:max, alpha = 0.1) +
    geom_vline(xintercept = low_con, color = "goldenrod") +
    geom_vline(xintercept = high_con, color = "goldenrod")

}

dnorm_half = function(x, mean, sd, pi){
  pi * dnorm(x, mean, sd)
}


