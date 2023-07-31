#' Title
#'
#' @param settings
#'
#' @return
#' @export
#'
#' @examples
get_t_min_max <- function(settings){
  m <- functionBody(settings$t_dist) %>% as.character() %>% stringr::str_match(., "min =\\s*(.*?)\\s*, max =")

  t_min <- m[2,2] %>% as.numeric()


  g <- functionBody(settings$t_dist) %>% as.character() %>% stringr::str_split(., pattern = "max =", simplify = TRUE) %>% as.matrix() #%>% parse_number()

  t_max <- g[2,2] %>% parse_number()

  tibble(t_min, t_max) %>% return()
}

#get_t_min_max <- function(settings){
#  tibble(t_min = attr(settings$t_dist, "min"), t_max = attr(settings$t_dist, "max")) %>% return()
#}
