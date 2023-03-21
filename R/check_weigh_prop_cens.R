#' Title
#'
#' Get a weighted sum of the proportion of censored observations in each component from possible data
#'
#' @param df
#' @param comp
#' @param side
#' @param cutoff
#'
#' @return
#' @export
#'
#' @examples
check_weigh_prop_cens <- function(df = a, comp = "1", side = "rc", cutoff = 0.9){
  if(side == "rc"){
    stage1 <- df %>%
      filter(c == comp) %>%
      group_by(rc) %>% summarise(p = sum(`P(C=c|y,t)`)) %>%
      mutate(tot = sum(p)) %>%
      mutate(weighted_prop_right_censored = p / tot) %>% filter(rc)
      if(nrow(stage1) > 0){
        stage1 %>%
          mutate(decision = if_else(
            weighted_prop_right_censored >= cutoff,
            "Excessive Censoring",
            "All Clear"
          )) %>% select(decision)
      }else{tibble(decision = "All Clear")}
  } else if(side == "lc"){
    stage1 <- df %>%
      filter(c == comp) %>%
      group_by(lc) %>% summarise(p = sum(`P(C=c|y,t)`)) %>% mutate(tot = sum(p)) %>% mutate(weighted_prop_left_censored = p / tot) %>% filter(lc)
   if(nrow(stage1) > 0){
    stage1 %>%
      mutate(decision = if_else(
        weighted_prop_leftt_censored >= cutoff,
        "Excessive Censoring",
        "All Clear"
      )) %>% select(decision)
     }else{tibble(decision = "All Clear")}
  }else{
    errorCondition("Choose rc or lc")
  }
}
