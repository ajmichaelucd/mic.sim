#' Title
#'
#' @param max_degree
#' @param ncomp
#' @param degree_sets
#' @param model
#' @param approach
#'
#' @return
#' @keywords internal
#'
#' @examples
create_degree_combinations_all = function(max_degree, ncomp, degree_sets = "matched", model, approach) {
  if(approach == "reduced"){
    ncomp_adjusted = ncomp - 1
  }else{
    ncomp_adjusted = ncomp
  }

  if (ncomp_adjusted > 1 & degree_sets == "independent") {
    deg_table = degree_combinations_independent(max_degree, model, ncomp_adjusted)
  } else{
    deg_table = map_dfc(1:ncomp_adjusted, ~degree_tib_all(.x, max_degree, model))
  }
  purrr::transpose(deg_table) %>% map(., unlist) %>% map(., unname) %>% return()
}

degree_tib_all = function(i, max_degree, model){
  if(model == "surv"){
    min_degree = 2
  }else{
    min_degree = 1
  }
  a = tibble(placeholder = min_degree:max_degree)
  colnames(a) <- paste0("comp", i, "_degree")
  return(a)
}

degree_combinations_independent =
  function(max_degree, model, ncomp) {
    table_intermediate = (degree_tib_all(1, max_degree, model))
    for (i in 2:ncomp) {
      table_intermediate %<>%
        reframe(.by = everything(),
                degree_tib_all(i, max_degree, model))
    }
    return(table_intermediate)
  }
