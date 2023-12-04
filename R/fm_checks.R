#' Title
#'
#' @param single_model_output_fm
#'
#' @return
#' @export
#'
#' @examples
fm_checks = function(single_model_output_fm){
  if (length(single_model_output_fm) == 1) {
    fm_convergence = FALSE
    print("did not converge")
  }else{
    fm_convergence = case_when(single_model_output_fm$converge %in% c("YES", "iterations") & ncomp > 1 ~ TRUE,
                               single_model_output_fm$ncomp == 1 ~ TRUE,
                               TRUE ~ FALSE)
    print("model converged, starting model checks")
    #model converged, so let's check the fit
  }


  if(fm_convergence & single_model_output_fm$ncomp > 1){
    max_scale <-
      max(single_model_output_fm$newmodel[[1]]$scale,
          single_model_output_fm$newmodel[[2]]$scale)
    max_difference_mu_hat <-
      max(abs(
        predict(single_model_output_fm$newmodel[[2]], newdata = visible_data) - predict(single_model_output_fm$newmodel[[1]], newdata = visible_data)
      ))
    if (max_scale ^ 2 > max_difference_mu_hat) {
      sigma_check = "stop"
      #censor_fm_check = NA_character_
      #fms_convergence = "tbd"
      if (verbose > 1) {print(paste0("sigma check results: ", sigma_check))}
    } else{

      sigma_check = "go"
      if (verbose > 1) {print(paste0("sigma check results: ", sigma_check))}
    }

    censor_fm_check <-
      check_for_excessive_censoring(single_model_output_fm, cutoff)

    if (verbose > 1) {print(paste0("censoring check results: ", censor_fm_check))}

    if (censor_fm_check == "BOTH") {
      fms_convergence = NA_character_
      single_model_output_fms = "PASS"
      if (verbose > 1) {
        print(
          "fit_model converged outside excessive censoring boundaries in both directions"
        )
      }
    } else if (censor_fm_check == "All Clear" & sigma_check == "go") {
      fms_convergence = NA_character_
      single_model_output_fms = "PASS"
      if (verbose > 1) {
        print("fit_model converged within excessive censoring boundaries")
      }
    } else if (censor_fm_check == "All Clear" & sigma_check == "stop") {
      fms_convergence = NA_character_
      single_model_output_fms = "PASS"
      if (verbose > 1) {
        print("fit_model converged within excessive censoring boundaries but violated sigma check, cannot find a side to use for fms")
      }
    } else{
      fms_convergence = "tbd"
    }
  }
  if(length(single_model_output_fm) > 1 & !fm_convergence & single_model_output_fm$ncomp > 1){ ##put the stuff for a model that didn't converge but generated output here, assign censor_fm_check as "RC", "LC", or both? skip sigma check

    sigma_check = NA_character_ #can't sigma check because one scale is missing
    censor_fm_check =
      case_when(
        is.na(single_model_output_fm$newmodel[[1]]$coefficients[1]) & is.na(single_model_output_fm$newmodel[[2]]$coefficients[1]) ~ "BOTH",
        is.na(single_model_output_fm$newmodel[[1]]$coefficients[1]) ~ "LC",
        is.na(single_model_output_fm$newmodel[[2]]$coefficients[1]) ~ "RC",
        TRUE ~ "BOTH"
      )
    fms_convergence = case_when(
      censor_fm_check == "BOTH" ~ NA,
      TRUE ~ "tbd"
    )
    if(is.na(fms_convergence)){single_model_output_fms = "PASS"}

  }
  if(length(single_model_output_fm) == 1 | single_model_output_fm$ncomp == 1){
    sigma_check = NA_character_
    censor_fm_check = NA_character_
    fms_convergence = "PASS"
  }
  return(
    list(
      sigma_check = sigma_check,
      censor_fm_check = censor_fm_check,
      fms_convergence = fms_convergence) )
}
