if(is.na(ecoff) & is.na(visual_split) &
   (!is.na(s_breakpoint) & !is.na(r_breakpoint))
   ){
  pi_bounds = pi_bounds %>%
    mutate(.by = t,
           susceptible = 1 - predict(lr_output_bkpt, newdata = tibble(t = t), type = "response"),
           resistant = predict(lr_output_bkpt, newdata = tibble(t = t), type = "response") )

  pi = pi +
    scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
    ggnewscale::new_scale_color() +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
    #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
    scale_color_manual(breaks = c("Susceptible", "Resistant"), values = c("#7CAE00", "#C77CFF"), name = "Other Prevalence") + #+ guides(linetype = "none")
    scale_linetype_discrete(name = "Estimate Source")

  s_line = case_when(
    grepl("≤",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
    grepl("=",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
    grepl("<",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
    TRUE ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2
  )
  r_line = case_when(
    grepl("≥",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
    grepl("=",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
    grepl(">",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2,
    TRUE ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1
  )

  mean = mean +
    ggnewscale::new_scale_color() +
    geom_hline(aes(yintercept = s_line, color = "Susceptible Breakpoint", linetype =  "Breakpoint")) +
    geom_hline(aes(yintercept = r_line, color = "Resistant Breakpoint", linetype =  "Breakpoint")) +
    scale_color_manual(breaks = c("Susceptible Breakpoint", "Resistant Breakpoint"), labels = c(TeX(paste0("Susceptible Breakpoint: ", s_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Resistant Breakpoint: ", r_breakpoint,r'($\mu$)',"g/mL"))), values = c("#7CAE00", "#C77CFF"), name = "Breakpoints and Cutoffs") +
    scale_linetype_manual(breaks=c("Fitted Model","Breakpoint"), values=c(1,5), guide = "none")  #+ guides(linetype = "none", color = "none")
}else if(
  !is.na(ecoff) & is.na(visual_split) &
  (is.na(s_breakpoint) & is.na(r_breakpoint))
  ){
  pi_bounds = pi_bounds %>%
    mutate(.by = t,
           wt = 1 - predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
           nwt = predict(lr_output_ecoff, newdata = tibble(t = t), type = "response") )


  pi = pi +
    scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
    ggnewscale::new_scale_color() +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +

    #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
    scale_color_manual(breaks = c("WT", "NWT"), values = c( "#fcbf07", "#0211a3"), name = "Other Prevalence")  + #guides(linetype = "none")
    scale_linetype_discrete(name = "Estimate Source")

  ecoff_line = case_when(
    grepl("≤", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
    grepl("=", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
    grepl("<", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2 - 1,
    TRUE ~ ecoff %>% as.character() %>% parse_number() %>% log2
  )

  mean = mean +
    ggnewscale::new_scale_color() +
    geom_hline(aes(yintercept = ecoff_line, color = "ECOFF", linetype =  "Cutoff")) +
    scale_color_manual(breaks = c("ECOFF"), values = c("#ffd700"), labels = c(TeX(paste0("ECOFF: ", ecoff,r'($\mu$)',"g/mL"))), name = "Breakpoints and Cutoffs") +
    scale_linetype_manual(breaks=c("Fitted Model","Cutoff"), values=c(1,2), guide = "none")  #+ guides(linetype = "none", color = "none")
}else if(
  is.na(ecoff) & !is.na(visual_split) &
  (is.na(s_breakpoint) & is.na(r_breakpoint))
){
  pi_bounds = pi_bounds %>%
    mutate(.by = t,
           c1vs = 1 - predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
           c2vs = predict(lr_output_visual_split, newdata = tibble(t = t), type = "response") )


  pi = pi +
    scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
    ggnewscale::new_scale_color() +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = c1vs, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = c2vs, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +

    #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
    scale_color_manual(breaks = c("Below Split", "Above Split"), values = c( "#DF4601", "#000000"), name = "Other Prevalence")  + #guides(linetype = "none")
    scale_linetype_discrete(name = "Estimate Source")

  visual_split_line = case_when(
    grepl("≤", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
    grepl("=", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
    grepl("<", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2 - 1,
    TRUE ~ visual_split %>% as.character() %>% parse_number() %>% log2
  )

  mean = mean +
    ggnewscale::new_scale_color() +
    geom_hline(aes(yintercept = visual_split_line, color = "Visual Split", linetype =  "Cutoff")) +
    scale_color_manual(breaks = c("Visual Split"), values = c("#DF4601"), labels = c(TeX(paste0("Visual Split: ", visual_split,r'($\mu$)',"g/mL"))), name = "Breakpoints and Cutoffs") +
    scale_linetype_manual(breaks=c("Fitted Model","Cutoff"), values=c(1,2), guide = "none")  #+ guides(linetype = "none", color = "none")

}else if(
  !is.na(ecoff) & !is.na(visual_split) &
  (is.na(s_breakpoint) & is.na(r_breakpoint))
){
  pi_bounds = pi_bounds %>%
    mutate(.by = t,
           wt = 1 - predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
           nwt = predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
           c1vs = 1 - predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
           c2vs = predict(lr_output_visual_split, newdata = tibble(t = t), type = "response") )



if(!is.null(skip) && "ecoff" %in% skip){
  pi = pi +
    scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
    ggnewscale::new_scale_color() +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = c1vs, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = c2vs, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
    #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Below Split", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
    scale_color_manual(breaks = c("Below Split", "Above Split"), values = c( "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
    scale_linetype_discrete(name = "Estimate Source")
}else{
  pi = pi +
    scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
    ggnewscale::new_scale_color() +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = c1vs, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = c2vs, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +
    #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Below Split", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
    scale_color_manual(breaks = c("Below Split", "Above Split",  "WT", "NWT"), values = c( "#DF4601", "#000000", "#fcbf07", "#0211a3"), name = "Other Prevalence") +  #+ guides(linetype = "none")
    scale_linetype_discrete(name = "Estimate Source")
}
  visual_split_line = case_when(
    grepl("≤",visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
    grepl("=",visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
    grepl("<",visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2 - 1,
    TRUE ~ visual_split %>% as.character() %>% parse_number() %>% log2
  )

  ecoff_line = case_when(
    grepl("≤", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
    grepl("=", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
    grepl("<", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2 - 1,
    TRUE ~ ecoff %>% as.character() %>% parse_number() %>% log2
  )

  if(visual_split_line == ecoff_line){
    visual_split_line = visual_split_line + 0.05
  }

  mean = mean +
    ggnewscale::new_scale_color() +
    geom_hline(aes(yintercept = visual_split_line, color = "Visual Split", linetype =  "Cutoff")) +
    geom_hline(aes(yintercept = ecoff_line, color = "ECOFF", linetype =  "Cutoff")) +

    scale_color_manual(breaks = c("Visual Split", "ECOFF"),
                       labels = c(TeX(paste0("Visual Split: ", visual_split,r'($\mu$)',"g/mL")), TeX(paste0("ECOFF: ", ecoff,r'($\mu$)',"g/mL"))),
                       values = c("#DF4601" , "#ffd700"), name = "Breakpoints and Cutoffs") +
    scale_linetype_manual(breaks=c("Fitted Model", "Cutoff"), values=c(1,2), guide = "none")  #+ guides(linetype = "none", color = "none")

}else if(
  !is.na(ecoff) & is.na(visual_split) &
  !(is.na(s_breakpoint) & is.na(r_breakpoint))
){
  pi_bounds = pi_bounds %>%
    mutate(.by = t,
           wt = 1 - predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
           nwt = predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
           susceptible = 1 - predict(lr_output_bkpt, newdata = tibble(t = t), type = "response"),
           resistant = predict(lr_output_bkpt, newdata = tibble(t = t), type = "response") )


if(!is.null(skip) && "ecoff" %in% skip){
  pi = pi +
    scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
    ggnewscale::new_scale_color() +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
    #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
    scale_color_manual(breaks = c("Susceptible", "Resistant"), values = c( "#7CAE00", "#C77CFF"), name = "Other Prevalence") +  #+ guides(linetype = "none")
    scale_linetype_discrete(name = "Estimate Source")
}else if(!is.null(skip) && "bkpts" %in% skip){
  pi = pi +
    scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
    ggnewscale::new_scale_color() +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +
    #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
    scale_color_manual(breaks = c( "WT", "NWT"), values = c("#fcbf07", "#0211a3"), name = "Other Prevalence") +  #+ guides(linetype = "none")
    scale_linetype_discrete(name = "Estimate Source")
}else{

  pi = pi +
    scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
    ggnewscale::new_scale_color() +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +
    #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
    scale_color_manual(breaks = c("Susceptible", "Resistant",  "WT", "NWT"), values = c( "#7CAE00", "#C77CFF", "#fcbf07", "#0211a3"), name = "Other Prevalence") +  #+ guides(linetype = "none")
    scale_linetype_discrete(name = "Estimate Source")
}


  s_line = case_when(
    grepl("≤",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
    grepl("=",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
    grepl("<",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
    TRUE ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2
  )
  r_line = case_when(
    grepl("≥",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
    grepl("=",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
    grepl(">",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2,
    TRUE ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1
  )

  ecoff_line = case_when(
    grepl("≤", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
    grepl("=", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
    grepl("<", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2 - 1,
    TRUE ~ ecoff %>% as.character() %>% parse_number() %>% log2
  )

  if(s_line == ecoff_line){
    s_line = s_line + 0.05
  }

  mean = mean +
    ggnewscale::new_scale_color() +
    geom_hline(aes(yintercept = s_line, color = "Susceptible Breakpoint", linetype =  "Breakpoint")) +
    geom_hline(aes(yintercept = r_line, color = "Resistant Breakpoint", linetype =  "Breakpoint")) +
    geom_hline(aes(yintercept = ecoff_line, color = "ECOFF", linetype =  "Cutoff")) +

    scale_color_manual(breaks = c("Susceptible Breakpoint", "Resistant Breakpoint", "ECOFF"),
                       labels = c(TeX(paste0("Susceptible Breakpoint: ", s_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Resistant Breakpoint: ", r_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("ECOFF: ", ecoff,r'($\mu$)',"g/mL"))),
                       values = c("#7CAE00", "#C77CFF", "#ffd700"), name = "Breakpoints and Cutoffs") +
    scale_linetype_manual(breaks=c("Fitted Model","Breakpoint", "Cutoff"), values=c(1,5,2), guide = "none")  #+ guides(linetype = "none", color = "none")

}else if(
  is.na(ecoff) & !is.na(visual_split) &
  !(is.na(s_breakpoint) & is.na(r_breakpoint))
){
  pi_bounds = pi_bounds %>%
    mutate(.by = t,
           c1vs = 1 - predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
           c2vs = predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
           susceptible = 1 - predict(lr_output_bkpt, newdata = tibble(t = t), type = "response"),
           resistant = predict(lr_output_bkpt, newdata = tibble(t = t), type = "response") )


if(!is.null(skip) && "bkpts" %in% skip){
  pi = pi +
    scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
    ggnewscale::new_scale_color() +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = c1vs, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = c2vs, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
    #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){(1 - predict(lr_output_visual_split, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Below Split", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_visual_split, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Above Split", linetype = "Logistic Regression")) +
    scale_color_manual(breaks = c(  "Below Split", "Above Split"), values = c( "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
    scale_linetype_discrete(name = "Estimate Source")
}else{

  pi = pi +
    scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
    ggnewscale::new_scale_color() +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = c1vs, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = c2vs, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
    #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){(1 - predict(lr_output_visual_split, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Below Split", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_visual_split, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Above Split", linetype = "Logistic Regression")) +
    scale_color_manual(breaks = c("Susceptible", "Resistant",  "Below Split", "Above Split"), values = c( "#7CAE00", "#C77CFF", "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
    scale_linetype_discrete(name = "Estimate Source")
}


  s_line = case_when(
    grepl("≤",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
    grepl("=",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
    grepl("<",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
    TRUE ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2
  )
  r_line = case_when(
    grepl("≥",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
    grepl("=",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
    grepl(">",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2,
    TRUE ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1
  )

  visual_split_line = case_when(
    grepl("≤", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
    grepl("=", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
    grepl("<", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2 - 1,
    TRUE ~ visual_split %>% as.character() %>% parse_number() %>% log2
  )

  if(s_line == visual_split_line){
    s_line = s_line + 0.05
  }

  mean = mean +
    ggnewscale::new_scale_color() +
    geom_hline(aes(yintercept = s_line, color = "Susceptible Breakpoint", linetype =  "Breakpoint")) +
    geom_hline(aes(yintercept = r_line, color = "Resistant Breakpoint", linetype =  "Breakpoint")) +
    geom_hline(aes(yintercept = visual_split_line, color = "Visual Split", linetype =  "Cutoff")) +

    scale_color_manual(breaks = c("Susceptible Breakpoint", "Resistant Breakpoint", "Visual Split"),
                       labels = c(TeX(paste0("Susceptible Breakpoint: ", s_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Resistant Breakpoint: ", r_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Visual Split: ", visual_split,r'($\mu$)',"g/mL"))),
                       values = c("#7CAE00", "#C77CFF", "#DF4601"), name = "Breakpoints and Cutoffs") +
    scale_linetype_manual(breaks=c("Fitted Model","Breakpoint", "Cutoff"), values=c(1,5,2), guide = "none")  #+ guides(linetype = "none", color = "none")

}else{
  pi_bounds = pi_bounds %>%
    mutate(.by = t,
           wt = 1 - predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
           nwt = predict(lr_output_ecoff, newdata = tibble(t = t), type = "response"),
           c1vs = 1 - predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
           c2vs = predict(lr_output_visual_split, newdata = tibble(t = t), type = "response"),
           susceptible = 1 - predict(lr_output_bkpt, newdata = tibble(t = t), type = "response"),
           resistant = predict(lr_output_bkpt, newdata = tibble(t = t), type = "response") )


if(!is.null(skip) && "bkpts" %in% skip & "ecoff" %in% skip){
  pi = pi +
    scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
    ggnewscale::new_scale_color() +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
    #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
    scale_color_manual(breaks = c("Below Split", "Above Split"), values = c( "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
    scale_linetype_discrete(name = "Estimate Source")
}else if(!is.null(skip) && "bkpts" %in% skip){
  pi = pi +
    scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
    ggnewscale::new_scale_color() +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
    #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
    scale_color_manual(breaks = c("WT", "NWT", "Below Split", "Above Split"), values = c(  "#fcbf07", "#0211a3", "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
    scale_linetype_discrete(name = "Estimate Source")
}else if(!is.null(skip) && "ecoff" %in% skip){
  pi = pi +
    scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
    ggnewscale::new_scale_color() +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
    #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
    scale_color_manual(breaks = c("Susceptible", "Resistant","Below Split", "Above Split"), values = c( "#7CAE00", "#C77CFF", "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
    scale_linetype_discrete(name = "Estimate Source")
}else{

  pi = pi +
    scale_color_manual(breaks = c("Component 1 Proportion", "Component 2 Proportion"), values = c("#e4190b", "#00999d"), labels = c(TeX(r'(Component 1 Prevalence: $\hat{\pi}_{1,t}$)'), TeX(r'(Component 2 Prevalence: $\hat{\pi}_{2,t}$)')), name = "Component Prevalence") +
    ggnewscale::new_scale_color() +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = susceptible, color = "Susceptible", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = resistant, color = "Resistant", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "WT", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "NWT", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = wt, color = "Below Split", linetype = "Logistic Regression"), data = pi_bounds) +
    geom_line(aes(x = offset_time_as_date(t, start_date), y = nwt, color = "Above Split", linetype = "Logistic Regression"), data = pi_bounds) +
    #geom_function(fun = function(t){(1 - predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "Susceptible", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_bkpt, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "Resistant", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){(1 - predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response"))}, aes(color = "WT", linetype = "Logistic Regression")) +
    #geom_function(fun = function(t){predict(lr_output_ecoff, newdata = data.frame(t = as_offset_time(x = t, start_date)), type = "response")}, aes(color = "NWT", linetype = "Logistic Regression")) +
    scale_color_manual(breaks = c("Susceptible", "Resistant",  "WT", "NWT", "Below Split", "Above Split"), values = c( "#7CAE00", "#C77CFF", "#fcbf07", "#0211a3", "#DF4601", "#000000"), name = "Other Prevalence") +  #+ guides(linetype = "none")
    scale_linetype_discrete(name = "Estimate Source")

}


  s_line = case_when(
    grepl("≤",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
    grepl("=",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2,
    grepl("<",s_breakpoint) ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
    TRUE ~ s_breakpoint %>% as.character() %>% parse_number() %>% log2
  )
  r_line = case_when(
    grepl("≥",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
    grepl("=",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1,
    grepl(">",r_breakpoint) ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2,
    TRUE ~ r_breakpoint %>% as.character() %>% parse_number() %>% log2 - 1
  )

  ecoff_line = case_when(
    grepl("≤", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
    grepl("=", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2,
    grepl("<", ecoff) ~ ecoff %>% as.character() %>% parse_number() %>% log2 - 1,
    TRUE ~ ecoff %>% as.character() %>% parse_number() %>% log2
  )

  visual_split_line = case_when(
    grepl("≤", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
    grepl("=", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2,
    grepl("<", visual_split) ~ visual_split %>% as.character() %>% parse_number() %>% log2 - 1,
    TRUE ~ visual_split %>% as.character() %>% parse_number() %>% log2
  )

  if(visual_split_line == ecoff_line){
    visual_split_line = visual_split_line + 0.05
  }

  if(s_line == ecoff_line){
    s_line = s_line + 0.05
  }
  if(s_line == visual_split_line){
    s_line = s_line + 0.05
  }

  mean = mean +
    ggnewscale::new_scale_color() +
    geom_hline(aes(yintercept = s_line, color = "Susceptible Breakpoint", linetype =  "Breakpoint")) +
    geom_hline(aes(yintercept = r_line, color = "Resistant Breakpoint", linetype =  "Breakpoint")) +
    geom_hline(aes(yintercept = ecoff_line, color = "ECOFF", linetype =  "Cutoff")) +
    geom_hline(aes(yintercept = visual_split_line, color = "Visual Split", linetype =  "Cutoff")) +

    scale_color_manual(breaks = c("Susceptible Breakpoint", "Resistant Breakpoint", "ECOFF", "Visual Split"),
                       labels = c(TeX(paste0("Susceptible Breakpoint: ", s_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("Resistant Breakpoint: ", r_breakpoint,r'($\mu$)',"g/mL")), TeX(paste0("ECOFF: ", ecoff,r'($\mu$)',"g/mL")), TeX(paste0("ECOFF: ", visual_split,r'($\mu$)',"g/mL"))),
                       values = c("#7CAE00", "#C77CFF", "#ffd700", "#DF4601"), name = "Breakpoints and Cutoffs") +
    scale_linetype_manual(breaks=c("Fitted Model","Breakpoint", "Cutoff", "Cutoff"), values=c(1,5,2,2), guide = "none")  #+ guides(linetype = "none", color = "none")

}
