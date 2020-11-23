
##this code generates a dataframe of direct and indirect effects using the causal mediation package
library(mediation)



#mediator is the list of dependent_vars2, the outcome is the dependent_vars1 and the the predictor is the independent_vars1 and independent_vars2. 
#mediator is x[[1]]
#outcome is x[[2]]
#predictor is x[[3]]
grid_df_3 <- expand.grid(c(dependent_vars2_group1, dependent_vars2_group2, dependent_vars2_group3), 
                         dependent_vars1, c(independent_vars1, independent_vars2))

counter <- 0

# 'for' loop to run over the dependent and independent vars
model_table_3_clinical <- do.call("rbind", apply(grid_df_3, 1, function(x){
  
  counter <<- counter + 1
  
  cat(paste0("Processed ", counter, "/", nrow(grid_df_3), "\n"))
  
  formula1 <- as.formula(paste0(x[[1]], "~", x[[3]], "+ age + sex + race + asthma+ log_income + site"))
  
  formula2 <- as.formula(paste0(x[[2]], "~", x[[3]], "+ age + sex + race + asthma + log_income + site + ", x[[1]]))
  
  model_mediator <- glm(formula1,  data = data)
  
  model_outcome  <- glm(formula2,  family="binomial", data = data)
  
  mediation_result <- tryCatch({mediate(
    model_mediator, 
    model_outcome, 
    sims = 100, #100
    treat = x[[3]],
    mediator =  x[[1]],
    boot = FALSE)
  }, error = function(e){
    mediation_result <- NA
  })
  
  if(is.null(mediation_result)){
    mediation_result <- NA
  }
  
  out_ACME = ifelse(is.na(mediation_result), NA, mediation_result$d.avg)
  out_ACME_uci = ifelse(is.na(mediation_result), NA, as.numeric((mediation_result$d.avg.ci)[2]))
  out_ACME_lci =  ifelse(is.na(mediation_result), NA, as.numeric((mediation_result$d.avg.ci)[1])) 
  out_pACME = ifelse(is.na(mediation_result), NA, mediation_result$d.avg.p)
  
  out_ADE = ifelse(is.na(mediation_result), NA, mediation_result$z.avg) 
  out_ADE_uci = ifelse(is.na(mediation_result), NA, as.numeric((mediation_result$z.avg.ci)[2])) 
  out_ADE_lci = ifelse(is.na(mediation_result), NA, as.numeric((mediation_result$z.avg.ci)[1])) 
  out_pADE = ifelse(is.na(mediation_result), NA, mediation_result$z.avg.p)  
  
  out_total = ifelse(is.na(mediation_result), NA, mediation_result$tau.coef) 
  out_total_uci = ifelse(is.na(mediation_result), NA, as.numeric((mediation_result$tau.ci)[2])) 
  out_total_lci = ifelse(is.na(mediation_result), NA, as.numeric((mediation_result$tau.ci)[1])) 
  out_ptotal = ifelse(is.na(mediation_result), NA, mediation_result$tau.p)
  
  out_prop = ifelse(is.na(mediation_result), NA, mediation_result$n.avg)
  out_prop_uci =ifelse(is.na(mediation_result), NA, as.numeric((mediation_result$n.avg.ci)[2]))
  out_prop_lci = ifelse(is.na(mediation_result), NA, as.numeric((mediation_result$n.avg.ci)[1]))
  out_pprop = ifelse(is.na(mediation_result), NA, mediation_result$n.avg.p)
  
  out <- data.frame("dependent_second_group" = x[[1]], 
                    "dependent_first_group" = x[[2]],
                    "independent_var" = x[[3]], 
                    "out_ACME" = out_ACME,
                    "out_ACME_uci" = out_ACME_uci,
                    "out_ACME_lci" = out_ACME_lci,
                    "out_pACME" = out_pACME,
                    "out_ADE" = out_ADE,
                    "out_ADE_uci" = out_ADE_uci,
                    "out_ADE_lci" = out_ADE_lci,
                    "out_pADE" = out_pADE,
                    "out_total" = out_total,
                    "out_total_uci" = out_total_uci,
                    "out_total_lci" = out_total_lci,
                    "out_ptotal" = out_ptotal,
                    "out_prop" = out_prop,
                    "out_prop_uci" = out_prop_uci,
                    "out_prop_lci" = out_prop_lci,
                    "out_pprop" = out_pprop)
  
  rownames(out) <- NULL
  out <- out %>% distinct()
  
  return(out)
  
})
)

saveRDS(model_table_3_clinical, "model_table_3_clinical.rds")
