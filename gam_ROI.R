##code to generate direct effect of snoring and other SDB variables on cortical morphometry variables using GAM

library(lmtest)
library(dplyr)
library(glue)
library(mgcv)
grid_df2 <- expand.grid(c(dependent_vars2_group1, dependent_vars2_group2, dependent_vars2_group3), 
                        c(independent_vars1, independent_vars2))
#x[1] will be dependent_vars2, x[2] will be independent_vars

counter <- 0

k = "re"

# 'for' loop to run over the dependent and independent vars
model_table_2 <- do.call("rbind", apply(grid_df2, 1, function(x){
  
  counter <<- counter + 1
  
  cat(paste0("Processed ", counter, "/", nrow(grid_df2), "\n"))
  
  predicted <- x[[1]]
  independent <- x[[2]]
  
  model1 <- tryCatch({
    gam(as.formula(glue("{predicted}~ age+sex+race + s(log_income) + s(device, bs=k)")), data = data)
  }, error = function(e){
    NA
  })
  
  model2 <- tryCatch({
    gam(as.formula(glue("{predicted}~ {independent} +age+sex+race+s(log_income) + s(device, bs=k)")), 
        data = data)
  }, error = function(e){
    NA
  })
  
  if(is.na(model1)){
    
    F_value = NA
    coeff = NA
    lower_bound = NA
    upper_bound = NA
    out_delta = NA
    out_p = NA
    
  } else {
    
    F_value = as.numeric(summary(model2)$pTerms.table[1,2])
    coeff = as.numeric(model2$coefficients[2])
    lower_bound = coeff - 1.96*(summary(model2)[2]$se[2])
    upper_bound = coeff + 1.96*(summary(model2)[2]$se[2])
    out_delta = as.numeric(round(summary(model2)$r.sq-summary(model1)$r.sq, digits = 5)*100)
    p = anova(model2, model1, test ="F" )
    
  }
  
  out <- data.frame("out_variable" = x[[1]], 
                    "independent_variable" = x[[2]],
                    "F_value" = F_value,
                    "coeff" = coeff,
                    "lower_bound" = lower_bound,
                    "upper_bound" = upper_bound,
                    "out_delta" = out_delta,
                    "out_p" = as.numeric(p$`Pr(>F)`[2]))
  
  rownames(out) <- NULL
  out <- out %>% distinct()
  
  return(out)
  
})
)


adjust_per <- 151

model_table_2$group <- rep(c(1:(nrow(model_table_2)/adjust_per)), each = adjust_per)

model_table_2 <- model_table_2 %>%
  group_by(group) %>%
  mutate(out_p_adj = p.adjust(out_p, method = "fdr")) %>%
  select(-group)
