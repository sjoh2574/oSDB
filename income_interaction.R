library(mgcv)
library(lmtest)




model1<- gam(cbcl_scr_syn_totprob_t ~(sleep_13_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model2<- gam(cbcl_scr_syn_totprob_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_13_p)+ s(log_income)+ti(sleep_13_p, log_income), data = test_sleep_all)
summary(model1)
summary(model2)
round(summary(model2)$r.sq-summary(model1)$r.sq, digits = 5)*100
lrtest(model1, model2)$Pr[2]


model3<- gam(cbcl_scr_syn_totprob_t ~sleep_14_p+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model4<- gam(cbcl_scr_syn_totprob_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_14_p)+ ti(log_income)+ti(sleep_14_p, log_income), data = test_sleep_all)
summary(model3)
summary(model4)
round(summary(model4)$r.sq-summary(model3)$r.sq, digits = 5)*100
lrtest(model3, model4)$Pr[2]

model5<- gam(cbcl_scr_syn_totprob_t ~(sleep_15_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model6<- gam(cbcl_scr_syn_totprob_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_15_p)+ ti(log_income)+ti(sleep_15_p, log_income), data = test_sleep_all)
summary(model5)
summary(model6)
round(summary(model6)$r.sq-summary(model5)$r.sq, digits = 5)*100
lrtest(model5, model6)$Pr[2]

model7<- gam(cbcl_scr_syn_totprob_t ~(sds_p_ss_sbd)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model8<- gam(cbcl_scr_syn_totprob_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sds_p_ss_sbd)+ ti(log_income)+ti(sds_p_ss_sbd, log_income), data = test_sleep_all)
summary(model7)
summary(model8)
round(summary(model8)$r.sq-summary(model7)$r.sq, digits = 5)*100
lrtest(model7, model8)$Pr[2]

###############
model1<- gam(cbcl_scr_syn_external_t ~(sleep_13_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model2<- gam(cbcl_scr_syn_external_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_13_p)+ ti(log_income)+ti(sleep_13_p, log_income), data = test_sleep_all)
summary(model1)
summary(model2)
round(summary(model2)$r.sq-summary(model1)$r.sq, digits = 5)*100
lrtest(model1, model2)$Pr[2]


model3<- gam(cbcl_scr_syn_external_t ~(sleep_14_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model4<- gam(cbcl_scr_syn_external_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_14_p)+ ti(log_income)+ti(sleep_14_p, log_income), data = test_sleep_all)
summary(model3)
summary(model4)
round(summary(model4)$r.sq-summary(model3)$r.sq, digits = 5)*100
lrtest(model3, model4)$Pr[2]

model5<- gam(cbcl_scr_syn_external_t ~(sleep_15_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model6<- gam(cbcl_scr_syn_external_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_15_p)+ ti(log_income)+ti(sleep_15_p, log_income), data = test_sleep_all)
summary(model5)
summary(model6)
round(summary(model6)$r.sq-summary(model5)$r.sq, digits = 5)*100
lrtest(model5, model6)$Pr[2]

model7<- gam(cbcl_scr_syn_external_t ~(sds_p_ss_sbd)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model8<- gam(cbcl_scr_syn_external_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sds_p_ss_sbd)+ ti(log_income)+ti(sds_p_ss_sbd, log_income), data = test_sleep_all)
summary(model7)
summary(model8)
round(summary(model8)$r.sq-summary(model7)$r.sq, digits = 5)*100
lrtest(model7, model8)$Pr[2]


##################
model1<- gam(cbcl_scr_syn_internal_t ~(sleep_13_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model2<- gam(cbcl_scr_syn_internal_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_13_p)+ ti(log_income)+ti(sleep_13_p, log_income), data = test_sleep_all)
summary(model1)
summary(model2)
round(summary(model2)$r.sq-summary(model1)$r.sq, digits = 5)*100
lrtest(model1, model2)$Pr[2]


model3<- gam(cbcl_scr_syn_internal_t ~(sleep_14_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model4<- gam(cbcl_scr_syn_internal_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_14_p)+ ti(log_income)+ti(sleep_14_p, log_income), data = test_sleep_all)
summary(model3)
summary(model4)
round(summary(model4)$r.sq-summary(model3)$r.sq, digits = 5)*100
lrtest(model3, model4)$Pr[2]

model5<- gam(cbcl_scr_syn_internal_t ~(sleep_15_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model6<- gam(cbcl_scr_syn_internal_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_15_p)+ ti(log_income)+ti(sleep_15_p, log_income), data = test_sleep_all)
summary(model5)
summary(model6)
round(summary(model6)$r.sq-summary(model5)$r.sq, digits = 5)*100
lrtest(model5, model6)$Pr[2]

model7<- gam(cbcl_scr_syn_internal_t ~(sds_p_ss_sbd)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model8<- gam(cbcl_scr_syn_internal_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sds_p_ss_sbd)+ ti(log_income)+ti(sds_p_ss_sbd, log_income), data = test_sleep_all)
summary(model7)
summary(model8)
round(summary(model8)$r.sq-summary(model7)$r.sq, digits = 5)*100
lrtest(model7, model8)$Pr[2]

###########

model1<- gam(cbcl_scr_syn_anxdep_t ~(sleep_13_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model2<- gam(cbcl_scr_syn_anxdep_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_13_p)+ ti(log_income)+ti(sleep_13_p, log_income), data = test_sleep_all)
summary(model1)
summary(model2)
round(summary(model2)$r.sq-summary(model1)$r.sq, digits = 5)*100
lrtest(model1, model2)$Pr[2]


model3<- gam(cbcl_scr_syn_anxdep_t ~(sleep_14_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model4<- gam(cbcl_scr_syn_anxdep_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_14_p)+ ti(log_income)+ti(sleep_14_p, log_income), data = test_sleep_all)
summary(model3)
summary(model4)
round(summary(model4)$r.sq-summary(model3)$r.sq, digits = 5)*100
lrtest(model3, model4)$Pr[2]

model5<- gam(cbcl_scr_syn_anxdep_t ~(sleep_15_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model6<- gam(cbcl_scr_syn_anxdep_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_15_p)+ ti(log_income)+ti(sleep_15_p, log_income), data = test_sleep_all)
summary(model5)
summary(model6)
round(summary(model6)$r.sq-summary(model5)$r.sq, digits = 5)*100
lrtest(model5, model6)$Pr[2]

model7<- gam(cbcl_scr_syn_anxdep_t ~(sds_p_ss_sbd)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model8<- gam(cbcl_scr_syn_anxdep_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sds_p_ss_sbd)+ ti(log_income)+ti(sds_p_ss_sbd, log_income), data = test_sleep_all)
summary(model7)
summary(model8)
round(summary(model8)$r.sq-summary(model7)$r.sq, digits = 5)*100
lrtest(model7, model8)$Pr[2]

######
model1<- gam(cbcl_scr_syn_withdep_t ~(sleep_13_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model2<- gam(cbcl_scr_syn_withdep_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_13_p)+ ti(log_income)+ti(sleep_13_p, log_income), data = test_sleep_all)
summary(model1)
summary(model2)
round(summary(model2)$r.sq-summary(model1)$r.sq, digits = 5)*100
lrtest(model1, model2)$Pr[2]


model3<- gam(cbcl_scr_syn_withdep_t ~(sleep_14_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model4<- gam(cbcl_scr_syn_withdep_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_14_p)+ ti(log_income)+ti(sleep_14_p, log_income), data = test_sleep_all)
summary(model3)
summary(model4)
round(summary(model4)$r.sq-summary(model3)$r.sq, digits = 5)*100
lrtest(model3, model4)$Pr[2]

model5<- gam(cbcl_scr_syn_withdep_t ~(sleep_15_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model6<- gam(cbcl_scr_syn_withdep_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_15_p)+ ti(log_income)+ti(sleep_15_p, log_income), data = test_sleep_all)
summary(model5)
summary(model6)
round(summary(model6)$r.sq-summary(model5)$r.sq, digits = 5)*100
lrtest(model5, model6)$Pr[2]

model7<- gam(cbcl_scr_syn_withdep_t ~(sds_p_ss_sbd)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model8<- gam(cbcl_scr_syn_withdep_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sds_p_ss_sbd)+ ti(log_income)+ti(sds_p_ss_sbd, log_income), data = test_sleep_all)
summary(model7)
summary(model8)
round(summary(model8)$r.sq-summary(model7)$r.sq, digits = 5)*100
lrtest(model7, model8)$Pr[2]

####
model1<- gam(cbcl_scr_syn_somatic_t ~(sleep_13_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model2<- gam(cbcl_scr_syn_somatic_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_13_p)+ ti(log_income)+ti(sleep_13_p, log_income), data = test_sleep_all)
summary(model1)
summary(model2)
round(summary(model2)$r.sq-summary(model1)$r.sq, digits = 5)*100
lrtest(model1, model2)$Pr[2]


model3<- gam(cbcl_scr_syn_somatic_t ~(sleep_14_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model4<- gam(cbcl_scr_syn_somatic_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_14_p)+ ti(log_income)+ti(sleep_14_p, log_income), data = test_sleep_all)
summary(model3)
summary(model4)
round(summary(model4)$r.sq-summary(model3)$r.sq, digits = 5)*100
lrtest(model3, model4)$Pr[2]

model5<- gam(cbcl_scr_syn_somatic_t ~(sleep_15_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model6<- gam(cbcl_scr_syn_somatic_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_15_p)+ ti(log_income)+ti(sleep_15_p, log_income), data = test_sleep_all)
summary(model5)
summary(model6)
round(summary(model6)$r.sq-summary(model5)$r.sq, digits = 5)*100
lrtest(model5, model6)$Pr[2]

model7<- gam(cbcl_scr_syn_somatic_t ~(sds_p_ss_sbd)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model8<- gam(cbcl_scr_syn_somatic_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sds_p_ss_sbd)+ ti(log_income)+ti(sds_p_ss_sbd, log_income), data = test_sleep_all)
summary(model7)
summary(model8)
round(summary(model8)$r.sq-summary(model7)$r.sq, digits = 5)*100
lrtest(model7, model8)$Pr[2]


###########
model1<- gam(cbcl_scr_syn_social_t ~(sleep_13_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model2<- gam(cbcl_scr_syn_social_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_13_p)+ ti(log_income)+ti(sleep_13_p, log_income), data = test_sleep_all)
summary(model1)
summary(model2)
round(summary(model2)$r.sq-summary(model1)$r.sq, digits = 5)*100
lrtest(model1, model2)$Pr[2]


model3<- gam(cbcl_scr_syn_social_t ~(sleep_14_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model4<- gam(cbcl_scr_syn_social_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_14_p)+ ti(log_income)+ti(sleep_14_p, log_income), data = test_sleep_all)
summary(model3)
summary(model4)
round(summary(model4)$r.sq-summary(model3)$r.sq, digits = 5)*100
lrtest(model3, model4)$Pr[2]

model5<- gam(cbcl_scr_syn_social_t ~(sleep_15_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model6<- gam(cbcl_scr_syn_social_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_15_p)+ ti(log_income)+ti(sleep_15_p, log_income), data = test_sleep_all)
summary(model5)
summary(model6)
round(summary(model6)$r.sq-summary(model5)$r.sq, digits = 5)*100
lrtest(model5, model6)$Pr[2]

model7<- gam(cbcl_scr_syn_social_t ~(sds_p_ss_sbd)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model8<- gam(cbcl_scr_syn_social_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sds_p_ss_sbd)+ ti(log_income)+ti(sds_p_ss_sbd, log_income), data = test_sleep_all)
summary(model7)
summary(model8)
round(summary(model8)$r.sq-summary(model7)$r.sq, digits = 5)*100
lrtest(model7, model8)$Pr[2]

###########
model1<- gam(cbcl_scr_syn_thought_t ~(sleep_13_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model2<- gam(cbcl_scr_syn_thought_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_13_p)+ ti(log_income)+ti(sleep_13_p, log_income), data = test_sleep_all)
summary(model1)
summary(model2)
round(summary(model2)$r.sq-summary(model1)$r.sq, digits = 5)*100
lrtest(model1, model2)$Pr[2]


model3<- gam(cbcl_scr_syn_thought_t ~(sleep_14_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model4<- gam(cbcl_scr_syn_thought_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_14_p)+ ti(log_income)+ti(sleep_14_p, log_income), data = test_sleep_all)
summary(model3)
summary(model4)
round(summary(model4)$r.sq-summary(model3)$r.sq, digits = 5)*100
lrtest(model3, model4)$Pr[2]

model5<- gam(cbcl_scr_syn_thought_t ~(sleep_15_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model6<- gam(cbcl_scr_syn_thought_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_15_p)+ ti(log_income)+ti(sleep_15_p, log_income), data = test_sleep_all)
summary(model5)
summary(model6)
round(summary(model6)$r.sq-summary(model5)$r.sq, digits = 5)*100
lrtest(model5, model6)$Pr[2]

model7<- gam(cbcl_scr_syn_thought_t ~(sds_p_ss_sbd)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model8<- gam(cbcl_scr_syn_thought_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sds_p_ss_sbd)+ ti(log_income)+ti(sds_p_ss_sbd, log_income), data = test_sleep_all)
summary(model7)
summary(model8)
round(summary(model8)$r.sq-summary(model7)$r.sq, digits = 5)*100
lrtest(model7, model8)$Pr[2]


###########
model1<- gam(cbcl_scr_syn_attention_t ~(sleep_13_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model2<- gam(cbcl_scr_syn_attention_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_13_p)+ ti(log_income)+ti(sleep_13_p, log_income), data = test_sleep_all)
summary(model1)
summary(model2)
round(summary(model2)$r.sq-summary(model1)$r.sq, digits = 5)*100
lrtest(model1, model2)$Pr[2]


model3<- gam(cbcl_scr_syn_attention_t ~(sleep_14_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model4<- gam(cbcl_scr_syn_attention_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_14_p)+ ti(log_income)+ti(sleep_14_p, log_income), data = test_sleep_all)
summary(model3)
summary(model4)
round(summary(model4)$r.sq-summary(model3)$r.sq, digits = 5)*100
lrtest(model3, model4)$Pr[2]

model5<- gam(cbcl_scr_syn_attention_t ~(sleep_15_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model6<- gam(cbcl_scr_syn_attention_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_15_p)+ ti(log_income)+ti(sleep_15_p, log_income), data = test_sleep_all)
summary(model5)
summary(model6)
round(summary(model6)$r.sq-summary(model5)$r.sq, digits = 5)*100
lrtest(model5, model6)$Pr[2]

model7<- gam(cbcl_scr_syn_attention_t ~(sds_p_ss_sbd)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model8<- gam(cbcl_scr_syn_attention_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sds_p_ss_sbd)+ ti(log_income)+ti(sds_p_ss_sbd, log_income), data = test_sleep_all)
summary(model7)
summary(model8)
round(summary(model8)$r.sq-summary(model7)$r.sq, digits = 5)*100
lrtest(model7, model8)$Pr[2]


###########
model1<- gam(cbcl_scr_syn_rulebreak_t ~(sleep_13_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model2<- gam(cbcl_scr_syn_rulebreak_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_13_p)+ ti(log_income)+ti(sleep_13_p, log_income), data = test_sleep_all)
summary(model1)
summary(model2)
round(summary(model2)$r.sq-summary(model1)$r.sq, digits = 5)*100
lrtest(model1, model2)$Pr[2]


model3<- gam(cbcl_scr_syn_rulebreak_t ~(sleep_14_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model4<- gam(cbcl_scr_syn_rulebreak_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_14_p)+ ti(log_income)+ti(sleep_14_p, log_income), data = test_sleep_all)
summary(model3)
summary(model4)
round(summary(model4)$r.sq-summary(model3)$r.sq, digits = 5)*100
lrtest(model3, model4)$Pr[2]

model5<- gam(cbcl_scr_syn_rulebreak_t ~(sleep_15_p)+age+sex+race+bmipct+asthma+(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model6<- gam(cbcl_scr_syn_rulebreak_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_15_p)+ ti(log_income)+ti(sleep_15_p, log_income), data = test_sleep_all)
summary(model5)
summary(model6)
round(summary(model6)$r.sq-summary(model5)$r.sq, digits = 5)*100
lrtest(model5, model6)$Pr[2]

model7<- gam(cbcl_scr_syn_rulebreak_t ~(sds_p_ss_sbd)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model8<- gam(cbcl_scr_syn_rulebreak_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sds_p_ss_sbd)+ ti(log_income)+ti(sds_p_ss_sbd, log_income), data = test_sleep_all)
summary(model7)
summary(model8)
round(summary(model8)$r.sq-summary(model7)$r.sq, digits = 5)*100
lrtest(model7, model8)$Pr[2]


###########
model1<- gam(cbcl_scr_syn_aggressive_t ~(sleep_13_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model2<- gam(cbcl_scr_syn_aggressive_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_13_p)+ ti(log_income)+ti(sleep_13_p, log_income), data = test_sleep_all)
summary(model1)
summary(model2)
round(summary(model2)$r.sq-summary(model1)$r.sq, digits = 5)*100
lrtest(model1, model2)$Pr[2]


model3<- gam(cbcl_scr_syn_aggressive_t ~(sleep_14_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model4<- gam(cbcl_scr_syn_aggressive_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_14_p)+ ti(log_income)+ti(sleep_14_p, log_income), data = test_sleep_all)
summary(model3)
summary(model4)
round(summary(model4)$r.sq-summary(model3)$r.sq, digits = 5)*100
lrtest(model3, model4)$Pr[2]

model5<- gam(cbcl_scr_syn_aggressive_t ~(sleep_15_p)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model6<- gam(cbcl_scr_syn_aggressive_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sleep_15_p)+ ti(log_income)+ti(sleep_15_p, log_income), data = test_sleep_all)
summary(model5)
summary(model6)
round(summary(model6)$r.sq-summary(model5)$r.sq, digits = 5)*100
lrtest(model5, model6)$Pr[2]

model7<- gam(cbcl_scr_syn_aggressive_t ~(sds_p_ss_sbd)+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all)
model8<- gam(cbcl_scr_syn_aggressive_t ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re")+ti(sds_p_ss_sbd)+ ti(log_income)+ti(sds_p_ss_sbd, log_income), data = test_sleep_all)
summary(model7)
summary(model8)
round(summary(model8)$r.sq-summary(model7)$r.sq, digits = 5)*100
lrtest(model7, model8)$Pr[2]
