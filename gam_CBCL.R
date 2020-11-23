library(mgcv)
library(lmtest)
model1 <- gam(sleep_13_p ~age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model2 <- gam(sleep_13_p ~sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model3 <- gam(sleep_13_p ~age+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model4 <- gam(sleep_13_p ~age+sex+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model5 <- gam(sleep_13_p ~age+sex+race+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model6 <- gam(sleep_13_p ~age+sex+race+bmipct+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model7 <- gam(sleep_13_p ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model8 <- gam(sleep_13_p ~age+sex+race+bmipct+asthma+s(log_income)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
anova(model1)
sum(influence(model1))
round(summary(model1)$r.sq-summary(model2)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model3)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model4)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model5)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model6)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model7)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model8)$r.sq,digits = 5)*100




model1 <- gam(sleep_14_p ~age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model2 <- gam(sleep_14_p ~sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model3 <- gam(sleep_14_p ~age+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model4 <- gam(sleep_14_p ~age+sex+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model5 <- gam(sleep_14_p ~age+sex+race+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model6 <- gam(sleep_14_p ~age+sex+race+bmipct+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model7 <- gam(sleep_14_p ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model8 <- gam(sleep_14_p ~age+sex+race+bmipct+asthma+s(log_income)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
anova(model1)
round(summary(model1)$r.sq-summary(model2)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model3)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model4)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model5)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model6)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model7)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model8)$r.sq,digits = 5)*100


model1 <- gam(sleep_15_p ~age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model2 <- gam(sleep_15_p ~sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model3 <- gam(sleep_15_p ~age+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model4 <- gam(sleep_15_p ~age+sex+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model5 <- gam(sleep_15_p ~age+sex+race+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model6 <- gam(sleep_15_p ~age+sex+race+bmipct+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model7 <- gam(sleep_15_p ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model8 <- gam(sleep_15_p ~age+sex+race+bmipct+asthma+s(log_income)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
anova(model1)
sum(influence(model1))
round(summary(model1)$r.sq-summary(model2)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model3)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model4)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model5)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model6)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model7)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model8)$r.sq,digits = 5)*100


model1 <- gam(sds_p_ss_sbd ~age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model2 <- gam(sds_p_ss_sbd ~sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model3 <- gam(sds_p_ss_sbd ~age+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model4 <- gam(sds_p_ss_sbd ~age+sex+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model5 <- gam(sds_p_ss_sbd ~age+sex+race+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model6 <- gam(sds_p_ss_sbd ~age+sex+race+bmipct+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model7 <- gam(sds_p_ss_sbd ~age+sex+race+bmipct+asthma+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
model8 <- gam(sds_p_ss_sbd ~age+sex+race+bmipct+asthma+s(log_income)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
anova(model1)
sum(influence(model1))
round(summary(model1)$r.sq-summary(model2)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model3)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model4)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model5)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model6)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model7)$r.sq,digits = 5)*100
round(summary(model1)$r.sq-summary(model8)$r.sq,digits = 5)*100