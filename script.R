model1 <- gam(cbcl_scr_syn_totprob_t ~sds_p_ss_sbd+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
summary(model1)
##plot income vs total comp
library(ggplot2)
dataset <- data.frame(x=test_sleep_all$sds_p_ss_sbd, y = model1$fitted.values, z = test_sleep_all$sds_p_ss_sbd)
ggplot(data = dataset, 
       aes(x = z, y = y, color = x)) +
  geom_point(size=0.1) +    geom_jitter(width = 0.1)+ geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3)) + 
  theme_classic(base_size=18) + scale_color_gradient(low="red", high="grey") + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + ylim(30,70)



model1 <- gam(cbcl_scr_syn_external_t ~sds_p_ss_sbd+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
summary(model1)
##plot income vs total comp
library(ggplot2)
dataset <- data.frame(x=test_sleep_all$sds_p_ss_sbd, y = model1$fitted.values, z = test_sleep_all$sds_p_ss_sbd)
ggplot(data = dataset, 
       aes(x = z, y = y, color = x)) +
  geom_point(size=0.1) +    geom_jitter(width = 0.1)+ geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3)) + 
  theme_classic(base_size=18) + scale_color_gradient(low="red", high="grey") + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + ylim(30,70)


model1 <- gam(cbcl_scr_syn_internal_t ~sds_p_ss_sbd+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
summary(model1)
##plot income vs total comp
library(ggplot2)
dataset <- data.frame(x=test_sleep_all$sds_p_ss_sbd, y = model1$fitted.values, z = test_sleep_all$sds_p_ss_sbd)
ggplot(data = dataset, 
       aes(x = z, y = y, color = x)) +
  geom_point(size=0.1) +    geom_jitter(width = 0.1)+ geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3)) + 
  theme_classic(base_size=18) + scale_color_gradient(low="red", high="grey") + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + ylim(30,70)


model1 <- gam(cbcl_scr_syn_anxdep_t ~sds_p_ss_sbd+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
summary(model1)
##plot income vs total comp
library(ggplot2)
dataset <- data.frame(x=test_sleep_all$sds_p_ss_sbd, y = model1$fitted.values, z = test_sleep_all$sds_p_ss_sbd)
ggplot(data = dataset, 
       aes(x = z, y = y, color = x)) +
  geom_point(size=0.1) +    geom_jitter(width = 0.1)+ geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3)) + 
  theme_classic(base_size=18) + scale_color_gradient(low="red", high="grey") + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + ylim(45,65)


model1 <- gam(cbcl_scr_syn_withdep_t ~sds_p_ss_sbd+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
summary(model1)
##plot income vs total comp
library(ggplot2)
dataset <- data.frame(x=test_sleep_all$sds_p_ss_sbd, y = model1$fitted.values, z = test_sleep_all$sds_p_ss_sbd)
ggplot(data = dataset, 
       aes(x = z, y = y, color = x)) +
  geom_point(size=0.1) +    geom_jitter(width = 0.1)+ geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3)) + 
  theme_classic(base_size=18) + scale_color_gradient(low="red", high="grey") + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + ylim(45,65)

model1 <- gam(cbcl_scr_syn_somatic_t ~sds_p_ss_sbd+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
summary(model1)
##plot income vs total comp
library(ggplot2)
dataset <- data.frame(x=test_sleep_all$sds_p_ss_sbd, y = model1$fitted.values, z = test_sleep_all$sds_p_ss_sbd)
ggplot(data = dataset, 
       aes(x = z, y = y, color = x)) +
  geom_point(size=0.1) +    geom_jitter(width = 0.1)+ geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3)) + 
  theme_classic(base_size=18) + scale_color_gradient(low="red", high="grey") + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + ylim(45,65)

model1 <- gam(cbcl_scr_syn_social_t ~sds_p_ss_sbd+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
summary(model1)
##plot income vs total comp
library(ggplot2)
dataset <- data.frame(x=test_sleep_all$sds_p_ss_sbd, y = model1$fitted.values, z = test_sleep_all$sds_p_ss_sbd)
ggplot(data = dataset, 
       aes(x = z, y = y, color = x)) +
  geom_point(size=0.1) +    geom_jitter(width = 0.1)+ geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3)) + 
  theme_classic(base_size=18) + scale_color_gradient(low="red", high="grey") + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + ylim(45,65)

model1 <- gam(cbcl_scr_syn_thought_t ~sds_p_ss_sbd+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
summary(model1)
##plot income vs total comp
library(ggplot2)
dataset <- data.frame(x=test_sleep_all$sds_p_ss_sbd, y = model1$fitted.values, z = test_sleep_all$sds_p_ss_sbd)
ggplot(data = dataset, 
       aes(x = z, y = y, color = x)) +
  geom_point(size=0.1) +    geom_jitter(width = 0.1)+ geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3)) + 
  theme_classic(base_size=18) + scale_color_gradient(low="red", high="grey") + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + ylim(45,65)


model1 <- gam(cbcl_scr_syn_attention_t ~sds_p_ss_sbd+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
summary(model1)
##plot income vs total comp
library(ggplot2)
dataset <- data.frame(x=test_sleep_all$sds_p_ss_sbd, y = model1$fitted.values, z = test_sleep_all$sds_p_ss_sbd)
ggplot(data = dataset, 
       aes(x = z, y = y, color = x)) +
  geom_point(size=0.1) +    geom_jitter(width = 0.1)+ geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3)) + 
  theme_classic(base_size=18) + scale_color_gradient(low="red", high="grey") + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + ylim(45,65)


model1 <- gam(cbcl_scr_syn_rulebreak_t ~sds_p_ss_sbd+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
summary(model1)
##plot income vs total comp
library(ggplot2)
dataset <- data.frame(x=test_sleep_all$sds_p_ss_sbd, y = model1$fitted.values, z = test_sleep_all$sds_p_ss_sbd)
ggplot(data = dataset, 
       aes(x = z, y = y, color = x)) +
  geom_point(size=0.1) +    geom_jitter(width = 0.1)+ geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3)) + 
  theme_classic(base_size=18) + scale_color_gradient(low="red", high="grey") + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + ylim(45,65)


model1 <- gam(cbcl_scr_syn_aggressive_t ~sds_p_ss_sbd+age+sex+race+bmipct+asthma+s(log_income)+s(education)+s(site, bs= "re"), data= test_sleep_all) ##identity for non-normal data (category)
summary(model1)
##plot income vs total comp
library(ggplot2)
dataset <- data.frame(x=test_sleep_all$sds_p_ss_sbd, y = model1$fitted.values, z = test_sleep_all$sds_p_ss_sbd)
ggplot(data = dataset, 
       aes(x = z, y = y, color = x)) +
  geom_point(size=0.1) +    geom_jitter(width = 0.1)+ geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3)) + 
  theme_classic(base_size=18) + scale_color_gradient(low="red", high="grey") + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + ylim(45,65)


