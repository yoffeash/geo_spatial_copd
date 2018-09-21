##### exacerbation rate #####

copd_region_num <- copd_region %>% 
  filter(time_on_study > 0) %>% 
  mutate(l_time_on_study = log(time_on_study)) %>% 
  mutate(l_years_on_study = log(years_on_study))

### univariate ###
# entire cohort
rate_exac_drug <- zeroinfl(Number_Exacerbs ~ trtgroup_label | trtgroup_label, 
                              data=copd_region_num, dist="negbin", offset=l_time_on_study, EM=TRUE)
summary(rate_exac_drug)

rate_exac_drug <- glm.nb(rate_exacerb ~ trtgroup_label, 
                            data=copd_region_num)
summary(rate_exac_drug)

rate_exac_drug_plot <- ggplot(data=copd_region, aes(x=rate_exacerb,fill=trtgroup_label)) + geom_histogram(position="dodge") + xlim(0,20)
rate_exac_drug_plot

# northeast
rate_exac_drug_ne <- zeroinfl(Number_Exacerbs ~ trtgroup_label | trtgroup_label, 
                                 data=subset(copd_region_num, region_2 == "Northeast"), dist="negbin", offset=l_time_on_study, EM=TRUE)
summary(rate_exac_drug_ne)

rate_exac_drug_ne <- glm.nb(rate_exacerb ~ trtgroup_label, 
                              data=subset(copd_region_num, region_2 == "Northeast"))
summary(rate_exac_drug_ne)

# midwest
rate_exac_drug_mw <- zeroinfl(Number_Exacerbs ~ trtgroup_label | trtgroup_label, 
                              data=subset(copd_region_num, region_2 == "Midwest"), dist="negbin", offset=l_time_on_study, EM=TRUE)
summary(rate_exac_drug_mw)

rate_exac_drug_mw <- glm.nb(rate_exacerb ~ trtgroup_label, 
                            data=subset(copd_region_num, region_2 == "Midwest"))
summary(rate_exac_drug_mw)

# south and west
rate_exac_drug_sw <- zeroinfl(Number_Exacerbs ~ trtgroup_label | trtgroup_label, 
                              data=subset(copd_region_num, region_2 == "South and West"), dist="negbin", offset=l_time_on_study, EM=TRUE)
summary(rate_exac_drug_sw)

rate_exac_drug_sw <- glm.nb(rate_exacerb ~ trtgroup_label, 
                            data=subset(copd_region_num, region_2 == "South and West"))
summary(rate_exac_drug_sw)


### multivariable rate with interaction term for region and treatment
rate_exac_multi_drug <- zeroinfl(Number_Exacerbs ~ age + goldclass + nowsmk +
                                   trtgroup_label + region_2_f + trtgroup_label*region_2_f | trtgroup_label, 
                                 data=copd_region_num, dist="negbin", offset=l_time_on_study, EM=TRUE)
summary(rate_exac_multi_drug)

rate_exac_multi_drug <- glm.nb(rate_exacerb ~ age + black + gender + sympF_C00 + activity_C00 + impactF_C00 + goldclass + nowsmk +
                                 trtgroup_label + region_2_f + trtgroup_label*region_2_f, 
                         data=copd_region_num)
summary(rate_exac_multi_drug)
