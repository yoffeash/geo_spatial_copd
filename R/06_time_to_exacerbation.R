### time to exacerbation ###

################################## univariate by region - any exacerbation, any season ##################################

## entire cohort
exac_fit_regional <- survfit(Surv(exacerbfolltime, exacerb) ~ region_2, data=copd_region)
exac_curv_regional <- ggsurvplot(exac_fit_regional, data=copd_region, pval = TRUE)
exac_curv_regional

### by season - any exacerbation, any region ###

## entire cohort
exac_fit_seasonal <- survfit(Surv(exacerbfolltime, exacerb) ~ season_name, data=copd_region)
exac_curv_seasonal <- ggsurvplot(exac_fit_seasonal, data=copd_region, pval = TRUE)
exac_curv_seasonal

### by season - any exacerbation, northeast ###

## entire cohort
exac_fit_seasonal_ne <- survfit(Surv(exacerbfolltime, exacerb) ~ season_name, data=subset(copd_region, region_2=="Northeast"))
exac_curv_seasonal_ne <- ggsurvplot(exac_fit_seasonal_ne, data=subset(copd_region, region_2=="Northeast" & trtgroup_label=="placebo"), pval = TRUE)
exac_curv_seasonal_ne

## placebo
exac_fit_seasonal_ne_placebo <- survfit(Surv(exacerbfolltime, exacerb) ~ season_name, data=subset(copd_region, region_2=="Northeast" & trtgroup_label=="placebo"))
exac_curv_seasonal_ne_placebo <- ggsurvplot(exac_fit_seasonal_ne_placebo, data=subset(copd_region, region_2=="Northeast" & trtgroup_label=="placebo"), pval = TRUE)
exac_curv_seasonal_ne_placebo

## azithro
exac_fit_seasonal_ne_azithro <- survfit(Surv(exacerbfolltime, exacerb) ~ season_name, data=subset(copd_region, region_2=="Northeast" & trtgroup_label=="azithro"))
exac_curv_seasonal_ne_azithro <- ggsurvplot(exac_fit_seasonal_ne_azithro, data=subset(copd_region, region_2=="Northeast" & trtgroup_label=="azithro"), pval = TRUE)
exac_curv_seasonal_ne_azithro

################################## univariate effect of drug by region ##################################

## entire cohort
exac_fit_drug <- survfit(Surv(exacerbfolltime, exacerb) ~ trtgroup_label, data=copd_region)
exac_curv_drug <- ggsurvplot(exac_fit_drug, data=copd_region, pval = TRUE)
exac_curv_drug

## northeast
exac_fit_drug_ne <- survfit(Surv(exacerbfolltime, exacerb) ~ trtgroup_label, data=subset(copd_region, region_2=="Northeast"))
exac_curv_drug_ne <- ggsurvplot(exac_fit_drug_ne, data=subset(copd_region, region_2=="Northeast"), pval = TRUE)
exac_curv_drug_ne

## south and west
exac_fit_drug_sw <- survfit(Surv(exacerbfolltime, exacerb) ~ trtgroup_label, data=subset(copd_region, region_2=="South and West"))
exac_curv_drug_sw <- ggsurvplot(exac_fit_drug_sw, data=subset(copd_region, region_2=="South and West"), pval = TRUE)
exac_curv_drug_sw

## midwest
exac_fit_drug_mw <- survfit(Surv(exacerbfolltime, exacerb) ~ trtgroup_label, data=subset(copd_region, region_2=="Midwest"))
exac_curv_drug_mw <- ggsurvplot(exac_fit_drug_mw, data=subset(copd_region, region_2=="Midwest"), pval = TRUE)
exac_curv_drug_mw

###############################multivariable effect by region##########################################
copd_region$region_2_f <- as.factor(copd_region$region_2)
contrasts(copd_region$region_2_f) <- contr.treatment(3, base=1)
timeto_exac_multi_drug <- coxph(Surv(exacerbfolltime, exacerb) ~ age + black + gender + sympF_C00 + activity_C00 + impactF_C00 +
                                  + goldclass + nowsmk +
                           trtgroup_label + region_2_f + trtgroup_label*region_2_f, data=copd_region)
summary(timeto_exac_multi_drug)


