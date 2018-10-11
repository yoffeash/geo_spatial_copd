### time to exacerbation ###

################################## univariate by region - any exacerbation, any season ##################################

## entire cohort
exac_fit_regional <- survfit(Surv(exacerbfolltime, exacerb) ~ region_2, data=copd_region)
exac_curv_regional <- ggsurvplot(exac_fit_regional, data=copd_region, pval = TRUE)
exac_curv_regional

### by season - any exacerbation, any region ###

## entire cohort
exac_fit_seasonal <- survfit(Surv(exacerbfolltime, exacerb) ~ season_name, data=copd_region)
exac_curv_seasonal <- ggsurvplot(exac_fit_seasonal, data=copd_region, pval = TRUE, risk.table=TRUE)
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
exac_curv_drug <- ggsurvplot(exac_fit_drug, data=copd_region, 
                             pval = TRUE, risk.table = TRUE, risk.table.y.text = FALSE,
                             legend.labs = c("Azithro","Placebo"), risk.table.height = 0.25, 
                             xlim= c(0,365))
exac_curv_drug$plot <- exac_curv_drug$plot + labs(title = "Entire Cohort")
exac_curv_drug$plot <- ggpar(exac_curv_drug$plot, font.title = "bold")


## northeast
exac_fit_drug_ne <- survfit(Surv(exacerbfolltime, exacerb) ~ trtgroup_label, data=subset(copd_region, region_2=="Northeast"))
exac_curv_drug_ne <- ggsurvplot(exac_fit_drug_ne, data=subset(copd_region, region_2=="Northeast"), pval = TRUE,
                                risk.table = TRUE, risk.table.y.text = FALSE,
                                legend.labs = c("Azithro","Placebo"), risk.table.height = 0.25, 
                                xlim= c(0,365))
exac_curv_drug_ne$plot <- exac_curv_drug_ne$plot + labs(title = "Northeast")
exac_curv_drug_ne$plot <- ggpar(exac_curv_drug_ne$plot, font.title = "bold")


## south and west
exac_fit_drug_sw <- survfit(Surv(exacerbfolltime, exacerb) ~ trtgroup_label, data=subset(copd_region, region_2=="South and West"))
exac_curv_drug_sw <- ggsurvplot(exac_fit_drug_sw, data=subset(copd_region, region_2=="South and West"), pval = TRUE,
                                risk.table = TRUE, risk.table.y.text = FALSE,
                                legend.labs = c("Azithro","Placebo"), risk.table.height = 0.25, 
                                xlim= c(0,365))
exac_curv_drug_sw$plot <- exac_curv_drug_sw$plot + labs(title = "South and West")
exac_curv_drug_sw$plot <- ggpar(exac_curv_drug_sw$plot, font.title = "bold")


## midwest
exac_fit_drug_mw <- survfit(Surv(exacerbfolltime, exacerb) ~ trtgroup_label, data=subset(copd_region, region_2=="Midwest"))
exac_curv_drug_mw <- ggsurvplot(exac_fit_drug_mw, data=subset(copd_region, region_2=="Midwest"), pval = TRUE,
                                risk.table = TRUE, risk.table.y.text = FALSE,
                                legend.labs = c("Azithro","Placebo"), risk.table.height = 0.25, 
                                xlim= c(0,365))
exac_curv_drug_mw$plot <- exac_curv_drug_mw$plot + labs(title = "Midwest")
exac_curv_drug_mw$plot <- ggpar(exac_curv_drug_mw$plot, font.title = "bold")


exac_splots_regional <- list()
exac_splots_regional[[1]] <- exac_curv_drug
exac_splots_regional[[2]] <- exac_curv_drug_mw
exac_splots_regional[[3]] <- exac_curv_drug_ne
exac_splots_regional[[4]] <- exac_curv_drug_sw
arrange_ggsurvplots(exac_splots_regional, print=TRUE, ncol=2, nrow=2)

###############################multivariable effect by region##########################################
contrasts(copd_region$region_2_f) <- contr.treatment(3, base=1) # midwest as reference group
timeto_exac_multi_drug <- coxph(Surv(exacerbfolltime, exacerb) ~ age + black + gender + sympF_C00 + activity_C00 + impactF_C00 +
                                  + goldclass + nowsmk + latitude +
                           trtgroup_label + region_2_f + trtgroup_label*region_2_f, data=copd_region)
summary(timeto_exac_multi_drug)


#######################################################################By objective Clustering#######################################################################
## cluster a - note as currently structured only using 2 cluster solution 
exac_fit_drug_cluster_a <- survfit(Surv(exacerbfolltime, exacerb) ~ trtgroup_label, data=subset(copd_region, region_cluster_2=="Cluster A"))
exac_curve_drug_cluster_a <- ggsurvplot(exac_fit_drug_cluster_a, data=subset(copd_region, region_cluster_2_f=="Cluster A"), pval = TRUE,
                                risk.table = TRUE, risk.table.y.text = FALSE,
                                legend.labs = c("Azithro","Placebo"), risk.table.height = 0.25, 
                                xlim= c(0,365))
exac_curve_drug_cluster_a$plot <- exac_curve_drug_cluster_a$plot + labs(title = "Cluster A")
exac_curve_drug_cluster_a$plot <- ggpar(exac_curve_drug_cluster_a$plot, font.title = "bold") + ylab("Proportion without Exacerbation")
exac_curve_drug_cluster_a


## cluster b
exac_fit_drug_cluster_b <- survfit(Surv(exacerbfolltime, exacerb) ~ trtgroup_label, data=subset(copd_region, region_cluster_2=="Cluster B"))
exac_curve_drug_cluster_b <- ggsurvplot(exac_fit_drug_cluster_b, data=subset(copd_region, region_cluster_2_f=="Cluster B"), pval = TRUE,
                                        risk.table = TRUE, risk.table.y.text = FALSE,
                                        legend.labs = c("Azithro","Placebo"), risk.table.height = 0.25, 
                                        xlim= c(0,365))
exac_curve_drug_cluster_b$plot <- exac_curve_drug_cluster_b$plot + labs(title = "Cluster B") + ylab("Proportion without Exacerbation")
exac_curve_drug_cluster_b$plot <- ggpar(exac_curve_drug_cluster_b$plot, font.title = "bold")
exac_curve_drug_cluster_b

exac_splots_regional_cluster <- list()
exac_splots_regional_cluster[[1]] <- exac_curve_drug_cluster_a
exac_splots_regional_cluster[[2]] <- exac_curve_drug_cluster_b
arrange_ggsurvplots(exac_splots_regional_cluster, print=TRUE, ncol=2, nrow=1)


###############################multivariable effect by cluster##########################################
contrasts(copd_region$region_cluster_2_f) <- contr.treatment(2, base=1)
timeto_exac_multi_drug_cluster <- coxph(Surv(exacerbfolltime, exacerb) ~ age + black + gender + sympF_C00 + activity_C00 + impactF_C00 +
                                  goldclass + nowsmk + latitude + priorexac +
                                  trtgroup_label + region_cluster_2_f + trtgroup_label*region_cluster_2_f, data=copd_region)
summary(timeto_exac_multi_drug_cluster)


