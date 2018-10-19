## time to exacerbation by cluster in STATCOPE ##

exac_fit_drug_cluster_a <- survfit(Surv(days_to_ex_1, exacerb) ~ STAGROUP, data=subset(stat_pre1, region_cluster_h_1=="Cluster A"))
exac_curve_drug_cluster_a <- ggsurvplot(exac_fit_drug_cluster_a, data=subset(stat_pre1, region_cluster_h_1=="Cluster A"), pval = TRUE,
                                        risk.table = TRUE, risk.table.y.text = FALSE,
                                        risk.table.height = 0.25, 
                                        xlim= c(0,365))
exac_curve_drug_cluster_a$plot <- exac_curve_drug_cluster_a$plot + labs(title = "Cluster A") + ylab("Proportion without Exacerbation")
exac_curve_drug_cluster_a$plot <- ggpar(exac_curve_drug_cluster_a$plot, font.title = "bold")
exac_curve_drug_cluster_a

exac_fit_drug_cluster_b <- survfit(Surv(days_to_ex_1, exacerb) ~ STAGROUP, data=subset(stat_pre1, region_cluster_h_1=="Cluster B"))
exac_curve_drug_cluster_b <- ggsurvplot(exac_fit_drug_cluster_b, data=subset(stat_pre1, region_cluster_h_1=="Cluster B"), pval = TRUE,
                                        risk.table = TRUE, risk.table.y.text = FALSE,
                                        risk.table.height = 0.25, 
                                        xlim= c(0,365))
exac_curve_drug_cluster_b$plot <- exac_curve_drug_cluster_b$plot + labs(title = "Cluster B") + ylab("Proportion without Exacerbation")
exac_curve_drug_cluster_b$plot <- ggpar(exac_curve_drug_cluster_b$plot, font.title = "bold")
exac_curve_drug_cluster_b

exac_fit_drug_cluster_c <- survfit(Surv(days_to_ex_1, exacerb) ~ STAGROUP, data=subset(stat_pre1, region_cluster_h_1=="Cluster C"))
exac_curve_drug_cluster_c <- ggsurvplot(exac_fit_drug_cluster_c, data=subset(stat_pre1, region_cluster_h_1=="Cluster C"), pval = TRUE,
                                        risk.table = TRUE, risk.table.y.text = FALSE,
                                        risk.table.height = 0.25, 
                                        xlim= c(0,365))
exac_curve_drug_cluster_c$plot <- exac_curve_drug_cluster_c$plot + labs(title = "Cluster C") + ylab("Proportion without Exacerbation")
exac_curve_drug_cluster_c$plot <- ggpar(exac_curve_drug_cluster_c$plot, font.title = "bold")
exac_curve_drug_cluster_c


t.test(stat_pre1$Number_EX~stat_pre1$region_cluster_h_1)
