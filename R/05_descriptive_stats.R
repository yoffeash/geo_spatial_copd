### descriptive statistics and differences in them by regions (using "region_2" variable) ###

## difference in regions based on age ##
age_aov <- aov(age ~ region_2, data=copd_region)
summary(age_aov)

pairwise.t.test(copd_region$age, copd_region$region_2, p.adjust.method = "bonferroni")

aggregate(age ~ region_2, copd_region, mean)

## difference in region based on GOLD stage ##
region_gold_table <- table(copd_region$region_2, copd_region$goldclass)
region_gold_table
chisq.test(region_gold_table)
CrossTable(copd_region$region_2, copd_region$goldclass)

## difference in region based smoking ##
region_smok_table <- table(copd_region$region_2, copd_region$nowsmk)
region_smok_table
chisq.test(region_smok_table)
CrossTable(copd_region$region_2, copd_region$nowsmk)


t.test(copd_region$Number_Exacerbs~copd_region$region_cluster_2)
