### descriptive statistics and differences in them by regions (using "region_2" variable) ###

## difference in regions based on age ##
age_aov <- aov(age ~ region_cluster_2, data=copd_region)
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

#############################################################################################################################

### descriptive statistics and differences in them by regions (using "region_cluster_2" variable) ###

## difference in regions based on age ##
age_aov <- aov(age ~ region_cluster_2, data=copd_region)
summary(age_aov)

t.test(copd_region$age~copd_region$region_cluster_2)

aggregate(age ~ region_cluster_2, copd_region, mean)

## difference in region based on GOLD stage ##
region_gold_table <- table(copd_region$region_cluster_2, copd_region$goldclass)
region_gold_table
chisq.test(region_gold_table)
CrossTable(copd_region$region_cluster_2, copd_region$goldclass)

## difference in region based smoking ##
region_smok_table <- table(copd_region$region_cluster_2, copd_region$nowsmk)
region_smok_table
chisq.test(region_smok_table)
CrossTable(copd_region$region_cluster_2, copd_region$nowsmk)

## difference in number of exacerbations 
t.test(copd_region$Number_Exacerbs~copd_region$region_cluster_h_1)

## difference in exacerbation rate by cluster
exac_rate_cluster <- exac_grouped_ninety_placebo_wide_clustered %>% 
  gather(period, rate, '2006-08-28':'2009-08-12') %>% 
  dplyr::group_by(cluster_label) 

exac_rate_cluster_summary <- exac_rate_cluster %>% dplyr::summarise(mean_exac_rate = mean(rate))

t.test(exac_rate_cluster$rate~exac_rate_cluster$cluster_label)

## difference in prior exacerbation by cluster
region_priorexac_table <- table(copd_region$region_cluster_2, copd_region$priorexac)
region_priorexac_table
chisq.test(region_priorexac_table)
CrossTable(copd_region$region_cluster_2, copd_region$priorexac)

## difference in oxygen use by cluster
region_oxygen_table <- table(copd_region$region_cluster_2, copd_region$oxygen)
region_oxygen_table
chisq.test(region_oxygen_table)
CrossTable(copd_region$region_cluster_2, copd_region$oxygen)