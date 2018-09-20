### differences in regions by baseline characteristics ###

pairwise <- list(c("Midwest","Northeast"), c("Northeast","South and West"), c("Midwest","South and West"))

## age 
ggplot(data=copd_region, aes(x=region_2, y=age)) + geom_boxplot() + stat_compare_means() + stat_compare_means(comparisons = pairwise)

## gold stage
ggplot(data=copd_region, aes(x=region_2, y=goldclass)) + geom_boxplot() + stat_compare_means() + stat_compare_means(comparisons = pairwise)

## exacerbation rate
ggplot(data=copd_region, aes(x=region_2, y=rate_exacerb)) + geom_boxplot() + stat_compare_means() + stat_compare_means(comparisons = pairwise) + ylim(0,15)

## smoking status
smok_region_table <- table(copd_region$region_2,copd_region$nowsmk)
chisq.test(smok_region_table)
smok_region_table

smok_center_table <- table(copd_region$clinic_name,copd_region$nowsmk)
chisq.test(smok_center_table)
smok_center_table
