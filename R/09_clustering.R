### ojective clustering ###

## clustering based on placebo exacerbation rate and location of center
#exac_grouped_ninety_placebo_wide[sapply(exac_grouped_ninety_placebo_wide, is.numeric)] <- 
#  lapply(exac_grouped_ninety_placebo_wide[sapply(exac_grouped_ninety_placebo_wide, is.numeric)], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)) # remove missing rate values and replace with mean
preProc <- preProcess(exac_grouped_ninety_placebo_wide,method=c("center","scale")) # center and scale data
exac_grouped_ninety_placebo_wide_pre <- predict(preProc, exac_grouped_ninety_placebo_wide)
exac_grouped_ninety_placebo_wide_pre <- as.data.frame(exac_grouped_ninety_placebo_wide_pre)

set.seed(9)
km = kmeans(exac_grouped_ninety_placebo_wide_pre[,5:17],2)
exac_grouped_ninety_placebo_wide_clustered <- exac_grouped_ninety_placebo_wide
exac_grouped_ninety_placebo_wide_clustered$cluster <- as.factor(km$cluster)
exac_grouped_ninety_placebo_wide_clustered = exac_grouped_ninety_placebo_wide_clustered %>% mutate(cluster_label = ifelse(cluster=="1","B", ifelse(cluster=="2","C","A")))
