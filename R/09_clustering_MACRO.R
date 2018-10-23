### ojective clustering ###

## clustering based on placebo exacerbation rate and location of center
#exac_grouped_ninety_placebo_wide[sapply(exac_grouped_ninety_placebo_wide, is.numeric)] <- 
#  lapply(exac_grouped_ninety_placebo_wide[sapply(exac_grouped_ninety_placebo_wide, is.numeric)], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)) # remove missing rate values and replace with mean
preProc <- preProcess(exac_grouped_ninety_placebo_wide,method=c("center","scale")) # center and scale data
exac_grouped_ninety_placebo_wide_pre <- predict(preProc, exac_grouped_ninety_placebo_wide)
exac_grouped_ninety_placebo_wide_pre <- as.data.frame(exac_grouped_ninety_placebo_wide_pre)

# kmeans
set.seed(9)
km = kmeans(exac_grouped_ninety_placebo_wide_pre[,5:17],2)
exac_grouped_ninety_placebo_wide_clustered <- exac_grouped_ninety_placebo_wide
exac_grouped_ninety_placebo_wide_clustered$cluster <- as.factor(km$cluster)
exac_grouped_ninety_placebo_wide_clustered = exac_grouped_ninety_placebo_wide_clustered %>% mutate(cluster_label = ifelse(cluster=="1","A", ifelse(cluster=="2","B","C")))

# hierarchical
set.seed(9)
x=as.matrix(exac_grouped_ninety_placebo_wide_pre[,5:17])
dd=as.dist(1-cor(t(x)))
hc.complete <- hclust(dd, method="complete")
plot(hc.complete, main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")
cutree(hc.complete, 2)
hc.average <- hclust(dd, method="average")
plot(hc.average, main="Average Linkage with Correlation-Based Distance", xlab="", sub="")
cutree(hc.average, 2)
hc.single <- hclust(dd, method="single")
plot(hc.single, main="Single Linkage with Correlation-Based Distance", xlab="", sub="")
cutree(hc.single, 2)

# hierarchical without preprocessing
set.seed(9)
y=as.matrix(exac_grouped_ninety_placebo_wide[,5:17])
dd=as.dist(1-cor(t(y)))
hc.complete <- hclust(dd, method="complete")
plot(hc.complete, main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")
cutree(hc.complete, 2)
hc.average <- hclust(dd, method="average")
plot(hc.average, main="Average Linkage with Correlation-Based Distance", xlab="", sub="")
cutree(hc.average, 2)
hc.single <- hclust(dd, method="single")
plot(hc.single, main="Single Linkage with Correlation-Based Distance", xlab="", sub="")
cutree(hc.single, 3)

# fuzzy using DTW
# http://www.stat.unc.edu/faculty/pipiras/timeseries/Multivariate_6_-_Classification_Clustering_-_Menu.html#clustering_algorithms_(for_time_series_objects)
hc_DTW <- tsclust(x, type = "fuzzy", k = 2, distance = "dtw", seed=9)
plot(hc_DTW)
hc_DTW@cluster



## clustering based on exacerbation rate in both placebo and intervention arms and location of center
#exac_grouped_ninety_placebo_wide[sapply(exac_grouped_ninety_placebo_wide, is.numeric)] <- 
#  lapply(exac_grouped_ninety_placebo_wide[sapply(exac_grouped_ninety_placebo_wide, is.numeric)], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)) # remove missing rate values and replace with mean
preProc <- preProcess(exac_grouped_ninety_wide,method=c("center","scale")) # center and scale data
exac_grouped_ninety_wide_pre <- predict(preProc, exac_grouped_ninety_wide)
exac_grouped_ninety_wide_pre <- as.data.frame(exac_grouped_ninety_wide_pre)

# kmeans
set.seed(9)
km = kmeans(exac_grouped_ninety_wide_pre[1:10,6:15],2)

# hierarchical
set.seed(9)
x=as.matrix(exac_grouped_ninety_wide_pre[1:10,6:15])
dd=as.dist(1-cor(t(x)))
hc.complete <- hclust(dd, method="complete")
plot(hc.complete, main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")
cutree(hc.complete, 2)
hc.average <- hclust(dd, method="average")
plot(hc.average, main="Average Linkage with Correlation-Based Distance", xlab="", sub="")
cutree(hc.average, 2)
hc.single <- hclust(dd, method="single")
plot(hc.single, main="Single Linkage with Correlation-Based Distance", xlab="", sub="")
cutree(hc.single, 2)

# hierarchical without preprocessing
set.seed(9)
y=as.matrix(exac_grouped_placebo_wide[1:10,6:15])
dd=as.dist(1-cor(t(y)))
hc.complete <- hclust(dd, method="complete")
plot(hc.complete, main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")
cutree(hc.complete, 2)
hc.average <- hclust(dd, method="average")
plot(hc.average, main="Average Linkage with Correlation-Based Distance", xlab="", sub="")
cutree(hc.average, 2)
hc.single <- hclust(dd, method="single")
plot(hc.single, main="Single Linkage with Correlation-Based Distance", xlab="", sub="")
cutree(hc.single, 3)

# fuzzy using DTW
# http://www.stat.unc.edu/faculty/pipiras/timeseries/Multivariate_6_-_Classification_Clustering_-_Menu.html#clustering_algorithms_(for_time_series_objects)
hc_DTW <- tsclust(x, type = "fuzzy", k = 2, distance = "dtw", seed=9)
plot(hc_DTW)
hc_DTW@cluster