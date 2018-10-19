### clustering in STATCOPE ###

## clustering based on placebo exacerbation rate and location of center
preProc <- preProcess(exac_grouped_ninety_wide_stat,method=c("center","scale")) # center and scale data
exac_grouped_ninety_wide_stat_pre <- predict(preProc, exac_grouped_ninety_wide_stat)
exac_grouped_ninety_wide_stat_pre <- as.data.frame(exac_grouped_ninety_wide_stat_pre)

# hierarchical
set.seed(9)
x=as.matrix(exac_grouped_ninety_wide_stat_pre[1:10,3:15])
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
y=as.matrix(exac_grouped_ninety_wide_stat[1:10,3:15])
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

# fuzzy using DTW or kshape
# http://www.stat.unc.edu/faculty/pipiras/timeseries/Multivariate_6_-_Classification_Clustering_-_Menu.html#clustering_algorithms_(for_time_series_objects)
# http://www1.cs.columbia.edu/~jopa/Papers/PaparrizosSIGMOD2015.pdf
hc_DTW <- tsclust(x, type = "partition", k = 2, distance = "sbd", max.iter=9e+99, seed=9 )
plot(hc_DTW)
hc_DTW@cluster

