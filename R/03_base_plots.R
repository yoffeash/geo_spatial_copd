### base plots ###

## sample plotting counts of exacerbations by date
ggplot(data=remove_missing(exac_long_full, na.rm=TRUE), aes(x=quarter, y=percent_exac, color=clinic)) + stat_summary(fun.y = "sum", geom="smooth")