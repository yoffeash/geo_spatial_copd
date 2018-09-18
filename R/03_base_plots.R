### base plots ###

## sample plotting counts of exacerbations by date *** note that this is not adjusted for enrollment issues and should not be used in its current form
ggplot(data=remove_missing(exac_long_full, na.rm=TRUE), aes(x=quarter, y=percent_exac, color=clinic)) + stat_summary(fun.y = "sum", geom="smooth")