### base plots ###

### by date and center ###

## sample plotting counts of exacerbations by date and center - entire cohort
ggplot(data=remove_missing(exac_grouped_ninety, na.rm=TRUE), aes(x=ninetyday, y=total_exac_percent, color=clinic_name)) + 
  geom_smooth(se=FALSE) +
  xlab("Date") + ylab("Percent of Center Reporting an Exacerbation") +
  labs(colour="Center") +
  ggtitle("Exacerbation Rate by Center - Entire Cohort")

## sample plotting counts of exacerbations by date and center - placebo
ggplot(data=remove_missing(exac_grouped_ninety_placebo, na.rm=TRUE), aes(x=ninetyday, y=total_exac_percent, color=clinic_name)) + 
  geom_smooth(se=FALSE) +
  xlab("Date") + ylab("Percent of Center Reporting an Exacerbation") +
  labs(colour="Center") +
  ggtitle("Exacerbation Rate by Center - Placebo Group")

## sample plotting counts of exacerbations by date and center - azithro
ggplot(data=remove_missing(exac_grouped_ninety_azithro, na.rm=TRUE), aes(x=ninetyday, y=total_exac_percent, color=clinic_name)) + 
  geom_smooth(se=FALSE) +
  xlab("Date") + ylab("Percent of Center Reporting an Exacerbation") +
  labs(colour="Center") +
  ggtitle("Exacerbation Rate by Center - Treatment Group")

### by date and region ###

ggplot(data=remove_missing(exac_grouped_ninety, na.rm=TRUE), aes(x=ninetyday, y=total_exac_percent, color=region_2)) + 
  geom_smooth(se=FALSE) +
  xlab("Date") + ylab("Percent of Region Reporting an Exacerbation") +
  labs(colour="Center") +
  ggtitle("Exacerbation Rate by Center - Entire Cohort")

## sample plotting counts of exacerbations by date and center - placebo
ggplot(data=remove_missing(exac_grouped_ninety_placebo, na.rm=TRUE), aes(x=ninetyday, y=total_exac_percent, color=region_2)) + 
  geom_smooth(se=FALSE) +
  xlab("Date") + ylab("Percent of Region Reporting an Exacerbation") +
  labs(colour="Center") +
  ggtitle("Exacerbation Rate by Center - Placebo Group")

## sample plotting counts of exacerbations by date and center - azithro
ggplot(data=remove_missing(exac_grouped_ninety_azithro, na.rm=TRUE), aes(x=ninetyday, y=total_exac_percent, color=region_2)) + 
  geom_smooth(se=FALSE) +
  xlab("Date") + ylab("Percent of Region Reporting an Exacerbation") +
  labs(colour="Center") +
  ggtitle("Exacerbation Rate by Center - Treatment Group")
