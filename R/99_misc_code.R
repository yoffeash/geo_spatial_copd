### miscl code ###

# count number of exacerbations in each month for each individual
# create a function
make_dummies <- function(v, prefix = '') {
  s <- sort(unique(v))
  d <- outer(v, s, function(v, s) 1L * (v == s))
  colnames(d) <- paste0(prefix, s)
  d
}

# add month variables
copd_pre4 <- copd_pre3 %>% 
  mutate_at(vars(contains("dateofexac")), .funs = funs(month = month(.)))

# bind the dummies to the original dataframe
copd_pre5 <- cbind(copd_pre4, 
                   make_dummies(copd_pre4$dateofexac1_month, prefix = 'exac1dummymonth'),
                   make_dummies(copd_pre4$dateofexac2_month, prefix = 'exac2dummymonth'),
                   make_dummies(copd_pre4$dateofexac3_month, prefix = 'exac3dummymonth'),
                   make_dummies(copd_pre4$dateofexac4_month, prefix = 'exac4dummymonth'),
                   make_dummies(copd_pre4$dateofexac5_month, prefix = 'exac5dummymonth'),
                   make_dummies(copd_pre4$dateofexac6_month, prefix = 'exac6dummymonth'),
                   make_dummies(copd_pre4$dateofexac7_month, prefix = 'exac7dummymonth'),
                   make_dummies(copd_pre4$dateofexac8_month, prefix = 'exac8dummymonth'),
                   make_dummies(copd_pre4$dateofexac9_month, prefix = 'exac9dummymonth'),
                   make_dummies(copd_pre4$dateofexac10_month, prefix = 'exac10dummymonth'),
                   make_dummies(copd_pre4$dateofexac11_month, prefix = 'exac11dummymonth'))

## export for apps
write_csv(exac_grouped_ninety, "app/data/exacerbations_grouped_ninety.csv")



# group by quarter and center
exac_grouped_quarter <- exac_long_full %>% 
  group_by(clinic, quarter) %>% 
  summarize(total_exac_percent = sum(percent_exac, na.rm=TRUE), mean_severity = mean(severity, na.rm=TRUE)) %>% 
  mutate(latitude = case_when(clinic=="A" ~ 39.2904, # add latitude
                              clinic=="B" ~ 33.5186,
                              clinic=="C" ~ 42.3601,
                              clinic=="D" ~ 39.7392,
                              clinic=="E" ~ 34.0522,
                              clinic=="F" ~ 44.9778,
                              clinic=="G" ~ 37.7749,
                              clinic=="H" ~ 42.2808,
                              clinic=="I" ~ 39.9526,
                              clinic=="J" ~ 40.4406)) %>% 
  mutate(longitude = case_when(clinic=="A" ~ -76.6122, # add longitude
                               clinic=="B" ~ -86.8104,
                               clinic=="C" ~ -71.0589,
                               clinic=="D" ~ -104.9903,
                               clinic=="E" ~ -118.2437,
                               clinic=="F" ~ -93.2650,
                               clinic=="G" ~ -122.4194,
                               clinic=="H" ~ -83.7430,
                               clinic=="I" ~ -75.1652,
                               clinic=="J" ~ -79.9959))

## sample code including kruskall wallis and boxplot for differences in ... (exacerbation rate, time to er, etc) by ... (season, center, region, etc)
kw_clinic <- baseline %>% select(rate_exacerb, clinic, season, timetoer, timetooff, Days_To_Onset1) %>% 
  mutate(rate_exacerb = as.numeric(rate_exacerb)) %>% 
  mutate(clinic = as.factor(clinic)) %>% 
  mutate(season = as.factor(season)) %>%  
  mutate(region_1 = case_when(clinic=="A" ~ "Mid-Atlantic", 
                              clinic=="B" ~ "South",
                              clinic=="C" ~ "Northeast",
                              clinic=="D" ~ "Mountain West",
                              clinic=="E" ~ "West Coast",
                              clinic=="F" ~ "Midwest",
                              clinic=="G" ~ "West Coast",
                              clinic=="H" ~ "Midwest",
                              clinic=="I" ~ "Northeast",
                              clinic=="J" ~ "Northeast")) %>% 
  mutate(region_1 = as.factor(region_1)) %>%
  drop_na()

kruskal.test(Days_To_Onset1 ~ season, data=subset(kw_clinic, region_1=="South" | region_1 == "West Coast" | region_1 == "Mountain West"))
kruskal.test(timetooff ~ season, data=subset(kw_clinic, region_1=="Northeast"))

stat.desc(kw_clinic$Days_To_Onset1)

histogram(~ rate_exacerb | clinic, data=kw_clinic)

ggplot(data=kw_clinic, aes(x=season, y=timetoer)) + 
  geom_boxplot() 
