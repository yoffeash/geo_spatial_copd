### import and manipulate data ###

# import data
baseline <- read_sas("data/raw_data/analysis_file_20121231.sas7bdat", 
         NULL)

copd_pre1 <- baseline %>% 
  mutate_at(vars(contains('Days_To_Onset')), funs(date_exac = (. + Rand_Date))) %>% # add days to onset to rand date to get sas date for each exacerbation
  select(ID, contains("Severity_EX"), contains("clinic"), Rand_Date, contains("_date_exac")) %>% # select only the severity, clinic and date information
  rename_at(vars(contains("Days_To_Onset")), # rename date of exacerbation variables
            funs(sub("Days_To_Onset","dateofexac",.))) %>%
  rename_at(vars(contains("_date_exac")), 
            funs(sub("_date_exac","",.))) %>% 
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

# split into date and severity datasets
exac_dates_wide <- copd_pre1 %>% select(ID, clinic, nclinic, latitude, longitude, dateofexac1:dateofexac11)
exac_severity_wide <- copd_pre1 %>% select(ID, clinic, nclinic, latitude, longitude, Severity_EX1:Severity_EX11)

# convert wide format date and severity datasets to long
exac_dates_long <- exac_dates_wide %>% gather(exacnumber, date, dateofexac1:dateofexac11)

# convert wide format date and severity datasets to long and rename exacnumber variable to exac1, exac2, etc....
exac_dates_long <- exac_dates_wide %>% 
  gather(exacnumber, date, dateofexac1:dateofexac11)
exac_dates_long$exacnumber <- str_replace_all(exac_dates_long$exacnumber, "dateofexac", "")

exac_severity_long <- exac_severity_wide %>% 
  gather(exacnumber, severity, Severity_EX1:Severity_EX11)
exac_severity_long$exacnumber <- str_replace_all(exac_severity_long$exacnumber, "Severity_EX", "")

# merge severity and date datasets to have long format dataset with date, center and exacnumber
exac_long <- left_join(exac_dates_long, exac_severity_long)

# convert dates to date and manipulate counts and rates of exacerbation by center
exac_long <- exac_long %>% 
  mutate(date = as.Date(date, origin='1960-01-01')) %>% 
  arrange(clinic) %>% # sort by date
  add_count(clinic) %>% # add a count by clinic
  rename(n_in_center = n) # rename n variable
  

# add list of all months between start (2006-03-17) and end (2010-04-02)
date_empty <- data.frame(date=seq(as.Date("2006-03-01"), as.Date("2010-04-30"), by="days"))

# merge empty date dataset with exac_long dataset
exac_long_full <- left_join(date_empty, exac_long)

# add quarter, month and week breaks
exac_long_full$quarter <- as.Date(cut(exac_long_full$date, breaks = "quarter"))
exac_long_full$month <- as.Date(cut(exac_long_full$date, breaks = "month"))
exac_long_full$week <- as.Date(cut(exac_long_full$date, breaks = "week"))
exac_long_full$ninetyday <- as.Date(cut(exac_long_full$date, breaks = "90 days"))

# proportional exacerbation
exac_long_full <- exac_long_full %>% 
  mutate(exacyesno = ifelse(is.na(severity),0,1)) %>% # add marker variable for exacerbation
  mutate(percent_exac = exacyesno/n_in_center*100) %>% 
  mutate(percent_exac = ifelse(is.na(percent_exac),0,percent_exac)) # add percentage of that center with exacerbations on that day

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

# group by 90 day increments and center
exac_grouped_ninety <- exac_long_full %>% 
  group_by(clinic, ninetyday) %>% 
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

