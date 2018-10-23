### import and manipulate data from MACRO ###

#################################################################################################################################################################
############################################ initial import and prepping of data - CRN Azithro Study Data Only ##################################################
#################################################################################################################################################################

# import data
baseline <- read_sas("data/raw_data/analysis_file_20121231.sas7bdat", 
         NULL)

#################################################################################################################################################################
################################### Manipulation to Create Center/Region specific Plots including Maps and Plots over Time ######################################
#################################################################################################################################################################

copd_pre1 <- baseline %>% 
  filter(time_on_study > 0) %>% 
  mutate_at(vars(contains('Days_To_Onset')), funs(date_exac = (. + Rand_Date))) %>% # add days to onset to rand date to get sas date for each exacerbation
  dplyr::select(ID, contains("Severity_EX"), contains("clinic"), Rand_Date, trtgroup, contains("_date_exac")) %>% # select only the severity, clinic and date information
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
exac_dates_wide <- copd_pre1 %>% dplyr::select(ID, clinic, nclinic, latitude, longitude, trtgroup, dateofexac1:dateofexac11)
exac_severity_wide <- copd_pre1 %>% dplyr::select(ID, clinic, nclinic, latitude, longitude, trtgroup, Severity_EX1:Severity_EX11)

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
  arrange(date) # sort by date


###### calculating monthly enrollment - very inelegant solution ####
enrollment_by_month_pre1 <- baseline %>% 
  filter(Days_In_Study > 0) %>% 
  dplyr::select(ID,Rand_Date,Days_In_Study,clinic) %>% 
  mutate(EndDate = Rand_Date + Days_In_Study) %>% 
  mutate(date_randomized = as.Date(Rand_Date, origin='1960-01-01')) %>% 
  mutate(date_end = as.Date(EndDate, origin='1960-01-01')) %>% 
  
  mutate(enrolled_2006_03_01 = ifelse(date_randomized<as.Date('2006-04-01') & date_end>=as.Date('2006-03-01'),1,0)) %>% 
  mutate(enrolled_2006_04_01 = ifelse(date_randomized<as.Date('2006-05-01') & date_end>=as.Date('2006-04-01'),1,0)) %>%
  mutate(enrolled_2006_05_01 = ifelse(date_randomized<as.Date('2006-06-01') & date_end>=as.Date('2006-05-01'),1,0)) %>%
  mutate(enrolled_2006_06_01 = ifelse(date_randomized<as.Date('2006-07-01') & date_end>=as.Date('2006-06-01'),1,0)) %>% 
  mutate(enrolled_2006_07_01 = ifelse(date_randomized<as.Date('2006-08-01') & date_end>=as.Date('2006-07-01'),1,0)) %>% 
  mutate(enrolled_2006_08_01 = ifelse(date_randomized<as.Date('2006-09-01') & date_end>=as.Date('2006-08-01'),1,0)) %>% 
  mutate(enrolled_2006_09_01 = ifelse(date_randomized<as.Date('2006-10-01') & date_end>=as.Date('2006-09-01'),1,0)) %>% 
  mutate(enrolled_2006_10_01 = ifelse(date_randomized<as.Date('2006-11-01') & date_end>=as.Date('2006-10-01'),1,0)) %>% 
  mutate(enrolled_2006_11_01 = ifelse(date_randomized<as.Date('2006-12-01') & date_end>=as.Date('2006-11-01'),1,0)) %>%
  mutate(enrolled_2006_12_01 = ifelse(date_randomized<as.Date('2007-01-01') & date_end>=as.Date('2006-12-01'),1,0)) %>%
  
  mutate(enrolled_2007_01_01 = ifelse(date_randomized<as.Date('2007-02-01') & date_end>=as.Date('2007-01-01'),1,0)) %>% 
  mutate(enrolled_2007_02_01 = ifelse(date_randomized<as.Date('2007-03-01') & date_end>=as.Date('2007-02-01'),1,0)) %>% 
  mutate(enrolled_2007_03_01 = ifelse(date_randomized<as.Date('2007-04-01') & date_end>=as.Date('2007-03-01'),1,0)) %>% 
  mutate(enrolled_2007_04_01 = ifelse(date_randomized<as.Date('2007-05-01') & date_end>=as.Date('2007-04-01'),1,0)) %>%
  mutate(enrolled_2007_05_01 = ifelse(date_randomized<as.Date('2007-06-01') & date_end>=as.Date('2007-05-01'),1,0)) %>% 
  mutate(enrolled_2007_06_01 = ifelse(date_randomized<as.Date('2007-07-01') & date_end>=as.Date('2007-06-01'),1,0)) %>%
  mutate(enrolled_2007_07_01 = ifelse(date_randomized<as.Date('2007-08-01') & date_end>=as.Date('2007-07-01'),1,0)) %>% 
  mutate(enrolled_2007_08_01 = ifelse(date_randomized<as.Date('2007-09-01') & date_end>=as.Date('2007-08-01'),1,0)) %>%
  mutate(enrolled_2007_09_01 = ifelse(date_randomized<as.Date('2007-10-01') & date_end>=as.Date('2007-09-01'),1,0)) %>% 
  mutate(enrolled_2007_10_01 = ifelse(date_randomized<as.Date('2007-11-01') & date_end>=as.Date('2007-10-01'),1,0)) %>%
  mutate(enrolled_2007_11_01 = ifelse(date_randomized<as.Date('2007-12-01') & date_end>=as.Date('2007-11-01'),1,0)) %>% 
  mutate(enrolled_2007_12_01 = ifelse(date_randomized<as.Date('2018-01-01') & date_end>=as.Date('2007-12-01'),1,0)) %>%
  
  mutate(enrolled_2008_01_01 = ifelse(date_randomized<as.Date('2008-02-01') & date_end>=as.Date('2008-01-01'),1,0)) %>% 
  mutate(enrolled_2008_02_01 = ifelse(date_randomized<as.Date('2008-03-01') & date_end>=as.Date('2008-02-01'),1,0)) %>% 
  mutate(enrolled_2008_03_01 = ifelse(date_randomized<as.Date('2008-04-01') & date_end>=as.Date('2008-03-01'),1,0)) %>% 
  mutate(enrolled_2008_04_01 = ifelse(date_randomized<as.Date('2008-05-01') & date_end>=as.Date('2008-04-01'),1,0)) %>%
  mutate(enrolled_2008_05_01 = ifelse(date_randomized<as.Date('2008-06-01') & date_end>=as.Date('2008-05-01'),1,0)) %>% 
  mutate(enrolled_2008_06_01 = ifelse(date_randomized<as.Date('2008-07-01') & date_end>=as.Date('2008-06-01'),1,0)) %>%
  mutate(enrolled_2008_07_01 = ifelse(date_randomized<as.Date('2008-08-01') & date_end>=as.Date('2008-07-01'),1,0)) %>% 
  mutate(enrolled_2008_08_01 = ifelse(date_randomized<as.Date('2008-09-01') & date_end>=as.Date('2008-08-01'),1,0)) %>%
  mutate(enrolled_2008_09_01 = ifelse(date_randomized<as.Date('2008-10-01') & date_end>=as.Date('2008-09-01'),1,0)) %>% 
  mutate(enrolled_2008_10_01 = ifelse(date_randomized<as.Date('2008-11-01') & date_end>=as.Date('2008-10-01'),1,0)) %>%
  mutate(enrolled_2008_11_01 = ifelse(date_randomized<as.Date('2008-12-01') & date_end>=as.Date('2008-11-01'),1,0)) %>% 
  mutate(enrolled_2008_12_01 = ifelse(date_randomized<as.Date('2009-01-01') & date_end>=as.Date('2008-12-01'),1,0)) %>%
  
  mutate(enrolled_2009_01_01 = ifelse(date_randomized<as.Date('2009-02-01') & date_end>=as.Date('2009-01-01'),1,0)) %>% 
  mutate(enrolled_2009_02_01 = ifelse(date_randomized<as.Date('2009-03-01') & date_end>=as.Date('2009-02-01'),1,0)) %>% 
  mutate(enrolled_2009_03_01 = ifelse(date_randomized<as.Date('2009-04-01') & date_end>=as.Date('2009-03-01'),1,0)) %>% 
  mutate(enrolled_2009_04_01 = ifelse(date_randomized<as.Date('2009-05-01') & date_end>=as.Date('2009-04-01'),1,0)) %>%
  mutate(enrolled_2009_05_01 = ifelse(date_randomized<as.Date('2009-06-01') & date_end>=as.Date('2009-05-01'),1,0)) %>% 
  mutate(enrolled_2009_06_01 = ifelse(date_randomized<as.Date('2009-07-01') & date_end>=as.Date('2009-06-01'),1,0)) %>%
  mutate(enrolled_2009_07_01 = ifelse(date_randomized<as.Date('2009-08-01') & date_end>=as.Date('2009-07-01'),1,0)) %>% 
  mutate(enrolled_2009_08_01 = ifelse(date_randomized<as.Date('2009-09-01') & date_end>=as.Date('2009-08-01'),1,0)) %>%
  mutate(enrolled_2009_09_01 = ifelse(date_randomized<as.Date('2009-10-01') & date_end>=as.Date('2009-09-01'),1,0)) %>% 
  mutate(enrolled_2009_10_01 = ifelse(date_randomized<as.Date('2009-11-01') & date_end>=as.Date('2009-10-01'),1,0)) %>%
  mutate(enrolled_2009_11_01 = ifelse(date_randomized<as.Date('2009-12-01') & date_end>=as.Date('2009-11-01'),1,0)) %>% 
  mutate(enrolled_2009_12_01 = ifelse(date_randomized<as.Date('2010-01-01') & date_end>=as.Date('2009-12-01'),1,0)) %>%
  
  mutate(enrolled_2010_01_01 = ifelse(date_randomized<as.Date('2010-02-01') & date_end>=as.Date('2010-01-01'),1,0)) %>% 
  mutate(enrolled_2010_02_01 = ifelse(date_randomized<as.Date('2010-03-01') & date_end>=as.Date('2010-02-01'),1,0)) %>% 
  mutate(enrolled_2010_03_01 = ifelse(date_randomized<as.Date('2010-04-01') & date_end>=as.Date('2010-03-01'),1,0)) %>% 
  mutate(enrolled_2010_04_01 = ifelse(date_randomized<as.Date('2010-05-01') & date_end>=as.Date('2010-04-01'),1,0)) %>%
  mutate(enrolled_2010_05_01 = ifelse(date_randomized<as.Date('2010-06-01') & date_end>=as.Date('2010-05-01'),1,0))


# wide format with enrollment by month  
enrollment_grouped_macro <- enrollment_by_month_pre1 %>% 
  dplyr::select(clinic,contains("enrolled_")) %>% 
  dplyr::group_by(clinic) %>% 
  dplyr::summarise_all(funs(sum)) %>% 
  rename_at(vars(contains("enrolled")), 
              funs(sub("enrolled_","",.))) %>% 
  gather(month,enrolled,2:52)
  
enrollment_grouped_macro$month <- str_replace_all(enrollment_grouped_macro$month,'_','-')
  
enrollment_grouped_macro <- enrollment_grouped_macro %>% spread(clinic, enrolled) %>% mutate(date=as.Date(month)) %>% dplyr::select(-month)
  
# add list of all months between start (2006-03-17) and end (2010-04-02)
date_empty <- data.frame(date=seq(as.Date("2006-03-01"), as.Date("2010-05-30"), by="days"))
  
enrollment_macro_pre2 <- left_join(date_empty,enrollment_grouped_macro)  
  
enrollment_by_month_macro <- enrollment_macro_pre2 %>% 
  mutate(A = zoo::na.locf(A, na.rm=T)) %>%
  mutate(B = zoo::na.locf(B, na.rm=T)) %>%
  mutate(C = zoo::na.locf(C, na.rm=T)) %>%
  mutate(D = zoo::na.locf(D, na.rm=T)) %>%
  mutate(E = zoo::na.locf(E, na.rm=T)) %>%
  mutate(F = zoo::na.locf(F, na.rm=T)) %>%
  mutate(G = zoo::na.locf(G, na.rm=T)) %>%
  mutate(H = zoo::na.locf(H, na.rm=T)) %>%
  mutate(I = zoo::na.locf(I, na.rm=T)) %>%
  mutate(J = zoo::na.locf(J, na.rm=T)) 
         
# merge enrollment dataset with exac_long dataset
exac_long_full <- left_join(enrollment_by_month_macro, exac_long)

# add quarter, week and 90 day breaks
exac_long_full$quarter <- as.Date(cut(exac_long_full$date, breaks = "quarter"))
exac_long_full$week <- as.Date(cut(exac_long_full$date, breaks = "week"))
exac_long_full$ninetyday <- as.Date(cut(exac_long_full$date, breaks = "90 days"))

# proportional exacerbation
exac_long_full <- exac_long_full %>% 
  dplyr::filter(date>"2006-07-01") %>% # remove before July 2006
  dplyr::filter(date<="2009-11-10") %>% 
  mutate(exacyesno = ifelse(is.na(severity),0,1)) %>% # add marker variable for exacerbation
  mutate(percent_exac = case_when(clinic=="A" ~ exacyesno/A*100, 
                                  clinic=="B" ~ exacyesno/B*100,
                                  clinic=="C" ~ exacyesno/C*100,
                                  clinic=="D" ~ exacyesno/D*100,
                                  clinic=="E" ~ exacyesno/E*100,
                                  clinic=="F" ~ exacyesno/F*100,
                                  clinic=="G" ~ exacyesno/G*100,
                                  clinic=="H" ~ exacyesno/H*100,
                                  clinic=="I" ~ exacyesno/I*100,
                                  clinic=="J" ~ exacyesno/J*100)) %>% 
  mutate(percent_exac = ifelse(is.na(percent_exac),0,percent_exac)) %>%  # add percentage of that center with exacerbations on that day based on enrollment
  mutate(trtgroup_label = ifelse(trtgroup==1,"azithro","placebo"))


### Note that analyses below are stratfied by treatment group where trtgrp 1:Azithro 2:Placebo ###

# group by 90 day increments and center - for both placebo and azithro
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
                               clinic=="J" ~ -79.9959)) %>% 
  mutate(clinic_name = case_when(clinic=="A" ~ "Maryland", # change clinic to name of center
                            clinic=="B" ~ "Birmingham",
                            clinic=="C" ~ "Boston",
                            clinic=="D" ~ "Denver",
                            clinic=="E" ~ "Los Angeles",
                            clinic=="F" ~ "Minnesota",
                            clinic=="G" ~ "San Francisco",
                            clinic=="H" ~ "Michigan",
                            clinic=="I" ~ "Philadelphia",
                            clinic=="J" ~ "Pittsburgh")) %>% 
  mutate(region_1 = case_when(clinic=="A" ~ "Mid-Atlantic", # add region
                              clinic=="B" ~ "South",
                              clinic=="C" ~ "Northeast",
                              clinic=="D" ~ "Mountain West",
                              clinic=="E" ~ "West Coast",
                              clinic=="F" ~ "Midwest",
                              clinic=="G" ~ "West Coast",
                              clinic=="H" ~ "Midwest",
                              clinic=="I" ~ "Northeast",
                              clinic=="J" ~ "Northeast")) %>% 
  mutate(region_2 = case_when(clinic=="A" ~ "Northeast", # add region 2
                              clinic=="B" ~ "South and West",
                              clinic=="C" ~ "Northeast",
                              clinic=="D" ~ "South and West",
                              clinic=="E" ~ "South and West",
                              clinic=="F" ~ "Midwest",
                              clinic=="G" ~ "South and West",
                              clinic=="H" ~ "Midwest",
                              clinic=="I" ~ "Northeast",
                              clinic=="J" ~ "Midwest"))

## export 90 day csv - for both placebo and azithro
# write_csv(exac_grouped_ninety, "app/data/exacerbations_grouped_ninety.csv")

## wide format 90 day rate file for clustering (both placebo and intervention)
exac_grouped_ninety_wide <- exac_grouped_ninety %>% 
  dplyr::select(clinic, clinic_name, ninetyday, total_exac_percent, latitude, longitude) %>% 
  spread(ninetyday,total_exac_percent, fill=0) %>% 
  dplyr::select(-'2006-05-30', -'2009-11-10')

# group by 90 day increments and center - for placebo
exac_grouped_ninety_placebo <- exac_long_full %>% 
  filter(trtgroup_label == "placebo") %>% 
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
                               clinic=="J" ~ -79.9959)) %>% 
  mutate(clinic_name = case_when(clinic=="A" ~ "Maryland", # change clinic to name of center
                            clinic=="B" ~ "Birmingham",
                            clinic=="C" ~ "Boston",
                            clinic=="D" ~ "Denver",
                            clinic=="E" ~ "Los Angeles",
                            clinic=="F" ~ "Minnesota",
                            clinic=="G" ~ "San Francisco",
                            clinic=="H" ~ "Michigan",
                            clinic=="I" ~ "Philadelphia",
                            clinic=="J" ~ "Pittsburgh")) %>% 
  mutate(region_1 = case_when(clinic=="A" ~ "Mid-Atlantic", # add region
                              clinic=="B" ~ "South",
                              clinic=="C" ~ "Northeast",
                              clinic=="D" ~ "Mountain West",
                              clinic=="E" ~ "West Coast",
                              clinic=="F" ~ "Midwest",
                              clinic=="G" ~ "West Coast",
                              clinic=="H" ~ "Midwest",
                              clinic=="I" ~ "Northeast",
                              clinic=="J" ~ "Northeast")) %>% 
  mutate(region_2 = case_when(clinic=="A" ~ "Northeast", # add region 2
                              clinic=="B" ~ "South and West",
                              clinic=="C" ~ "Northeast",
                              clinic=="D" ~ "South and West",
                              clinic=="E" ~ "South and West",
                              clinic=="F" ~ "Midwest",
                              clinic=="G" ~ "South and West",
                              clinic=="H" ~ "Midwest",
                              clinic=="I" ~ "Northeast",
                              clinic=="J" ~ "Midwest"))

## export 90 day csv - for placebo
# write_csv(exac_grouped_ninety_placebo, "app/data/exacerbations_grouped_ninety_placebo.csv")

## wide format 90 day placebo rate file for clustering 
exac_grouped_ninety_placebo_wide <- exac_grouped_ninety_placebo %>% 
  dplyr::select(clinic, clinic_name, ninetyday, total_exac_percent, latitude, longitude) %>% 
  spread(ninetyday,total_exac_percent, fill=0) %>% 
  dplyr::select(-'2006-05-30', -'2009-11-10')

# group by 90 day increments and center - for azithro
exac_grouped_ninety_azithro <- exac_long_full %>% 
  filter(trtgroup_label == "azithro") %>% 
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
                               clinic=="J" ~ -79.9959)) %>% 
  mutate(clinic_name = case_when(clinic=="A" ~ "Maryland", # change clinic to name of center
                            clinic=="B" ~ "Birmingham",
                            clinic=="C" ~ "Boston",
                            clinic=="D" ~ "Denver",
                            clinic=="E" ~ "Los Angeles",
                            clinic=="F" ~ "Minnesota",
                            clinic=="G" ~ "San Francisco",
                            clinic=="H" ~ "Michigan",
                            clinic=="I" ~ "Philadelphia",
                            clinic=="J" ~ "Pittsburgh")) %>% 
  mutate(region_1 = case_when(clinic=="A" ~ "Mid-Atlantic", # add region
                              clinic=="B" ~ "South",
                              clinic=="C" ~ "Northeast",
                              clinic=="D" ~ "Mountain West",
                              clinic=="E" ~ "West Coast",
                              clinic=="F" ~ "Midwest",
                              clinic=="G" ~ "West Coast",
                              clinic=="H" ~ "Midwest",
                              clinic=="I" ~ "Northeast",
                              clinic=="J" ~ "Northeast")) %>% 
  mutate(region_2 = case_when(clinic=="A" ~ "Northeast", # add region 2
                              clinic=="B" ~ "South and West",
                              clinic=="C" ~ "Northeast",
                              clinic=="D" ~ "South and West",
                              clinic=="E" ~ "South and West",
                              clinic=="F" ~ "Midwest",
                              clinic=="G" ~ "South and West",
                              clinic=="H" ~ "Midwest",
                              clinic=="I" ~ "Northeast",
                              clinic=="J" ~ "Midwest"))

## export 90 day csv - for azithro
# write_csv(exac_grouped_ninety_azithro, "app/data/exacerbations_grouped_ninety_azithro.csv")

#################################################################################################################################################################
################################### Manipulation in order to perform center and region specific analyses of outcomes ############################################
#################################################################################################################################################################

# add region options to baseline
copd_region <- baseline %>% 
  filter(time_on_study > 0) %>% 
  mutate(region_1 = case_when(clinic=="A" ~ "Mid-Atlantic", # add region 1
                              clinic=="B" ~ "South",
                              clinic=="C" ~ "Northeast",
                              clinic=="D" ~ "Mountain West",
                              clinic=="E" ~ "West Coast",
                              clinic=="F" ~ "Midwest",
                              clinic=="G" ~ "West Coast",
                              clinic=="H" ~ "Midwest",
                              clinic=="I" ~ "Northeast",
                              clinic=="J" ~ "Northeast")) %>% 
  mutate(region_2 = case_when(clinic=="A" ~ "Northeast", # add region 2
                              clinic=="B" ~ "South and West",
                              clinic=="C" ~ "Northeast",
                              clinic=="D" ~ "South and West",
                              clinic=="E" ~ "South and West",
                              clinic=="F" ~ "Midwest",
                              clinic=="G" ~ "South and West",
                              clinic=="H" ~ "Midwest",
                              clinic=="I" ~ "Northeast",
                              clinic=="J" ~ "Midwest")) %>% 
  mutate(region_3 = case_when(clinic=="A" ~ "South", # add region 3
                              clinic=="B" ~ "South",
                              clinic=="C" ~ "North",
                              clinic=="D" ~ "South",
                              clinic=="E" ~ "South",
                              clinic=="F" ~ "North",
                              clinic=="G" ~ "South",
                              clinic=="H" ~ "North",
                              clinic=="I" ~ "North",
                              clinic=="J" ~ "North")) %>% 
  mutate(region_cluster_1 = case_when(clinic=="A" ~ "Cluster A", 
                                       clinic=="B" ~ "Cluster B",
                                       clinic=="C" ~ "Cluster A",
                                       clinic=="D" ~ "Cluster B",
                                       clinic=="E" ~ "Cluster B",
                                       clinic=="F" ~ "Cluster A",
                                       clinic=="G" ~ "Cluster B",
                                       clinic=="H" ~ "Cluster A",
                                       clinic=="I" ~ "Cluster C",
                                       clinic=="J" ~ "Cluster C")) %>%
  mutate(region_cluster_2 = case_when(clinic=="A" ~ "Cluster B", 
                                      clinic=="B" ~ "Cluster A",
                                      clinic=="C" ~ "Cluster B",
                                      clinic=="D" ~ "Cluster A",
                                      clinic=="E" ~ "Cluster A",
                                      clinic=="F" ~ "Cluster B",
                                      clinic=="G" ~ "Cluster A",
                                      clinic=="H" ~ "Cluster B",
                                      clinic=="I" ~ "Cluster A",
                                      clinic=="J" ~ "Cluster A")) %>%
  mutate(region_cluster_h_1 = case_when(clinic=="A" ~ "Cluster A", 
                                         clinic=="B" ~ "Cluster B",
                                         clinic=="C" ~ "Cluster A",
                                         clinic=="D" ~ "Cluster B",
                                         clinic=="E" ~ "Cluster B",
                                         clinic=="F" ~ "Cluster A",
                                         clinic=="G" ~ "Cluster B",
                                         clinic=="H" ~ "Cluster A",
                                         clinic=="I" ~ "Cluster A",
                                         clinic=="J" ~ "Cluster B")) %>% 
  mutate(clinic_name = case_when(clinic=="A" ~ "Maryland", # change clinic to name of center
                                 clinic=="B" ~ "Birmingham",
                                 clinic=="C" ~ "Boston",
                                 clinic=="D" ~ "Denver",
                                 clinic=="E" ~ "Los Angeles",
                                 clinic=="F" ~ "Minnesota",
                                 clinic=="G" ~ "San Francisco",
                                 clinic=="H" ~ "Michigan",
                                 clinic=="I" ~ "Philadelphia",
                                 clinic=="J" ~ "Pittsburgh")) %>% 
  mutate(season_name = case_when(season==1 ~ "January to March", # add season names
                                 season==2 ~ "April to June",
                                 season==3 ~ "July to September",
                                 season==4 ~ "October to December")) %>% 
  mutate(season_d = case_when(season == 1 ~ "Fall and Winter", # add dichotomized season
                              season == 2 ~ "Spring and Summer",
                              season == 3 ~ "Spring and Summer",
                              season == 4 ~ "Fall and Winter")) %>% 
  mutate(trtgroup_label = ifelse(trtgroup==1,"azithro","placebo")) %>% # add treatment label
  mutate(exacyesno = ifelse(is.na(Severity_EX1),0,1)) %>% # add yesnoexac to regional data 
  mutate(priorexac = ifelse(ster1yr==1 | hosp1yr ==1, 1, 0))

copd_region$region_2_f <- as.factor(copd_region$region_2)
copd_region$trtgroup_label_f <- as.factor(copd_region$trtgroup_label)
copd_region$region_cluster_1_f <- as.factor(copd_region$region_cluster_1)
copd_region$region_cluster_2_f <- as.factor(copd_region$region_cluster_2)
copd_region$region_cluster_h_1_f <- as.factor(copd_region$region_cluster_h_1)

  

## note that additional manipulation of inidividual datasets for limited analyses occurs in respective analysis files ##

