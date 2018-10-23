## STATCOPE data manipulation ##


#################################################################################################################################################################
############################################ initial import and prepping of data - CRN STATCOPE Study Data Only ##################################################
#################################################################################################################################################################

# import data
baseline_stat <- read_sas("data/raw_data/sta_analysis_file.sas7bdat", 
                     NULL)

#################################################################################################################################################################
################################### Manipulation to Create Center/Region specific Plots including Maps and Plots over Time ######################################
#################################################################################################################################################################

stat_pre_rate <- baseline_stat %>% filter(DaysInStudy > 0) %>% 
  mutate_at(vars(contains('Days_To_Onset')), funs(date_exac = (. + Rand_Date))) %>% # add days to onset to rand date to get sas date for each exacerbation
  dplyr::select(ID, contains("Severity_EX"), contains("clinic"), Rand_Date, STAGROUP, contains("_date_exac")) %>% # select only the severity, clinic and date information
  rename_at(vars(contains("Days_To_Onset")), # rename date of exacerbation variables
            funs(sub("Days_To_Onset","dateofexac",.))) %>%
  rename_at(vars(contains("_date_exac")), 
            funs(sub("_date_exac","",.)))

# split into date and severity datasets
exac_dates_wide_stat <- stat_pre_rate %>% dplyr::select(ID, SubClinic, STAGROUP, dateofexac1:dateofexac11)
exac_severity_wide_stat <- stat_pre_rate %>% dplyr::select(ID, SubClinic, STAGROUP, Severity_EX1:Severity_EX11)

# convert wide format date and severity datasets to long and rename exacnumber variable to exac1, exac2, etc....
exac_dates_long_stat <- exac_dates_wide_stat %>% 
  gather(exacnumber, date, dateofexac1:dateofexac11)
exac_dates_long_stat$exacnumber <- str_replace_all(exac_dates_long_stat$exacnumber, "dateofexac", "")

exac_severity_long_stat <- exac_severity_wide_stat %>% 
  gather(exacnumber, severity, Severity_EX1:Severity_EX11)
exac_severity_long_stat$exacnumber <- str_replace_all(exac_severity_long_stat$exacnumber, "Severity_EX", "")

# merge severity and date datasets to have long format dataset with date, center and exacnumber
exac_long_stat <- left_join(exac_dates_long_stat, exac_severity_long_stat)

# convert dates to date and manipulate counts and rates of exacerbation by center
exac_long_stat <- exac_long_stat %>% 
  mutate(date = as.Date(date, origin='1960-01-01')) %>% 
  arrange(date) # sort by date

###### calculating monthly enrollment - inelegant solution ####
enrollment_stat_pre1 <- baseline_stat %>% 
  filter(DaysInStudy > 0) %>% 
  dplyr::select(ID,Rand_Date,DaysInStudy,SubClinic) %>% 
  mutate(EndDate = Rand_Date + DaysInStudy) %>% 
  mutate(date_randomized = as.Date(Rand_Date, origin='1960-01-01')) %>% 
  mutate(date_end = as.Date(EndDate, origin='1960-01-01')) %>% 
  mutate(clinic=substr(SubClinic,1,1)) %>% 
  mutate(enrolled_2010_03_01 = ifelse(date_randomized<as.Date('2010-04-01') & date_end>=as.Date('2010-03-01'),1,0)) %>% 
  mutate(enrolled_2010_04_01 = ifelse(date_randomized<as.Date('2010-05-01') & date_end>=as.Date('2010-04-01'),1,0)) %>%
  mutate(enrolled_2010_05_01 = ifelse(date_randomized<as.Date('2010-06-01') & date_end>=as.Date('2010-05-01'),1,0)) %>%
  mutate(enrolled_2010_06_01 = ifelse(date_randomized<as.Date('2010-07-01') & date_end>=as.Date('2010-06-01'),1,0)) %>% 
  mutate(enrolled_2010_07_01 = ifelse(date_randomized<as.Date('2010-08-01') & date_end>=as.Date('2010-07-01'),1,0)) %>% 
  mutate(enrolled_2010_08_01 = ifelse(date_randomized<as.Date('2010-09-01') & date_end>=as.Date('2010-08-01'),1,0)) %>% 
  mutate(enrolled_2010_09_01 = ifelse(date_randomized<as.Date('2010-10-01') & date_end>=as.Date('2010-09-01'),1,0)) %>% 
  mutate(enrolled_2010_10_01 = ifelse(date_randomized<as.Date('2010-11-01') & date_end>=as.Date('2010-10-01'),1,0)) %>% 
  mutate(enrolled_2010_11_01 = ifelse(date_randomized<as.Date('2010-12-01') & date_end>=as.Date('2010-11-01'),1,0)) %>%
  mutate(enrolled_2010_12_01 = ifelse(date_randomized<as.Date('2011-01-01') & date_end>=as.Date('2010-12-01'),1,0)) %>%
  mutate(enrolled_2011_01_01 = ifelse(date_randomized<as.Date('2011-02-01') & date_end>=as.Date('2011-01-01'),1,0)) %>% 
  mutate(enrolled_2011_02_01 = ifelse(date_randomized<as.Date('2011-03-01') & date_end>=as.Date('2011-02-01'),1,0)) %>% 
  mutate(enrolled_2011_03_01 = ifelse(date_randomized<as.Date('2011-04-01') & date_end>=as.Date('2011-03-01'),1,0)) %>% 
  mutate(enrolled_2011_04_01 = ifelse(date_randomized<as.Date('2011-05-01') & date_end>=as.Date('2011-04-01'),1,0)) %>%
  mutate(enrolled_2011_05_01 = ifelse(date_randomized<as.Date('2011-06-01') & date_end>=as.Date('2011-05-01'),1,0)) %>% 
  mutate(enrolled_2011_06_01 = ifelse(date_randomized<as.Date('2011-07-01') & date_end>=as.Date('2011-06-01'),1,0)) %>%
  mutate(enrolled_2011_07_01 = ifelse(date_randomized<as.Date('2011-08-01') & date_end>=as.Date('2011-07-01'),1,0)) %>% 
  mutate(enrolled_2011_08_01 = ifelse(date_randomized<as.Date('2011-09-01') & date_end>=as.Date('2011-08-01'),1,0)) %>%
  mutate(enrolled_2011_09_01 = ifelse(date_randomized<as.Date('2011-10-01') & date_end>=as.Date('2011-09-01'),1,0)) %>% 
  mutate(enrolled_2011_10_01 = ifelse(date_randomized<as.Date('2011-11-01') & date_end>=as.Date('2011-10-01'),1,0)) %>%
  mutate(enrolled_2011_11_01 = ifelse(date_randomized<as.Date('2011-12-01') & date_end>=as.Date('2011-11-01'),1,0)) %>% 
  mutate(enrolled_2011_12_01 = ifelse(date_randomized<as.Date('2012-01-01') & date_end>=as.Date('2011-12-01'),1,0)) %>%

  mutate(enrolled_2012_01_01 = ifelse(date_randomized<as.Date('2012-02-01') & date_end>=as.Date('2012-01-01'),1,0)) %>% 
  mutate(enrolled_2012_02_01 = ifelse(date_randomized<as.Date('2012-03-01') & date_end>=as.Date('2012-02-01'),1,0)) %>% 
  mutate(enrolled_2012_03_01 = ifelse(date_randomized<as.Date('2012-04-01') & date_end>=as.Date('2012-03-01'),1,0)) %>% 
  mutate(enrolled_2012_04_01 = ifelse(date_randomized<as.Date('2012-05-01') & date_end>=as.Date('2012-04-01'),1,0)) %>%
  mutate(enrolled_2012_05_01 = ifelse(date_randomized<as.Date('2012-06-01') & date_end>=as.Date('2012-05-01'),1,0)) %>% 
  mutate(enrolled_2012_06_01 = ifelse(date_randomized<as.Date('2012-07-01') & date_end>=as.Date('2012-06-01'),1,0)) %>%
  mutate(enrolled_2012_07_01 = ifelse(date_randomized<as.Date('2012-08-01') & date_end>=as.Date('2012-07-01'),1,0)) %>% 
  mutate(enrolled_2012_08_01 = ifelse(date_randomized<as.Date('2012-09-01') & date_end>=as.Date('2012-08-01'),1,0)) %>%
  mutate(enrolled_2012_09_01 = ifelse(date_randomized<as.Date('2012-10-01') & date_end>=as.Date('2012-09-01'),1,0)) %>% 
  mutate(enrolled_2012_10_01 = ifelse(date_randomized<as.Date('2012-11-01') & date_end>=as.Date('2012-10-01'),1,0)) %>%
  mutate(enrolled_2012_11_01 = ifelse(date_randomized<as.Date('2012-12-01') & date_end>=as.Date('2012-11-01'),1,0)) %>% 
  mutate(enrolled_2012_12_01 = ifelse(date_randomized<as.Date('2013-01-01') & date_end>=as.Date('2012-12-01'),1,0)) %>%
  
  mutate(enrolled_2013_01_01 = ifelse(date_randomized<as.Date('2013-02-01') & date_end>=as.Date('2013-01-01'),1,0)) %>% 
  mutate(enrolled_2013_02_01 = ifelse(date_randomized<as.Date('2013-03-01') & date_end>=as.Date('2013-02-01'),1,0)) %>% 
  mutate(enrolled_2013_03_01 = ifelse(date_randomized<as.Date('2013-04-01') & date_end>=as.Date('2013-03-01'),1,0)) %>% 
  mutate(enrolled_2013_04_01 = ifelse(date_randomized<as.Date('2013-05-01') & date_end>=as.Date('2013-04-01'),1,0)) %>%
  mutate(enrolled_2013_05_01 = ifelse(date_randomized<as.Date('2013-06-01') & date_end>=as.Date('2013-05-01'),1,0)) %>% 
  mutate(enrolled_2013_06_01 = ifelse(date_randomized<as.Date('2013-07-01') & date_end>=as.Date('2013-06-01'),1,0)) %>%
  mutate(enrolled_2013_07_01 = ifelse(date_randomized<as.Date('2013-08-01') & date_end>=as.Date('2013-07-01'),1,0)) %>% 
  mutate(enrolled_2013_08_01 = ifelse(date_randomized<as.Date('2013-09-01') & date_end>=as.Date('2013-08-01'),1,0)) %>%
  mutate(enrolled_2013_09_01 = ifelse(date_randomized<as.Date('2013-10-01') & date_end>=as.Date('2013-09-01'),1,0)) %>% 
  mutate(enrolled_2013_10_01 = ifelse(date_randomized<as.Date('2013-11-01') & date_end>=as.Date('2013-10-01'),1,0)) %>%
  mutate(enrolled_2013_11_01 = ifelse(date_randomized<as.Date('2013-12-01') & date_end>=as.Date('2013-11-01'),1,0)) %>% 
  mutate(enrolled_2013_12_01 = ifelse(date_randomized<as.Date('2014-01-01') & date_end>=as.Date('2013-12-01'),1,0)) %>%
  
  mutate(enrolled_2014_01_01 = ifelse(date_randomized<as.Date('2014-02-01') & date_end>=as.Date('2014-01-01'),1,0)) 

# wide format with enrollment by month  
enrollment_grouped <- enrollment_stat_pre1 %>% 
  dplyr::select(clinic,contains("enrolled_")) %>% 
  dplyr::group_by(clinic) %>% 
  dplyr::summarise_all(funs(sum)) %>% 
  rename_at(vars(contains("enrolled")), 
            funs(sub("enrolled_","",.))) %>% 
  gather(month,enrolled,2:48)

enrollment_grouped$month <- str_replace_all(enrollment_grouped$month,'_','-')

enrollment_grouped_stat <- enrollment_grouped %>% spread(clinic, enrolled) %>% mutate(date=as.Date(month)) %>% dplyr::select(-month)
  
date_empty_stat <- data.frame(date=seq(as.Date("2010-03-01"), as.Date("2014-01-30"), by="days"))

enrollment_stat_pre1 <- left_join(date_empty_stat,enrollment_grouped_stat)

# fill in enrollment numbers
enrollment_by_month_stat <- enrollment_stat_pre1 %>% 
  mutate(A = zoo::na.locf(A, na.rm=T)) %>%
  mutate(B = zoo::na.locf(B, na.rm=T)) %>%
  mutate(C = zoo::na.locf(C, na.rm=T)) %>%
  mutate(D = zoo::na.locf(D, na.rm=T)) %>%
  mutate(E = zoo::na.locf(E, na.rm=T)) %>%
  mutate(F = zoo::na.locf(F, na.rm=T)) %>%
  mutate(G = zoo::na.locf(G, na.rm=T)) %>%
  mutate(H = zoo::na.locf(H, na.rm=T)) %>%
  mutate(I = zoo::na.locf(I, na.rm=T)) %>%
  mutate(J = zoo::na.locf(J, na.rm=T)) %>% 
  mutate(K = zoo::na.locf(K, na.rm=T)) %>%
  mutate(L = zoo::na.locf(L, na.rm=T)) %>%
  mutate(M = zoo::na.locf(M, na.rm=T)) %>%
  mutate(N = zoo::na.locf(N, na.rm=T)) %>%
  mutate(P = zoo::na.locf(P, na.rm=T)) %>%
  mutate(Q = zoo::na.locf(Q, na.rm=T)) %>%
  mutate(R = zoo::na.locf(R, na.rm=T)) %>%
  mutate(S = zoo::na.locf(S, na.rm=T))
  
# merge enrollment dataset with exac_long dataset
exac_long_full_stat <- left_join(enrollment_by_month_stat, exac_long_stat)
  
# add quarter, week, month and 90 day breaks
exac_long_full_stat$quarter <- as.Date(cut(exac_long_full_stat$date, breaks = "quarter"))
exac_long_full_stat$week <- as.Date(cut(exac_long_full_stat$date, breaks = "week"))
exac_long_full_stat$ninetyday <- as.Date(cut(exac_long_full_stat$date, breaks = "90 days"))
exac_long_full_stat$month <- as.Date(cut(exac_long_full_stat$date, breaks = "month"))
  
# proportional exacerbation
exac_long_full_stat <- exac_long_full_stat %>% 
  mutate(exacyesno = ifelse(is.na(severity),0,1)) %>% # add marker variable for exacerbation
  mutate(clinic=substr(SubClinic,1,1)) %>% 
  mutate(percent_exac = case_when(clinic=="A" ~ exacyesno/A*100, 
                                  clinic=="B" ~ exacyesno/B*100,
                                  clinic=="C" ~ exacyesno/C*100,
                                  clinic=="D" ~ exacyesno/D*100,
                                  clinic=="E" ~ exacyesno/E*100,
                                  clinic=="F" ~ exacyesno/F*100,
                                  clinic=="G" ~ exacyesno/G*100,
                                  clinic=="H" ~ exacyesno/H*100,
                                  clinic=="I" ~ exacyesno/I*100,
                                  clinic=="J" ~ exacyesno/J*100,
                                  clinic=="K" ~ exacyesno/K*100,
                                  clinic=="L" ~ exacyesno/L*100,
                                  clinic=="M" ~ exacyesno/M*100,
                                  clinic=="N" ~ exacyesno/N*100,
                                  clinic=="P" ~ exacyesno/P*100,
                                  clinic=="Q" ~ exacyesno/Q*100,
                                  clinic=="R" ~ exacyesno/R*100,
                                  clinic=="S" ~ exacyesno/S*100)) %>% 
  mutate(percent_exac = ifelse(is.na(percent_exac),0,percent_exac)) %>%  # add percentage of that center with exacerbations on that day based on enrollment
  mutate(trtgroup_label = ifelse(STAGROUP=="S","statin","placebo"))
  
# group by 90 day increments and center - for both placebo and azithro
exac_grouped_ninety_stat <- exac_long_full_stat %>% 
  group_by(clinic, ninetyday) %>% 
  summarize(total_exac_percent = sum(percent_exac, na.rm=TRUE), mean_severity = mean(severity, na.rm=TRUE))
  
## wide format 90 day rate file for clustering 
exac_grouped_ninety_wide_stat <- exac_grouped_ninety_stat %>% 
  dplyr::select(clinic, ninetyday, total_exac_percent) %>% 
  spread(ninetyday,total_exac_percent, fill=0)

# group by quarter increments and center - for both placebo and azithro
exac_grouped_quarter_stat <- exac_long_full_stat %>% 
  group_by(clinic, quarter) %>% 
  summarize(total_exac_percent = sum(percent_exac, na.rm=TRUE), mean_severity = mean(severity, na.rm=TRUE))

## wide format quarter rate file for clustering 
exac_grouped_quarter_wide_stat <- exac_grouped_quarter_stat %>% 
  dplyr::select(clinic, quarter, total_exac_percent) %>% 
  spread(quarter,total_exac_percent, fill=0)


stat_pre1 <- baseline_stat %>% filter(DaysInStudy > 0) %>% # remove those who have days in study < 0
  mutate(Days_To_Onset1 = ifelse(Days_To_Onset1 <= 0, 0, Days_To_Onset1)) %>% 
  mutate(days_to_ex_1 = ifelse(is.na(Days_To_Onset1),DaysInStudy,Days_To_Onset1)) %>% 
  mutate(clinic=substr(SubClinic,1,1)) %>% 
  mutate(exacerb = ifelse(is.na(Severity_EX1),0,1)) %>% 
  mutate(region_cluster_h_1 = case_when(clinic=="A" ~ "Cluster C", 
                                        clinic=="B" ~ "Cluster A",
                                        clinic=="C" ~ "Cluster C",
                                        clinic=="D" ~ "Cluster B",
                                        clinic=="E" ~ "Cluster A",
                                        clinic=="F" ~ "Cluster C",
                                        clinic=="G" ~ "Cluster B",
                                        clinic=="H" ~ "Cluster C",
                                        clinic=="I" ~ "Cluster C",
                                        clinic=="J" ~ "Cluster B"))
 


