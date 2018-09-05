### import and manipulate data ###

# import data
baseline <- read_sas("data/raw_data/analysis_file_20121231.sas7bdat", 
         NULL)


# convert dates to date
copd_base <- baseline %>% mutate(date_randomized = as.Date(Rand_Date, origin='1960-01-01'))

copd_pre1 <- copd_base %>% 
  mutate_at(vars(contains('Days_To_Onset')), .funs = funs(date_exac = as.Date(. + Rand_Date, origin='1960-01-01'))) 

# select only the location and date information
copd_pre2 <- copd_pre1 %>% select(ID, contains("Severity_EX"), contains("clinic"), latitude, contains("_date_exac"))

# rename date of exacerbation variables
copd_pre3 <- copd_pre2 %>% 
  rename_at(vars(contains("Days_To_Onset")), 
                                     funs(sub("Days_To_Onset","dateofexac",.))) %>%
  rename_at(vars(contains("_date_exac")), 
            funs(sub("_date_exac","",.)))
  
# add month variables
copd_pre4 <- copd_pre3 %>% 
  mutate_at(vars(contains("dateofexac")), .funs = funs(month = month(.)))

# count number of exacerbations in each month for each individual
# create a function
make_dummies <- function(v, prefix = '') {
  s <- sort(unique(v))
  d <- outer(v, s, function(v, s) 1L * (v == s))
  colnames(d) <- paste0(prefix, s)
  d
}

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

                                                                                                 