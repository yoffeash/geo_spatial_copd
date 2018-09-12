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

