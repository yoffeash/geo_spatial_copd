### prep weather data ###

## normal temperature ##
# load data
norm_temp_pre1 <- read_csv("data/raw_data/macro_sites_normal_2010.csv")

# clean names and format variables and names
norm_temp_pre1 <- clean_names(norm_temp_pre1) 
norm_temp_pre1$date <- ymd(norm_temp_pre1$date)
norm_temp_pre1 <- norm_temp_pre1 %>% 
  mutate(clinic = case_when(station_name=="BALTIMORE WASHINGTON INTERNATIONAL AIRPORT MD US" ~ "A",
                                 station_name=="BIRMINGHAM AIRPORT AL US" ~ "B",
                                 station_name=="BOSTON MA US" ~ "C",
                                 station_name=="DENVER STAPLETON CO US" ~ "D",
                                 station_name=="LOS ANGELES INTERNATIONAL AIRPORT CA US" ~ "E",
                                 station_name=="MINNEAPOLIS ST PAUL INTERNATIONAL AIRPORT MN US" ~ "F",
                                 station_name=="SAN FRANCISCO INTERNATIONAL AIRPORT CA US" ~ "G",
                                 station_name=="DETROIT CITY AIRPORT MI US" ~ "H",
                                 station_name=="PHILADELPHIA INTERNATIONAL AIRPORT PA US" ~ "I",
                                 station_name=="PITTSBURGH ASOS PA US" ~ "J"))

# create other years
norm_temp_2006 <- norm_temp_pre1 %>% 
  mutate(date=date-years(4))


## measured temperature ##
temp_pre1 <- read_csv("data/raw_data/macro_sites_2006_03_01-2014_01_31.csv")

