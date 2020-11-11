# Data Cleaning script

# Load required cleaning packages
library(tidyverse)
library(janitor)
library(readxl)
library(assertr)
library(here)

# Read in data
birds <- read_xls("../task_3/raw_data/seabirds.xls", 
                  sheet = "Bird data by record ID")
ships <- read_xls("../task_3/raw_data/seabirds.xls", 
                  sheet = "Ship data by record ID")
bird_codes <- read_xls("../task_3/raw_data/seabirds.xls", 
                      sheet = "Bird data codes")
ship_codes <- read_xls("../task_3/raw_data/seabirds.xls", 
                       sheet = "Ship data codes")


#Select desired columns and clean/rename names

birds <- birds %>%
  clean_names() %>% 
  rename("species_common_name" = 
         "species_common_name_taxon_age_sex_plumage_phase",
         "species_scientific_name" = 
         "species_scientific_name_taxon_age_sex_plumage_phase",
         "bird_record" = "record",
         "bird_count" = "count"
  ) %>% 
  select("bird_record", 
         "record_id", 
         "species_common_name",
         "species_scientific_name",
         "species_abbreviation",
         "bird_count",
  )

ships <- ships %>%
  clean_names() %>%
  rename("ship_record" =
           "record",
         "season" = 
           "seasn"
  ) %>% 
  select("ship_record",
         "record_id",
         "date",
         "time",
         "lat",
         "long",
         "obs",
         "season",
         "month"
  )

# Perform left join birds and ships to return all matching records with birds 
#data

bird_observations <- left_join(birds, ships, by = "record_id")

#--------------Recoding time column--------------------------------------------

bird_observations <- bird_observations %>% 
  mutate(time = str_remove(time, pattern = "1899-12-31 "))

# -------Removing the rows where no bird counts were definitely recorded-------

bird_observations <- bird_observations %>% 
  filter(!str_detect(species_common_name, pattern = "NO BIRDS RECORDED"))

#----------------Cleaning species common names---------------------------------

bird_observations <- bird_observations %>% 
  mutate(species_common_name = 
           str_remove(species_common_name, 
                      pattern = ("[A-Z]{2,}[0-9]*")
           )) %>%
  mutate(species_common_name =
           str_remove(species_common_name,
                      pattern = ("[A-Z]{2,}")
           )) %>% 
  
  mutate(species_common_name = 
           str_remove(species_common_name, 
                      pattern = " sensu lato"
           )) %>% 
  
  mutate(species_common_name = 
           str_remove(species_common_name, 
                      pattern = "[(][A-z]+[)]"
           )) %>% 
  
  mutate(species_common_name = 
           str_remove(species_common_name, 
                      pattern = "[ ]M$"
           )) %>% 
  
  mutate(species_common_name =
           trimws(species_common_name))

#--------------Cleaning species scientific name-------------------------------

bird_observations <- bird_observations %>% 
  mutate(species_scientific_name = 
           str_remove(species_scientific_name, 
                      pattern = ("[A-Z]{2,}[0-9]*")
           )) %>%
  
  mutate(species_scientific_name = 
           str_remove(species_scientific_name, 
                      pattern = ("[A-Z]{2,}")
           )) %>% 
  
  mutate(species_scientific_name =
           trimws(species_scientific_name)) %>% 
  
  mutate(species_scientific_name = 
           str_remove(species_scientific_name, 
                      pattern = "[ ]M$"
           ))

# ----------------Cleaning species abbreviation--------------------------------

bird_observations <- bird_observations %>% 
  mutate(species_abbreviation = 
           str_remove(species_abbreviation, 
                      pattern = ("[ ][A-Z]{2,}[0-9]*")
           ))%>%
  
  mutate(species_abbreviation =
           str_remove(species_abbreviation,
                      pattern = ("[ ][A-Z]{2,}")
           )) %>% 
  
  mutate(species_abbreviation = 
           str_remove(species_abbreviation, 
                      pattern = "[ ]M$"
           )) %>% 
  
  mutate(species_abbreviation =
           trimws(species_abbreviation))


# ---------Recoding month column by changing numbers to months-----------------

bird_observations <- bird_observations %>% 
  mutate(month = recode(month, 
                        "1" = "January",
                        "2" = "February",
                        "3" = "March",
                        "4" = "April",
                        "5" = "May",
                        "6" = "June",
                        "7" = "July",
                        "8" = "August",
                        "9" = "September",
                        "10" = "October",
                        "11" = "November",
                        "12" = "December")
  )


#Imputing the NA values with value of 1, assumed it would be unlikely to 
#miss multiple birds and adding 1 will not skew results significantly

bird_observations <- bird_observations %>% 
  mutate(bird_count = coalesce(bird_count, 1, na.rm = TRUE))


#--------Write cleaned data to csv file-----------------------------------------

write_csv(bird_observations, file = "clean_data/cleaned_bird_observations")












