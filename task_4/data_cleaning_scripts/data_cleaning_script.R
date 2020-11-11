# Load packages required
library(tidyverse)
library(janitor)
library(assertr)
library(readxl)

#---------------------read in data---------------------------------------------

candy_2015 <- read_xlsx("../raw_data/boing-boing-candy-2015.xlsx") %>% 
  select(c("Timestamp":"[York Peppermint Patties]"))

candy_2016 <- read_xlsx("../raw_data/boing-boing-candy-2016.xlsx") %>% 
  select(c("Timestamp":"[York Peppermint Patties]"))

candy_2017 <- read_xlsx("../raw_data/boing-boing-candy-2017.xlsx") %>% 
  select(c("Internal ID":"Q6 | York Peppermint Patties"))

# 2015 data - fixing column names, adding required structure for merging with
# other data sets and selecting relevant columns-------------------------------

candy_2015 <- clean_names(candy_2015) %>% 
  mutate(internal_id = seq.int(1, 5630),
         gender = "unknown",
         country = "unknown",
         year = 2015) %>% 
  rename(age = how_old_are_you,
         trick_or_treating = 
           are_you_going_actually_going_trick_or_treating_yourself) %>% 
  select(year,
         internal_id, 
         age, 
         gender, 
         trick_or_treating, 
         country, 
         butterfinger:york_peppermint_patties)

#--------moving candy types and feelings to column as value-attribute pairs----

candy_2015_tidy <- candy_2015 %>% 
  pivot_longer(cols = c(butterfinger:york_peppermint_patties),
               names_to = "candy_type",
               values_to = "candy_feelings")


# 2016 data - fixing column names, adding required structure for merging with
# other data sets and selecting relevant columns-------------------------------

candy_2016 <- clean_names(candy_2016) %>%
  
  mutate(internal_id = seq.int(6000, 7258),
         year = 2016) %>%
  
  rename(trick_or_treating =
           are_you_going_actually_going_trick_or_treating_yourself,
         gender = your_gender,
         age = how_old_are_you,
         country = 
           which_country_do_you_live_in) %>% 
  
  select(year,
         internal_id,
         age,
         gender,
         trick_or_treating,
         country,
         x100_grand_bar:york_peppermint_patties)


#------moving candy types and feelings to column as value-attribute pairs------

candy_2016_tidy <- candy_2016 %>% 
  pivot_longer(cols = c(x100_grand_bar:york_peppermint_patties),
               names_to = "candy_type",
               values_to = "candy_feelings")

# 2017 data - fixing column names, adding required structure for merging with
# other data sets and selecting relevant columns

candy_2017 <- clean_names(candy_2017) %>% 
  
  mutate(year = 2017) %>% 
  
  rename(trick_or_treating = q1_going_out,
         gender = q2_gender,
         age = q3_age,
         country = q4_country) %>% 
  
  select(year,
         internal_id,
         age,
         gender,
         trick_or_treating,
         country,
         q6_100_grand_bar:q6_york_peppermint_patties)

#------moving candy types and feelings to column as value-attribute pairs------

candy_2017_tidy <- candy_2017 %>% 
  pivot_longer(cols = c(q6_100_grand_bar:q6_york_peppermint_patties),
               names_to = "candy_type",
               values_to = "candy_feelings")

#-------appending all data sets together---------------------------------------

all_candy_data <- bind_rows(candy_2015_tidy, candy_2016_tidy, candy_2017_tidy)






#----------write cleaned, combined data set to csv file------------------------

write_csv(all_candy_data, file = "../clean_data/cleaned_candy_data")







