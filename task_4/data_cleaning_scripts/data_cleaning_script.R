# Load packages required
library(tidyverse)
library(janitor)
library(readxl)

#---------------------read in data---------------------------------------------

candy_2015 <- read_xlsx("../task_4/raw_data/boing-boing-candy-2015.xlsx") %>% 
  select(c("Timestamp":"[York Peppermint Patties]"))

candy_2016 <- read_xlsx("../task_4/raw_data/boing-boing-candy-2016.xlsx") %>% 
  select(c("Timestamp":"[York Peppermint Patties]"))

candy_2017 <- read_xlsx("../task_4/raw_data/boing-boing-candy-2017.xlsx") %>% 
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

# extract age values as those with 2 numbers, then replace N/A's with --------
# median of age

all_candy_data <- all_candy_data %>% 
  mutate(age = str_extract(age, pattern = "[0-9]{2}")) %>%
  mutate(age = as.numeric(age)) %>% 
  mutate(age = coalesce(age, median(age, na.rm = TRUE)))

# Replacing values in the gender column to reduce no. of categories-------------

all_candy_data <- all_candy_data %>% 
  mutate(
    gender = if_else(
      gender == "I'd rather not say", "Undisclosed",
      gender
    )
  ) %>% 
  mutate(gender = replace_na(gender, "Unknown")) %>% 
  mutate(gender = if_else(
    gender == "unknown", "Unknown",
    gender
  )) 

# replacing NA's in the trick or treating column with "Unknown"-----------------
all_candy_data <- all_candy_data %>% 
  mutate(trick_or_treating = replace_na(trick_or_treating, "Unknown"))

#cleaning the country column the hard way - PART 1------------------------------

all_candy_data <- all_candy_data %>% 
  mutate(country_clean = tolower(country)) %>% 
  mutate(country_clean = case_when(
    startsWith(country_clean,"uni") ~ "usa",
    startsWith(country_clean, "usa") ~ "usa",
    startsWith(country_clean, "u.") ~ "usa",
    startsWith(country_clean, "us") ~ "usa",
    startsWith(country_clean, "u s") ~ "usa",
    startsWith(country_clean, "trump") ~ "usa",
    startsWith(country_clean, "mur") ~ "usa",
    startsWith(country_clean, "new") ~ "usa",
    startsWith(country_clean, "north carolina") ~ "usa",
    startsWith(country_clean, "pitts") ~ "usa",
    startsWith(country_clean, "the netherlands") ~ "netherlands",
    startsWith(country_clean, "god") ~ "unknown",
    startsWith(country_clean, "not the") ~ "unknown",
    startsWith(country_clean, "espa") ~ "spain",
    TRUE ~ as.character(country_clean))) %>% 
  mutate(country_clean = case_when(
    endsWith(country_clean, "usa") ~ "usa",
    endsWith(country_clean, "sa!") ~ "usa",
    endsWith(country_clean, "ates") ~ "usa",
    endsWith(country_clean, "merica") ~ "usa",
    endsWith(country_clean, "stetes") ~ "usa",
    endsWith(country_clean, "ayy") ~ "usa",
    endsWith(country_clean, "tates.") ~ "usa",
    endsWith(country_clean, "yyy") ~ "usa",
    endsWith(country_clean, "merca") ~ "usa",
    endsWith(country_clean, "tates") ~ "usa",
    endsWith(country_clean, "alaska") ~ "usa",
    endsWith(country_clean, "england") ~ "uk",
    TRUE ~ as.character(country_clean))) 

#cleaning the country column - the hard way PART 2------------------------------

all_candy_data <- all_candy_data %>% 
  mutate(country_clean = case_when(
    endsWith(country_clean, "6") ~ "unknown",
    endsWith(country_clean, "2") ~ "unknown",
    endsWith(country_clean, "lately") ~ "unknown",
    endsWith(country_clean, "ud") ~ "unknown",
    endsWith(country_clean, "tube") ~ "unknown",
    endsWith(country_clean, "loathing") ~ "unknown",
    endsWith(country_clean, "anymore") ~ "unknown",
    endsWith(country_clean, "1") ~ "unknown",
    TRUE ~ as.character(country_clean))) 

#cleaning the country column - the hard way PART 3------------------------------

all_candy_data <- all_candy_data %>% 
  mutate(country_clean = case_when(
    endsWith(country_clean, "equator") ~ "unknown",
    endsWith(country_clean, " one") ~ "unknown",
    endsWith(country_clean, "ones") ~ "unknown",
    endsWith(country_clean, "old men") ~ "unknown",
    endsWith(country_clean, ".0") ~ "unknown",
    endsWith(country_clean, "where") ~ "unknown",
    endsWith(country_clean, "see above") ~ "unknown",
    endsWith(country_clean, "5") ~ "unknown",
    endsWith(country_clean, "scotland") ~ "uk",
    TRUE ~ as.character(country_clean))) 

# clean candy_type column to reduce distinct variables-------------------------

all_candy_data <- all_candy_data %>% 
  mutate(candy_type = str_remove(candy_type, pattern = "q6_")) %>% 
  mutate(candy_type = str_remove(candy_type, pattern = "_a_k_a")) %>% 
  mutate(candy_type = str_remove(candy_type, pattern = "_a_friend_to_diabetes")) %>%
  mutate(candy_type = str_remove(candy_type, pattern = "_mary_janes")) %>% 
  mutate(candy_type = case_when(
    startsWith(candy_type, "x") ~ "100_grand_bar",
    startsWith(candy_type, "boxo") ~ "box_o_raisins",
    TRUE ~ as.character(candy_type)
  ))

# force lower case on the candy_feelings column---------------------------------

all_candy_data <- all_candy_data %>% 
  mutate(candy_feelings = str_to_lower(candy_feelings))


#----------write cleaned, combined data set to csv file------------------------

write_csv(all_candy_data, file = "../task_4/clean_data/cleaned_candy_data")







