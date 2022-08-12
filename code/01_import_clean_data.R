# open and skim EUROPEAN VALUES STUDY - EVS 2017
library(dplyr)
library(haven)
library(psych)
library(questionr)

df <- haven::read_sav("../data/American_Mind_2008-2018.sav")

df %>% select(discuss_GW) %>% questionr::freq()

# dichotomize dependent variable



# select variables
df <- df %>% select(
  discuss_GW, wave,happening,cause_original,sci_consensus,worry,
  harm_personally,harm_US,harm_dev_countries,harm_future_gen,harm_plants_animals,when_harm_US,
  gender, age, generation, educ_category, income_category, race, ideology, party,  region9,
  service_attendance,marit_status,employment,house_size,house_type,house_own)

# clean data, remove unnecessary levels
df %>% psych::describe()

# remove "-1" (refused) and "0" (don't know) level in all 
df[df == -1] <- NA
df[df == 0] <- NA
df %>% psych::describe()
