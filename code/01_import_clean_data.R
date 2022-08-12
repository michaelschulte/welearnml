# open and skim EUROPEAN VALUES STUDY - EVS 2017
library(dplyr)
library(haven)
library(psych)
library(questionr)

df <- haven::read_sav("../data/American_Mind_2008-2018.sav")

df %>% select(discuss_GW) %>% questionr::freq()

# dichotomize dependent variable



# select variables


# clean data, remove unnecessary levels


# 

