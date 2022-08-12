# open and skim EUROPEAN VALUES STUDY - EVS 2017
library(dplyr)
library(haven)
library(psych)
library(questionr)

df <- haven::read_sav("../data/evs2017.sav")

str(df$v13)
df %>% select(v13) %>% table() 
df %>% select(v13) %>% questionr::freq()

str(df$v62)
df %>% select(v62) %>% table() 
df %>% select(v62) %>% questionr::freq()

