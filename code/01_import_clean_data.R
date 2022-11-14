# open and skim EUROPEAN VALUES STUDY - EVS 2017

if (!require("pacman")){
  install.packages("pacman")
}

pacman::p_load(dplyr,haven,psych,questionr,tidymodels,readr)

df <- haven::read_sav("data/American_Mind_2008-2018.sav")

# clean data, remove unnecessary levels
# df %>% psych::describe()

# remove "-1" (refused) and "0" (don't know) level in all 
df[df == -1] <- NA
df[df == 0] <- NA
#df %>% psych::describe()

df %>% select(discuss_GW) %>% questionr::freq()

# dichotomize dependent variable
df <- df %>%
  mutate_at(vars(discuss_GW),
            ~(discuss_GW = case_when(
              . == 1 ~ 0, 
              . == 2 ~ 0, 
              . == 3 ~ 1,
              . == 4 ~ 1))) %>% 
  mutate(discuss_GW = as.factor(discuss_GW))


# check
df %>% select(discuss_GW) %>% questionr::freq()

# reduce to most interesting variables
df <- df %>% select(
  discuss_GW, happening,cause_original,sci_consensus,worry,
  #harm_personally,harm_US,harm_dev_countries,harm_future_gen,harm_plants_animals,when_harm_US, generation, 
  gender, age, educ_category, income_category, race, ideology, party,  region4,
  marit_status,employment,house_size,house_own)


df <- 
df %>%
  filter(!is.na(discuss_GW)) %>%
  select(!starts_with('harm_')) %>%
  filter(!is.na(sci_consensus))

check_na <- 
df %>%
  select(starts_with('harm')) %>%
  is.na() %>%
  tibble() %>% 
  mutate(sum = rowSums(across(everything()))) 


table(df$happening)
df <- 
  df %>%
  mutate(happening = case_when(
    happening < 3 ~ 1,
    happening == 3 ~ 2))


table(df$cause_original)
df <-
  df %>%
  mutate(cause_original = case_when(
    cause_original == 1 ~ 1,
    cause_original == 2 ~ 2,
    cause_original == 3 ~ 2,
    cause_original == 4 ~ 2
    ))



table(df$sci_consensus)
df <- df %>% mutate(sci_consensus = case_when(
  sci_consensus == 4 ~ 2,
  sci_consensus != 4 ~ 1
))


# race as factor
df <- df %>%
  mutate(race = as.factor(race))

# recode party
df <- 
  df %>%
  mutate_at(vars(party),
            ~(party = case_when(
              . == 1 ~ 1, 
              . == 2 ~ 2, 
              . == 3 ~ 3,
              TRUE ~ NA_real_))) %>% 
  mutate(party = as.factor(party))


# recode region
df <- df %>%
  mutate(region4 = as.factor(region4))

# recode relationship status
df <- 
  df %>%
  mutate_at(vars(marit_status),
            ~(marit_status = case_when(
              . == 1 ~ 1, 
              . == 2 ~ 0, 
              . == 3 ~ 0,
              . == 4 ~ 0,
              . == 5 ~ 0,
              . == 6 ~ 1,
              TRUE ~ NA_real_))) %>% 
  mutate(marit_status = as.factor(marit_status))

questionr::freq(df$marit_status)


# recode employment
df <- 
  df %>%
  mutate_at(vars(employment),
            ~(employment = case_when(
              . < 3 ~ 1, 
              . > 2 ~ 0,
              TRUE ~ NA_real_))) %>% 
  mutate(employment = as.factor(employment))



# recode region
df <- df %>%
  mutate(house_own = as.factor(house_own))
questionr::freq(df$house_own)



# resampling
df_split <- rsample::initial_split(df, prop = 0.75, strata = discuss_GW)

df_training <- df_split %>% training()
df_testing <- df_split %>% testing()

# save 
saveRDS(df_training, file = "data/df_training.rds")
saveRDS(df_testing, file = "data/df_testing.rds")
write_rds(df, file = 'data/df.rds')
