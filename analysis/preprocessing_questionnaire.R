

# Load libraries ----------------------------------------------------------

library(pacman)
p_load(tidyverse,
       readxl,
       here)

# Load raw data -----------------------------------------------------------

raw_data <- read_excel(here::here("data", "questionnaire_results.xlsx"))

# Data cleaning -----------------------------------------------------------

# create vector of question names
question_items <- colnames(raw_data)[8:38]
# removing example text in brackets etc
question_items_shorter <- gsub("\\(.*?\\)", "", question_items)

# removing random escaped characters
question_items <- gsub("\n","",question_items,fixed=T)
question_items <- gsub("\r","",question_items,fixed=T)

# clean covariates
covariates <- raw_data %>% 
  as_tibble() %>% 
  select(1:7, 39:40) %>% 
  rename(
    above_18 = 1,
    username = 2, 
    age = 3, 
    gender = 4, 
    country = 5,
    how_often_play_chess = 6,
    how_often_study_chess = 7,
    date_completed = 8,
    token = 9) 

# ensure factor variables are correctly ordered
covariates$how_often_play_chess <- factor(covariates$how_often_play_chess, 
                                          levels = c("Less than once per week",
                                                     "Once per week",
                                                     "Two times per week",
                                                     "More than three times per week",
                                                     "Everyday"))

covariates$how_often_study_chess <- factor(covariates$how_often_study_chess, 
                                           levels = c("Less than once per week",
                                                      "Once per week",
                                                      "Two times per week",
                                                      "More than three times per week",
                                                      "Everyday"))


# clean questions
questions <- raw_data %>% 
  as_tibble() %>% 
  select(8:38) %>% 
  rename(Q = 1:31)

# combine
data_clean       <- bind_cols(covariates,questions)

# Checking that all participants are over 18: YES THEY ARE
# unique(data_clean$above_18)
# removing that column
data_clean <- select(data_clean, -above_18)


# Creating data frame for likert plots ------------------------------------

# 1: Create a vector of the text descriptions of likert choices 1-5
choices  = c("Highly Disagree", "Disagree", "Neutral", "Agree", "Highly Agree")

questions_named <- tibble::as_tibble(sapply(questions, factor, levels=1:5, labels=choices, ordered=TRUE))

questions_named <- questions_named %>% 
  rename_with(everything(), .fn = function(x) question_items_shorter)

questions_named[1:31] <- lapply(questions_named[1:31], factor, levels = choices)
questions_named <- as.data.frame(questions_named)

# Save data ---------------------------------------------------------------

write_csv(data_clean, path = here::here("data", "preprocessed_questionnaire.csv"))
save(data_clean, questions_named, questions, question_items, covariates, choices, file = here::here("data", "preprocessed_questionnaire.RData"))
