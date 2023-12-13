library(tidyverse)
library(here)

here::i_am("R/001_tidy_data.R")

ind <- read.csv(here::here("data", "ses21_postelection_subset.csv"))
ps <- read.csv(here::here("data", "psf.csv"))

### Convert post-stratification variables to factors
### This will help checking
ps <- ps |>
    mutate_at(vars(age, sex, quals, ethnicity, past_list_vote, geogcode),
              factor)

### (1) Age
age_breaks <- c(-Inf, 15, 19, 24,
                29, 39, 49, 64, 74, Inf)

age_labels <- c("0-15", "16-19",
                "20-24", "25-29",
                "30-39", "40-49",
                "50-64", "65-74",
                "75+")

ind <- ind |>
    mutate(age = cut(age,
                     age_breaks,
                     age_labels))

a <- nrow(ind)
ind <- ind |>
    filter(age != "0-15") |>
    mutate(age = factor(age, levels = age_labels[-1]))
b <- nrow(ind)

loss_age <- a - b

stopifnot(all(levels(ind$age) == levels(ps$age)))

### (2) Sex
ind <- ind |>
    mutate(sex = factor(birthSex,
                        levels = levels(ps$sex)))

a <- nrow(ind)
ind <- ind |>
    filter(!is.na(sex))
b <- nrow(ind)

loss_sex <- a - b

### (3) Qualifications
ind <- ind |>
    mutate(quals = case_when(profile_education_level %in% c("University or CNAA higher degree (e.g. M.Sc, Ph.D)",
                                          "University or CNAA first degree (e.g. BA, B.Sc, B.Ed)",
                                          "University diploma",
                                          "Teaching qualification (not degree)",
                                          "Nursing qualification (e.g. SEN, SRN, SCM, RGN)") ~ "NVQ4+",
                             profile_education_level %in% c("onc",
                                          "ONC",
                                          "GCE A level or Higher Certificate",
                                          "Scottish Higher Certificate") ~ "NVQ3 only",
                             profile_education_level %in% c("Recognised trade apprenticeship completed") ~ "Trade Apprenticeships",
                             profile_education_level %in% c("Youth training certificate/skillseekers",
                                          "City & Guilds certificate - advanced",
                                          "Scottish Ordinary/ Lower Certificate",
                                          "CSE grade 1, GCE O level, GCSE, School Certificate") ~
                                 "NVQ2 only",
                             profile_education_level %in% c("Clerical and commercial",
                                          "City & Guilds certificate",
                                          "CSE grades 2-5") ~ "NVQ1 only",
                             profile_education_level %in% c("No formal qualifications",
                                          "Don't know",
                                          "Prefer not to say") ~ "no qualifications (NVQ)",
                             profile_education_level %in% c("Other technical, professional or higher qualification") ~
                                 "other qualifications (NVQ)",
                             TRUE ~ NA_character_))

ind <- ind |>
    mutate(quals = factor(quals,
                          levels = levels(ps$quals)))


a <- nrow(ind)
ind <- ind |>
    filter(!is.na(quals))
b <- nrow(ind)

loss_quals <- a - b

### (4) Ethnicity
ind <- ind |>
    mutate(ethnicity = case_when(ethnicity_new %in% c("Any other White background",
                                                      "English / Welsh / Scottish / Northern Irish / British",
                                                      "Gypsy or Irish Traveller",
                                                      "Irish") ~ "white",
                                 ethnicity_new1617 %in% c("Any other White background",
                                                      "English / Welsh / Scottish / Northern Irish / British",
                                                      "Gypsy or Irish Traveller",
                                                      "Irish") ~ "white",
                                 !is.na(ethnicity_new) ~ "ethnic minority",
                                 !is.na(ethnicity_new1617) ~ "ethnic minority"))



ind <- ind %>%
    mutate(ethnicity = factor(ethnicity,
                              levels = levels(ps$ethnicity)))
           
a <- nrow(ind)
ind <- ind |>
    filter(!is.na(ethnicity))
b <- nrow(ind)

loss_ethn <- a - b

### (5) Past list vote
ind <- ind |>
    mutate(turnout = case_when(w2_voted == "I definitely voted in the Scottish Parliament election" ~ 1,
                               TRUE ~ 0))

                                   
ind <- ind |>
    mutate(past_list_vote = case_when(w2_vb15 == "Scottish Conservative and Unionist" ~ "Con",
                               w2_vb15 == "Scottish Labour" ~ "Lab",
                               w2_vb15 == "Scottish Liberal Democrat" ~ "LibDem",
                               w2_vb15 == "Scottish National Party" ~ "SNP",
                               w2_vb15 == "Scottish Family Party" ~ "SNP",
                               w2_vb15 == "All for Unity" ~ "Other",
                               w2_vb15 == "Reform UK" ~ "Other",
                               w2_vb15 == "Scottish Greens" ~ "Green",
                               w2_vb15 == "UK Independence Party (UKIP)" ~ "Other",
                               w2_vb15 == "Alba Party" ~ "Other",
                               w2_vb15 == "Don't know" ~ "DNV",
                               w2_vb15 == "Some other party" ~ "Other",
                               TRUE ~ NA_character_))

ind <- ind |>
    mutate(past_list_vote = case_when(turnout == 1 ~ past_list_vote,
                                      turnout == 0 ~ "DNV"))


ind <- ind |>
    mutate(past_list_vote = factor(past_list_vote,
                                   levels = levels(ps$past_list_vote)))

a <- nrow(ind)
ind <- ind |>
    filter(!is.na(past_list_vote))
b <- nrow(ind)

loss_pastvote <- a - b

### (6) Geogcode
lu <- read.csv(here::here("data", "2021_results_over_vap.csv")) |>
    dplyr::select(geogcode, ScotPcon2014)

ind <- merge(ind, lu,
             by = "ScotPcon2014",
             all.x = TRUE, all.y = FALSE)


ind <- ind |>
    mutate(geogcode = factor(geogcode,
                             levels = levels(ps$geogcode)))


a <- nrow(ind)
ind <- ind |>
    filter(!is.na(geogcode))
b <- nrow(ind)

loss_geogcode <- a - b

### (7) Our dependent variable, current support for independence

ind <- ind |>
    mutate(dv = case_when(w2_uk10 == 1 ~ 1L,
                          w2_uk10 == 2 ~ 0L,
                          TRUE ~ NA_integer_))

a <- nrow(ind)
ind <- ind |>
    filter(!is.na(dv))
b <- nrow(ind)

loss_dv <- a - b

### Remove some variables
ind <- ind |>
    dplyr::select(geogcode, age, sex, quals, ethnicity, past_list_vote,
                  dv)

### Save this
saveRDS(ind, file = here::here("working",
                               "tidy_ind_data.rds"))
