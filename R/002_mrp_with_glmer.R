library(tidyverse)
library(here)
library(lme4)

set.seed(3082)
here::i_am("R/002_mrp_with_glmer.R")

### Read in the post-stratification frame
ps <- read.csv(here::here("data",
                         "psf.csv"))

ps <- ps |>
    mutate(ethnicity = coalesce(ethnicity, "white"))

### Check these are all factors
ps <- ps |>
    mutate_at(vars(c(geogcode, age, sex, quals, ethnicity,
                     past_list_vote)),
              factor)

### Read in the individual data
ind <- readRDS(here::here("working",
                           "tidy_ind_data.rds"))

### Read in the aggregate-level predictors
agg <- read.csv(here::here("data", "2021_results_over_vap.csv"))

### Merge on to both sets of data
ind <- left_join(ind, agg, by = "geogcode")
ps <- left_join(ps, agg, by = "geogcode")


### Set up the model formula
f <- dv ~ (1|geogcode) + (1|age) + sex + (1|quals) + ethnicity +
    (1|past_list_vote) + SNP + Lab

mod <- glmer(f, data = ind, family = binomial)

### Generate predictions from the model
ps$mu <- predict(mod, newdata = ps)
ps$predprob <- plogis(ps$mu)

### Aggregate up to constituency
consty <- ps |>
    group_by(geogcode, ScotPcon2014) |>
    summarize(mean_indy_support = weighted.mean(predprob, w8),
              .groups = "drop")

consty |>
    arrange(desc(mean_indy_support)) |>
    head()

consty |>
    arrange(mean_indy_support) |>
    head()


### What about if we try and expand the formula a little bit?
f2 <- update(f, . ~ . + DNV)

mod2 <- glmer(f2, data = ind, family = binomial)

