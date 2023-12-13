library(tidyverse)
library(here)
library(brms)

set.seed(3082)
here::i_am("R/003_mrp_with_brms.R")

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

### Run the model
mod <- brm(f,
           family = bernoulli,
           data = ind,
           cores = 4,
           chains = 4,
           prior = set_prior("normal(0, 1)", class = "b"),
           control = list(adapt_delta = 0.9),
           warmup = 2000,
           iter = 2250
           )

### Generate predictions
preds <- posterior_epred(mod,
                newdata = ps,
                ndraws = 100)

### transpose them so that the predictions are on the columns
preds <- t(preds)
preds <- as.data.frame(preds)

ps <- cbind(ps, preds)

ps <- ps |>
    pivot_longer(cols = starts_with("V"),
                 names_to = "iter",
                 values_to = "predprob")

consty <- ps |>
    group_by(geogcode, ScotPcon2014, iter) |>
    summarize(indy_support = weighted.mean(predprob,
                                           w8),
              .groups = "drop") |>
    group_by(geogcode, ScotPcon2014) |>
    summarize(indy_support_mean = mean(indy_support),
              indy_support_lo = quantile(indy_support, 0.05),
              indy_support_hi = quantile(indy_support, 0.95))

consty |>
    arrange(desc(indy_support_mean)) |>
    head()

consty |>
    arrange(indy_support_mean) |>
    head()

