install.packages("haven")
install.packages("lfe")
install.packages("did")
install.packages("tidyverse")

library(did)
library(tidyverse)
library(haven)
library(lfe)

read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

castle <- read_data("castle.dta")

#--- global variables
crime1 <- c("jhcitizen_c", "jhpolice_c", 
            "murder", "homicide", 
            "robbery", "assault", "burglary",
            "larceny", "motor", "robbery_gun_r")

demo <- c("emo", "blackm_15_24", "whitem_15_24", 
          "blackm_25_44", "whitem_25_44")

# variables dropped to prevent colinearity
dropped_vars <- c("r20004", "r20014",
                  "r20024", "r20034",
                  "r20044", "r20054",
                  "r20064", "r20074",
                  "r20084", "r20094",
                  "r20101", "r20102", "r20103",
                  "r20104", "trend_9", "trend_46",
                  "trend_49", "trend_50", "trend_51"
)

lintrend <- castle %>%
  select(starts_with("trend")) %>% 
  colnames %>% 
  # remove due to colinearity
  subset(.,! . %in% dropped_vars) 

region <- castle %>%
  select(starts_with("r20")) %>% 
  colnames %>% 
  # remove due to colinearity
  subset(.,! . %in% dropped_vars) 


exocrime <- c("l_lacerny", "l_motor")
spending <- c("l_exp_subsidy", "l_exp_pubwelfare")


xvar <- c(
  "blackm_15_24", "whitem_15_24", "blackm_25_44", "whitem_25_44",
  "l_exp_subsidy", "l_exp_pubwelfare",
  "l_police", "unemployrt", "poverty", 
  "l_income", "l_prisoner", "l_lagprisoner"
)

law <- c("cdl")

castle_treat = castle %>% 
  group_by(state) %>%
  filter(cdl != 0) %>%
  slice(1) %>%
  select(year, state) %>%
  rename(first_treat = year)

castle_did = left_join(castle, castle_treat, by = "state")

castle_attg = att_gt(yname = "l_homicide",
                     gname = "first_treat",
                     idname = "sid",
                     xformla = ~1,
                     data = castle_did)

data = mpdta

out1 <- att_gt(yname="lemp",
               tname="year",
               idname="countyreal",
               gname="first.treat",
               xformla=~1,
               data=mpdta)
ggdid(out1)

summary(out1)

es_out = aggte(out1, type = "dynamic")
ggdid(es_out)

es_out = aggte(out1, type = "dynamic", balance_e=1)
ggdid(es_out)

