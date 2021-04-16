# replication of Cheng and Hoekstra (2013), sort of
# beginning section 9.6.7 in Cunningham's Causal Inference: The Mixtape

# note: everything up to the bacondecomp stuff is just taken from the R code
# provided on the mixtape website 
# (https://mixtape.scunning.com/difference-in-differences.html)

# install and load the packages we'll use
install.packages("bacondecomp")
install.packages("haven")
install.packages("lfe")
library(bacondecomp)
library(tidyverse)
library(haven)
library(lfe)

##############################################################################
#                   Data manipulations and other replications                #
##############################################################################

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

dd_formula <- as.formula(
  paste("l_homicide ~ ",
        paste(
          paste(xvar, collapse = " + "),
          paste(region, collapse = " + "),
          paste(lintrend, collapse = " + "),
          paste("post", collapse = " + "), sep = " + "),
        "| year + sid | 0 | sid"
  )
)

# Fixed effect regression using post as treatment variable 
# this should be the top panel of table 9.8
# note, I think the coefficient in the table should be 0.0769 rather than 0.069
dd_reg <- felm(dd_formula, weights = castle$popwt, data = castle)
summary(dd_reg)

##############################################################################
#                                 Bacondecomp                                #
##############################################################################

# run and save the decomposition without controls
# this is the DD Comparison panel in table 9.8
df_bacon = bacon(l_homicide ~ post,
                 data = castle,
                 id_var = "state",
                 time_var = "year")

# plot the estimates and weights (without controls)
# this is figure 9.26 
# note: (I actually think the hline should be 0.0769)
ggplot(df_bacon) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  geom_point() +
  geom_hline(yintercept = 0.069) +
  labs(x = "Weight", y = "Estimate", shape = "Type")