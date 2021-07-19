# replication of Cheng and Hoekstra (2013), sort of
# beginning section 9.6.7 in Cunningham's Causal Inference: The Mixtape

# note: everything up to the bacondecomp stuff is just taken from the R code
# provided on the mixtape website 
# (https://mixtape.scunning.com/difference-in-differences.html)

# install and load the packages we'll use
install.packages("bacondecomp")
install.packages("haven")
install.packages("lfe")
install.packages("devtools")# if devtools package not installed
devtools::install_github("asheshrambachan/HonestDiD")

library(HonestDiD)
library(bacondecomp)
library(tidyverse)
library(haven)
library(lfe)

##############################################################################
#                  Load data and estimate standard event study               #
##############################################################################


# load the data
read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

castle <- read_data("castle.dta")

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

region <- castle %>%
  select(starts_with("r20")) %>% 
  colnames %>% 
  # remove due to colinearity
  subset(.,! . %in% dropped_vars) 


# generate leads and lags
castle <- castle %>%
  mutate(
    time_til = year - treatment_date,
    lead1 = case_when(time_til == -1 ~ 1, TRUE ~ 0),
    lead2 = case_when(time_til == -2 ~ 1, TRUE ~ 0),
    lead3 = case_when(time_til == -3 ~ 1, TRUE ~ 0),
    lead4 = case_when(time_til == -4 ~ 1, TRUE ~ 0),
    lead5 = case_when(time_til == -5 ~ 1, TRUE ~ 0),
    lead6 = case_when(time_til == -6 ~ 1, TRUE ~ 0),
    lead7 = case_when(time_til == -7 ~ 1, TRUE ~ 0),
    lead8 = case_when(time_til == -8 ~ 1, TRUE ~ 0),
    lead9 = case_when(time_til == -9 ~ 1, TRUE ~ 0),
    
    lag0 = case_when(time_til == 0 ~ 1, TRUE ~ 0),
    lag1 = case_when(time_til == 1 ~ 1, TRUE ~ 0),
    lag2 = case_when(time_til == 2 ~ 1, TRUE ~ 0),
    lag3 = case_when(time_til == 3 ~ 1, TRUE ~ 0),
    lag4 = case_when(time_til == 4 ~ 1, TRUE ~ 0),
    lag5 = case_when(time_til == 5 ~ 1, TRUE ~ 0)
  )

# event study estimation formula 
event_study_formula <- as.formula(
  paste("l_homicide ~ + ",
        paste(
          paste(region, collapse = " + "),
          paste(paste("lead", 1:9, sep = ""), collapse = " + "),
          paste(paste("lag", 1:5, sep = ""), collapse = " + "), sep = " + "),
        "| year + state | 0 | sid"
  ),
)

# estimate event study
event_study_reg <- felm(event_study_formula, weights = castle$popwt, data = castle)
summary(event_study_reg)


# plot event study
# order of the coefficients for the plot
plot_order <- c("lead9", "lead8", "lead7", 
                "lead6", "lead5", "lead4", "lead3", 
                "lead2", "lead1", "lag1", 
                "lag2", "lag3", "lag4", "lag5")

# grab the clustered standard errors
# and average coefficient estimates
# from the regression, label them accordingly
# add a zero'th lag for plotting purposes
leadslags_plot <- tibble(
  sd = c(event_study_reg$cse[plot_order], 0),
  mean = c(coef(event_study_reg)[plot_order], 0),
  label = c(-9,-8,-7,-6, -5, -4, -3, -2, -1, 1,2,3,4,5, 0)
)

# This version has a point-range at each
# estimated lead or lag
# comes down to stylistic preference at the
# end of the day!
leadslags_plot %>%
  ggplot(aes(x = label, y = mean,
             ymin = mean-1.96*sd, 
             ymax = mean+1.96*sd)) +
  geom_hline(yintercept = 0.035169444, color = "red") +
  geom_pointrange() +
  theme_minimal() +
  xlab("Years before and after castle doctrine expansion") +
  ylab("log(Homicide Rate)") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_vline(xintercept = 0,
             linetype = "dashed")


##############################################################################
#                           HonestDiD Event Study                            #
##############################################################################

# Extract coefficients of regression associated with event study coefficients
coefIndex = which(grepl(x = dimnames(event_study_reg$coefficients)[[1]],
                        pattern = "lead|lag"))
betahat = event_study_reg$beta[coefIndex, ]

# Extract estimated variance-covariance matrix of event study coefficients
sigma = event_study_reg$clustervcv[coefIndex, coefIndex]

# Construct vector of event times and the scalar reference period
timeVec = c(seq(from = -9, to = -1, by = 1), seq(from = 1, to = 5, by = 1))
referencePeriod = 0
postPeriodIndices = which(timeVec > 0)
prePeriodIndices = which(timeVec < 0)

# Construct standard errors associated with event study coefficients
stdErrors = summary(event_study_reg)$coefficients[coefIndex,2]

 # Create list containing objects produced by the event study
castle_event_study = list(
    betahat = betahat,
    sigma = sigma,
    timeVec = timeVec,
    referencePeriod = referencePeriod,
    prePeriodIndices = prePeriodIndices,
    postPeriodIndices = postPeriodIndices,
    stdErrors = stdErrors
)

# number of pre- and post-periods
numPrePeriods = length(castle_event_study$prePeriodIndices)
numPostPeriods = length(castle_event_study$postPeriodIndices)

# create l_vec corresponding with 1 year of exposure (it's really 1 - (-0))
l_vec = basisVector(1, numPostPeriods)

# construct robust confidence intervals for Delta^{SD}(M) 
DeltaSD_RobustResults = createSensitivityResults(betahat = castle_event_study$betahat,
                                         sigma = castle_event_study$sigma,
                                         numPrePeriods = numPrePeriods,
                                         numPostPeriods = numPostPeriods,
                                         l_vec = l_vec,
                                         Mvec = seq(from = 0, to = 0.04, by = 0.005))

head(DeltaSD_RobustResults)

# construct dataframe with OLS confidence intervale for theta
OriginalResults = constructOriginalCS(betahat = castle_event_study$betahat,
                                      sigma = castle_event_study$sigma,
                                      numPrePeriods = numPrePeriods,
                                      numPostPeriods = numPostPeriods,
                                      l_vec = l_vec )

# construct the sensitivity plot
DeltaSD_SensitivityPlot = createSensitivityPlot(robustResults = DeltaSD_RobustResults,
                                                originalResults = OriginalResults)

DeltaSD_SensitivityPlot



