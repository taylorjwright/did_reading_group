library(tidyverse)
devtools::install_github("asheshrambachan/HonestDiD")
library(HonestDiD)
library(readstata13)
install.packages("Rglpk")
library(Rglpk)
library(lfe)
library(haven)

# Load in LWdata_RawData.dta
LWdata_RawData = haven::read_dta(system.file("extdata", "LWdata_RawData.dta",
                                             package = "HonestDiD"))

LWd
# Estimate event study using lfe package
EmpFemale.EventStudy = lfe::felm(emp ~ rtESV13 + rtESV14 + rtESV15 +
                                   rtESV16 + rtESV17 + rtESV18 +
                                   rtESV19 + rtESV110 + rtESV111 + # End Pre-periods 
                                   rtESV113 + rtESV114 + rtESV115 +
                                   rtESV116 + rtESV117 + rtESV118 +
                                   rtESV119 + rtESV120 + rtESV121 +
                                   rtESV122 + rtESV123 + rtESV124 +
                                   rtESV125 + rtESV126 + rtESV127 +
                                   rtESV128 + rtESV129 + rtESV130 +
                                   rtESV131 + rtESV132 + rtESV133 +
                                   rtESV134 + rtESV135 + # End post-periods 
                                   yearsfcor + yearsflr + aveitc + fscontrol +
                                   asian + black + hispanic + other | 
                                   factor(PUS_SURVEY_YEAR)*factor(BIRTHYEAR) + 
                                   factor(PUS_SURVEY_YEAR) + factor(BIRTHSTATE) |
                                   0 | BIRTHSTATE,
                                 data = LWdata_RawData,
                                 weights = LWdata_RawData$nobs)

# Extract coefficients of regression associated with event study coefficients
coefIndex = which(grepl(x = dimnames(EmpFemale.EventStudy$coefficients)[[1]],
                        pattern = "rtESV"))
betahat = EmpFemale.EventStudy$beta[coefIndex, ]

# Extract estimated variance-covariance matrix of event study coefficients
sigma = EmpFemale.EventStudy$clustervcv[coefIndex, coefIndex]

#Rescale by 100 so that results will be in units of percentage points
betahat = 100 * betahat
sigma = 100^2 * sigma

# Construct vector of event times and the scalar reference period
timeVec = c(seq(from = -11, to = -3, by = 1), seq(from = -1, to = 21, by = 1))
referencePeriod = -2
postPeriodIndices = which(timeVec > -2)
prePeriodIndices = which(timeVec < -2)

# Construct standard errors associated with event study coefficients
stdErrors = summary(EmpFemale.EventStudy)$coefficients[coefIndex,2]

# Create list containing objects produced by the event study
LWdata_EventStudy = list(
  betahat = betahat,
  sigma = sigma,
  timeVec = timeVec,
  referencePeriod = referencePeriod,
  prePeriodIndices = prePeriodIndices,
  postPeriodIndices = postPeriodIndices,  
  stdErrors = stdErrors
)

numPrePeriods = length(LWdata_EventStudy$prePeriodIndices)
numPostPeriods = length(LWdata_EventStudy$postPeriodIndices)

# what is the l vector that we want to specify here?
# deltaSD imposes that the change in the slppe of the underlying trend is no more than M between consecutive periods (if M=0 then parallel trends linear)


# Recommend creating plots showing how robust confidence sets vary under differnt assumptions about delta (let M vary)

# So need to run regressions from their paper, save the beta, sigma, get number of periods, specify the l_vec
l_vec = basisVector(15 - (-2), numPostPeriods)
l_vec

data('LWdata_EventStudy', package = "HonestDiD")

DeltaSD_RobustResults = createSensitivityResults(betahat = LWdata_EventStudy$betahat,
                                                 sigma = LWdata_EventStudy$sigma,
                                                 numPrePeriods = numPrePeriods,
                                                 numPostPeriods = numPostPeriods,
                                                 l_vec = l_vec,
                                                 Mvec = seq(from = 0, to = 0.04, by = 0.005))

head(DeltaSD_RobustResults)

# construct dataframe with OLS confidence interval for theta
originalresults = constructOriginalCS(betahat = LWdata_EventStudy$betahat,
                                      sigma = LWdata_EventStudy$sigma,
                                      numPrePeriods = numPrePeriods,
                                      numPostPeriods = numPostPeriods,
                                      l_vec = l_vec)

DeltaSD_SensitivityPlot =  createSensitivityPlot(robustResults = DeltaSD_RobustResults, 
                                                 originalResults = originalresults)

# So what this is showing is what happens to the estimated beta when the slope of the differential trend is allowed to change by no more than M in consecutive periods. So at M = 0 the difference in trends is linear (see panel a of figure 1) 
DeltaSD_SensitivityPlot


DeltaSDD_RobustResults = createSensitivityResults(betahat = LWdata_EventStudy$betahat,
                                                  sigma = LWdata_EventStudy$sigma,
                                                  monotonicityDirection = "decreasing",
                                                  numPrePeriods = numPrePeriods,
                                                  numPostPeriods = numPostPeriods,
                                                  l_vec = l_vec,
                                                  Mvec = seq(from = 0, to = 0.04, by = 0.005))

DeltaSDD_SensitivityPlot =  createSensitivityPlot(robustResults = DeltaSDD_RobustResults, 
                                                  originalResults = originalresults)

DeltaSDD_SensitivityPlot



