# replication of Stevenson and Wolfers (2006, QJE)
# beginning page 21 of Goodman-Bacon (2021)

# install and load the packages we'll use
install.packages("bacondecomp")
install.packages("tidyverse")
install.packages("readstata13")
library(bacondecomp)
library(tidyverse)
library(readstata13)

# load the divorce data that comes with the Stata ado file
# data with the R package (bacondecomp::divorce) doesn't seem
# to be the same as used in the replication in the paper (I might be wrong)
divorce = read.dta13("http://pped.org/bacon_example.dta")

# get twfe estimate
two_way_fe = lm(asmrs ~ post + factor(stfips) + factor(year), data = divorce)

# view the estimates, coefficient on post is the effect we're interested in
summary(two_way_fe)
# see that it's pretty much the -3.08 (s.e. = 1.13) from page 22

# run and save the decomposition without controls
bacon_divorce = bacon(asmrs ~ post,
                 data = divorce,
                 id_var = "stfips",
                 time_var = "year")

# plot the estimates and weights (without controls)
# this is figure 6
ggplot(bacon_divorce) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = "Weight", y = "Estimate", shape = "Type")

# run and save the decomposition with controls (per capita income)
bacon_divorce_controls = bacon(asmrs ~ post + pcinc,
                         data = divorce,
                         id_var = "stfips",
                         time_var = "year")

# note the change in the output with controls
# the reason for this is explained on page 7 of the FAQ:
# "Why does the output of bacondecomp differ with and without controls?"

# plot the estimates and weights (with controls)
ggplot(bacon_divorce_controls$two_by_twos) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = "Weight", y = "Estimate", shape = "Type")