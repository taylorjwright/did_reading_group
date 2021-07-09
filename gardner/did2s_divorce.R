### Code heavily cribbed and adapted from Kyle Butts' did2s README
### https://github.com/kylebutts/did2s

### 0 — install and load packages

devtools::install_github("kylebutts/did2s")
install.packages("fixest") # need to have at least fixest 0.9.0

library(tidyverse)
library(readstata13)
library(did2s) 
library(fixest)
library(broom)

### 1 — load our example divorce data from Stevenson and Wolfers (2006, QJE) via
### Goodman-Bacon and Austin Nichols
divorce = read.dta13("http://pped.org/bacon_example.dta")

### 2 — generate yeasr relative to treatment
divorce = divorce %>% 
  rename(treat_year = `_nfd`) %>% # rename to be more explicit
  mutate(
    rel_year = if_else(is.na(treat_year) == "TRUE", Inf, year - treat_year)
  )

### 3 — get our static did2s estimate
did2s_static = did2s(
  data         = divorce, 
  yname        = "asmrs",
  first_stage  = ~ 0 | stfips + year, 
  second_stage = ~i(post, ref = FALSE),
  treatment    = "post",
  cluster_var  = "stfips"
)

esttable(did2s_static)

### 4 — get our bog standard static twfe 

twfe = feols(asmrs ~ post | factor(stfips) + factor(year), data = divorce)

esttable(twfe)

### 5 — get our event study did2s estimates
did2s_es = did2s(
  data         = divorce, 
  yname        = "asmrs",
  first_stage  = ~ 0 | stfips + year, 
  second_stage = ~i(rel_year, ref = c(-1, Inf)),
  treatment    = "post",
  cluster_var  = "stfips"
)

esttable(did2s_es)

### clean up the estimates
plot_did2s_es = broom::tidy(did2s_es) %>%
  filter(str_detect(term, "rel_year::")) %>% 
  select(rel_year = term, estimate, se = std.error) %>% 
  mutate(
    rel_year = as.numeric(str_remove(rel_year, "rel_year::")),
    ci_lower = estimate - 1.96 * se,
    ci_upper = estimate + 1.96 * se,
    group = "Two-Stage Estimate"
  ) %>%
  filter(rel_year <= 17 & rel_year >= -8) # Like figure 5 in Goodman-Bacon

### X — get our twfe event study 
twfe_es = feols(asmrs ~ i(rel_year, ref = c(-1, Inf)) | 
                  factor(stfips) + factor(year), data = divorce)

esttable(twfe_es)

### clean up the estimates
plot_twfe_es = broom::tidy(twfe_es) %>%
  filter(str_detect(term, "rel_year::")) %>%
  select(rel_year = term, estimate, se = std.error) %>%
  mutate(
    rel_year = as.numeric(str_remove(rel_year, "rel_year::")),
    ci_lower = estimate - 1.96 * se,
    ci_upper = estimate + 1.96 * se,
    group = "TWFE Estimate"
  ) %>%
  filter(rel_year <= 17 & rel_year >= -8) 

### combine the estimates
plot_es = plot_did2s_es %>% 
  bind_rows(., plot_twfe_es)

### 7 — plot our did2s and twfe event study estimates
ggplot() + 
  # 0 effect
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  # Confidence Intervals
  geom_errorbar(
    data = plot_es, 
    mapping = aes(x = rel_year, ymin = ci_lower, ymax = ci_upper, color = group), 
    width = 0.3
  ) +
  # Estimates
  geom_point(data = plot_es, 
             mapping = aes(x = rel_year, y = estimate, color = group), 
             size = 2
  ) +
  # Label
  scale_x_continuous(breaks = -8:17, minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(from = -18, to = 10, by = 4), minor_breaks = NULL) +
  scale_color_manual(values = c("Two-Stage Estimate" = "steelblue", "TWFE Estimate" = "#82b446")) +
  labs(x = "Relative Time", y = "Estimate", color = NULL, title = NULL) +
  theme(legend.position = "bottom")
