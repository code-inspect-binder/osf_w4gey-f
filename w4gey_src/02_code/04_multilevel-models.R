# PREPARATIONS ##################################################################################################

## Load libraries -----------------------------------------------------------------------------------------------
library(pbkrtest)
library(performance)
library(ggdist)
library(lme4)
library(lmerTest)




## Create ds with complete cases for relevant variables ---------------------------------------------------------

# Note:
# - Political orientation was not measured in Vietnam, Pakistan, Myanmar, Lebanon, Kyrgyzstan, Kazakhstan, 
#   Jordan, Iraq, Iran, Egypt, and China
# - Religiosity was not measured in Iran

ds_complete <- ds %>% 
  select(antisci, 
         sex_rec, age, edu_9cat, edu_9cat, income_10cat, town_size_8cat, religod, polorntright,
         ix_fotp, ix_popx, ix_uaix,
         medialgcy, country_name) %>% drop_na()

nrow(ds_complete)




# MULTILEVEL MODELS #############################################################################################

## Bayesian multilevel models -----------------------------------------------------------------------------------

### Specify priors ----------------------------------------------------------------------------------------------

# We use relatively weakly informative priors (see Lai, 2019; Nalborczyk et al., 2019)
priors_mlm <- c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(gamma(2, 0.2), class = sd),
                prior(student_t(4, 0, 5), class = sigma),
                prior(lkj(2), class = cor))




### Fit Model 1 -------------------------------------------------------------------------------------------------
brmsfit1 <- brm(antisci ~ 
                  # sociodemographic and attitudinal covariates
                  sex_rec + age + edu_9cat + income_10cat + town_size_8cat + religod + polorntright +
                  # legacy media use
                  medialgcy +
                  # varying intercepts and slopes of media use across countries
                  (medialgcy | country_name),
                data = ds_complete,
                prior = priors_mlm,
                chains = 4,
                warmup = 1000,
                iter = 4000,
                cores = 4,
                seed = 101)


#saveRDS(brmsfit1, file = "03_models/brmsfit1.rds")
brmsfit1 <- readRDS(file = "03_models/brmsfit1.rds")




### Fit Model 2 -------------------------------------------------------------------------------------------------
brmsfit2 <- brm(antisci ~ 
                  # sociodemographic and attitudinal covariates
                  sex_rec + age + edu_9cat + income_10cat + town_size_8cat + religod + polorntright +
                  # legacy media use
                  medialgcy +
                  # country-level indicators
                  ix_fotp +
                  ix_popx +
                  ix_uaix +
                  # cross-level interactions of media use and country-level indicators
                  ix_fotp:medialgcy +
                  ix_popx:medialgcy +
                  ix_uaix:medialgcy +
                  # varying intercepts and slopes of media use across countries
                  (medialgcy | country_name),
                data = ds_complete,
                prior = priors_mlm,
                chains = 4,
                warmup = 1000,
                iter = 4000,
                cores = 4,
                seed = 101)

#saveRDS(brmsfit2, file = "03_models/brmsfit2.rds")
brmsfit2 <- readRDS(file = "03_models/brmsfit2.rds")




### Model summaries ---------------------------------------------------------------------------------------------

# General summaries
summary(brmsfit1, prob = 0.89, robust = F) %>% print(digits = 3) # 89% Credible Intervals and means
summary(brmsfit2, prob = 0.89, robust = F) %>% print(digits = 3) # 89% Credible Intervals and means

bayestestR::hdi(brmsfit1, ci = 0.89, "all") %>% as.data.frame() %>% mutate_if(is.numeric, round, 3) # 89% HDIs
bayestestR::hdi(brmsfit2, ci = 0.89, "all") %>% as.data.frame() %>% mutate_if(is.numeric, round, 3) # 89% HDIs


# 89% HDI of sigma (as they are not provided in bayestestR output)
prepare_predictions(brmsfit1)$dpars$sigma %>% HDInterval::hdi(credMass = 0.89)
prepare_predictions(brmsfit2)$dpars$sigma %>% HDInterval::hdi(credMass = 0.89)


# Detailed summary of all estimates
posterior_summary(brmsfit1)
posterior_summary(brmsfit2)


# sjPlot summaries (package computes ETIs instead of HDIs)
tab_model(brmsfit1, 
          show.ci = .89, 
          show.se = T, 
          digits = 3,
          title = "Bayesian multilevel model predicting anti-science attitudes
          (varying intercepts and slopes of media use across countries)",
          dv.labels = "Anti-science attitudes",
          pred.labels = c("(Intercept)", "Gender (male)", "Age", "Education", "Income", "Town size",
                          "Religiosity", "Political orientation (right)",
                          "Legacy media use"),
          string.est = "Estimate",
          string.se = "Estimated error",
          string.ci = "ETI (89%)")

tab_model(brmsfit2, 
          show.ci = .89, 
          show.se = T, 
          digits = 3,
          title = "Bayesian multilevel model predicting anti-science attitudes
          (varying intercepts and slopes of media use across countries)",
          dv.labels = "Anti-science attitudes",
          pred.labels = c("(Intercept)", "Gender (male)", "Age", "Education", "Income", "Town size",
                          "Religiosity", "Political orientation (right)",
                          "Legacy media use",
                          "Freedom of the press (country-level)", 
                          "Prevalence of populist rhetoric (country-level)", 
                          "Uncertainty avoidance (country-level)", 
                          "Legacy media use x Freedom of the press (country-level)",
                          "Legacy media use x Prevalence of populist rhetoric (country-level)",
                          "Legacy media use x Uncertainty avoidance (country-level)"),
          string.est = "Estimate",
          string.se = "Estimated error",
          string.ci = "ETI (89%)")

tab_model(brmsfit1, brmsfit2,
          show.ci = .89, show.se = T, digits = 3,
          title = "Bayesian multilevel models predicting anti-science attitudes
          (varying intercepts and slopes of media use across countries)",
          dv.labels = "Anti-science attitudes",
          pred.labels = c("(Intercept)", "Gender (male)", "Age", "Education", "Income", "Town size",
                          "Religiosity", "Political orientation (right)",
                          "Legacy media use",
                          "Freedom of the press (country-level)", 
                          "Prevalence of populist rhetoric (country-level)", 
                          "Uncertainty avoidance (country-level)", 
                          "Legacy media use x Freedom of the press (country-level)",
                          "Legacy media use x Prevalence of populist rhetoric (country-level)",
                          "Legacy media use x Uncertainty avoidance (country-level)"),
          string.est = "Estimate",
          string.se = "Estimated error",
          string.ci = "ETI (89%)")




### ICCs --------------------------------------------------------------------------------------------------------
icc(brmsfit1)
icc(brmsfit2)




### Assess model fit --------------------------------------------------------------------------------------------

# Compute and compare LOO and WAIC
# 
# More info in Nalborczyk et al. (2019): "When additional data is not available, cross-validation techniques 
# can be used to obtain an approximation of the model's predictive abilities, among which the Bayesian 
# leave-one-out-cross-validation (LOO-CV, Vehtari, Gelman, & Gabry, 2017). Another useful tool, and 
# asymptotically equivalent to the LOO-CV, is the Watanabe Akaike Information Criterion (WAIC, Watanabe, 2010), 
# which can be conceived as a generalisation of the Akaike Information Criterion (AIC, Akaike, 1974)."

brmsfit1_crits <- add_criterion(brmsfit1, criterion = c("loo", "waic"))
brmsfit2_crits <- add_criterion(brmsfit2, criterion = c("loo", "waic"))

#saveRDS(brmsfit1_crits, file = "03_models/brmsfit1_crits.rds")
#saveRDS(brmsfit2_crits, file = "03_models/brmsfit2_crits.rds")
brmsfit1_crits <- readRDS(file = "03_models/brmsfit1_crits.rds")
brmsfit2_crits <- readRDS(file = "03_models/brmsfit2_crits.rds")


# LOOs
LOO(brmsfit1_crits)
LOO(brmsfit2_crits)


# WAICs
WAIC(brmsfit1_crits)
WAIC(brmsfit2_crits)


# Compare models
loo_compare(brmsfit1_crits, brmsfit2_crits, criterion = "loo")  # LOO
loo_compare(brmsfit1_crits, brmsfit2_crits, criterion = "waic") # WAIC




## Plot models --------------------------------------------------------------------------------------------------

### Prepare data ------------------------------------------------------------------------------------------------
brmsfit1_post <- as.array(brmsfit1)
brmsfit2_post <- as.array(brmsfit2)




### Interval plots of varying effects of legacy media use -------------------------------------------------------

#### Interval plot (using bayesplot) ----------------------------------------------------------------------------

# Get plot data
brmsfit1_plotdata <- mcmc_intervals_data(brmsfit1_post, 
                                         regex_pars = ",medialgcy",
                                         prob = 0.50,
                                         prob_outer = 0.89,
                                         point_est = "mean")


# Some transformations
brmsfit1_plotdata <- rbind(
  select(brmsfit1_plotdata, Parameter = parameter, mean = m,
         width = inner_width, CI_l = l, CI_h = h),
  select(brmsfit1_plotdata, Parameter = parameter, mean = m,
         width = outer_width, CI_l = ll, CI_h = hh)) %>%
  mutate(Parameter = str_replace(Parameter, "\\[", "\\."),
         Parameter = str_replace(Parameter, "\\]", "\\."),
         Parameter = str_replace(Parameter, "\\,", "\\.")) %>%
  arrange(Parameter, width)


# Add 50% and 89% HDIs and country/continent names
brmsfit1_plotdata <- bayestestR::hdi(brmsfit1, ci = c(0.50, 0.89), effects = "random") %>% 
  as.data.frame() %>%
  filter(grepl("\\.medialgcy", Parameter)) %>%
  left_join(brmsfit1_plotdata, by = c("Parameter", "CI" = "width")) %>%
  rename(HDI_l = CI_low, HDI_h = CI_high, width = CI) %>%
  mutate(country = str_remove_all(Parameter, str_c(c("r_country_name\\.", "\\.medialgcy\\."), collapse = "|"))) %>%
  mutate(country = str_replace(country, "\\.", " ")) %>%
  left_join(summarise(group_by(ds, country = country_name), continent = first(country_continent)), "country") 


# Prepare some vectors to specify annotations and their positions later on
brmsranefs_means <- brmsfit1_plotdata %>% filter(width == .89) %>% arrange(mean) %>% pull(mean)
brmsranefs_hdi_l <- brmsfit1_plotdata %>% filter(width == .89) %>% arrange(mean) %>% pull(HDI_l)
brmsranefs_hdi_h <- brmsfit1_plotdata %>% filter(width == .89) %>% arrange(mean) %>% pull(HDI_h)


# Plot
brmsfit1_plotdata %>%
  ggplot(aes(y = reorder(country, mean), 
             x = mean, 
             xmin = HDI_l, xmax = HDI_h,
             color = continent)) + 
  geom_pointinterval(aes(size = -width)) +
  stat_summary(aes(label = paste0(sprintf("%0.2f", round(..x.., 2)), 
                                  " [", 
                                  sprintf("%0.2f", round(brmsranefs_hdi_l, 2)), 
                                  ", ", 
                                  sprintf("%0.2f", round(brmsranefs_hdi_h, 2)), 
                                  "]")), 
               geom = "text", size = 3, hjust = 0, color = "black", 
               position = position_nudge(x = 0.6 - brmsranefs_means)) +
  scale_color_viridis_d(name = "Continent", option = "inferno", begin = 0.1, end = 0.9, direction = -1) +
  coord_cartesian(xlim = c(-0.5, 0.5), clip = "off") +
  labs(title = "Mean estimates and 89% HDIs of media effects across countries",
       subtitle = "Bayesian multilevel model predicting effects of legacy media use on anti-science attitudes") +
  ylab("") + xlab("Estimate") +
  theme_minimal() +
  vline_at(0) +
  theme(plot.margin = unit(c(1, 7, 1, 1), "lines"), legend.position = "bottom")

#ggsave("04_plots/brmsfit1_ranef-intervalsplot.png", width = 8, height = 12)




#### Interval plot (using sjPlot) -------------------------------------------------------------------------------
plot_model(brmsfit1, type = "re",
           sort.est = T,
           show.values = F,
           dot.size = 2,
           line.size = 1,
           bpe = "mean",
           prob.inner = 0.50,
           prob.outer = 0.89,
           bpe.color = "black",
           title = "Mean estimates and 89% CIs of media effects across countries",
           axis.labels = labels)$medialgcy +
  scale_x_discrete(labels = rownames(ranef(brmsfit1, pars = "medialgcy") %>% 
                                       data.frame() %>% 
                                       rename("Estimate" = 1) %>%
                                       arrange(Estimate))) +
  ylim(-0.5, 0.5) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "grey50"))




### Forest plots of varying effects of legacy media use ---------------------------------------------------------

#### Area density plot (using bayesplot) ------------------------------------------------------------------------
color_scheme_set("red")

mcmc_labels <- ranef(brmsfit1) %>% 
  data.frame() %>% 
  arrange(country_name.Estimate.medialgcy) %>% 
  rownames() %>%
  paste0("r_country_name[", ., ",medialgcy]") %>%
  str_replace(" ", ".")

mcmc_areas(brmsfit1_post, 
           regex_pars = ",medialgcy",
           prob = 0.50,
           prob_outer = 0.89,
           point_est = "mean") +
  vline_at(0) +
  scale_y_discrete(
    limits = mcmc_labels,
    labels = c("r_country_name[Turkey,medialgcy]" = "Turkey",
               "r_country_name[Bangladesh,medialgcy]" = "Bangladesh",
               "r_country_name[Tajikistan,medialgcy]" = "Tajikistan",
               "r_country_name[Bolivia,medialgcy]" = "Bolivia",
               "r_country_name[Serbia,medialgcy]" = "Serbia",
               "r_country_name[Puerto.Rico,medialgcy]" = "Puerto Rico",
               "r_country_name[Cyprus,medialgcy]" = "Cyprus",
               "r_country_name[Peru,medialgcy]" = "Peru",
               "r_country_name[Nicaragua,medialgcy]" = "Nicaragua",
               "r_country_name[Germany,medialgcy]" = "Germany",
               "r_country_name[Macau,medialgcy]" = "Macau",
               "r_country_name[Australia,medialgcy]" = "Australia",
               "r_country_name[Ecuador,medialgcy]" = "Ecuador",
               "r_country_name[New.Zealand,medialgcy]" = "New Zealand",
               "r_country_name[Ukraine,medialgcy]" = "Ukraine",
               "r_country_name[Nigeria,medialgcy]" = "Nigeria",
               "r_country_name[Colombia,medialgcy]" = "Colombia",
               "r_country_name[Hong.Kong,medialgcy]" = "Hong Kong",
               "r_country_name[Japan,medialgcy]" = "Japan",
               "r_country_name[Zimbabwe,medialgcy]" = "Zimbabwe",
               "r_country_name[Malaysia,medialgcy]" = "Malaysia",
               "r_country_name[Russia,medialgcy]" = "Russia",
               "r_country_name[Taiwan,medialgcy]" = "Taiwan",
               "r_country_name[Tunisia,medialgcy]" = "Tunisia",
               "r_country_name[Indonesia,medialgcy]" = "Indonesia",
               "r_country_name[Chile,medialgcy]" = "Chile",
               "r_country_name[Argentina,medialgcy]" = "Argentina",
               "r_country_name[Mexico,medialgcy]" = "Mexico",
               "r_country_name[Guatemala,medialgcy]" = "Guatemala",
               "r_country_name[Brazil,medialgcy]" = "Brazil",
               "r_country_name[United.States,medialgcy]" = "United States",
               "r_country_name[Philippines,medialgcy]" = "Philippines",
               "r_country_name[Romania,medialgcy]" = "Romania",
               "r_country_name[Greece,medialgcy]" = "Greece",
               "r_country_name[Andorra,medialgcy]" = "Andorra",
               "r_country_name[South.Korea,medialgcy]" = "South Korea",
               "r_country_name[Ethiopia,medialgcy]" = "Ethiopia",
               "r_country_name[Thailand,medialgcy]" = "Thailand")) +
  labs(title = "Bayesian multilevel model predicting anti-science attitudes across countries",
       subtitle = "Country-level effects of legacy media use") + 
  theme_minimal()

#ggsave("04_plots/brmsfit1_ranef-areasplot.png", width = 10, height = 10)




#### Ridges density plot (using bayesplot) ----------------------------------------------------------------------
mcmc_areas_ridges(brmsfit1_post, regex_pars = ",medialgcy") +
  vline_at(0) +
  scale_y_discrete(
    limits = mcmc_labels,
    labels = c("r_country_name[Turkey,medialgcy]" = "Turkey",
               "r_country_name[Bangladesh,medialgcy]" = "Bangladesh",
               "r_country_name[Tajikistan,medialgcy]" = "Tajikistan",
               "r_country_name[Bolivia,medialgcy]" = "Bolivia",
               "r_country_name[Serbia,medialgcy]" = "Serbia",
               "r_country_name[Puerto.Rico,medialgcy]" = "Puerto Rico",
               "r_country_name[Cyprus,medialgcy]" = "Cyprus",
               "r_country_name[Peru,medialgcy]" = "Peru",
               "r_country_name[Nicaragua,medialgcy]" = "Nicaragua",
               "r_country_name[Germany,medialgcy]" = "Germany",
               "r_country_name[Macau,medialgcy]" = "Macau",
               "r_country_name[Australia,medialgcy]" = "Australia",
               "r_country_name[Ecuador,medialgcy]" = "Ecuador",
               "r_country_name[New.Zealand,medialgcy]" = "New Zealand",
               "r_country_name[Ukraine,medialgcy]" = "Ukraine",
               "r_country_name[Nigeria,medialgcy]" = "Nigeria",
               "r_country_name[Colombia,medialgcy]" = "Colombia",
               "r_country_name[Hong.Kong,medialgcy]" = "Hong Kong",
               "r_country_name[Japan,medialgcy]" = "Japan",
               "r_country_name[Zimbabwe,medialgcy]" = "Zimbabwe",
               "r_country_name[Malaysia,medialgcy]" = "Malaysia",
               "r_country_name[Russia,medialgcy]" = "Russia",
               "r_country_name[Taiwan,medialgcy]" = "Taiwan",
               "r_country_name[Tunisia,medialgcy]" = "Tunisia",
               "r_country_name[Indonesia,medialgcy]" = "Indonesia",
               "r_country_name[Chile,medialgcy]" = "Chile",
               "r_country_name[Argentina,medialgcy]" = "Argentina",
               "r_country_name[Mexico,medialgcy]" = "Mexico",
               "r_country_name[Guatemala,medialgcy]" = "Guatemala",
               "r_country_name[Brazil,medialgcy]" = "Brazil",
               "r_country_name[United.States,medialgcy]" = "United States",
               "r_country_name[Philippines,medialgcy]" = "Philippines",
               "r_country_name[Romania,medialgcy]" = "Romania",
               "r_country_name[Greece,medialgcy]" = "Greece",
               "r_country_name[Andorra,medialgcy]" = "Andorra",
               "r_country_name[South.Korea,medialgcy]" = "South Korea",
               "r_country_name[Ethiopia,medialgcy]" = "Ethiopia",
               "r_country_name[Thailand,medialgcy]" = "Thailand")) +
  labs(title = "Bayesian multilevel regression predicting anti-science attitudes across countries",
       subtitle = "Country-level effects of legacy media use") + 
  theme_minimal()

#ggsave("04_plots/brmsfit1_ranef-ridgesplot.png", width = 10, height = 10)




## Frequentist multilevel models --------------------------------------------------------------------------------

### Fit Model 1 -------------------------------------------------------------------------------------------------
lmerfit1 <- lmer(antisci ~ 
                   # sociodemographic and attitudinal covariates
                   sex_rec + age + edu_9cat + income_10cat + town_size_8cat + religod + polorntright +
                   # legacy media use
                   medialgcy +
                   # varying intercepts and slopes of media use across countries
                   (medialgcy | country_name),
                 data = ds_complete, REML = T) 


#saveRDS(lmerfit1, file = "03_models/lmerfit1.rds")
lmerfit1 <- readRDS(file = "03_models/lmerfit1.rds")




### Fit Model 2 -------------------------------------------------------------------------------------------------
lmerfit2 <- lmer(antisci ~ 
                   # sociodemographic and attitudinal covariates
                   sex_rec + age + edu_9cat + income_10cat + town_size_8cat + religod + polorntright +
                   # legacy media use
                   medialgcy +
                   # country-level indicators
                   ix_fotp +
                   ix_popx +
                   ix_uaix +
                   # cross-level interactions of media use and country-level indicators
                   ix_fotp:medialgcy +
                   ix_popx:medialgcy +
                   ix_uaix:medialgcy +
                   # varying intercepts and slopes of media use across countries
                   (medialgcy | country_name),
                 data = ds_complete, REML = T) 


#saveRDS(lmerfit2, file = "03_models/lmerfit2.rds")
lmerfit2 <- readRDS(file = "03_models/lmerfit2.rds")




### Model summaries ---------------------------------------------------------------------------------------------

# General summaries
summary(lmerfit1)
summary(lmerfit2)




### Model comparisons -------------------------------------------------------------------------------------------
anova(lmerfit1, lmerfit2)




# REFERENCES ----------------------------------------------------------------------------------------------------

# Akaike, H. (1974). A new look at the statistical model identification. IEEE Transactions on Automatic Control, 
#     19(6), 716-723. https://doi.org/10.1109/TAC.1974.1100705
# Lai, M. (2019). Course handouts for bayesian data analysis class. 
#     https://bookdown.org/marklhc/notes_bookdown/hierarchical-multilevel-models.html#varying-sigma
# Nalborczyk, L., Batailler, C., Lovenbruck, H., Vilain, A., & Bürkner, P.-C. (2019). An introduction to 
#     Bayesian multilevel models using brms: A case study of gender effects on vowel variability in standard 
#     Indonesian. Journal of Speech, Language, and Hearing Research, 62(5), 1225-1242. 
#     https://doi.org/10.1044/2018_JSLHR-S-18-0006
# Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical bayesian model evaluation using leave-one-out 
#     cross-validation and waic. Statistics and Computing, 27(5), 1413-1432. 
#     https://doi.org/10.1007/s11222-016-9696-4
# Watanabe, S. (2010). Asymptotic equivalence of bayes cross validation and widely applicable information 
#     criterion in singular learning theory. Journal of Machine Learning Research, 11, 3571-3594.

