# PREPARATIONS ##################################################################################################

## Load libraries -----------------------------------------------------------------------------------------------
library(sjlabelled)
library(sjPlot)
library(scales)
library(brms)
library(bayestestR)
library(bayesplot)
library(ggridges)
library(ggstance)
library(viridis)




## Functions ----------------------------------------------------------------------------------------------------

# Collapse response frequencies from 10 to 5 options 
# 1-2 = completely disagree, 3-4 = rather disagree, 5-6 = undecided, 7-8 = rather agree, 9-10 = completely agree
collapse10to5 <- function(var) {
  var <- as.numeric(var)
  case_when(var %in% 1:2 ~ 1,
            var %in% 3:4 ~ 2,
            var %in% 5:6 ~ 3,
            var %in% 7:8 ~ 4,
            var %in% 9:10 ~ 5,
            T ~ NA_real_)
}


# Recode NAs to 0 for response frequency tables
recodeNAto0 <- function(var) {
  case_when(is.na(var) ~ 0, T ~ var)
}


# Compute response frequency table for recoded items
proptable <- function(var) {
  table(ds$country_name, var) %>% 
  addmargins() %>%
  data.frame() %>%
  rename(country = 1, response = 2, prop = 3) %>%
  spread(response, prop) %>%
  mutate(`0` = `0`/Sum, `1` = `1`/Sum, `2` = `2`/Sum, `3` = `3`/Sum, `4` = `4`/Sum, `5` = `5`/Sum) 
}


# Plot response frequency table for recoded items
propplot <- function(proptable_05cats, itemlabel) {
  
  # Re-arrange data
  data <- proptable_05cats %>%
    arrange(`5`) %>%
    select(-Sum) %>%
    gather(response, prop, `0`:`5`)
  
  # Plot stacked bar chart
  data %>%
    ggplot(aes(x = fct_inorder(fct_drop(country)), y = prop, fill = response)) +
    geom_bar(stat = "identity", position = "fill")  +
    geom_text(aes(label = paste0(round(prop * 100, 0), "%")), 
              position = position_fill(vjust = 0.5), color = "white", size = 3) +
    coord_flip() + 
    ggtitle(label = "Relative response frequencies across countries",
            subtitle = itemlabel) +
    scale_y_continuous(labels = label_percent(accuracy = 1L), 
                       breaks = c(seq(0, 1, 0.1))) +
    guides(fill = guide_legend(reverse = T,
                               title = "Response")) +
    scale_fill_manual(values = c("grey50", viridis_pal(option = "inferno", begin = 0.1, end = 0.6)(5)),
                      labels = c("don't know/no answer",
                                 "completely disagree", 
                                 "rather disagree", 
                                 "undecided", 
                                 "rather agree", 
                                 "completely agree")) + 
    theme_minimal() +
    theme(legend.position = "bottom", 
          plot.title = element_text(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.x = element_line(colour = "grey50"))

}


# Plot means across countries
plotmeans <- function(data, itemlabel){

  # Prepare some vectors to specify annotations and their positions later on
  means <- data %>%
    arrange(mean) %>%
    pull(mean)
  
  xmax <- max(data$mean)
  xmin <- min(data$mean)
  
  # Create plot
  data %>%
    ggplot(aes(y = reorder(country_name, mean), 
               x = mean, 
               color = continent)) + 
    geom_pointrange(aes(xmin = mean - se, xmax = mean + se, color = continent)) +
    stat_summary(aes(label = sprintf("%0.2f", round(..x.., 2))), 
                 geom = "text", size = 3, hjust = 0, color = "black",
                 position = position_nudge(x = xmax + xmin/5 - means)) +
    scale_color_viridis_d(name = "Continent", option = "inferno", begin = 0.1, end = 0.9, direction = -1) +
    coord_cartesian(xlim = c(xmin - xmin/20, xmax + xmax/20), clip = "off") +
    ggtitle(label = "Means and SEs across countries",
            subtitle = itemlabel) +
    ylab("") + xlab("Mean (error bars show SEs)") +
    theme_minimal() + 
    theme(plot.margin = unit(c(1, 6, 1, 1), "lines"), legend.position = "bottom")
}




# DESCRIPTIVE ANALYSIS ##########################################################################################

## Unweighted response frequencies across countries -------------------------------------------------------------
collapse10to5(ds$sci_dpdfaith) %>% 
  recodeNAto0() %>% 
  proptable() %>%
  slice(1:49) %>% 
  propplot(get_label(ds$sci_dpdfaith))

#ggsave("04_plots/propplot_sci_dpdfaith.pdf", width = 8, height = 12)


collapse10to5(ds$sci_rghtwrng) %>% 
  recodeNAto0() %>% 
  proptable() %>% 
  slice(1:49) %>% 
  propplot(get_label(ds$sci_rghtwrng))

#ggsave("04_plots/propplot_sci_rghtwrng.pdf", width = 8, height = 12)


collapse10to5(ds$sci_imptknow) %>% 
  recodeNAto0() %>% 
  proptable() %>% 
  slice(1:49) %>% 
  propplot(get_label(ds$sci_imptknow))

#ggsave("04_plots/propplot_sci_imptknow.pdf", width = 8, height = 12)




## Unweighted item means ----------------------------------------------------------------------------------------

# Summary statistics
summstats(ds, sci_dpdfaith)
summstats(ds, sci_rghtwrng)
summstats(ds, sci_imptknow)


# Summary statistics across countries
summstats(ds, sci_dpdfaith, country_name)
summstats(ds, sci_rghtwrng, country_name)
summstats(ds, sci_imptknow, country_name)


# Plots of means with standard errors
summstats(ds, sci_dpdfaith, country_name) %>% plotmeans(itemlabel = get_label(ds$sci_dpdfaith))
#ggsave("04_plots/freqmeans_sci_dpdfaith.pdf", width = 8, height = 12)

summstats(ds, sci_rghtwrng, country_name) %>% plotmeans(itemlabel = get_label(ds$sci_rghtwrng))
#ggsave("04_plots/freqmeans_sci_rghtwrng.pdf", width = 8, height = 12)

summstats(ds, sci_imptknow, country_name) %>% plotmeans(itemlabel = get_label(ds$sci_imptknow))
#ggsave("04_plots/freqmeans_sci_imptknow.pdf", width = 8, height = 12)




## Posterior distributions of MCMC draws for anti-science attitudes ---------------------------------------------

# Estimate null model intercept for full sample
brmsmeans_antisci_full <- brm(antisci ~ 1, 
                              data = ds,
                              chains = 4,
                              warmup = 1000,
                              iter = 4000,
                              cores = 4,
                              seed = 101)


#saveRDS(brmsmeans_antisci_full, file = "03_models/brmsmeans_antisci_full.rds")
brmsmeans_antisci_full <- readRDS(file = "03_models/brmsmeans_antisci_full.rds")


# Extract intercept (mean estimate)
mean(as.matrix(brmsmeans_antisci_full)[,"b_Intercept"])


# Compute 89% HDI
as.matrix(brmsmeans_antisci_full)[,"b_Intercept"] %>% bayestestR::hdi(ci = 0.89)


# Split dataset based on country
ds_split <- split(ds, f = ds$country_name)


# Estimate null model intercepts for each country separately
brmsmeans_antisci_cntr <- brm_multiple(antisci ~ 1, 
                                       data = ds_split, 
                                       combine = F,
                                       chains = 4,
                                       warmup = 1000,
                                       iter = 4000,
                                       cores = 4,
                                       seed = 101)


#saveRDS(brmsmeans_antisci_cntr, file = "03_models/brmsmeans_antisci_cntr.rds")
brmsmeans_antisci_cntr <- readRDS(file = "03_models/brmsmeans_antisci_cntr.rds")


# Extract intercepts (mean estimates)
brmsmeans_antisci_cntr_post <- data.frame(matrix(ncol = 0, nrow = 12000))

for(i in 1:length(unique(ds$country_name))) {
  brmsmeans_antisci_cntr_post %<>% cbind.data.frame(as.matrix(brmsmeans_antisci_cntr[[i]])[,"b_Intercept"])
  colnames(brmsmeans_antisci_cntr_post)[i] <- sort(unique(ds$country_name))[i]
}


# Gather, pre-compute means and 89% HDIs per country, add continents
brmsmeans_antisci_cntr_post %<>% gather(key = "country_name", value = "antisci_mean") %>%
  group_by(country_name) %>%
  mutate(antisci_cntry_mean = mean(antisci_mean), bayestestR::hdi(antisci_mean, ci = 0.89)) %>%
  left_join(summarise(group_by(ds, country_name), continent = first(country_continent)), by = "country_name") 


# Prepare some vectors to specify annotations and their positions later on
brmsmeans_means <- arrange(brmsmeans_antisci_cntr_post, antisci_cntry_mean) %>% pull(antisci_cntry_mean) %>% unique()
brmsmeans_hdi_l <- arrange(brmsmeans_antisci_cntr_post, antisci_cntry_mean) %>% pull(CI_low) %>% unique()
brmsmeans_hdi_h <- arrange(brmsmeans_antisci_cntr_post, antisci_cntry_mean) %>% pull(CI_high) %>% unique()


# Make density ridge plot
ggplot(data = brmsmeans_antisci_cntr_post,
       aes(x = antisci_mean, 
           y = fct_reorder(country_name, antisci_mean, .fun = mean),
           fill = factor(continent))) +
  stat_density_ridges(inherit.aes = T, calc_ecdf = F, quantile_lines = T, 
                      quantiles = 2, alpha = 0.7, scale = 3.5) +
  stat_summary(aes(label = paste0(sprintf("%0.2f", round(..x.., 2)), 
                                  " [", 
                                  sprintf("%0.2f", round(brmsmeans_hdi_l, 2)), 
                                  ", ", 
                                  sprintf("%0.2f", round(brmsmeans_hdi_h, 2)), 
                                  "]")), 
               geom = "text", size = 3, hjust = 0, color = "black", 
               position = position_nudge(x = 6.7 - brmsmeans_means)) +
  scale_fill_viridis_d(name = "Continent", option = "inferno", begin = 0.1, end = 0.9, direction = -1) +
  coord_cartesian(xlim = c(3.7, 6.3), clip = "off") +
  ggtitle(label = "Posterior probability distributions of anti-science attitudes across countries",
          subtitle = "Annotations provide distribution means and 89% HDIs") +
  xlab("Anti-science attitudes") + ylab("") +
  theme_minimal() +
  theme(plot.margin = unit(c(1, 10, 1, 1), "lines"), legend.position = "bottom")

#ggsave("04_plots/brmsmeans_antisci_cntr.pdf", width = 8, height = 12)




## Frequentist estimation of means ------------------------------------------------------------------------------

# Full sample
mean(ds$antisci, na.rm = T)


# Across countries
group_by(ds, country_name) %>% 
  summarise(antisci_mean = mean(antisci, na.rm = T)) %>%
  arrange(-antisci_mean) %>% 
  print(n = nrow(.))


