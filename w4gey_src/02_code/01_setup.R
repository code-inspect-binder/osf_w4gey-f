# PREPARATIONS ##################################################################################################

## Set working directory ----------------------------------------------------------------------------------------
setwd("XXX")



## Load libraries -----------------------------------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(data.table)
library(readxl)
library(moments)




## Functions ----------------------------------------------------------------------------------------------------

# Check if there are any implausible values (e.g. which are not labelled as NA)
checknas <- function(var) {
  as_factor(var) %>% 
    summary() %>% 
    data.frame() %>% 
    setNames("count") 
}


# Compute M, median, SD, skewness, and n
summstats <- function(data, var, group) {
  var <- enquo(var)
  if(missing(group)) {
    data %>%
      summarise(n = n(),
                mean = mean(!!var, na.rm = T),
                sd = sd(!!var, na.rm = T),
                se = sd(!!var, na.rm = T)/sqrt(n()),
                median = median(!!var, na.rm = T),
                skewness = skewness(!!var, na.rm = T),
                min = min(!!var, na.rm = T),
                max = max(!!var, na.rm = T))
  } else {
    group <- enquo(group)
    data %>% 
      group_by(!!group) %>%
      summarise(n = n(),
                mean = mean(!!var, na.rm = T),
                sd = sd(!!var, na.rm = T),
                se = sd(!!var, na.rm = T)/sqrt(n()),
                median = median(!!var, na.rm = T),
                skewness = skewness(!!var, na.rm = T),
                min = min(!!var, na.rm = T),
                max = max(!!var, na.rm = T),
                continent = first(country_continent))
  }
}




# LOAD RAW DATA #################################################################################################

## WVS data -----------------------------------------------------------------------------------------------------
# Source: https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp (retrieved 09/04/21)
wvs7 <- readRDS(file = "01_data/WVS_Cross-National_Wave_7_R_v1_6.rds")




## Country names and ISO codes ----------------------------------------------------------------------------------
# Source: https://datahub.io/core/country-codes (retrieved 11/04/21)
ciso <- read_csv(file = "01_data/country-codes_csv.csv", na = "NA")




## Freedom of the press -----------------------------------------------------------------------------------------
# Source: https://freedomhouse.org/sites/default/files/2021-02/All_data_FIW_2013-2021.xlsx (retrieved 11/04/21)
fotp <- read_xlsx(path = "01_data/All_data_FIW_2013-2021.xlsx", "FIW13-21", range = "A2:AR1259", na = "N/A")




## Prevalence of populist rhetoric in political discourse -------------------------------------------------------
# Source: https://doi.org/10.7910/DVN/WMGTNS (retrieved 13/04/21)
popu <- read_csv(file = "01_data/Global Party Survey by Party SPSS V2_1_Apr_2020-2.csv")




## Uncertainty avoidance ----------------------------------------------------------------------------------------
# Source: https://geerthofstede.com/research-and-vsm/dimension-data-matrix/ (retrieved 12/04/21)
hofs <- read_delim(file = "01_data/6-dimensions-for-website-2015-08-16.csv", delim = ";", na = "#NULL!")




# PREPARE WVS DATA ##############################################################################################

# Country names and continents
ciso %<>% select(country_iso = `ISO3166-1-Alpha-3`, 
                 country_name = `CLDR display name`,
                 country_continent = `Region Name`) %>%
  mutate(country_continent = case_when(country_name == "Taiwan" ~ "Asia", T ~ country_continent),
         country_name = case_when(country_name == "US" ~ "United States", T ~ country_name))


# Differentiate between Northern America and Latin America
ciso %<>% mutate(country_continent = case_when(country_name == "United States" ~ "Northern America",
                                               country_name == "Bermuda" ~ "Northern America",
                                               country_name == "Canada" ~ "Northern America",
                                               country_name == "Greenland" ~ "Northern America",
                                               country_name == "St. Pierre & Miquelon" ~ "Northern America",
                                               country_continent == "Americas" ~ "Latin America",
                                               T ~ country_continent))


# Merge country and continent names with WVS data
wvs7 %<>% left_join(ciso, by = c("B_COUNTRY_ALPHA" = "country_iso"))


# Select relevant variables
ds <- select(wvs7,
             id = D_INTERVIEW,
             year = A_YEAR,
             country_iso = B_COUNTRY_ALPHA,
             country_name = country_name,
             country_continent = country_continent,
             town_size_8cat = G_TOWNSIZE,
             sex = Q260,
             age = Q262,
             edu_9cat = Q275,
             income_10cat = Q288,
             religod = Q164,
             polorntright = Q240,
             sci_lesseasy = Q158,
             sci_opnxtgen = Q159,
             sci_dpdfaith = Q160,
             sci_rghtwrng = Q161,
             sci_imptknow = Q162,
             sci_worseoff = Q163,
             media_newsppr = Q201,
             media_tv = Q202,
             media_radio = Q203)


# Check whether there are any implausible values and how many NAs there are
checknas(ds$sex)
checknas(ds$age)
checknas(ds$edu_9cat)
checknas(ds$income_10cat)
checknas(ds$town_size_8cat)
checknas(ds$religod)
checknas(ds$polorntright) # was not measured in all countries, thus many NA's
checknas(ds$sci_lesseasy)
checknas(ds$sci_opnxtgen)
checknas(ds$sci_dpdfaith)
checknas(ds$sci_rghtwrng)
checknas(ds$sci_imptknow)
checknas(ds$sci_worseoff)
checknas(ds$media_newsppr)
checknas(ds$media_tv)
checknas(ds$media_radio)


# Recode sex (1 = male)
ds$sex_rec <- (1 - ds$sex) + 1


# Invert negatively worded attitude items
ds$sci_lesseasy <- 11 - ds$sci_lesseasy
ds$sci_opnxtgen <- 11 - ds$sci_opnxtgen
ds$sci_worseoff <- 11 - ds$sci_worseoff





# ATTACH COUNTRY-LEVEL INDICATORS ###############################################################################

## Freedom of the press -----------------------------------------------------------------------------------------

# Select relevant variables
fotp %<>% select(country_name = `Country/Territory`, year = Edition, ix_fotp = Total)


# Merge by country and year
ds %<>% left_join(fotp, by = c("country_name", "year"))


# No scores for some countries and years, so we use proxies:
# - Hong Kong 2019 score (59) as proxy for Macau 2019 score
# - Puerto Rico 2016 score (90) as proxy for Puerto Rico 2018 score (year of WVS survey)
select(filter(fotp, year == 2019, country_name == "Hong Kong"), country_name, ix_fotp) 
select(filter(fotp, year == 2016, country_name == "Puerto Rico"), country_name, ix_fotp)

ds %<>% mutate(ix_fotp = case_when(country_name == "Macau" ~ 59, 
                                   country_name == "Puerto Rico" ~ 90,
                                   TRUE ~ ix_fotp))


# Confirm that we have no missing scores:
summstats_ix_fotp <- group_by(ds, country_name) %>% summarise(ix_fotp = first(ix_fotp))

arrange(summstats_ix_fotp, ix_fotp) %>% print(n = nrow(.))




## Prevalence of populist rhetoric in political discourse -------------------------------------------------------

# Select relevant variables:
# - V8_Scale: Pluralist vs populist rhetoric of a given party, 
#             0 = Strongly favors pluralist rhetoric, 10 = Strongly favors populist rhetoric
# - V9:       Importance of populist rhetoric for a given party,
#             0 = no importance, 10 = great importance
popu %<>% select(country_iso = ISO, party = Partyname, pop_rhetoric = V8_Scale, pop_saliency = V9)


# Compute populism score (row mean of means of pop_rhetoric and pop_saliency across all parties per country)
popu %<>% group_by(country_iso) %>% summarise(pop_rhetoric_m = mean(pop_rhetoric, na.rm = T),
                                              pop_saliency_m = mean(pop_saliency, na.rm = T)) %>%
  mutate(ix_popx = rowMeans(select(., pop_rhetoric_m, pop_saliency_m), na.rm = T))


# Merge by country and year
ds %<>% left_join(popu, by = "country_iso")


# No scores for some countries, but based on the literature we determine:
# 
# - Andorra:     No populist parties (Eiermann et al., 2017), so we assign a low score (5.00) 
# - China:       Recent literature suggests that the Chinese political system and its leader Xi Jinping feature 
#                several aspects of populism, authoritarianism, and anti-pluralism, so we assign a relatively 
#                high score (7.00) (Devinney & Hartwell, 2020; Guan & Yang, 2020; Zhang, 2020)
# - Hong Kong:   Scholars suggest that political discourse in Hong Kong features some populist elements, which is
#                partly due to populist rhetoric of politicians/parties, but mostly due to grassroots movements 
#                and localist groups, so we assign a medium score (6.00) (Aslanidis, 2017; Lam-Knott, 2020; 
#                Ng & Kennedy, 2019; Tang, 2017)
# - Puerto Rico: Use United States as proxy (5.93)
ds %<>% mutate(ix_popx = case_when(country_iso == "AND" ~ 5.00, 
                                   country_iso == "CHN" ~ 7.00,
                                   country_iso == "HKG" ~ 6.00,
                                   country_iso == "PRI" ~ 5.93,
                                   TRUE ~ ix_popx))


# Confirm that we have no missing scores:
summstats_ix_popx <- group_by(ds, country_name) %>% summarise(ix_popx = first(ix_popx))

arrange(summstats_ix_popx, ix_popx) %>% print(n = nrow(.))




## Uncertainty avoidance ----------------------------------------------------------------------------------------

# Select relevant variables and prepare data
hofs %<>% select(country_name = country, ix_uaix = uai) %>%
  mutate(country_name = case_when(country_name == "U.S.A." ~ "United States",
                                  country_name == "Korea South" ~ "South Korea",
                                  TRUE ~ country_name))


# Merge by country and year
ds %<>% left_join(hofs, by = "country_name")


# Some scores missing, but using some proxies and based on the literature we determine:
# 
# - Andorra: 86 (use Spain)
# - Bolivia: 87*
# - Cyprus: 93 (mean of Greece 100 and Turkey 85)
# - Egypt: 80*
# - Ethiopia: 55*
# - Iraq: 85*
# - Jordan: 65*
# - Kazakhstan: 88*
# - Kyrgyzstan: 88 (use Kazakhstan)
# - Lebanon: 50*
# - Macau: 29 (use Hong Kong)
# - Myanmar: 85 (https://infogram.com/myanmar-5-dimensions-of-culture-1g4qpzjg8ojlm1y)
# - Nicaragua: 68 (mean of Costa Rica 86 and Honduras 50)
# - Nigeria: 55*
# - Puerto Rico: 38*
# - Tajikistan: 88 (use Kazakhstan)
# - Tunisia: 75*
# - Ukraine: 95*
# - Zimbabwe: 44 (use Mozambique)
#
# *estimates by Hofstede et al.: https://www.hofstede-insights.com/product/compare-countries/
ds %<>% mutate(ix_uaix = case_when(country_name == "Andorra" ~ 86,
                                   country_name == "Bolivia" ~ 87,
                                   country_name == "Cyprus" ~ 93,
                                   country_name == "Egypt" ~ 80,
                                   country_name == "Ethiopia" ~ 55,
                                   country_name == "Iraq" ~ 85,
                                   country_name == "Jordan" ~ 65,
                                   country_name == "Kazakhstan" ~ 88,
                                   country_name == "Kyrgyzstan" ~ 88,
                                   country_name == "Lebanon" ~ 50,
                                   country_name == "Macau" ~ 29,
                                   country_name == "Myanmar" ~ 85,
                                   country_name == "Nicaragua" ~ 68,
                                   country_name == "Nigeria" ~ 55,
                                   country_name == "Puerto Rico" ~ 38,
                                   country_name == "Tajikistan" ~ 88,
                                   country_name == "Tunisia" ~ 75,
                                   country_name == "Ukraine" ~ 95,
                                   country_name == "Zimbabwe" ~ 44,
                                   TRUE ~ ix_uaix))


# Confirm that we have no missing scores:
summstats_ix_uaix <- group_by(ds, country_name) %>% summarise(ix_uaix = first(ix_uaix))

arrange(summstats_ix_uaix, ix_uaix) %>% print(n = nrow(.))




# SAMPLE CHARACTERISTICS ----------------------------------------------------------------------------------------

# Overall sample size
nrow(ds)


# Sample size for multilevel models
countrylist_mlms <- c("Andorra", "Argentina", "Australia", "Bangladesh", "Bolivia", "Brazil",
                      "Chile", "Colombia", "Cyprus", "Ecuador", "Ethiopia", "Germany",
                      "Greece", "Guatemala", "Hong Kong", "Indonesia", "Japan", "Macau",
                      "Malaysia", "Mexico", "New Zealand", "Nicaragua", "Nigeria", "Peru",
                      "Philippines", "Puerto Rico", "Romania", "Russia", "Serbia",
                      "South Korea", "Taiwan", "Tajikistan", "Thailand", "Tunisia", "Turkey",
                      "Ukraine", "United States", "Zimbabwe")

filter(ds, country_name %in% countrylist_mlms) %>% nrow()


# Summary statistics of key sociodemographic variables (overall)
summstats(ds, age)
summstats(ds, sex_rec)
summstats(ds, edu_9cat)
summstats(ds, income_10cat)


# M, SD, range of key sociodemographic variables (across countries)
summstats(ds, age, country_name) %>% print(n = nrow(.))
summstats(ds, sex_rec, country_name) %>% print(n = nrow(.)) 
summstats(ds, edu_9cat, country_name) %>% print(n = nrow(.))
summstats(ds, income_10cat, country_name) %>% print(n = nrow(.))


# Summary statistics of country-level indicators
summstats(summstats_ix_fotp, ix_fotp)
summstats(summstats_ix_uaix, ix_uaix)
summstats(summstats_ix_popx, ix_popx)




# REFERENCES ----------------------------------------------------------------------------------------------------

# Aslanidis, P. (2017). Populism and social movements. In C. Rovira Kaltwasser, P. Taggart, P. Ochoa Espejo, 
#     & P. Ostiguy (Eds.), The oxford handbook of populism (pp. 306-325). Oxford University Press.
# Devinney, T. M., & Hartwell, C. A. (2020). Varieties of populism. Global Strategy Journal, 10(1), 32-66. 
#     https://doi.org/10.1002/gsj.1373
# Eiermann, M., Mounk, Y., & Gultchin, L. (2017, December 29). European populism: Trends, threats and future 
#     prospects. Tony Blair Institute for Global Change. https://institute.global/insight/renewing-centre/
#     european-populism-trends-threats-and-future-prospects 
# Guan, T., & Yang, Y. (2020). Rights-oriented or responsibility-oriented? Two subtypes of populism in 
#     contemporary china. International Political Science Review, Online First. https://doi.org/10.1177/
#     0192512120925555
# Lam-Knott, S. (2020). "Populism" in hong kong's contemporary politics. The Immanent Frame. 
#     https://tif.ssrc.org/2020/02/26/populism-in-hong-kongs-contemporary-politics/
# Ng, H.-Y., & Kennedy, K. J. (2019). Localist groups and populist radical regionalism in hong kong. China: An
#      International Journal, 17(4), 111-134.
# Tang, G. (2017). Media populism in post-handover hong kong: An investigation of media framing of public 
#     finance. Chinese Journal of Communication, 10(4), 433-449. https://doi.org/10.1080/17544750.2017.1394889
# Zhang, C. (2020). Right-wing populism with chinese characteristics? Identity, otherness and global 
#     imaginaries in debating world politics online. European Journal of International Relations, 26(1), 88-115. 
#     https://doi.org/10.1177/1354066119850253
