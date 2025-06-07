# PREPARATIONS ##################################################################################################

## Load libraries -----------------------------------------------------------------------------------------------
library(lavaan)
library(psych)




## Functions ----------------------------------------------------------------------------------------------------

# Specify ESEM formula
esem.formula <- function(loadings_dt, anchors){
  
  # make is_anchor variable
  loadings_dt[, is_anchor := 0]
  for (l in names(anchors)) loadings_dt[latent != l & item == anchors[l], is_anchor := 1]
  
  # make syntax column per item; syntax is different depending on is_anchor
  loadings_dt[is_anchor == 0, syntax := paste0("start(",value,")*", item)]
  loadings_dt[is_anchor == 1, syntax := paste0(value,"*", item)]
  
  #Make syntax for each latent variable
  each_syntax <- function (l){
    paste(l, "=~", paste0(loadings_dt[latent == l, syntax], collapse = "+"),"\n")
  }
  
  # Put all syntaxes together
  paste(sapply(unique(loadings_dt$latent), each_syntax), collapse = " ")
}




# COMPUTE SCORES ################################################################################################

## Anti-science attitudes ---------------------------------------------------------------------------------------

### Run multilevel exploratory factor analysis to test factor structure of anti-science items -------------------
# We will rely on multi-group Exploratory Structural Equation Modeling (ESEM) using lavaan
# 
# Resources:
# - Basic paper (Asparouhov & Muthén, 2009)
# - ESEM as an integrative framework of CFA, SEM, and EFA (Marsh et al., 2011)
# - ESEM can handle clustered data, i.e. multiple groups, e.g. countries (Marsh et al., 2014)
# - Example of multi-group-ESEM with multiple countries (Tomás et al., 2014)
# 
# - RG: www.researchgate.net/post/How_to_handle_data_of_different_countries_in_exploratory_factor_analysis
# - Tutorials for single-group ESEM using lavaan: 
#     o https://vankesteren.github.io/efast/efa_lavaan
#     o https://msilvestrin.me/post/esem/ 



#### Step 1: Prepare data ---------------------------------------------------------------------------------------
antisci_items <- c("sci_dpdfaith", "sci_rghtwrng", "sci_imptknow")

esem_anti_data <- ds %>% select(country_iso, all_of(antisci_items))



#### Step 2: Determine number of factors ------------------------------------------------------------------------
set.seed(101)
select(esem_anti_data, -country_iso) %>% fa.parallel(fm = "ml", fa = "fa", # parallel analysis suggests 1 factor
                                                     main = "", 
                                                     ylabel = "Eigenvalues of Factors") 

#dev.print(png, file = "04_plots/parallel_anti_1F.png", width = 7, height = 5, units = 'in', res = 300)
#dev.off()



#### Step 3: Common exploratory factor analysis using GeominQ rotation ------------------------------------------
# (because GeominQ does not emphasize getting rid of cross-loadings)
esem_anti_efa <- select(esem_anti_data, -country_iso) %>% fa(fm = "ml", rotate = "geominQ", nfactor = 1)



#### Step 4: Specify ESEM ---------------------------------------------------------------------------------------

# What distinguishes ESEM from EFA and CFA is that we fit a model with the following characteristics:
# - All cross-loadings and factor variances fixed
# - All cross-loadings set to be estimated with the EFA loadings as starting points, except that one indicator
#   in each factor is set as an anchor, all its cross-loadings are fixed to be equal to the loading in the 
#   original EFA
# - The anchors should be indicators with high loading in one factor and low loadings in all the others. We 
#   use lavaan, where we set starting points for our indicators using the start() modificator

# Extract loadings
esem_anti_loadings <- matrix(esem_anti_efa$loadings, nrow = 3, ncol = 1) %>% 
  data.table() %>% 
  set_names(c("antisci")) %>%
  mutate(item = antisci_items) %>%
  melt("item", variable.name = "latent")


# Specify anchor vector (choose items with highest loading)
anchors_anti <- c(antisci = "sci_rghtwrng")


# Make model
esem_anti_model <- esem.formula(esem_anti_loadings, anchors_anti)



#### Step 5: Fit varying-intercept ESEM model -------------------------------------------------------------------
esem_anti_fit <- cfa(esem_anti_model, esem_anti_data, 
                     group = "country_iso", 
                     estimator = "MLM", 
                     group.equal = "loadings")


# Get summaries
summary(esem_anti_fit, header = T, fit.measures = T, estimates = F, standardized = T)


# Inspect Cronbach's Alpha
select(ds, sci_dpdfaith, sci_rghtwrng, sci_imptknow) %>% psych::alpha()


# Compute anti-science score and fix NaNs
ds$antisci <- select(ds, sci_dpdfaith, sci_rghtwrng, sci_imptknow) %>% rowMeans(na.rm = T)

ds$antisci[is.nan(ds$antisci)] <- NA_real_



#### Step 6: Get summary statistics -----------------------------------------------------------------------------
summstats(ds, antisci)




## Legacy media use ---------------------------------------------------------------------------------------------

### Recode legacy media use items -------------------------------------------------------------------------------
ds$media_newsppr_rec <- 5 - ds$media_newsppr
ds$media_tv_rec <- 5 - ds$media_tv
ds$media_radio_rec <- 5 - ds$media_radio



### Inspect Cronbach's Alpha  -----------------------------------------------------------------------------------
ds %>% select(media_newsppr_rec, media_tv_rec, media_radio_rec) %>% psych::alpha()



### Compute legacy media score and fix NaNs ---------------------------------------------------------------------
ds$medialgcy <- ds %>% select(media_newsppr_rec, media_tv_rec, media_radio_rec) %>% rowMeans(na.rm = T)

ds$medialgcy[is.nan(ds$medialgcy)] <- NA_real_



### Get summary statistics --------------------------------------------------------------------------------------
summstats(ds, medialgcy)





# TEST RELIABILITY OF ALTERNATIVE ANTISCI SCORE #################################################################

### Run MEFA to test factor structure of all 6 anti-science items (e.g., 3 original and 3 inverted) -------------

#### Step 1: Prepare data ---------------------------------------------------------------------------------------
antisci_items_alt <- c("sci_lesseasy", "sci_opnxtgen", "sci_dpdfaith", 
                       "sci_rghtwrng", "sci_imptknow", "sci_worseoff")

esem_anti_data_alt <- ds %>% select(country_iso, all_of(antisci_items_alt))



#### Step 2: Determine number of factors ------------------------------------------------------------------------
set.seed(101)
select(esem_anti_data_alt, -country_iso) %>% 
  fa.parallel(fm = "ml", fa = "fa", # parallel analysis suggests 2 factors
              main = "", 
              ylabel = "Eigenvalues of Factors") 

#dev.print(png, file = "04_plots/parallel_anti_2F.png", width = 7, height = 5, units = 'in', res = 300)
#dev.off()



#### Step 3: Common exploratory factor analysis -----------------------------------------------------------------
esem_anti_efa_alt2F <- select(esem_anti_data_alt, -country_iso) %>% 
  fa(fm = "ml", rotate = "geominQ", nfactor = 2)

esem_anti_efa_alt2F # all reverse-coded items load on one factor, all original items on another factor



#### Step 4: Specify ESEM ---------------------------------------------------------------------------------------

# Extract loadings
esem_anti_loadings_alt2F <- matrix(esem_anti_efa_alt2F$loadings, nrow = 6, ncol = 2) %>% 
  data.table() %>% 
  set_names(c("antisci_inverted", "antisci_original")) %>%
  mutate(item = antisci_items_alt) %>%
  melt("item", variable.name = "latent")


# Specify anchor vectors
anchors_anti_alt2F <- c(antisci_inverted = "sci_opnxtgen", antisci_original = "sci_rghtwrng")


# Make model
esem_anti_model_alt2F <- esem.formula(esem_anti_loadings_alt2F, anchors_anti_alt2F)



#### Step 5: Fit varying-intercept ESEM model -------------------------------------------------------------------
esem_anti_fit_alt2F <- cfa(esem_anti_model_alt2F, esem_anti_data_alt, 
                           group = "country_iso", 
                           estimator = "MLM", 
                           group.equal = "loadings")


# Get summaries
summary(esem_anti_fit_alt2F, header = T, fit.measures = T, estimates = F, standardized = T)


# Inspect Cronbach's Alpha for each factor
select(ds, sci_dpdfaith, sci_rghtwrng, sci_imptknow) %>% psych::alpha()
select(ds, sci_lesseasy, sci_opnxtgen, sci_worseoff) %>% psych::alpha()





### Compare 2-factor MEFA with 1-factor MEFA with all 6 anti-science items (e.g., 3 original and 3 inverted) ----

#### Step 1: Common exploratory factor analysis -----------------------------------------------------------------
esem_anti_efa_alt1F <- select(esem_anti_data_alt, -country_iso) %>% 
  fa(fm = "ml", rotate = "geominQ", nfactor = 1) # force 1 factor

esem_anti_efa_alt1F # all inverted items load on one factor, all original items on another factor



#### Step 2: Specify ESEM ---------------------------------------------------------------------------------------

# Extract loadings
esem_anti_loadings_alt1F <- matrix(esem_anti_efa_alt1F$loadings, nrow = 6, ncol = 1) %>% 
  data.table() %>% 
  set_names(c("antisci")) %>%
  mutate(item = antisci_items_alt) %>%
  melt("item", variable.name = "latent")


# Specify anchor vectors
anchors_anti_alt1F <- c(antisci = "sci_lesseasy")


# Make model
esem_anti_model_alt1F <- esem.formula(esem_anti_loadings_alt1F, anchors_anti_alt1F)



#### Step 3: Fit varying-intercept ESEM model -------------------------------------------------------------------
esem_anti_fit_alt1F <- cfa(esem_anti_model_alt1F, esem_anti_data_alt, 
                           group = "country_iso", 
                           estimator = "MLM", 
                           group.equal = "loadings")


# Get summaries
summary(esem_anti_fit_alt1F, header = T, fit.measures = T, estimates = F, standardized = T) # Very bad fit


# Inspect Cronbach's Alpha
select(ds, sci_lesseasy, sci_opnxtgen, sci_dpdfaith, sci_rghtwrng, sci_imptknow, sci_worseoff) %>% psych::alpha()






# REFERENCES ----------------------------------------------------------------------------------------------------

# Asparouhov, T., & Muthén, B. (2009). Exploratory structural equation modeling. Structural Equation Modeling: 
#     A Multidisciplinary Journal, 16(3), 397-438. https://doi.org/10.1080/10705510903008204
# Marsh, H. W., Liem, G. A. D., Martin, A. J., Morin, A. J. S., & Nagengast, B. (2011). Methodological 
#     measurement fruitfulness of exploratory structural equation modeling (ESEM): New approaches to key 
#     substantive issues in motivation and engagement. Journal of Psychoeducational Assessment, 29(4), 322-346. 
#     https://doi.org/10.1177/0734282911406657
# Marsh, H. W., Morin, A. J. S., Parker, P. D., & Kaur, G. (2014). Exploratory structural equation modeling: An 
#     integration of the best features of exploratory and confirmatory factor analysis. Annual Review of 
#     Clinical Psychology, 10, 85-110. https://doi.org/10.1146/annurev-clinpsy-032813-153700
# Tomás, I., Marsh, H. W., González-Romá, V., Valls, V., & Nagengast, B. (2014). Testing measurement invariance 
#     across spanish and english versions of the physical self-description questionnaire: An application of 
#     exploratory structural equation modeling. Journal of Sport & Exercise Psychology, 36(2), 179-188. 
#     https://doi.org/10.1123/jsep.2013-0070

