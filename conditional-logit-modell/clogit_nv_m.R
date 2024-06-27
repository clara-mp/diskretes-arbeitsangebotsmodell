# Read in data file
data_nv_m <- read.csv2("data_hm_nv_m.csv")

library(apollo) # Load apollo package 

database <- data_nv_m

#initialize model 
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = "clogit_nv_m",
  modelDescr = "Conditonal logit model - male, not married",
  indivID    ="pid",
  mixing     = FALSE,
  HB= FALSE,
  nCores     = 1, 
  outputDirectory = "Estimation_results"
)

##### Define model parameters depending on your attributes and model specification! ####
# set values to 0 for conditonal logit model

apollo_beta =c (mu_income = 0,
                mu_leisure = 0,
                mu_income_sq = 0,
                mu_leisure_sq = 0,
                mu_income_leisure = 0,
                mu_age_income = 0,
                mu_age_sq_income = 0,
                mu_east_income = 0,
                mu_age_leisure = 0,
                mu_age_sq_leisure = 0,
                mu_east_leisure = 0,
                mu_child_leisure = 0 ) 

### specify parameters that should be kept fixed, here = none
apollo_fixed = c()

### validate 
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  #### List of utilities (later integrated in mnl_settings below)  ####
  # Define utility functions here:
  V = list()
  V[["alt1"]] = mu_income * log(income_1+1) + mu_leisure * log(leisure_1+1) + mu_income_sq * log(income_1^2+1) + 
    mu_leisure_sq * log(leisure_1^2+1) + mu_income_leisure * log(income_1+1) * log(leisure_1+1) + mu_age_income * Alter * log(income_1+1) +
    mu_age_sq_income * Alter^2 * log(income_1+1) + mu_east_income * ostdeutsch * log(income_1+1) + 
    mu_age_leisure * Alter * log(leisure_1+1) + mu_age_sq_leisure * Alter^2 * log(leisure_1+1) + 
    mu_east_leisure * ostdeutsch * log(leisure_1+1) + mu_child_leisure * Kind_u7 * log(leisure_1+1)
  
  V[["alt2"]] = mu_income * log(income_2+1) + mu_leisure * log(leisure_2+1) + mu_income_sq * log(income_2^2+1) + 
    mu_leisure_sq * log(leisure_2^2+1) + mu_income_leisure * log(income_2+1) * log(leisure_2+1) + mu_age_income * Alter * log(income_2+1) +
    mu_age_sq_income * Alter^2 * log(income_2+1) + mu_east_income * ostdeutsch * log(income_2+1) + 
    mu_age_leisure * Alter * log(leisure_2+1) + mu_age_sq_leisure * Alter^2 * log(leisure_2+1) + 
    mu_east_leisure * ostdeutsch * log(leisure_2+1) + mu_child_leisure * Kind_u7 * log(leisure_2+1)
  
  V[["alt3"]] = mu_income * log(income_3+1) + mu_leisure * log(leisure_3+1) + mu_income_sq * log(income_3^2+1) + 
    mu_leisure_sq * log(leisure_3^2+1) + mu_income_leisure * log(income_3+1) * log(leisure_3+1) + mu_age_income * Alter * log(income_3+1) +
    mu_age_sq_income * Alter^2 * log(income_3+1) + mu_east_income * ostdeutsch * log(income_3+1) + 
    mu_age_leisure * Alter * log(leisure_3+1) + mu_age_sq_leisure * Alter^2 * log(leisure_3+1) + 
    mu_east_leisure * ostdeutsch * log(leisure_3+1) + mu_child_leisure * Kind_u7 * log(leisure_3+1)
  
  V[["alt4"]] = mu_income * log(income_4+1) + mu_leisure * log(leisure_4+1) + mu_income_sq * log(income_4^2+1) + 
    mu_leisure_sq * log(leisure_4^2+1) + mu_income_leisure * log(income_4+1) * log(leisure_4+1) + mu_age_income * Alter * log(income_4+1) +
    mu_age_sq_income * Alter^2 * log(income_4+1) + mu_east_income * ostdeutsch * log(income_4+1) + 
    mu_age_leisure * Alter * log(leisure_4+1) + mu_age_sq_leisure * Alter^2 * log(leisure_4+1) + 
    mu_east_leisure * ostdeutsch * log(leisure_4+1) + mu_child_leisure * Kind_u7 * log(leisure_4+1)
  
  V[["alt5"]] = mu_income * log(income_5+1) + mu_leisure * log(leisure_5+1) + mu_income_sq * log(income_5^2+1) + 
    mu_leisure_sq * log(leisure_5^2+1) + mu_income_leisure * log(income_5+1) * log(leisure_5+1) + mu_age_income * Alter * log(income_5+1) +
    mu_age_sq_income * Alter^2 * log(income_5+1) + mu_east_income * ostdeutsch * log(income_5+1) + 
    mu_age_leisure * Alter * log(leisure_5+1) + mu_age_sq_leisure * Alter^2 * log(leisure_5+1) + 
    mu_east_leisure * ostdeutsch * log(leisure_5+1) + mu_child_leisure * Kind_u7 * log(leisure_5+1)
  
  V[["alt6"]] = mu_income * log(income_6+1) + mu_leisure * log(leisure_6+1) + mu_income_sq * log(income_6^2+1) + 
    mu_leisure_sq * log(leisure_6^2+1) + mu_income_leisure * log(income_6+1) * log(leisure_6+1) + mu_age_income * Alter * log(income_6+1) +
    mu_age_sq_income * Alter^2 * log(income_6+1) + mu_east_income * ostdeutsch * log(income_6+1) + 
    mu_age_leisure * Alter * log(leisure_6+1) + mu_age_sq_leisure * Alter^2 * log(leisure_6+1) + 
    mu_east_leisure * ostdeutsch * log(leisure_6+1) + mu_child_leisure * Kind_u7 * log(leisure_6+1)
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3, alt4=4, alt5=5, alt6=6),
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = Arbeitszeitkategorien,
    V             = V#,  # tell function to use list vector defined above
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws - nur bei Mixed Logit!
  #P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}  

# ################################################################# #
#### MODEL ESTIMATION                                            ##
# ################################################################# #
# estimate model with bfgs algorithm

clogit_nv_m = apollo_estimate(apollo_beta, apollo_fixed,
                                          apollo_probabilities, apollo_inputs, 
                                          estimate_settings=list(maxIterations=400,
                                                                 estimationRoutine="bfgs",
                                                                 hessianRoutine="analytic"))


# ################################################################# #
#### MODEL OUTPUTS                                               ##
# ################################################################# #
apollo_saveOutput(clogit_nv_m)

summary(clogit_nv_m)

### Use the estimated model to make predictions
predictions_base = apollo_prediction(clogit_nv_m, apollo_probabilities, apollo_inputs,
                                     prediction_settings=list(runs=30))


## Income increases by 10% 
database$income_1 = 1.1*database$income_1
database$income_2 = 1.1*database$income_2
database$income_3 = 1.1*database$income_3
database$income_4 = 1.1*database$income_4
database$income_5 = 1.1*database$income_5  
database$income_6 = 1.1*database$income_6  

### Rerun predictions with the new data
apollo_inputs = apollo_validateInputs()
predictions_new = apollo_prediction(clogit_nv_m, apollo_probabilities, apollo_inputs)  

### Return to original data
database$income_1 = 1/1.1*database$income_1
database$income_2 = 1/1.1*database$income_2
database$income_3 = 1/1.1*database$income_3
database$income_4 = 1/1.1*database$income_4
database$income_5 = 1/1.1*database$income_5
database$income_6 = 1/1.1*database$income_6
apollo_inputs = apollo_validateInputs()

### work with predictions at estimates
predictions_base=predictions_base[["at_estimates"]]

### Compute change in probabilities
change=(predictions_new-predictions_base)/predictions_base

### Summary of changes (possible presence of NAs for unavailable alternatives)
summary(change)

## worktime base 
p_1b <- sum(predictions_base$alt1)/sum(predictions_base$Observation)
p_2b <- sum(predictions_base$alt2)/sum(predictions_base$Observation)
p_3b <- sum(predictions_base$alt3)/sum(predictions_base$Observation)
p_4b <- sum(predictions_base$alt4)/sum(predictions_base$Observation)
p_5b <- sum(predictions_base$alt5)/sum(predictions_base$Observation)
p_6b <- sum(predictions_base$alt6)/sum(predictions_base$Observation)

worktime_base <- 0 * p_1b + 10 * p_2b + 20 * p_3b + 30 * p_4b + 40 * p_5b + 60 * p_6b

## worktime with increased net wage (10%) 
p_1n <- sum(predictions_new$alt1)/sum(predictions_base$Observation)
p_2n <- sum(predictions_new$alt2)/sum(predictions_base$Observation)
p_3n <- sum(predictions_new$alt3)/sum(predictions_base$Observation)
p_4n <- sum(predictions_new$alt4)/sum(predictions_base$Observation)
p_5n <- sum(predictions_new$alt5)/sum(predictions_base$Observation)
p_6n <- sum(predictions_new$alt6)/sum(predictions_base$Observation)

worktime_new <- 0 * p_1n + 10 * p_2n + 20 * p_3n + 30 * p_4n + 40 * p_5n + 60 * p_6n

## ELasticity
elasticity_nv_m <- ((worktime_new/worktime_base)-1) /1.1

