# Read in data file
data_v <- read.csv2("data_hm_v.csv")

library(apollo) # Load apollo package 

database <- data_v

#initialize model 

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = "clogit_v",
  modelDescr = "Conditonal logit model - married",
  indivID    ="hid",
  mixing     = FALSE,
  HB= FALSE,
  nCores     = 1, 
  outputDirectory = "Estimation_results"
)


##### Define model parameters depending on your attributes and model specification! ####
# set values to 0 for conditonal logit model

apollo_beta =c (mu_income = 0,
                mu_income_sq = 0,
                mu_leisure_m = 0,
                mu_leisure_m_sq = 0,
                mu_leisure_f = 0,
                mu_leisure_f_sq = 0,
                mu_income_leisure_m = 0,
                mu_income_leisure_f = 0,
                mu_leisure_m_f = 0,
                mu_age_m_income = 0,
                mu_age_m_sq_income = 0,
                mu_age_f_income = 0,
                mu_age_f_sq_income = 0,
                mu_east_income = 0,
                mu_migration_f_income = 0,
                mu_migration_f_leisure_f = 0,
                mu_age_f_leisure_f = 0,
                mu_age_f_sq_leisure_f = 0,
                mu_east_leisure_f = 0,
                mu_child_leisure_f = 0,
                mu_migration_m_income = 0,
                mu_migration_m_leisure_m = 0,
                mu_age_m_leisure_m = 0,
                mu_age_m_sq_leisure_m = 0,
                mu_east_leisure_m = 0,
                mu_child_leisure_m = 0,
                mu_migration_m_leisure_m_leisure_f = 0,
                mu_east_leisure_m_leisure_f = 0)

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
  V[["alt1"]] = mu_income * log(income_1+1) + mu_income_sq * log(income_1^2+1) + mu_leisure_m * log(leisure_1.m+1) +  mu_leisure_m_sq * log(leisure_1.m^2+1) +
    mu_leisure_f * log(leisure_1.f+1) +  mu_leisure_f_sq * log(leisure_1.f^2+1) + mu_income_leisure_m * log(income_1+1) * log(leisure_1.m+1) + 
    mu_income_leisure_f * log(income_1+1) * log(leisure_1.f+1) + mu_leisure_m_f * log(leisure_1.m+1) * log(leisure_1.f+1) + mu_age_m_income * Alter.m * log(income_1+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_1+1) + mu_age_f_income * Alter.f * log(income_1+1) + mu_age_f_sq_income * Alter.f^2 * log(income_1+1) + 
    mu_east_income * ostdeutsch * log(income_1+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_1+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_1.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_1.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_1.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_1.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_1.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_1+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_1.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_1.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_1.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_1.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_1.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_1.f+1) * log(leisure_1.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_1.f+1) * log(leisure_1.m+1)
  
  V[["alt2"]] = mu_income * log(income_2+1) + mu_income_sq * log(income_2^2+1) + mu_leisure_m * log(leisure_2.m+1) +  mu_leisure_m_sq * log(leisure_2.m^2+1) +
    mu_leisure_f * log(leisure_1.f+1) +  mu_leisure_f_sq * log(leisure_1.f^2+1) + mu_income_leisure_m * log(income_2+1) * log(leisure_2.m+1) + 
    mu_income_leisure_f * log(income_2+1) * log(leisure_1.f+1) + mu_leisure_m_f * log(leisure_2.m+1) * log(leisure_1.f+1) + mu_age_m_income * Alter.m * log(income_2+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_2+1) + mu_age_f_income * Alter.f * log(income_2+1) + mu_age_f_sq_income * Alter.f^2 * log(income_2+1) + 
    mu_east_income * ostdeutsch * log(income_2+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_2+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_1.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_1.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_1.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_1.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_1.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_2+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_2.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_2.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_2.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_2.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_2.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_1.f+1) * log(leisure_2.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_1.f+1) * log(leisure_2.m+1)
  
  V[["alt3"]] = mu_income * log(income_3+1) + mu_income_sq * log(income_3^2+1) + mu_leisure_m * log(leisure_3.m+1) +  mu_leisure_m_sq * log(leisure_3.m^2+1) +
    mu_leisure_f * log(leisure_1.f+1) +  mu_leisure_f_sq * log(leisure_1.f^2+1) + mu_income_leisure_m * log(income_3+1) * log(leisure_3.m+1) + 
    mu_income_leisure_f * log(income_3+1) * log(leisure_1.f+1) + mu_leisure_m_f * log(leisure_3.m+1) * log(leisure_1.f+1) + mu_age_m_income * Alter.m * log(income_3+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_3+1) + mu_age_f_income * Alter.f * log(income_3+1) + mu_age_f_sq_income * Alter.f^2 * log(income_3+1) + 
    mu_east_income * ostdeutsch * log(income_3+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_3+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_1.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_1.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_1.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_1.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_1.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_3+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_3.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_3.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_3.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_3.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_3.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_1.f+1) * log(leisure_3.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_1.f+1) * log(leisure_3.m+1)
  
  V[["alt4"]] = mu_income * log(income_4+1) + mu_income_sq * log(income_4^2+1) + mu_leisure_m * log(leisure_4.m+1) +  mu_leisure_m_sq * log(leisure_4.m^2+1) +
    mu_leisure_f * log(leisure_1.f+1) +  mu_leisure_f_sq * log(leisure_1.f^2+1) + mu_income_leisure_m * log(income_4+1) * log(leisure_4.m+1) + 
    mu_income_leisure_f * log(income_4+1) * log(leisure_1.f+1) + mu_leisure_m_f * log(leisure_4.m+1) * log(leisure_1.f+1) + mu_age_m_income * Alter.m * log(income_4+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_4+1) + mu_age_f_income * Alter.f * log(income_4+1) + mu_age_f_sq_income * Alter.f^2 * log(income_4+1) + 
    mu_east_income * ostdeutsch * log(income_4+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_4+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_1.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_1.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_1.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_1.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_1.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_4+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_4.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_4.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_4.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_4.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_4.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_1.f+1) * log(leisure_4.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_1.f+1) * log(leisure_4.m+1)
  
  V[["alt5"]] = mu_income * log(income_5+1) + mu_income_sq * log(income_5^2+1) + mu_leisure_m * log(leisure_5.m+1) +  mu_leisure_m_sq * log(leisure_5.m^2+1) +
    mu_leisure_f * log(leisure_1.f+1) +  mu_leisure_f_sq * log(leisure_1.f^2+1) + mu_income_leisure_m * log(income_5+1) * log(leisure_5.m+1) + 
    mu_income_leisure_f * log(income_5+1) * log(leisure_1.f+1) + mu_leisure_m_f * log(leisure_5.m+1) * log(leisure_1.f+1) + mu_age_m_income * Alter.m * log(income_5+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_5+1) + mu_age_f_income * Alter.f * log(income_5+1) + mu_age_f_sq_income * Alter.f^2 * log(income_5+1) + 
    mu_east_income * ostdeutsch * log(income_5+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_5+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_1.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_1.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_1.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_1.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_1.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_5+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_5.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_5.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_5.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_5.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_5.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_1.f+1) * log(leisure_5.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_1.f+1) * log(leisure_5.m+1)
  
  V[["alt6"]] =  mu_income * log(income_6+1) + mu_income_sq * log(income_6^2+1) + mu_leisure_m * log(leisure_6.m+1) +  mu_leisure_m_sq * log(leisure_6.m^2+1) +
    mu_leisure_f * log(leisure_1.f+1) +  mu_leisure_f_sq * log(leisure_1.f^2+1) + mu_income_leisure_m * log(income_6+1) * log(leisure_6.m+1) + 
    mu_income_leisure_f * log(income_6+1) * log(leisure_1.f+1) + mu_leisure_m_f * log(leisure_6.m+1) * log(leisure_1.f+1) + mu_age_m_income * Alter.m * log(income_6+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_6+1) + mu_age_f_income * Alter.f * log(income_6+1) + mu_age_f_sq_income * Alter.f^2 * log(income_6+1) + 
    mu_east_income * ostdeutsch * log(income_6+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_6+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_1.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_1.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_1.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_1.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_1.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_6+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_6.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_6.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_6.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_6.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_6.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_1.f+1) * log(leisure_6.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_1.f+1) * log(leisure_6.m+1)
  
  V[["alt7"]] = mu_income * log(income_7+1) + mu_income_sq * log(income_7^2+1) + mu_leisure_m * log(leisure_6.m+1) +  mu_leisure_m_sq * log(leisure_6.m^2+1) +
    mu_leisure_f * log(leisure_2.f+1) +  mu_leisure_f_sq * log(leisure_2.f^2+1) + mu_income_leisure_m * log(income_7+1) * log(leisure_6.m+1) + 
    mu_income_leisure_f * log(income_7+1) * log(leisure_2.f+1) + mu_leisure_m_f * log(leisure_6.m+1) * log(leisure_2.f+1) + mu_age_m_income * Alter.m * log(income_7+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_7+1) + mu_age_f_income * Alter.f * log(income_7+1) + mu_age_f_sq_income * Alter.f^2 * log(income_7+1) + 
    mu_east_income * ostdeutsch * log(income_7+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_7+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_2.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_2.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_2.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_2.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_2.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_7+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_6.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_6.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_6.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_6.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_6.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_2.f+1) * log(leisure_6.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_2.f+1) * log(leisure_6.m+1)
  
  V[["alt8"]] = mu_income * log(income_8+1) + mu_income_sq * log(income_8^2+1) + mu_leisure_m * log(leisure_2.m+1) +  mu_leisure_m_sq * log(leisure_2.m^2+1) +
    mu_leisure_f * log(leisure_2.f+1) +  mu_leisure_f_sq * log(leisure_2.f^2+1) + mu_income_leisure_m * log(income_8+1) * log(leisure_2.m+1) + 
    mu_income_leisure_f * log(income_8+1) * log(leisure_2.f+1) + mu_leisure_m_f * log(leisure_2.m+1) * log(leisure_2.f+1) + mu_age_m_income * Alter.m * log(income_8+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_8+1) + mu_age_f_income * Alter.f * log(income_8+1) + mu_age_f_sq_income * Alter.f^2 * log(income_8+1) + 
    mu_east_income * ostdeutsch * log(income_8+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_8+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_2.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_2.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_2.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_2.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_2.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_8+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_2.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_2.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_2.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_2.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_2.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_2.f+1) * log(leisure_2.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_2.f+1) * log(leisure_2.m+1)
  
  V[["alt9"]] = mu_income * log(income_9+1) + mu_income_sq * log(income_9^2+1) + mu_leisure_m * log(leisure_3.m+1) +  mu_leisure_m_sq * log(leisure_3.m^2+1) +
    mu_leisure_f * log(leisure_2.f+1) +  mu_leisure_f_sq * log(leisure_2.f^2+1) + mu_income_leisure_m * log(income_9+1) * log(leisure_3.m+1) + 
    mu_income_leisure_f * log(income_9+1) * log(leisure_2.f+1) + mu_leisure_m_f * log(leisure_3.m+1) * log(leisure_2.f+1) + mu_age_m_income * Alter.m * log(income_9+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_9+1) + mu_age_f_income * Alter.f * log(income_9+1) + mu_age_f_sq_income * Alter.f^2 * log(income_9+1) + 
    mu_east_income * ostdeutsch * log(income_9+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_9+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_2.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_2.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_2.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_2.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_2.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_9+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_3.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_3.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_3.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_3.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_3.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_2.f+1) * log(leisure_3.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_2.f+1) * log(leisure_3.m+1)
  
  V[["alt10"]] = mu_income * log(income_10+1) + mu_income_sq * log(income_10^2+1) + mu_leisure_m * log(leisure_4.m+1) +  mu_leisure_m_sq * log(leisure_4.m^2+1) +
    mu_leisure_f * log(leisure_2.f+1) +  mu_leisure_f_sq * log(leisure_2.f^2+1) + mu_income_leisure_m * log(income_10+1) * log(leisure_4.m+1) + 
    mu_income_leisure_f * log(income_10+1) * log(leisure_2.f+1) + mu_leisure_m_f * log(leisure_4.m+1) * log(leisure_2.f+1) + mu_age_m_income * Alter.m * log(income_10+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_10+1) + mu_age_f_income * Alter.f * log(income_10+1) + mu_age_f_sq_income * Alter.f^2 * log(income_10+1) + 
    mu_east_income * ostdeutsch * log(income_10+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_10+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_2.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_2.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_2.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_2.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_2.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_10+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_4.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_4.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_4.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_4.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_4.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_2.f+1) * log(leisure_4.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_2.f+1) * log(leisure_4.m+1)
  
  V[["alt11"]] = mu_income * log(income_11+1) + mu_income_sq * log(income_11^2+1) + mu_leisure_m * log(leisure_5.m+1) +  mu_leisure_m_sq * log(leisure_5.m^2+1) +
    mu_leisure_f * log(leisure_2.f+1) +  mu_leisure_f_sq * log(leisure_2.f^2+1) + mu_income_leisure_m * log(income_11+1) * log(leisure_5.m+1) + 
    mu_income_leisure_f * log(income_11+1) * log(leisure_2.f+1) + mu_leisure_m_f * log(leisure_5.m+1) * log(leisure_2.f+1) + mu_age_m_income * Alter.m * log(income_11+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_11+1) + mu_age_f_income * Alter.f * log(income_11+1) + mu_age_f_sq_income * Alter.f^2 * log(income_11+1) + 
    mu_east_income * ostdeutsch * log(income_11+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_11+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_2.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_2.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_2.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_2.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_2.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_11+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_5.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_5.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_5.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_5.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_5.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_2.f+1) * log(leisure_5.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_2.f+1) * log(leisure_5.m+1)
  
  V[["alt12"]] = mu_income * log(income_12+1) + mu_income_sq * log(income_12^2+1) + mu_leisure_m * log(leisure_6.m+1) +  mu_leisure_m_sq * log(leisure_6.m^2+1) +
    mu_leisure_f * log(leisure_2.f+1) +  mu_leisure_f_sq * log(leisure_2.f^2+1) + mu_income_leisure_m * log(income_12+1) * log(leisure_6.m+1) + 
    mu_income_leisure_f * log(income_12+1) * log(leisure_2.f+1) + mu_leisure_m_f * log(leisure_6.m+1) * log(leisure_2.f+1) + mu_age_m_income * Alter.m * log(income_12+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_12+1) + mu_age_f_income * Alter.f * log(income_12+1) + mu_age_f_sq_income * Alter.f^2 * log(income_12+1) + 
    mu_east_income * ostdeutsch * log(income_12+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_12+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_2.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_2.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_2.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_2.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_2.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_12+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_6.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_6.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_6.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_6.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_6.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_2.f+1) * log(leisure_6.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_2.f+1) * log(leisure_6.m+1)
  
  V[["alt13"]] = mu_income * log(income_13+1) + mu_income_sq * log(income_13^2+1) + mu_leisure_m * log(leisure_1.m+1) +  mu_leisure_m_sq * log(leisure_1.m^2+1) +
    mu_leisure_f * log(leisure_3.f+1) +  mu_leisure_f_sq * log(leisure_3.f^2+1) + mu_income_leisure_m * log(income_13+1) * log(leisure_1.m+1) + 
    mu_income_leisure_f * log(income_13+1) * log(leisure_3.f+1) + mu_leisure_m_f * log(leisure_1.m+1) * log(leisure_3.f+1) + mu_age_m_income * Alter.m * log(income_13+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_13+1) + mu_age_f_income * Alter.f * log(income_13+1) + mu_age_f_sq_income * Alter.f^2 * log(income_13+1) + 
    mu_east_income * ostdeutsch * log(income_13+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_13+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_3.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_3.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_3.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_3.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_3.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_13+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_1.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_1.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_1.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_1.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_1.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_3.f+1) * log(leisure_1.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_3.f+1) * log(leisure_1.m+1)
  
  V[["alt14"]] = mu_income * log(income_14+1) + mu_income_sq * log(income_14^2+1) + mu_leisure_m * log(leisure_2.m+1) +  mu_leisure_m_sq * log(leisure_2.m^2+1) +
    mu_leisure_f * log(leisure_3.f+1) +  mu_leisure_f_sq * log(leisure_3.f^2+1) + mu_income_leisure_m * log(income_14+1) * log(leisure_2.m+1) + 
    mu_income_leisure_f * log(income_14+1) * log(leisure_3.f+1) + mu_leisure_m_f * log(leisure_2.m+1) * log(leisure_3.f+1) + mu_age_m_income * Alter.m * log(income_14+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_14+1) + mu_age_f_income * Alter.f * log(income_14+1) + mu_age_f_sq_income * Alter.f^2 * log(income_14+1) + 
    mu_east_income * ostdeutsch * log(income_14+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_14+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_3.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_3.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_3.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_3.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_3.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_14+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_2.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_2.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_2.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_2.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_2.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_3.f+1) * log(leisure_2.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_3.f+1) * log(leisure_2.m+1)
  
  V[["alt15"]] = mu_income * log(income_15+1) + mu_income_sq * log(income_15^2+1) + mu_leisure_m * log(leisure_3.m+1) +  mu_leisure_m_sq * log(leisure_3.m^2+1) +
    mu_leisure_f * log(leisure_3.f+1) +  mu_leisure_f_sq * log(leisure_3.f^2+1) + mu_income_leisure_m * log(income_15+1) * log(leisure_3.m+1) + 
    mu_income_leisure_f * log(income_15+1) * log(leisure_3.f+1) + mu_leisure_m_f * log(leisure_3.m+1) * log(leisure_3.f+1) + mu_age_m_income * Alter.m * log(income_15+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_15+1) + mu_age_f_income * Alter.f * log(income_15+1) + mu_age_f_sq_income * Alter.f^2 * log(income_15+1) + 
    mu_east_income * ostdeutsch * log(income_15+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_15+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_3.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_3.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_3.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_3.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_3.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_15+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_3.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_3.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_3.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_3.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_3.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_3.f+1) * log(leisure_3.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_3.f+1) * log(leisure_3.m+1)
  
  V[["alt16"]] = mu_income * log(income_16+1) + mu_income_sq * log(income_16^2+1) + mu_leisure_m * log(leisure_4.m+1) +  mu_leisure_m_sq * log(leisure_4.m^2+1) +
    mu_leisure_f * log(leisure_3.f+1) +  mu_leisure_f_sq * log(leisure_3.f^2+1) + mu_income_leisure_m * log(income_16+1) * log(leisure_4.m+1) + 
    mu_income_leisure_f * log(income_16+1) * log(leisure_3.f+1) + mu_leisure_m_f * log(leisure_4.m+1) * log(leisure_3.f+1) + mu_age_m_income * Alter.m * log(income_16+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_16+1) + mu_age_f_income * Alter.f * log(income_16+1) + mu_age_f_sq_income * Alter.f^2 * log(income_16+1) + 
    mu_east_income * ostdeutsch * log(income_16+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_16+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_3.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_3.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_3.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_3.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_3.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_16+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_4.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_4.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_4.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_4.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_4.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_3.f+1) * log(leisure_4.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_3.f+1) * log(leisure_4.m+1)
  
  V[["alt17"]] = mu_income * log(income_17+1) + mu_income_sq * log(income_17^2+1) + mu_leisure_m * log(leisure_5.m+1) +  mu_leisure_m_sq * log(leisure_5.m^2+1) +
    mu_leisure_f * log(leisure_3.f+1) +  mu_leisure_f_sq * log(leisure_3.f^2+1) + mu_income_leisure_m * log(income_17+1) * log(leisure_5.m+1) + 
    mu_income_leisure_f * log(income_17+1) * log(leisure_3.f+1) + mu_leisure_m_f * log(leisure_5.m+1) * log(leisure_3.f+1) + mu_age_m_income * Alter.m * log(income_17+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_17+1) + mu_age_f_income * Alter.f * log(income_17+1) + mu_age_f_sq_income * Alter.f^2 * log(income_17+1) + 
    mu_east_income * ostdeutsch * log(income_17+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_17+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_3.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_3.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_3.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_3.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_3.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_17+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_5.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_5.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_5.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_5.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_5.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_3.f+1) * log(leisure_5.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_3.f+1) * log(leisure_5.m+1)
  
  V[["alt18"]] = mu_income * log(income_18+1) + mu_income_sq * log(income_18^2+1) + mu_leisure_m * log(leisure_6.m+1) +  mu_leisure_m_sq * log(leisure_6.m^2+1) +
    mu_leisure_f * log(leisure_3.f+1) +  mu_leisure_f_sq * log(leisure_3.f^2+1) + mu_income_leisure_m * log(income_18+1) * log(leisure_6.m+1) + 
    mu_income_leisure_f * log(income_18+1) * log(leisure_3.f+1) + mu_leisure_m_f * log(leisure_6.m+1) * log(leisure_3.f+1) + mu_age_m_income * Alter.m * log(income_18+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_18+1) + mu_age_f_income * Alter.f * log(income_18+1) + mu_age_f_sq_income * Alter.f^2 * log(income_18+1) + 
    mu_east_income * ostdeutsch * log(income_18+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_18+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_3.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_3.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_3.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_3.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_3.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_18+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_6.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_6.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_6.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_6.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_6.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_3.f+1) * log(leisure_6.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_3.f+1) * log(leisure_6.m+1)
  
  V[["alt19"]] = mu_income * log(income_19+1) + mu_income_sq * log(income_19^2+1) + mu_leisure_m * log(leisure_1.m+1) +  mu_leisure_m_sq * log(leisure_1.m^2+1) +
    mu_leisure_f * log(leisure_4.f+1) +  mu_leisure_f_sq * log(leisure_4.f^2+1) + mu_income_leisure_m * log(income_19+1) * log(leisure_1.m+1) + 
    mu_income_leisure_f * log(income_19+1) * log(leisure_4.f+1) + mu_leisure_m_f * log(leisure_1.m+1) * log(leisure_4.f+1) + mu_age_m_income * Alter.m * log(income_19+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_19+1) + mu_age_f_income * Alter.f * log(income_19+1) + mu_age_f_sq_income * Alter.f^2 * log(income_19+1) + 
    mu_east_income * ostdeutsch * log(income_19+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_19+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_4.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_4.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_4.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_4.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_4.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_19+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_1.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_1.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_1.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_1.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_1.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_4.f+1) * log(leisure_1.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_4.f+1) * log(leisure_1.m+1)
  
  V[["alt20"]] = mu_income * log(income_20+1) + mu_income_sq * log(income_20^2+1) + mu_leisure_m * log(leisure_2.m+1) +  mu_leisure_m_sq * log(leisure_2.m^2+1) +
    mu_leisure_f * log(leisure_4.f+1) +  mu_leisure_f_sq * log(leisure_4.f^2+1) + mu_income_leisure_m * log(income_20+1) * log(leisure_2.m+1) + 
    mu_income_leisure_f * log(income_20+1) * log(leisure_4.f+1) + mu_leisure_m_f * log(leisure_2.m+1) * log(leisure_4.f+1) + mu_age_m_income * Alter.m * log(income_20+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_20+1) + mu_age_f_income * Alter.f * log(income_20+1) + mu_age_f_sq_income * Alter.f^2 * log(income_20+1) + 
    mu_east_income * ostdeutsch * log(income_20+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_20+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_4.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_4.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_4.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_4.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_4.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_20+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_2.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_2.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_2.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_2.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_2.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_4.f+1) * log(leisure_2.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_4.f+1) * log(leisure_2.m+1)
  
  V[["alt21"]] = mu_income * log(income_21+1) + mu_income_sq * log(income_21^2+1) + mu_leisure_m * log(leisure_3.m+1) +  mu_leisure_m_sq * log(leisure_3.m^2+1) +
    mu_leisure_f * log(leisure_4.f+1) +  mu_leisure_f_sq * log(leisure_4.f^2+1) + mu_income_leisure_m * log(income_21+1) * log(leisure_3.m+1) + 
    mu_income_leisure_f * log(income_21+1) * log(leisure_4.f+1) + mu_leisure_m_f * log(leisure_3.m+1) * log(leisure_4.f+1) + mu_age_m_income * Alter.m * log(income_21+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_21+1) + mu_age_f_income * Alter.f * log(income_21+1) + mu_age_f_sq_income * Alter.f^2 * log(income_21+1) + 
    mu_east_income * ostdeutsch * log(income_21+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_21+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_4.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_4.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_4.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_4.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_4.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_21+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_3.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_3.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_3.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_3.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_3.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_4.f+1) * log(leisure_3.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_4.f+1) * log(leisure_3.m+1)
  
  V[["alt22"]] = mu_income * log(income_22+1) + mu_income_sq * log(income_22^2+1) + mu_leisure_m * log(leisure_4.m+1) +  mu_leisure_m_sq * log(leisure_4.m^2+1) +
    mu_leisure_f * log(leisure_4.f+1) +  mu_leisure_f_sq * log(leisure_4.f^2+1) + mu_income_leisure_m * log(income_22+1) * log(leisure_4.m+1) + 
    mu_income_leisure_f * log(income_22+1) * log(leisure_4.f+1) + mu_leisure_m_f * log(leisure_4.m+1) * log(leisure_4.f+1) + mu_age_m_income * Alter.m * log(income_22+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_22+1) + mu_age_f_income * Alter.f * log(income_22+1) + mu_age_f_sq_income * Alter.f^2 * log(income_22+1) + 
    mu_east_income * ostdeutsch * log(income_22+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_22+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_4.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_4.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_4.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_4.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_4.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_22+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_4.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_4.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_4.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_4.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_4.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_4.f+1) * log(leisure_4.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_4.f+1) * log(leisure_4.m+1)
  
  V[["alt23"]] = mu_income * log(income_23+1) + mu_income_sq * log(income_23^2+1) + mu_leisure_m * log(leisure_5.m+1) +  mu_leisure_m_sq * log(leisure_5.m^2+1) +
    mu_leisure_f * log(leisure_4.f+1) +  mu_leisure_f_sq * log(leisure_4.f^2+1) + mu_income_leisure_m * log(income_23+1) * log(leisure_5.m+1) + 
    mu_income_leisure_f * log(income_23+1) * log(leisure_4.f+1) + mu_leisure_m_f * log(leisure_5.m+1) * log(leisure_4.f+1) + mu_age_m_income * Alter.m * log(income_23+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_23+1) + mu_age_f_income * Alter.f * log(income_23+1) + mu_age_f_sq_income * Alter.f^2 * log(income_23+1) + 
    mu_east_income * ostdeutsch * log(income_23+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_23+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_4.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_4.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_4.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_4.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_4.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_23+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_5.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_5.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_5.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_5.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_5.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_4.f+1) * log(leisure_5.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_4.f+1) * log(leisure_5.m+1)
  
  V[["alt24"]] = mu_income * log(income_24+1) + mu_income_sq * log(income_24^2+1) + mu_leisure_m * log(leisure_6.m+1) +  mu_leisure_m_sq * log(leisure_6.m^2+1) +
    mu_leisure_f * log(leisure_4.f+1) +  mu_leisure_f_sq * log(leisure_4.f^2+1) + mu_income_leisure_m * log(income_24+1) * log(leisure_6.m+1) + 
    mu_income_leisure_f * log(income_24+1) * log(leisure_4.f+1) + mu_leisure_m_f * log(leisure_6.m+1) * log(leisure_4.f+1) + mu_age_m_income * Alter.m * log(income_24+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_24+1) + mu_age_f_income * Alter.f * log(income_24+1) + mu_age_f_sq_income * Alter.f^2 * log(income_24+1) + 
    mu_east_income * ostdeutsch * log(income_24+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_24+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_4.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_4.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_4.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_4.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_4.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_24+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_6.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_6.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_6.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_6.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_6.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_4.f+1) * log(leisure_6.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_4.f+1) * log(leisure_6.m+1)
  
  V[["alt25"]] = mu_income * log(income_25+1) + mu_income_sq * log(income_25^2+1) + mu_leisure_m * log(leisure_1.m+1) +  mu_leisure_m_sq * log(leisure_1.m^2+1) +
    mu_leisure_f * log(leisure_5.f+1) +  mu_leisure_f_sq * log(leisure_5.f^2+1) + mu_income_leisure_m * log(income_25+1) * log(leisure_1.m+1) + 
    mu_income_leisure_f * log(income_25+1) * log(leisure_5.f+1) + mu_leisure_m_f * log(leisure_1.m+1) * log(leisure_5.f+1) + mu_age_m_income * Alter.m * log(income_25+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_25+1) + mu_age_f_income * Alter.f * log(income_25+1) + mu_age_f_sq_income * Alter.f^2 * log(income_25+1) + 
    mu_east_income * ostdeutsch * log(income_25+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_25+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_5.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_5.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_5.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_5.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_5.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_25+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_1.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_1.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_1.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_1.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_1.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_5.f+1) * log(leisure_1.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_5.f+1) * log(leisure_1.m+1)
  
  V[["alt26"]] = mu_income * log(income_26+1) + mu_income_sq * log(income_26^2+1) + mu_leisure_m * log(leisure_2.m+1) +  mu_leisure_m_sq * log(leisure_2.m^2+1) +
    mu_leisure_f * log(leisure_5.f+1) +  mu_leisure_f_sq * log(leisure_5.f^2+1) + mu_income_leisure_m * log(income_26+1) * log(leisure_2.m+1) + 
    mu_income_leisure_f * log(income_26+1) * log(leisure_5.f+1) + mu_leisure_m_f * log(leisure_2.m+1) * log(leisure_5.f+1) + mu_age_m_income * Alter.m * log(income_26+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_26+1) + mu_age_f_income * Alter.f * log(income_26+1) + mu_age_f_sq_income * Alter.f^2 * log(income_26+1) + 
    mu_east_income * ostdeutsch * log(income_26+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_26+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_5.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_5.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_5.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_5.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_5.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_26+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_2.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_2.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_2.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_2.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_2.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_5.f+1) * log(leisure_2.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_5.f+1) * log(leisure_2.m+1)
  
  V[["alt27"]] = mu_income * log(income_27+1) + mu_income_sq * log(income_27^2+1) + mu_leisure_m * log(leisure_3.m+1) +  mu_leisure_m_sq * log(leisure_3.m^2+1) +
    mu_leisure_f * log(leisure_5.f+1) +  mu_leisure_f_sq * log(leisure_5.f^2+1) + mu_income_leisure_m * log(income_27+1) * log(leisure_3.m+1) + 
    mu_income_leisure_f * log(income_27+1) * log(leisure_5.f+1) + mu_leisure_m_f * log(leisure_3.m+1) * log(leisure_5.f+1) + mu_age_m_income * Alter.m * log(income_27+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_27+1) + mu_age_f_income * Alter.f * log(income_27+1) + mu_age_f_sq_income * Alter.f^2 * log(income_27+1) + 
    mu_east_income * ostdeutsch * log(income_27+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_27+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_5.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_5.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_5.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_5.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_5.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_27+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_3.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_3.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_3.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_3.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_3.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_5.f+1) * log(leisure_3.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_5.f+1) * log(leisure_3.m+1)
  
  V[["alt28"]] = mu_income * log(income_28+1) + mu_income_sq * log(income_28^2+1) + mu_leisure_m * log(leisure_4.m+1) +  mu_leisure_m_sq * log(leisure_4.m^2+1) +
    mu_leisure_f * log(leisure_5.f+1) +  mu_leisure_f_sq * log(leisure_5.f^2+1) + mu_income_leisure_m * log(income_28+1) * log(leisure_4.m+1) + 
    mu_income_leisure_f * log(income_28+1) * log(leisure_5.f+1) + mu_leisure_m_f * log(leisure_4.m+1) * log(leisure_5.f+1) + mu_age_m_income * Alter.m * log(income_28+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_28+1) + mu_age_f_income * Alter.f * log(income_28+1) + mu_age_f_sq_income * Alter.f^2 * log(income_28+1) + 
    mu_east_income * ostdeutsch * log(income_28+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_28+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_5.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_5.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_5.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_5.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_5.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_28+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_4.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_4.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_4.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_4.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_4.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_5.f+1) * log(leisure_4.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_5.f+1) * log(leisure_4.m+1)
  
  V[["alt29"]] = mu_income * log(income_29+1) + mu_income_sq * log(income_29^2+1) + mu_leisure_m * log(leisure_5.m+1) +  mu_leisure_m_sq * log(leisure_5.m^2+1) +
    mu_leisure_f * log(leisure_5.f+1) +  mu_leisure_f_sq * log(leisure_5.f^2+1) + mu_income_leisure_m * log(income_29+1) * log(leisure_5.m+1) + 
    mu_income_leisure_f * log(income_29+1) * log(leisure_5.f+1) + mu_leisure_m_f * log(leisure_5.m+1) * log(leisure_5.f+1) + mu_age_m_income * Alter.m * log(income_29+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_29+1) + mu_age_f_income * Alter.f * log(income_29+1) + mu_age_f_sq_income * Alter.f^2 * log(income_29+1) + 
    mu_east_income * ostdeutsch * log(income_29+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_29+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_5.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_5.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_5.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_5.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_5.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_29+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_5.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_5.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_5.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_5.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_5.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_5.f+1) * log(leisure_5.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_5.f+1) * log(leisure_5.m+1)
  
  V[["alt30"]] = mu_income * log(income_30+1) + mu_income_sq * log(income_30^2+1) + mu_leisure_m * log(leisure_6.m+1) +  mu_leisure_m_sq * log(leisure_6.m^2+1) +
    mu_leisure_f * log(leisure_5.f+1) +  mu_leisure_f_sq * log(leisure_5.f^2+1) + mu_income_leisure_m * log(income_30+1) * log(leisure_6.m+1) + 
    mu_income_leisure_f * log(income_30+1) * log(leisure_5.f+1) + mu_leisure_m_f * log(leisure_6.m+1) * log(leisure_5.f+1) + mu_age_m_income * Alter.m * log(income_30+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_30+1) + mu_age_f_income * Alter.f * log(income_30+1) + mu_age_f_sq_income * Alter.f^2 * log(income_30+1) + 
    mu_east_income * ostdeutsch * log(income_30+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_30+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_5.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_5.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_5.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_5.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_5.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_30+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_6.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_6.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_6.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_6.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_6.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_5.f+1) * log(leisure_6.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_5.f+1) * log(leisure_6.m+1)
  
  V[["alt31"]] = mu_income * log(income_31+1) + mu_income_sq * log(income_31^2+1) + mu_leisure_m * log(leisure_1.m+1) +  mu_leisure_m_sq * log(leisure_1.m^2+1) +
    mu_leisure_f * log(leisure_6.f+1) +  mu_leisure_f_sq * log(leisure_6.f^2+1) + mu_income_leisure_m * log(income_31+1) * log(leisure_1.m+1) + 
    mu_income_leisure_f * log(income_31+1) * log(leisure_6.f+1) + mu_leisure_m_f * log(leisure_1.m+1) * log(leisure_6.f+1) + mu_age_m_income * Alter.m * log(income_31+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_31+1) + mu_age_f_income * Alter.f * log(income_31+1) + mu_age_f_sq_income * Alter.f^2 * log(income_31+1) + 
    mu_east_income * ostdeutsch * log(income_31+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_31+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_6.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_6.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_6.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_6.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_6.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_31+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_1.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_1.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_1.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_1.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_1.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_6.f+1) * log(leisure_1.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_6.f+1) * log(leisure_1.m+1)
  
  V[["alt32"]] = mu_income * log(income_32+1) + mu_income_sq * log(income_32^2+1) + mu_leisure_m * log(leisure_2.m+1) +  mu_leisure_m_sq * log(leisure_2.m^2+1) +
    mu_leisure_f * log(leisure_6.f+1) +  mu_leisure_f_sq * log(leisure_6.f^2+1) + mu_income_leisure_m * log(income_32+1) * log(leisure_2.m+1) + 
    mu_income_leisure_f * log(income_32+1) * log(leisure_6.f+1) + mu_leisure_m_f * log(leisure_2.m+1) * log(leisure_6.f+1) + mu_age_m_income * Alter.m * log(income_32+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_32+1) + mu_age_f_income * Alter.f * log(income_32+1) + mu_age_f_sq_income * Alter.f^2 * log(income_32+1) + 
    mu_east_income * ostdeutsch * log(income_32+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_32+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_6.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_6.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_6.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_6.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_6.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_32+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_2.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_2.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_2.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_2.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_2.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_6.f+1) * log(leisure_2.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_6.f+1) * log(leisure_2.m+1)
  
  V[["alt33"]] = mu_income * log(income_33+1) + mu_income_sq * log(income_33^2+1) + mu_leisure_m * log(leisure_3.m+1) +  mu_leisure_m_sq * log(leisure_3.m^2+1) +
    mu_leisure_f * log(leisure_6.f+1) +  mu_leisure_f_sq * log(leisure_6.f^2+1) + mu_income_leisure_m * log(income_33+1) * log(leisure_3.m+1) + 
    mu_income_leisure_f * log(income_33+1) * log(leisure_6.f+1) + mu_leisure_m_f * log(leisure_3.m+1) * log(leisure_6.f+1) + mu_age_m_income * Alter.m * log(income_33+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_33+1) + mu_age_f_income * Alter.f * log(income_33+1) + mu_age_f_sq_income * Alter.f^2 * log(income_33+1) + 
    mu_east_income * ostdeutsch * log(income_33+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_33+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_6.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_6.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_6.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_6.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_6.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_33+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_3.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_3.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_3.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_3.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_3.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_6.f+1) * log(leisure_3.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_6.f+1) * log(leisure_3.m+1)
  
  V[["alt34"]] = mu_income * log(income_34+1) + mu_income_sq * log(income_34^2+1) + mu_leisure_m * log(leisure_4.m+1) +  mu_leisure_m_sq * log(leisure_4.m^2+1) +
    mu_leisure_f * log(leisure_6.f+1) +  mu_leisure_f_sq * log(leisure_6.f^2+1) + mu_income_leisure_m * log(income_34+1) * log(leisure_4.m+1) + 
    mu_income_leisure_f * log(income_34+1) * log(leisure_6.f+1) + mu_leisure_m_f * log(leisure_4.m+1) * log(leisure_6.f+1) + mu_age_m_income * Alter.m * log(income_34+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_34+1) + mu_age_f_income * Alter.f * log(income_34+1) + mu_age_f_sq_income * Alter.f^2 * log(income_34+1) + 
    mu_east_income * ostdeutsch * log(income_34+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_34+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_6.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_6.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_6.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_6.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_6.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_34+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_4.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_4.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_4.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_4.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_4.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_6.f+1) * log(leisure_4.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_6.f+1) * log(leisure_4.m+1)
  
  V[["alt35"]] = mu_income * log(income_35+1) + mu_income_sq * log(income_35^2+1) + mu_leisure_m * log(leisure_5.m+1) +  mu_leisure_m_sq * log(leisure_5.m^2+1) +
    mu_leisure_f * log(leisure_6.f+1) +  mu_leisure_f_sq * log(leisure_6.f^2+1) + mu_income_leisure_m * log(income_35+1) * log(leisure_5.m+1) + 
    mu_income_leisure_f * log(income_35+1) * log(leisure_6.f+1) + mu_leisure_m_f * log(leisure_5.m+1) * log(leisure_6.f+1) + mu_age_m_income * Alter.m * log(income_35+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_35+1) + mu_age_f_income * Alter.f * log(income_35+1) + mu_age_f_sq_income * Alter.f^2 * log(income_35+1) + 
    mu_east_income * ostdeutsch * log(income_35+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_35+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_6.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_6.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_6.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_6.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_6.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_35+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_5.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_5.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_5.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_5.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_5.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_6.f+1) * log(leisure_5.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_6.f+1) * log(leisure_5.m+1)
  
  V[["alt36"]] = mu_income * log(income_36+1) + mu_income_sq * log(income_36^2+1) + mu_leisure_m * log(leisure_6.m+1) +  mu_leisure_m_sq * log(leisure_6.m^2+1) +
    mu_leisure_f * log(leisure_6.f+1) +  mu_leisure_f_sq * log(leisure_6.f^2+1) + mu_income_leisure_m * log(income_36+1) * log(leisure_6.m+1) + 
    mu_income_leisure_f * log(income_36+1) * log(leisure_6.f+1) + mu_leisure_m_f * log(leisure_6.m+1) * log(leisure_6.f+1) + mu_age_m_income * Alter.m * log(income_36+1) +
    mu_age_m_sq_income * Alter.m^2 * log(income_36+1) + mu_age_f_income * Alter.f * log(income_36+1) + mu_age_f_sq_income * Alter.f^2 * log(income_36+1) + 
    mu_east_income * ostdeutsch * log(income_36+1) + mu_migration_f_income * Migrationshintergrund.f * log(income_36+1) + mu_migration_f_leisure_f * Migrationshintergrund.f * log(leisure_6.f+1) + 
    mu_age_f_leisure_f * Alter.f * log(leisure_6.f+1) + mu_age_f_sq_leisure_f * Alter.f^2 * log(leisure_6.f+1) + mu_east_leisure_f * ostdeutsch * log(leisure_6.f+1) +
    mu_child_leisure_f * Kind_u7 * log(leisure_6.f+1) + mu_migration_m_income * Migrationshintergrund.m * log(income_36+1) + 
    mu_migration_m_leisure_m * Migrationshintergrund.m * log(leisure_6.m+1) + mu_age_m_leisure_m * Alter.m * log(leisure_6.m+1) + 
    mu_age_m_sq_leisure_m * Alter.m^2 * log(leisure_6.m+1) + mu_east_leisure_m * ostdeutsch * log(leisure_6.m+1) + mu_child_leisure_m * Kind_u7 * log(leisure_6.m+1) +
    mu_migration_m_leisure_m_leisure_f * Migrationshintergrund.m * log(leisure_6.f+1) * log(leisure_6.m+1) + 
    mu_east_leisure_m_leisure_f * ostdeutsch * log(leisure_6.f+1) * log(leisure_6.m+1)
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1="1:1", alt2="1:2", alt3="1:3", alt4="1:4", alt5="1:5", alt6="1:6",
                      alt7="2:1", alt8="2:2", alt9="2:3", alt10="2:4", alt11="2:5", alt12="2:6",
                      alt13="3:1", alt14="3:2", alt15="3:3", alt16="3:4", alt17="3:5", alt18="3:6",
                      alt19="4:1", alt20="4:2", alt21="4:3", alt22="4:4", alt23="4:5", alt24="4:6",
                      alt25="5:1", alt26="5:2", alt27="5:3", alt28="5:4", alt29="5:5", alt30="5:6",
                      alt31="6:1", alt32="6:2", alt33="6:3", alt34="6:4", alt35="6:5", alt36="6:6"),
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = Arbeitszeitkategorien,
    V             = V#,  # tell function to use list vector defined above
  )
  
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
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

clogit_v = apollo_estimate(apollo_beta, apollo_fixed,
                              apollo_probabilities, apollo_inputs, 
                              estimate_settings=list(maxIterations=400,
                                                     estimationRoutine="bfgs",
                                                     hessianRoutine="analytic"))


# ################################################################# #
#### MODEL OUTPUTS                                               ##
# ################################################################# #
apollo_saveOutput(clogit_v)

summary(clogit_v)  

### Use the estimated model to make predictions

predictions_base = apollo_prediction(clogit_v, apollo_probabilities, apollo_inputs,
                                     prediction_settings=list(runs=30))

################################################################
### Estimate Elasticities                                     ###
################################################################

## Income increases by 10% 
database$income_1 = 1.1*database$income_1
database$income_2 = 1.1*database$income_2
database$income_3 = 1.1*database$income_3
database$income_4 = 1.1*database$income_4
database$income_5 = 1.1*database$income_5  
database$income_6 = 1.1*database$income_6  
database$income_7 = 1.1*database$income_7
database$income_8 = 1.1*database$income_8
database$income_9 = 1.1*database$income_9
database$income_10 = 1.1*database$income_10
database$income_11 = 1.1*database$income_11
database$income_12 = 1.1*database$income_12
database$income_13 = 1.1*database$income_13
database$income_14 = 1.1*database$income_14
database$income_15 = 1.1*database$income_15  
database$income_16 = 1.1*database$income_16  
database$income_17 = 1.1*database$income_17
database$income_18 = 1.1*database$income_18
database$income_19 = 1.1*database$income_19 
database$income_20 = 1.1*database$income_20
database$income_21 = 1.1*database$income_21
database$income_22 = 1.1*database$income_22
database$income_23 = 1.1*database$income_23
database$income_24 = 1.1*database$income_24
database$income_25 = 1.1*database$income_25  
database$income_26 = 1.1*database$income_26  
database$income_27 = 1.1*database$income_27
database$income_28 = 1.1*database$income_28
database$income_29 = 1.1*database$income_29
database$income_30 = 1.1*database$income_30
database$income_31 = 1.1*database$income_31
database$income_32 = 1.1*database$income_32
database$income_33 = 1.1*database$income_33
database$income_34 = 1.1*database$income_34
database$income_35 = 1.1*database$income_35  
database$income_36 = 1.1*database$income_36  

### Rerun predictions with the new data
apollo_inputs = apollo_validateInputs()
predictions_new = apollo_prediction(clogit_v, apollo_probabilities, apollo_inputs)  

### Return to original data
database$income_1 = 1/1.1*database$income_1
database$income_2 = 1/1.1*database$income_2
database$income_3 = 1/1.1*database$income_3
database$income_4 = 1/1.1*database$income_4
database$income_5 = 1/1.1*database$income_5  
database$income_6 = 1/1.1*database$income_6  
database$income_7 = 1/1.1*database$income_7
database$income_8 = 1/1.1*database$income_8
database$income_9 = 1/1.1*database$income_9
database$income_10 = 1/1.1*database$income_10
database$income_11 = 1/1.1*database$income_11
database$income_12 = 1/1.1*database$income_12
database$income_13 = 1/1.1*database$income_13
database$income_14 = 1/1.1*database$income_14
database$income_15 = 1/1.1*database$income_15  
database$income_16 = 1/1.1*database$income_16  
database$income_17 = 1/1.1*database$income_17
database$income_18 = 1/1.1*database$income_18
database$income_19 = 1/1.1*database$income_19 
database$income_20 = 1/1.1*database$income_20
database$income_21 = 1/1.1*database$income_21
database$income_22 = 1/1.1*database$income_22
database$income_23 = 1/1.1*database$income_23
database$income_24 = 1/1.1*database$income_24
database$income_25 = 1/1.1*database$income_25  
database$income_26 = 1/1.1*database$income_26  
database$income_27 = 1/1.1*database$income_27
database$income_28 = 1/1.1*database$income_28
database$income_29 = 1/1.1*database$income_29
database$income_30 = 1/1.1*database$income_30
database$income_31 = 1/1.1*database$income_31
database$income_32 = 1/1.1*database$income_32
database$income_33 = 1/1.1*database$income_33
database$income_34 = 1/1.1*database$income_34
database$income_35 = 1/1.1*database$income_35  
database$income_36 = 1/1.1*database$income_36
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
p_7b <- sum(predictions_base$alt7)/sum(predictions_base$Observation)
p_8b <- sum(predictions_base$alt8)/sum(predictions_base$Observation)
p_9b <- sum(predictions_base$alt9)/sum(predictions_base$Observation)
p_10b <- sum(predictions_base$alt10)/sum(predictions_base$Observation)
p_11b <- sum(predictions_base$alt11)/sum(predictions_base$Observation)
p_12b <- sum(predictions_base$alt12)/sum(predictions_base$Observation)
p_13b <- sum(predictions_base$alt13)/sum(predictions_base$Observation)
p_14b <- sum(predictions_base$alt14)/sum(predictions_base$Observation)
p_15b <- sum(predictions_base$alt15)/sum(predictions_base$Observation)
p_16b <- sum(predictions_base$alt16)/sum(predictions_base$Observation)
p_17b <- sum(predictions_base$alt17)/sum(predictions_base$Observation)
p_18b <- sum(predictions_base$alt18)/sum(predictions_base$Observation)
p_19b <- sum(predictions_base$alt19)/sum(predictions_base$Observation)
p_20b <- sum(predictions_base$alt20)/sum(predictions_base$Observation)
p_21b <- sum(predictions_base$alt21)/sum(predictions_base$Observation)
p_22b <- sum(predictions_base$alt22)/sum(predictions_base$Observation)
p_23b <- sum(predictions_base$alt23)/sum(predictions_base$Observation)
p_24b <- sum(predictions_base$alt24)/sum(predictions_base$Observation)
p_25b <- sum(predictions_base$alt25)/sum(predictions_base$Observation)
p_26b <- sum(predictions_base$alt26)/sum(predictions_base$Observation)
p_27b <- sum(predictions_base$alt27)/sum(predictions_base$Observation)
p_28b <- sum(predictions_base$alt28)/sum(predictions_base$Observation)
p_29b <- sum(predictions_base$alt29)/sum(predictions_base$Observation)
p_30b <- sum(predictions_base$alt30)/sum(predictions_base$Observation)
p_31b <- sum(predictions_base$alt31)/sum(predictions_base$Observation)
p_32b <- sum(predictions_base$alt32)/sum(predictions_base$Observation)
p_33b <- sum(predictions_base$alt33)/sum(predictions_base$Observation)
p_34b <- sum(predictions_base$alt34)/sum(predictions_base$Observation)
p_35b <- sum(predictions_base$alt35)/sum(predictions_base$Observation)
p_36b <- sum(predictions_base$alt36)/sum(predictions_base$Observation)

## worktime with increased net wage (10%) 
p_1n <- sum(predictions_new$alt1)/sum(predictions_base$Observation)
p_2n <- sum(predictions_new$alt2)/sum(predictions_base$Observation)
p_3n <- sum(predictions_new$alt3)/sum(predictions_base$Observation)
p_4n <- sum(predictions_new$alt4)/sum(predictions_base$Observation)
p_5n <- sum(predictions_new$alt5)/sum(predictions_base$Observation)
p_6n <- sum(predictions_new$alt6)/sum(predictions_base$Observation)
p_7n <- sum(predictions_new$alt7)/sum(predictions_base$Observation)
p_8n <- sum(predictions_new$alt8)/sum(predictions_base$Observation)
p_9n <- sum(predictions_new$alt9)/sum(predictions_base$Observation)
p_10n <- sum(predictions_new$alt10)/sum(predictions_base$Observation)
p_11n <- sum(predictions_new$alt11)/sum(predictions_base$Observation)
p_12n <- sum(predictions_new$alt12)/sum(predictions_base$Observation)
p_13n <- sum(predictions_new$alt13)/sum(predictions_base$Observation)
p_14n <- sum(predictions_new$alt14)/sum(predictions_base$Observation)
p_15n <- sum(predictions_new$alt15)/sum(predictions_base$Observation)
p_16n <- sum(predictions_new$alt16)/sum(predictions_base$Observation)
p_17n <- sum(predictions_new$alt17)/sum(predictions_base$Observation)
p_18n <- sum(predictions_new$alt18)/sum(predictions_base$Observation)
p_19n <- sum(predictions_new$alt19)/sum(predictions_base$Observation)
p_20n <- sum(predictions_new$alt20)/sum(predictions_base$Observation)
p_21n <- sum(predictions_new$alt21)/sum(predictions_base$Observation)
p_22n <- sum(predictions_new$alt22)/sum(predictions_base$Observation)
p_23n <- sum(predictions_new$alt23)/sum(predictions_base$Observation)
p_24n <- sum(predictions_new$alt24)/sum(predictions_base$Observation)
p_25n <- sum(predictions_new$alt25)/sum(predictions_base$Observation)
p_26n <- sum(predictions_new$alt26)/sum(predictions_base$Observation)
p_27n <- sum(predictions_new$alt27)/sum(predictions_base$Observation)
p_28n <- sum(predictions_new$alt28)/sum(predictions_base$Observation)
p_29n <- sum(predictions_new$alt29)/sum(predictions_base$Observation)
p_30n <- sum(predictions_new$alt30)/sum(predictions_base$Observation)
p_31n <- sum(predictions_new$alt31)/sum(predictions_base$Observation)
p_32n <- sum(predictions_new$alt32)/sum(predictions_base$Observation)
p_33n <- sum(predictions_new$alt33)/sum(predictions_base$Observation)
p_34n <- sum(predictions_new$alt34)/sum(predictions_base$Observation)
p_35n <- sum(predictions_new$alt35)/sum(predictions_base$Observation)
p_36n <- sum(predictions_new$alt36)/sum(predictions_base$Observation)


##Elasticity women married

worktime_base_f <- 0 * (p_1b + p_2b + p_3b + p_4b + p_5b + p_6b) +
  10 * (p_7b + p_8b + p_9b + p_10b + p_11b + p_12b) +
  20 * (p_13b + p_14b + p_15b + p_16b + p_17b + p_18b) +
  30 * (p_19b + p_20b + p_21b + p_22b + p_23b + p_24b) +
  40 * (p_25b + p_26b + p_27b + p_28b + p_29b + p_30b) +
  60 * (p_31b + p_32b + p_33b + p_34b + p_35b + p_36b)

worktime_new_f <- 0 * (p_1n + p_2n + p_3n + p_4n + p_5n + p_6n) +
  10 * (p_7n + p_8n + p_9n + p_10n + p_11n + p_12n) +
  20 * (p_13n + p_14n + p_15n + p_16n + p_17n + p_18n) +
  30 * (p_19n + p_20n + p_21n + p_22n + p_23n + p_24n) +
  40 * (p_25n + p_26n + p_27n + p_28n + p_29n + p_30n) +
  60 * (p_31n + p_32n + p_33n + p_34n + p_35n + p_36n)

## ELasticity 
elasticity_v_f <- ((worktime_new_f/worktime_base_f)-1) /1.1

### Elasticity men married

worktime_base_m <- 0 * (p_1b + p_7b + p_13b + p_19b + p_25b + p_31b) +
  10 * (p_2b + p_8b + p_14b + p_20b + p_26b + p_32b) +
  20 * (p_3b + p_9b + p_15b + p_21b + p_27b + p_33b) +
  30 * (p_4b + p_10b + p_16b + p_22b + p_28b + p_34b) +
  40 * (p_5b + p_11b + p_17b + p_23b + p_29b + p_35b) +
  60 * (p_6b + p_12b + p_18b + p_24b + p_30b + p_36b)

worktime_new_m <- 0 * (p_1n + p_7n + p_13n + p_19n + p_25n + p_31n) +
  10 * (p_2n + p_8n + p_14n + p_20n + p_26n + p_32n) +
  20 * (p_3n + p_9n + p_15n + p_21n + p_27n + p_33n) +
  30 * (p_4n + p_10n + p_16n + p_22n + p_28n + p_34n) +
  40 * (p_5n + p_11n + p_17n + p_23n + p_29n + p_35n) +
  60 * (p_6n + p_12n + p_18n + p_24n + p_30n + p_36n)

## ELasticity
elasticity_v_m <- ((worktime_new_m/worktime_base_m)-1) /1.1

