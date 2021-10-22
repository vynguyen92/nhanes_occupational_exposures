run_individual_chem_rf_regression <- function(x
                                              , occupations_to_exclude_type
                                              , occupations_to_exclude
                                              , regression_eqt)
{
  library("tidyverse")
  library("caret")
  library("ranger")
  
  df_nhanes <- x
  
  chem_codename <- df_nhanes$chem_copy %>%
    unique(.)
  print(chem_codename)
  
  if(occupations_to_exclude_type == "None")
  {
    
  } else if(occupations_to_exclude_type == "Occupation Missing") {
    
    df_nhanes <- df_nhanes %>%
      filter(VNSECTORCOLLARCURR != occupations_to_exclude)
    
  } else if(occupations_to_exclude_type == "Unemployment") {
    
    df_nhanes <- df_nhanes %>%
      filter(!(VNSECTORCOLLARCURR %in% occupations_to_exclude))
    
  }
  
  subset_nhanes <- df_nhanes %>%
    select(-c("SEQN", "chemical_codename_use", "chem_copy"))
  print(colnames(subset_nhanes))
  set.seed(1)
  in_training <- createDataPartition(df_nhanes$chem_value
                                     , p = 0.80
                                     , list = FALSE)
  
  # training <- subset_nhanes[in_training,]
  # testing <- subset_nhanes[-in_training,]

  fit_control <- trainControl(method = "cv"
                              , number = 10
                              , search = "random")

  set.seed(1)
  rf_fit <- train(as.formula(regression_eqt)
                  , data = subset_nhanes
                  , method = "ranger"
                  , metric = "Rsquared"
                  , tuneLength = 5
                  , trControl = fit_control)
  print(rf_fit)
  
}