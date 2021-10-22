run_individual_chem_en_regression <- function(x
                                              , occupations_to_exclude_type
                                              , occupations_to_exclude
                                              , regression_eqt)
{
  library("tidyverse")
  library("glmnet")
  library("broom")
  
  df_nhanes <- x
  
  chem_codename <- df_nhanes$chem_copy %>%
    unique(.)
  # print(chem_codename)
  
  if(occupations_to_exclude_type == "None")
  {
    
  } else if(occupations_to_exclude_type == "Occupation Missing") {
    
    df_nhanes <- df_nhanes %>%
      filter(VNSECTORCOLLARCURR != occupations_to_exclude)
    
  } else if(occupations_to_exclude_type == "Unemployment") {
    
    df_nhanes <- df_nhanes %>%
      filter(!(VNSECTORCOLLARCURR %in% occupations_to_exclude))
    
  }
  
  # print(colnames(df_nhanes))
  
  colname_y <- regression_eqt %>%
    str_split(., " ~ ", simplify = TRUE) %>%
    .[1]
  # print(colname_y)
  
  y <- df_nhanes %>%
    pull(all_of(colname_y))
  
  colnames_x <- regression_eqt %>%
    str_split(., " ~ ", simplify = TRUE) %>%
    .[2] %>%
    str_split(., " \\+ ", simplify = TRUE) %>%
    as.vector(.)
  # print(colnames_x)
  
  x_dataframe <- df_nhanes %>%
    select(all_of(colnames_x))
  
  x <- model.matrix(~.
                    , data = x_dataframe) %>%
    .[,-1]
  # View(x)
  
  # Set the random number generator to ensure reproducible results 
  set.seed(1)
  
  # Run 10-fold cross-validated cox model with survey weights
  cv_models <- cv.glmnet(x = x
                         , y = y
                         , family = "gaussian"
                         , nfolds = 10
                         , parallel = TRUE)
  # print(coef(cv_models, s = "lambda.min"))
  
  lambda_types <- c("lambda.min", "lambda.1se")
  num_lambda_types <- length(lambda_types)
  
  for(i in seq(num_lambda_types))
  {
    lambda_type_i <- lambda_types[i]
    
    predictions <- predict(cv_models
                           , x 
                           , s = lambda_type_i)
    
    df_stats_i <- lm(y ~ predictions) %>%
      glance(.) %>%
      mutate(lambda_type = lambda_type_i)
    
    if(i == 1)
    {
      df_stats <- df_stats_i
    } else {
      df_stats <- df_stats %>%
        full_join(.
                  , df_stats_i
                  , by = colnames(.))
    }
  }

  return(df_stats)
  
}