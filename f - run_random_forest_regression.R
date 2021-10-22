run_random_forest_regression <- function(dataset_merged
                                     , vector_equtions
                                     , df_stats
                                     , df_include_occupations
                                     , statistics_type
                                     , inclusion_statement = "")
{
  library(tidyverse)
  # print(colnames(dataset_merged))
  
  occupations_exclude <- df_include_occupations %>%
    filter(include == "no") %>%
    pull(VNSECTORCOLLARCURR)
  
  vector_chemical_codenames <- df_stats %>%
    filter(include == "yes") %>%
    pull(chemical_codename) #%>%
  # .[1:2] %>%
  # c(., "LBXCOT")
  # print(vector_chemical_codenames)
  
  vector_chemical_codenames <- "LBXBPB"
  
  vector_covariates <- vector_equtions %>%
    gsub("chem_value ~ ", "", .) %>%
    paste(., collapse = " + ") %>%
    str_split(., "\\s\\+\\s") %>%
    unlist(., use.names = FALSE) %>%
    unique(.)
  # print(vector_covariates)
  
  if(inclusion_statement != "")
  {
    dataset_merged <- dataset_merged %>%
      filter(eval(parse(text = inclusion_statement)))
  }
  
  
  dataset_long <- dataset_merged %>%
    select("SEQN"
           , all_of(vector_covariates)
           , all_of(vector_chemical_codenames)) %>%
    filter(!(VNSECTORCOLLARCURR %in% occupations_exclude)) %>%
    gather(., chemical_codename_use, chem_value, all_of(vector_chemical_codenames)) %>%
    na.omit(.) %>%
    mutate(chem_copy = chemical_codename_use) %>%
    filter(chem_value != 0) %>%
    mutate(chem_value = log10(chem_value))
  # print(str(dataset_long))
  
  excluded_occupations <- list("Occupation Missing" = "Occupation Missing"
                               , "Unemployment" = c("Disabled"
                                                    , "Going to school"
                                                    , "Looking for work"
                                                    , "On layoff"
                                                    , "Retired"
                                                    , "Taking care of house or family"
                                                    , "Unable to work for health reasons"
                                                    , "Occupation Missing")
                                , "None" = "None")
  
  excluded_occupations <- excluded_occupations["None"]
  
  num_include_occupation_missing <- length(excluded_occupations)
  
  num_equations <- length(vector_equtions)
  
  for(i in seq(num_equations))
  {
    formula_eqt <- vector_equtions[i]
    print(formula_eqt)
    
    for(j in seq(num_include_occupation_missing))
    {
      excluded_occupations_j <- excluded_occupations[[j]]
      # print(excluded_occupations_j)
      
      excluded_type_j <- names(excluded_occupations)[j]
      # print(excluded_type_j)
      
      df_regressions_j <- dataset_long %>%
        group_by(chemical_codename_use) %>%
        do(run_individual_chem_rf_regression(.
                                             , excluded_type_j
                                             , excluded_occupations_j
                                             , formula_eqt
                                             )) %>%
        ungroup(.)
    }
  }
}