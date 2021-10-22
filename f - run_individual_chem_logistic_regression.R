run_individual_chem_logistic_regression <- function(x
                                                    , occupations_to_exclude_type
                                                    , occupations_to_exclude
                                                    , regression_eqt
                                                    , type_statistics
                                                    , df_survey_weights)
{
  library("broom")
  library("survey")
  
  chem_codename <- x$chem_copy %>%
    unique(.)
  # print(chem_codename)
  
  if(occupations_to_exclude_type == "None")
  {
    
  } else if(occupations_to_exclude_type == "Occupation Missing") {
    
    x <- x %>%
      filter(VNSECTORCOLLARCURR != occupations_to_exclude)
    
  } else if(occupations_to_exclude_type == "Unemployment") {
    
    x <- x %>%
      filter(!(VNSECTORCOLLARCURR %in% occupations_to_exclude))
    
  }
  # print(unique(x$VNSECTORCOLLARCURR) %>% sort(.))
  
  if("VNSECTORCOLLARCURR" %in% colnames(x))
  {
    x <- x %>%
      mutate(VNSECTORCOLLARCURR = relevel(factor(VNSECTORCOLLARCURR)
                                          , ref = "Public Administration - White"))
  } else if("VNBLUETOGCURR" %in% colnames(x)) {
    
    x <- x %>%
      mutate(VNBLUETOGCURR = relevel(factor(VNBLUETOGCURR)
                                     , ref = "Public Administration - White"))
    
  }
  
  if(chem_codename == "LBXCOT" & grepl("VNSMOKING", regression_eqt))
  {
    regression_eqt <- regression_eqt %>%
      gsub(" \\+ VNSMOKING", "", .)
    
  } 
  
  if(is_empty(df_survey_weights) == TRUE)
  {
    model <- glm(formula = as.formula(regression_eqt)
                 , data = x
                 , family = "binomial")
  } else {
    
    wt_chem_codename <- paste("WT_"
                              , chem_codename
                              , sep = "")
    # print(wt_chem_codename)
    
    subset_survey_weights <- df_survey_weights %>%
      select("SEQN", all_of(wt_chem_codename)) 
    
    index_survey_weights <- which(colnames(subset_survey_weights) == wt_chem_codename)
    colnames(subset_survey_weights)[index_survey_weights] <- "survey_weights"
    
    
    x <- x %>%
      left_join(.
                , subset_survey_weights 
                , by = "SEQN") %>%
      na.omit(.)
    
    unique_cycle_info <- x %>%
      summarise(SDDSRVYR = unique(SDDSRVYR)
                , denominator = unique(SDDSRVYR) %>%
                  length(.)) %>%
      group_by(SDDSRVYR, denominator) %>%
      do(boolean_multiplier = ifelse(c(1,2) %in% .$SDDSRVYR
                                     , TRUE
                                     , FALSE)) %>%
      ungroup(.) %>%
      mutate(numerator = ifelse(SDDSRVYR %in% c(1,2) 
                                & boolean_multiplier == "c(TRUE, TRUE)"
                                , 2
                                , 1)) %>%
      mutate(multiplier = numerator/denominator)
    # View(unique_cycle_info)
    
    x <- x %>%
      left_join(.
                , unique_cycle_info %>%
                  select("SDDSRVYR", "multiplier")
                , by = c("SDDSRVYR")) %>%
      mutate(adjusted_weights = multiplier*survey_weights) %>%
      mutate(survey_clusters = cluster(paste(SDMVPSU
                                             , SDMVSTRA)))
    # print(colnames(x))
    
    
    
    # regression_eqt <- paste(regression_eqt
    #                         , "+ cluster(clusters_survey)")
    
    vector_covariates <- regression_eqt %>%
      gsub("chem_value ~ ", "", .) %>%
      paste(., collapse = " + ") %>%
      str_split(., "\\s\\+\\s") %>%
      unlist(., use.names = FALSE) %>%
      unique(.)
    # print(regression_eqt)
    # print(vector_covariates)
    
    
    
    df_num_psu_per_stratum <- x %>%
      select("SDMVSTRA"
             , "SDMVPSU") %>%
      unique(.) %>%
      group_by(SDMVSTRA) %>%
      summarise(num_unique_psu = length(SDMVPSU)) %>%
      ungroup(.) %>%
      filter(num_unique_psu == 1)
    
    problematic_stratum <- df_num_psu_per_stratum %>%
      pull(SDMVSTRA)
    # View(df_num_psu_per_stratum)
    
    x <- x %>%
      filter(!(SDMVSTRA %in% problematic_stratum))
    
    clusters_survey <- x$survey_clusters
    
    adjusted_survey_weights <- x$adjusted_weights
    
    subset_x <- x %>%
      select("chem_value", all_of(vector_covariates))
    
    # # options(survey.lonely.psu="remove")
    # NHANES.svy <- svydesign(strata = ~SDMVSTRA
    #                         , id = ~SDMVPSU
    #                         , weights = ~adjusted_weights
    #                         , data = x
    #                         , nest = TRUE
    #                         )
    # 
    # model <- svyglm(as.formula(regression_eqt)
    #                 , design = NHANES.svy
    #                 , na.action = na.omit
    #                 , family = quasibinomial())
    # print(glance(model))
    
    model <- glm(formula = chem_value ~ . + cluster(clusters_survey)
                 , weights = adjusted_survey_weights
                 , data = subset_x
                 , family = quasibinomial())
    print(glance(model))
  }
  
  if(type_statistics == "coefficients")
  {
    df_stats <- tidy(model) %>%
      filter(grepl("cluster", term) == FALSE)
    # print(df_stats)
    
  } else if(type_statistics == "models") {
    
    null_model <- glm(chem_value ~ 1
                   , data = x
                   , family = "binomial")
    
    loglik_null <- glance(null_model)$logLik
    
    num_participants <- length(x$chem_value)
    
    r_square_mcfadden <- 1-logLik(model)/logLik(null_model) 
    # print(r_square_mcfadden)
    
    df_stats <- glance(model) %>%
      mutate(r.squared_mcfadden = 1 - logLik/loglik_null
             , r.squared_coxsnell = 1 - exp((2*(loglik_null - logLik))/num_participants)
             , r.squared_nagelkerke = r.squared_coxsnell/(1-exp(loglik_null)*(2/num_participants))) %>%
      mutate(r.squared_mcfadden = ifelse(r.squared_mcfadden < 0
                                         , 0
                                         , r.squared_mcfadden))
     
    
  }
  print(df_stats)
  
  
  return(df_stats)
}