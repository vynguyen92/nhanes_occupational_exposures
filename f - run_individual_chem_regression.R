run_individual_chem_regression <- function(x
                                           , occupations_to_exclude_type
                                           , occupations_to_exclude
                                           , regression_eqt
                                           , type_statistics
                                           , df_survey_weights
                                           , type_of_weighted_model)
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
  # print(levels(x$VNSECTORCOLLARCURR))

  if(chem_codename == "LBXCOT" & grepl("VNSMOKING", regression_eqt))
  {
    regression_eqt <- regression_eqt %>%
      gsub(" \\+ VNSMOKING", "", .)

  }
  # print(regression_eqt)

  if(is_empty(df_survey_weights) == TRUE)
  {
    model <- lm(formula = as.formula(regression_eqt)
                , data = x)
  } else {
    
    wt_chem_codename <- paste("WT_"
                              , chem_codename
                              , sep = "")
    # print(wt_chem_codename)

    subset_survey_weights <- df_survey_weights %>%
      select("SEQN", all_of(wt_chem_codename))
    

    index_survey_weights <- which(colnames(subset_survey_weights) == wt_chem_codename)
    colnames(subset_survey_weights)[index_survey_weights] <- "survey_weights"
    # print(str(subset_survey_weights))

    x <- x %>%
      left_join(.
                , subset_survey_weights
                , by = "SEQN") %>%
      na.omit(.)
    # print(str(x))
    
    unique_cycle_info <- x %>%
      summarise(SDDSRVYR = unique(SDDSRVYR)
                , denominator = unique(SDDSRVYR) %>%
                  length(.)) #%>%
      # group_by(SDDSRVYR, denominator) %>%
    # View(unique_cycle_info)


    unique_cycle_info <- unique_cycle_info %>%
      mutate(intersect_1_2 = intersect(c(1,2), unique_cycle_info$SDDSRVYR) %>%
      toString(.)) %>%
      mutate(boolean_multiplier = case_when(SDDSRVYR %in% c(1,2) & intersect_1_2 == "1, 2" ~ TRUE
                                            , SDDSRVYR %in% c(1,2) & intersect_1_2 == "1" ~ FALSE
                                            , SDDSRVYR %in% c(1,2) & intersect_1_2 == "2" ~ FALSE
                                            , TRUE ~ FALSE
                                            )) %>%
      mutate(numerator = ifelse(boolean_multiplier == TRUE
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
    # print(str(x))
   
    # regression_eqt <- paste(regression_eqt
    #                     , "+ cluster(clusters_survey)"
    #                     )

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
    # View(df_num_psu_per_stratum)

    problematic_stratum <- df_num_psu_per_stratum %>%
      pull(SDMVSTRA)
    # View(df_num_psu_per_stratum)

    x <- x %>%
      filter(!(SDMVSTRA %in% problematic_stratum))
    # print(str(x))

    clusters_survey <- x$survey_clusters

    adjusted_survey_weights <- x$adjusted_weights

    

    # options(survey.lonely.psu="remove")
   

    if(type_of_weighted_model == "svyglm")
    {
      NHANES.svy <- svydesign(strata = ~SDMVSTRA
                              , id = ~SDMVPSU
                              , weights = ~adjusted_weights
                              , data = x
                              , nest = TRUE)
      # print(str(NHANES.svy))
      
      model <- svyglm(as.formula(regression_eqt)
                      , design = NHANES.svy
                      , na.action = na.omit)

      r_squares <- svyrsquared.2(model
                                 , "chem_value")

    } else if(type_of_weighted_model == "lm") {

      subset_x <- x %>%
        select("chem_value", all_of(vector_covariates))
      
      model <- lm(formula = chem_value ~ . + cluster(clusters_survey)
                  , weights = adjusted_survey_weights
                  , data = subset_x)
    }
    
  }
  
  if(type_statistics == "coefficients")
  {
    df_stats <- tidy(model) %>%
      filter(grepl("cluster", term) == FALSE)
    # print(df_stats)

  } else if(type_statistics == "models") {

    if(is_empty(df_survey_weights) == TRUE)
    {
      df_stats <- glance(model)
      
    } else {
      if(type_of_weighted_model == "svyglm")
      {
        df_stats <- data.frame(chemical_codename_use = chem_codename
                               , r.squared = r_squares$rsq
                               , adj.r.squared = r_squares$adj.r2
                               , type_of_weighted_model = "svyglm")
        
      } else if(type_of_weighted_model == "lm") {
        
        df_stats <- glance(model) %>%
          mutate(type_of_weighted_model = "svyglm")
      }
    }
    

  }

  return(df_stats)
  
}