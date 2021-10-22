calculate_statistics_on_percentages_above_be <- function(dataset_merged
                                                         , df_include_occupations
                                                         , df_stats
                                                         , vector_covariates
                                                         , df_survey_weights = NULL)
{
  library("tidyverse")
  library("readxl")
  
  
  
  df_chem_colors <- read_excel("NHANES - Color for Chemical Family.xlsx")
  
  df_biomonitoring_equivalents <- read_excel("NHANES - Master List of Files 1c.xlsx"
                                             , sheet = "Biomonitoring Equivalents")
  # View(df_biomonitoring_equivalents)
  
  
  endpoint_types <- df_biomonitoring_equivalents %>%
    filter(!is.na(biomonitoring_equivalents)) %>%
    pull(endpoint_type) %>%
    unique(.) 
  
  num_endpoint_types <- length(endpoint_types)
  # print(num_endpoint_types)
  
  list_stats_perc_above_be <- list()
  
  for(i in seq(num_endpoint_types))
  {
    endpoint_type_i <- endpoint_types[i]
    print(endpoint_type_i)
    
    subset_biomonitoring_equivalents_i <- df_biomonitoring_equivalents %>%
      filter(endpoint_type == endpoint_type_i) %>%
      group_by(chemical_codename
               , chemical_name
               , units
               , cas_num
               , biological_medium) %>%
      summarise(biomonitoring_equivalents = min(biomonitoring_equivalents)) %>%
      ungroup(.) %>%
      filter(!is.na(biomonitoring_equivalents))
    # View(subset_biomonitoring_equivalents_i)
    
    occupations_exclude <- df_include_occupations %>%
      filter(include == "no") %>%
      pull(VNSECTORCOLLARCURR)
    # print(occupations_exclude)
    
    vector_chemical_codenames <- df_stats %>%
      filter(include == "yes") %>%
      pull(chemical_codename) %>%
      intersect(.
                , subset_biomonitoring_equivalents_i %>%
                  pull(chemical_codename))
    # .[1:2] %>%
    # c(., "LBXCOT")
    # print(vector_chemical_codenames)
    
    # vector_chemical_codenames <- c("LBXBCD", "LBXVXY", "URXUAS")
    
    dataset_long <- dataset_merged %>%
      select("SEQN"
             , all_of(vector_covariates)
             , all_of(vector_chemical_codenames)) %>%
      filter(!(VNSECTORCOLLARCURR %in% occupations_exclude)) %>%
      gather(., chemical_codename, chem_value, all_of(vector_chemical_codenames)) %>%
      na.omit(.) %>%
      filter(chem_value != 0) %>%
      left_join(.
                , subset_biomonitoring_equivalents_i %>%
                  select("chemical_codename"
                         , "chemical_name"
                         , "units"
                         , "biological_medium"
                         , "biomonitoring_equivalents")
                , by = "chemical_codename") %>%
      na.omit(.) %>%
      mutate(boolean_above_be = chem_value > biomonitoring_equivalents)
    # View(dataset_long %>% filter(chemical_codename == "VNSUMDEHP"))
    
    # print(which(dataset_long$chemical_codename == "VNSUMDEHP"))
    # print(str(dataset_long))

    if(is_empty(df_survey_weights) == TRUE)
    {
      analysis_type <- "unweighted"
      print(analysis_type)
      # View(dataset_long)

      dataset_perc_above_be_long <- dataset_long %>%
        group_by(chemical_codename, VNSECTORCOLLARCURR) %>%
        summarise(num_above_be = sum(boolean_above_be)
                  , total_participants = length(boolean_above_be)
                  , perc_above_be = num_above_be/total_participants*100) %>%
        ungroup(.)
      # View(dataset_perc_above_be_long)

    } else {

      analysis_type <- "weighted"

      vector_wt_chem_codenames <- paste("WT_"
                                        , vector_chemical_codenames
                                        , sep = "")

      subset_survey_weights <- df_survey_weights %>%
        select("SEQN", all_of(vector_wt_chem_codenames)) %>%
        pivot_longer(cols = all_of(vector_wt_chem_codenames)
                     , names_to = "chemical_codename"
                     , values_to = "survey_weights") %>%
        na.omit(.) %>%
        mutate(chemical_codename = gsub("^WT_"
                                        , ""
                                        , chemical_codename))
      # print(str(subset_survey_weights))

      dataset_long <- dataset_long %>%
        left_join(.
                  , subset_survey_weights
                  , by = c("SEQN", "chemical_codename"))
      # print(str(dataset_long))

      unique_cycle_info <- dataset_long %>%
        group_by(chemical_codename) %>%
        summarise(SDDSRVYR = unique(SDDSRVYR)
                  , denominator = unique(SDDSRVYR) %>%
                    length(.)
        ) %>%
        ungroup(.)

      multiplier_info <- unique_cycle_info %>%
        group_by(chemical_codename) %>%
        do(boolean_multiplier = ifelse(c(1,2) %in% .$SDDSRVYR
                                       , TRUE
                                       , FALSE))
      # View(multiplier_info)

      unique_cycle_info <- unique_cycle_info %>%
        full_join(.
                  , multiplier_info
                  , by = "chemical_codename") %>%
        mutate(numerator = ifelse(SDDSRVYR %in% c(1,2)
                                  & boolean_multiplier == "c(TRUE, TRUE)"
                                  , 2
                                  , 1)) %>%
        mutate(multiplier = numerator/denominator)
      # View(unique_cycle_info)

      dataset_long <- dataset_long %>%
        left_join(.
                  , unique_cycle_info %>%
                    select("chemical_codename", "SDDSRVYR", "multiplier")
                  , by = c("chemical_codename", "SDDSRVYR")) %>%
        mutate(adjusted_weights = multiplier*survey_weights)
      View(dataset_long)

      dataset_perc_above_be_long <- dataset_long %>%
        group_by(chemical_codename, VNSECTORCOLLARCURR) %>%
        summarise(num_above_be = sum(adjusted_weights[boolean_above_be == TRUE]
                                     , na.rm = TRUE)
                  , total_participants = sum(adjusted_weights
                                             , na.rm = TRUE)
                  , perc_above_be = num_above_be/total_participants*100) %>%
        ungroup(.)
      View(dataset_perc_above_be_long)
    }

    dataset_stats_perc <- dataset_perc_above_be_long %>%
      group_by(chemical_codename) %>%
      summarise(min_perc = min(perc_above_be)
                , median_perc = median(perc_above_be)
                , mean_perc = mean(perc_above_be)
                , sd_perc = sd(perc_above_be)
                , max_perc = max(perc_above_be)) %>%
      ungroup(.)

    list_stats_perc_above_be[[endpoint_type_i]] <- dataset_stats_perc
  }
    
  return(list_stats_perc_above_be)
}