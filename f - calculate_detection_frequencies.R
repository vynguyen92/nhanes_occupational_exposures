#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##########################  FUNCTION TO CALCULATE DETECTION FREQUENCIES OF THE CHEMICALS  #####################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function calculates the detection frequency of the chemicals and exclude those based on having 
#          a low frequency relative to a user-defined threshold
#
# Inputs: dataset_comments - dataframe of the cleaned comment dataset subsetted to user's study population
#         dataset_study_pop - dataframe of containing the study population
#         dataset_document_cleaning - data frame of the cleaning documentation for the chemical datasets
#         threshold_freq - numeric value dictating the minimum threshold of detection frequency (%) to exclude
#                          chemicals based on low detection frequencies
#
# Outputs: df_detection_frequencies - dataframe of detection frequency for each chemical and which chemicals to 
#                                     include or exclude 

calculate_detection_frequencies <- function(dataset_comments
                                            , dataset_study_pop
                                            , dataset_chemicals
                                            , dataset_demographics
                                            , dataset_document_cleaning
                                            , include_sector_collar_dataset
                                            , threshold_freq)
{
  
  library("mgsub")
  
  # Determine all column names that aren't the participant ID nor the study year to extract all codenames
  # pertaining to the comments
  comments_codename <- colnames(dataset_comments) %>%
    .[which(!(. %in% c("SEQN", "SDDSRVYR")))] #%>%
  # .[1:5]
  # print(comments_codename)
  
  # comments_codename <- c("URDUCSLC", "LBDBCOLC")
  
  vector_sector_collar_include <- include_sector_collar_dataset %>%
    filter(include == "yes") %>%
    pull(VNSECTORCOLLARCURR)
  # print(vector_sector_collar_include)
  
  dataset_study_pop <- dataset_study_pop %>%
    select("SEQN", "VNSECTORCOLLARCURR", "SDDSRVYR") %>%
    mutate(VNSECTORCOLLARCURR = ifelse(is.na(VNSECTORCOLLARCURR)
                                       , "Occupation Missing"
                                       , VNSECTORCOLLARCURR)) %>%
    filter(VNSECTORCOLLARCURR %in% vector_sector_collar_include) %>%
    full_join(.
              , dataset_demographics[,c("SEQN", "RIDAGEYR")]
              , by = "SEQN") %>%
    full_join(.
              , dataset_chemicals %>% select(-SDDSRVYR)
              , by = "SEQN") %>%
    filter(RIDAGEYR >= 16) %>%
    filter(SDDSRVYR <= 8) 
  # print(dim(dataset_study_pop))
  
  seqn_sector_collar_include <- dataset_study_pop %>%
    select("SEQN") %>%
    unlist(., use.names = FALSE)

  # Reformat into a long dataset with values of the comments in one column
  comments_long <- dataset_comments %>%
    right_join(.
              , dataset_study_pop[,c("SEQN", "VNSECTORCOLLARCURR", "RIDAGEYR")]
              , by = "SEQN") %>%
    select("SEQN", "SDDSRVYR", "VNSECTORCOLLARCURR", "RIDAGEYR", all_of(comments_codename)) %>%
    filter(SEQN %in% seqn_sector_collar_include) %>%
    gather(., comments_codename_use, value, all_of(comments_codename)) %>%
    na.omit(.)
  # View((comments_long))
  # print(dim(comments_long))

  df_detection_frequencies_each <- comments_long %>%
    group_by(comments_codename_use, VNSECTORCOLLARCURR) %>%
    # Calculate the detection frequencies
    summarise(num_above_LOD = sum(value == 0)
              , num_below_LOD = sum(value == 1)
              , num_exceeding_LOD = sum(value == 2)
              , total_participants = n()
              , perc_above_LOD = num_above_LOD/total_participants*100
              , perc_below_LOD = num_below_LOD/total_participants*100
              , perc_exceeding_LOD = num_exceeding_LOD/total_participants*100) %>%
    ungroup(.)
  # View(df_detection_frequencies_each)
  
  df_fraction_detection_frequency <- df_detection_frequencies_each %>%
    group_by(comments_codename_use) %>%
    summarise(variance_across_groups = var(perc_above_LOD)
              , mean_across_groups = mean(perc_above_LOD)
              , num_sector_collars = length(perc_above_LOD)
              # num_above_0.5 = sum(perc_above_LOD >= 50)
              # , perc_above_0.5 = sum(perc_above_LOD >= 50)/length(perc_above_LOD)*100
              ) %>%
    ungroup(.)
  # View(df_fraction_detection_frequency)
  
  df_detection_frequencies <- comments_long %>%
    group_by(comments_codename_use) %>%
    # Calculate the detection frequencies
    summarise(num_above_LOD = sum(value == 0)
              , num_below_LOD = sum(value == 1)
              , num_exceeding_LOD = sum(value == 2)
              , total_participants = n()
              , perc_above_LOD = num_above_LOD/total_participants*100
              , perc_below_LOD = num_below_LOD/total_participants*100
              , perc_exceeding_LOD = num_exceeding_LOD/total_participants*100) %>%
    ungroup(.) %>%
    full_join(.
              , df_fraction_detection_frequency
              , by = "comments_codename_use") %>%
    # Merge in identifier information on the chemicals
    left_join(.
              , dataset_document_cleaning[,c("comments_codename_use"
                                             , "chemical_codename_use"
                                             , "chemical_name"
                                             , "units"
                                             , "chem_family"
                                             , "chem_family_shortened")] %>%
                unique(.)
              , by = "comments_codename_use") %>%
    rename(.
           , chemical_codename = chemical_codename_use) %>%
    # Add a column to dictate which chemicals to include or exclude
    mutate(include = ifelse(perc_above_LOD > threshold_freq
                            , "yes"
                            , "no"))
  # View(df_detection_frequencies)

  # Extract a vector of the chemical codenames to include
  codenames_include <- df_detection_frequencies %>%
    filter(include == "yes") %>%
    select(chemical_codename) %>%
    unique(.) %>%
    unlist(., use.names = FALSE)

  # Determine the chemical codenames that are not lipid-adjusted
  codenames_non_lipid_adjusted <- codenames_include %>%
    .[grepl("[A-a0-9]{6}LA$",.)] %>%
    gsub("LA$", "", .) %>%
    gsub("LBX199", "LBD199", .)
  # print(codenames_non_lipid_adjusted)

  # Determine the row indices that pertain to the non lipid-adjusted chemicals
  index_codename_non_lipid_adjusted  <- which(df_detection_frequencies$chemical_codename %in%
                                                codenames_non_lipid_adjusted)

  # Determine the number of chemicals that are not lipid-adjusted
  num_codenames_non_lipid_adjusted <- length(index_codename_non_lipid_adjusted)
  # print(paste("Number of chemicals that are NOT lipid adjusted", num_codenames_non_lipid_adjusted))

  # Indicate that non lipid-adjusted chemicals should be excluded from further analysis because we tend to use
  # the lipid-adjusted counterparts
  df_detection_frequencies[index_codename_non_lipid_adjusted, "include"] <- rep("no"
                                                                                , num_codenames_non_lipid_adjusted)

  # df_detection_frequencies <- df_detection_frequencies %>%
  #   mutate(include = ifelse(chem_family == "Dietary Components"
  #                           , "no"
  #                           , include))

  list_detection_frequency <- list("each_sector_collar" = df_detection_frequencies_each
                                   , "average" = df_detection_frequencies)
  
  return(list_detection_frequency)
}