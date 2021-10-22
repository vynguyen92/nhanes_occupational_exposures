#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#################  FUNCTION TO EXCLUDE INDICATORS WHEN PARTICIPANTS DO NOT HAVE MEASUREMENTS  #################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function exclude comment indicators when the participants do not have chemical measurements
#
# Inputs: dataset_comments - dataframe of the compiled comments dataset
#         dataset_chemicals - dataframe of the complied chemical biomarker dataset
#         df_doc_cleaning - dataframe dictating the cleaning documentation for the chemicals
#
# Outputs: dataset_clean - dataframe of the comments dataset where participants who do not have chemical measure-
#                          ments are excluded.

exclude_extraneous_indicators <- function(dataset_comments
                                          , dataset_chemicals
                                          , df_doc_cleaning)
{
  # Define a subset of chemicals that have been documented to have extraneous indicators
  subset_doc_cleaning <- df_doc_cleaning %>%
    filter(LOD_notes == "Extraneous comment indicators")
  # View(subset_doc_cleaning)
  
  # Determine the number of cases (chemicals in which cycle) that have extraneous indicators
  num_problems <- nrow(subset_doc_cleaning)
  
  for(i in seq(num_problems))
  {
    # Extra information pertaining to one problematic case
    subset_cleaning_i <- subset_doc_cleaning[i,]
    # print(subset_cleaning_i)
    
    # Determine the chemical codename for that problematic case
    chemical_codename_i <- subset_cleaning_i %>%
      select(chemical_codename) %>%
      unlist(., use.names = FALSE)
    # print(chemical_codename_i)
    
    # Determine the comment codename for that problematic case
    comment_codename_i <- subset_cleaning_i %>%
      select(comments_codename_use) %>%
      unlist(., use.names = FALSE)
    # print(comment_codename_i)
    
    # Extract the chemical biomarker measurements for that chemical 
    chemical_measurements_i <- dataset_chemicals[,chemical_codename_i]
    # print(unique(chemical_measurements_i) %>% length(.))

    # Extract the chemical biomarker measurements for that chemical comment
    comments_i <- dataset_comments[,comment_codename_i]
    # print(str(comments_i))
    # print((comments_i) %>% unique(.))

    # Form a dataset with the chemical measurements, comments, and study year
    df_measurements <- data.frame(mesurements = chemical_measurements_i
                                  , comments = comments_i
                                  , study_year = dataset_chemicals$study_year)
    # View(df_measurements %>% unique(.))

    # Determine the participants who do not have chemical measurements but a comment indicator in 
    # the problematic cycle
    index_to_exclude <- which(is.na(df_measurements$mesurements) 
                              & !is.na(df_measurements$comments)
                              & df_measurements$study_year == subset_cleaning_i$SDDSRVYR)
    # print(df_measurements[index_to_exclude,] %>% nrow(.))
    
    # Exclude participants who do not have chemical measurements but a comment indicator
    dataset_comments[index_to_exclude, comment_codename_i] <- NA
  }
  
  dataset_comments_clean <- dataset_comments
  
  return(dataset_comments_clean)
}