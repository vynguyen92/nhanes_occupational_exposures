#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#################  FUNCTION TO FORM A DATASET TO INDICATE MEASUREMENTS ABOVE OR BELOW THE LOD #################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function forms a dataset containing indicators to dicate whether measurements are above or below
#          the LOD.
#
# Inputs: dataset_unclean - dataframe of the compiled dataset
#         list_document_cleaning - list of datasets dictating the cleaning documentation
#         name_dataset - string of the name of the dataset to clean
#
# Outputs: dataset_clean - dataframe of the comments dataset where rows are the participants and columns are the
#                          comment codenames.

form_comments_dataset <- function(dataset_unclean
                                  , list_document_cleaning
                                  , name_dataset = "Chemicals")
{
  # Extract the documentation dataset for the chemicals
  dataset_document_cleaning <- list_document_cleaning[[name_dataset]]
  
  # Extract the codenames of the chemical comments 
  comment_codenames <- dataset_document_cleaning %>%
    filter(is.na(codenames_for_sum) == TRUE) %>%
    select("comment_codename") %>%
    unique(.) %>%
    .[!is.na(.)]
  # print(comment_codenames)
  
  # Form a dataset with the participant identifier, the chemical comments, and the study_year
  dataset_clean <- dataset_unclean[,c("SEQN"
                                      , comment_codenames
                                      , "study_year")]
  # print(str(comments_unclean))
  
  # Determine the codenames with the correction
  corrected_codenames <- dataset_document_cleaning %>%
    filter(is.na(corrected_comment_codename) == FALSE) %>%
    filter(is.na(LOD_notes) == TRUE) %>%
    select("corrected_comment_codename") %>%
    unique(.) %>%
    unlist(., use.names = FALSE) #%>%
    # .[which(. == "VNURXPFUAL")]
  # print(corrected_codenames)

  print("Ensure a chemical comment has only one codename")

  # For chemicals with multiple comment codenames, harmonize the codenames
  dataset_clean <- resolve_multiple_codenames(corrected_codenames
                                              , "corrected_comment_codename"
                                              , "comment_codename"
                                              , dataset_document_cleaning
                                              , dataset_clean
                                              , "study_year")
  # print(str(dataset_clean))

  # dataset_clean %>% select(LBDBFOAL, SSBPFOAL, study_year) %>% unique(.) %>% View(.)
  
  # Define a vector of the harmonized comment codenames
  included_codenames_unique <- dataset_document_cleaning %>%
    filter(is.na(codenames_for_sum) == TRUE) %>%
    filter(is.na(LOD_notes) == TRUE) %>%
    select("comments_codename_use") %>%
    unique(.) %>%
    unlist(., use.names = FALSE) %>%
    .[!is.na(.)]
  # print(included_codenames_unique)
 

  # Define the comment dataset to contain the participant identifier, harmonized comment codenames, and the study year
  dataset_final <- dataset_clean %>%
    select("SEQN"
           , all_of(included_codenames_unique)
           , "study_year") %>%
    # Rename the column "study_year" to SDDSRVYR to match with the other types of dataset
    rename(.
           , "SDDSRVYR" = "study_year")
  # print(str(dataset_final))

  # Ensure that the column for study years is labeled
  attr(dataset_final$SDDSRVYR, "label") <- "Data Release Number"

  print("Include indicators for participants being above or below the LOD for cycles with missing comment codename")
  
  # For study years where there is not a comment codename but an LOD, indicate whether participants are above or below
  # the LOD
  dataset_final <- indicate_relation_to_LOD_comment_missing(dataset_final
                                                            , dataset_unclean
                                                            , dataset_document_cleaning)
  
  print("Exclude indicators when participants do not have measurements")

  # Exclude indicators of whether a participant's biomarker levels are below or above the LOD when participants do not 
  # have measurements
  dataset_final <- exclude_extraneous_indicators(dataset_final
                                                 , dataset_unclean
                                                 , dataset_document_cleaning)
  
  print("Combine comments together for PFASs")
  
  # Include indicators of being below or above LOD for PFOS and PFOA, which need to be summed using branched and linear 
  # isomers 
  dataset_final <- combine_variables_together_for_pfas(dataset_final
                                                       , dataset_unclean
                                                       , dataset_document_cleaning
                                                       , "comments")
  
  print("Sum metabolites of parent compounds")
  
  # Include summation of metabolites for certain parent compounds to help compare to biomonitoring equivalents
  dataset_final <- include_summation_of_chemicals(dataset_final
                                                  , dataset_document_cleaning
                                                  , "comments"
                                                  , dataset_unclean)
  
  # View(dataset_final %>%
  #        select("SDDSRVYR", "LBDPFOSL", "LBDPFOAL") %>%
  #        unique(.))
  
  print("Exclude measurements due to drastic changes in LOD")

  # Exclude measurements from study years for chemicals that show drastic changes in the LOD
  dataset_final <- exclude_measurements_based_changes_in_lod(dataset_final
                                                             , dataset_document_cleaning
                                                             , "comments_codename_use"
                                                             , "SDDSRVYR")
  
  print("Include comments that are reasonable (i.e. values are 0, 1, or 2)")
  
  # Exclude unreasonable comments (e.g. there is a participant with a comment of 37)
  dataset_final <- ensure_reasonable_comments(dataset_final
                                              , dataset_unclean
                                              , dataset_document_cleaning)
  
  # Ensure that the study year is numeric as a data type
  dataset_final$SDDSRVYR <- as.numeric(dataset_final$SDDSRVYR)
  
  # Assign the name of the codename for the study year
  attr(dataset_final$SDDSRVYR, "label") <- "Data release cycle"
  
  # Ensure that the participant ID is an integer
  dataset_final$SEQN <- as.integer(dataset_final$SEQN)
  
  # Assign the name of the codename for the participant ID
  attr(dataset_final$SEQN, "label") <- "Respondent sequence number"

  return(dataset_final)
}