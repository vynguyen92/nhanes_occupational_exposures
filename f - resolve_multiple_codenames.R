#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#########################  FUNCTION TO ENSURE THAT ONE VARIABLE ONLY HAS ONE CODENAME #########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function harmonizes the codenames to ensure that a variable has only one codename.
#
# Inputs: vector_corrected_codenames - vector of the harmonized codenames
#         colname_corrected_codename - string of the column name with the harmonized codenames
#         colname_old_codename - string of the column name of the original (not harmonized) codenames
#         df_doc_cleaning - dataframe of the dataset containing the cleaning documentation
#         df_unclean - dataframe of the unclean dataset
#         colname_cycle - string of the column name pertaining to the study years
#
# Outputs: df_clean - dataframe of the dataset where rows are the participants and columns are the harmonized
#                     codenames.

resolve_multiple_codenames <- function(vector_corrected_codenames
                                       , colname_corrected_codename
                                       , colname_old_codename
                                       , df_doc_cleaning
                                       , df_unclean
                                       , colname_cycle)
{
  # Determine the number of codenames that needs to be harmonized
  num_corrected_codenames <- length(vector_corrected_codenames)
  # print(num_corrected_codenames)
  
  for(i in seq(num_corrected_codenames))
  {
    # Determine the corrected codename for a given variable
    corrected_codename_i <- vector_corrected_codenames[i]
    print(corrected_codename_i)
    
    # Determine the row indices that pertaining to this codename in the documentation dataset
    index_corrected_codename_i <- which(df_doc_cleaning[,colname_corrected_codename] == corrected_codename_i)
    # print(index_corrected_codename_i)
    
    # Extract the rows of the documentation dataset that only pertaining to this given codename
    subset_document_clean <- df_doc_cleaning %>%
      .[index_corrected_codename_i,]
    # print(subset_document_clean)
    
    # Determine the unharmonized codenames that pertained to the corrected codename
    old_codenames <- subset_document_clean[,colname_old_codename] %>%
      unique(.) %>%
      unlist(., use.names = FALSE)
    # print(old_codenames)
    
    # Determine the number of unharmonized codename for this one variable 
    num_old_codenames <- length(old_codenames)
    # print(num_old_codenames)
    
    for(j in seq(num_old_codenames))
    {
      # Determine one of the unharmonized codename
      old_codename_j <- old_codenames[j]
      # print(old_codename_j)
      
      # Determine the row indices that pertaining to this unharmonized codename in the documentation dataset
      index_old_codename_j <- which(subset_document_clean[,colname_old_codename] == old_codename_j)
      # print(index_old_codename_j)
      
      # Extract the rows of the documentation dataset that only pertaining to this given unharmonized codename
      subset_document_clean_j <- subset_document_clean %>%
        .[index_old_codename_j,]
      # print(subset_document_clean_j)
      
      # Determine the study years that are affect by the unharmonized codename
      survey_years_j <- subset_document_clean_j$SDDSRVYR %>%
        unique(.)
      # print(survey_years_j)

      # Determine the row indices of participants who are in the affected study years
      index_participants <- which(df_unclean[,colname_cycle] %in% survey_years_j)
      # print(index_participants)
      
      # print(str(dataset_clean[index_participants,corrected_codename_i]))
      # Take the values of the participants from the vector of unharmonized codename and store it into the 
      # vector of the harmonized codename
      df_unclean[index_participants, corrected_codename_i] <- df_unclean[index_participants, old_codename_j]
      # print(str(dataset_clean[index_participants,corrected_codename_i]))
      
    }
    
    # View(df_unclean[,c(corrected_codename_i
    #                    , old_codenames
    #                    , colname_cycle)] %>%
    #        unique(.))
  }
  # Define the dataset as cleaned with all the codename harmonized
  df_clean <- df_unclean
  # View(df_clean)
  
  return(df_clean)
}