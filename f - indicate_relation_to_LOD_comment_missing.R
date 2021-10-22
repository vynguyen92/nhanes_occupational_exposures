#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###############  FUNCTION TO INDICATE RELATION TO THE LOD WHEN THE COMMENT CODENAME IS MISSING  ###############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function indicates whether measurements are above or below the LOD when the comment codename 
#          is missing but the LOD is available
#
# Inputs: df_after_harmonization - dataframe of the comment dataset with the codenames harmonized
#         df_original - dataframe of original unclean chemical dataset with the chemical measurements and comments
#                       that aren't harmonized
#         df_doc_cleaning - dataframe of the dataset containing the cleaning documentation
#
# Outputs: df_clean - dataframe of the dataset where rows are the participants and columns are the harmonized
#                     codenames. Participants with measurements are indicated as above or below the LOD.

indicate_relation_to_LOD_comment_missing <- function(df_after_harmonization
                                                     , df_original
                                                     , df_doc_cleaning)
{
  
  # Extract the rows where comment codename exists
  df_doc_cleaning_chem_with_comments <- df_doc_cleaning %>%
    filter(!is.na(comment_codename) == TRUE)
  # View(df_doc_cleaning_chem_with_comments)
  
  # View(df_doc_cleaning_chem_with_comments %>%
  #        select("chemical_codename"
  #               , "cas_num"
  #               , "corrected_chemical_codename"
  #               , "SDDSRVYR"
  #               , "LOD"))
  
  # Extract the rows where the comment codename is missing and the LOD exists
  print( df_doc_cleaning %>%
          filter( !is.na(LOD) == TRUE 
                 & is.na(comments_codename_use) == TRUE) %>%
          pull(chemical_codename))
  
  
  df_doc_cleaning_missing_comment <- df_doc_cleaning %>%
    filter(grepl("Missing comment codename|Insufficient comment indicators|Calculated"
                 , LOD_notes) == TRUE 
           & !is.na(LOD) == TRUE 
           & !is.na(comments_codename_use) == TRUE)
  # View(df_doc_cleaning_missing_comment)

  # Determine the comment codename  of chemical that have missing comments
  chem_missing_comment <- unique(df_doc_cleaning_missing_comment$chemical_codename)
  # print(chem_missing_comment)

  # # Used for debugging. Set to one comment codename of interest
  # chem_missing_comment <- "LBXBPB"

  # Determine the number of chemicals with missing comments
  num_chem_missing_comment <- length(chem_missing_comment)

  
  for(j in seq(num_chem_missing_comment))
  {
    # Extract the omment codename that has a missing comment codename
    chem_missing_j <- chem_missing_comment[j]
    # print(chem_missing_j)

    # Extract the rows pertaining to the chemical with the missing comment codename
    subset_doc_miss_cleaning <- df_doc_cleaning_missing_comment %>%
      filter(chemical_codename == chem_missing_j)
    
    # print(subset_doc_miss_cleaning %>% 
    #         select(chemical_codename
    #                , cas_num
    #                , corrected_chemical_codename
    #                , SDDSRVYR
    #                , LOD))

    # Extract the missing comment codename
    comment_codename_j <- df_doc_cleaning_chem_with_comments %>%
      filter(chemical_codename == chem_missing_j) %>%
      select("comments_codename_use") %>%
      unique(.) %>%
      unlist(., use.names = FALSE)
    # print(comment_codename_j)

    if(is_empty(comment_codename_j) == FALSE)
    {
      # Determine the number of cycles that needs to be fixed
      num_cycles_to_fix <- nrow(subset_doc_miss_cleaning)
      
      
      for(k in seq(num_cycles_to_fix))
      {
        # Extract the row pertaining to a cycle that needs to include the indicators
        subset_doc_miss_cleaning_k <- subset_doc_miss_cleaning[k,]
        # print(subset_doc_miss_cleaning_k)

        # Determine the chemical codename pertaining to this comment codename
        chemical_codename_k <- subset_doc_miss_cleaning_k %>%
          select("chemical_codename") %>%
          unlist(., use.names = FALSE)
        # print(chemical_codename_k)

        # Determine the affected study year
        cycle_num_k <- subset_doc_miss_cleaning_k %>%
          select("SDDSRVYR") %>%
          unlist(., use.names = FALSE)
        # print(cycle_num_k)

        # Determine the row indices from the comment dataset after harmonizing the codenames
        index_cycle_k_df_final <- which(df_after_harmonization$SDDSRVYR == cycle_num_k)
        
        # Determine the row indices from the original comment dataset without harmonizing the codenames
        index_cycle_k_unclean <- which(df_original$study_year == cycle_num_k)

        # Determine the LOD for the affect study year
        lod_k <- subset_doc_miss_cleaning_k %>%
          select(LOD) %>%
          unlist(., use.names = FALSE)
        
        # Determine whether the measurements was below (1) or above (0) the LOD for the affect study year
        df_after_harmonization[index_cycle_k_df_final, comment_codename_j] <- if_else(df_original[index_cycle_k_unclean
                                                                                          , chemical_codename_k] < lod_k
                                                                             , 1
                                                                             , 0)

        # # Check if the indicators are correctly assigned by comparing the measurements and the indicators
        # View(cbind(df_after_harmonization[index_cycle_k_df_final,c("SDDSRVYR", comment_codename_j)]
        #             , df_original[index_cycle_k_unclean, c("study_year",chemical_codename_k)]
        #             ) %>%
        #         unique(.) %>%
        #         arrange(eval(parse(text = chemical_codename_k))) 
        #       )

      }

    # No extra processing if the comment codename is missing, because these are the dietary factors, 
    # which will be excluded
    } else {

    }
  }
  
  # Define the dataset as clean with indicators dictating whether measurements are above or below the LOD
  df_clean <- df_after_harmonization
  
  # data.frame(df_after_harmonization$LBDNFOSL, df_original$SSNPFOS, df_original$study_year) %>% unique(.) %>% View(.)
  
  return(df_clean)
}