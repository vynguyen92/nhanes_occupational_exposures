#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#############################  FUNCTION TO INCLUDE COMMENT INDICATORS FOR PFASs  ##############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function include comment indicators or measurements for PFASs that needed to be summed using  
#          linear and branched isomers 
#
# Inputs: dataset_new - dataframe of the cleaned dataset
#         dataset_old - dataframe of the uncleaned dataset that contains the isomers
#         df_doc_cleaning - dataframe dictating the cleaning documentation for the chemicals
#         motif - string (e.g. "comments" or "chemicals") to indicate which type of variable needs to be combine
#
# Outputs: dataset_clean - dataframe of the comments or chemical biomarkers dataset where information of PFOS and 
#                          PFOA are included

combine_variables_together_for_pfas <- function(dataset_new
                                                , dataset_old
                                                , df_doc_cleaning
                                                , motif)
{
  # Depending on whether the correction is done on comments or chemicals, define which column names for the variable
  # to use and which codename to use for NHANES cycles
  if(motif == "comments")
  {
    codename_to_use <- "comments_codename_use"
    codename_cycle <- "SDDSRVYR"
    
  } else if(motif == "chemicals") {
    codename_to_use <- "chemical_codename_use"
    codename_cycle <- "study_year"
  }
  
  # Define a variable name that is the column indicate which codename is resulting from summing up the linear and 
  # branched isomers
  column_name_combine <- paste(motif
                               , "combine_into"
                               , sep = "_")
  
  # Determine the row indices in the cleaning documentation dataset that are problematic
  index_combine_exist <- which(!is.na(df_doc_cleaning[,column_name_combine]) == TRUE)
  
  # Define a subset of the cleaning documentation dataset pertaining to PFASs isomers that need to be summed up
  subset_doc_cleaning <- df_doc_cleaning[index_combine_exist,]
  # View(subset_doc_cleaning)
  
  # Determine the codename of the comment or chemicals that are affected
  affected_codenames <- subset_doc_cleaning %>%
    select(all_of(column_name_combine)) %>%
    unique(.) %>%
    unlist(., use.names = FALSE)
  # print(affected_codenames)

  # Determine the number of affected comments or chemicals
  num_affected_codenames <- length(affected_codenames)

  for(i in seq(num_affected_codenames))
  {
    # Determine the codename of a given PFAS
    affected_codename_i <- affected_codenames[i]
    # print(affected_codename_i)

    # Determine the row indices of the cleaning documentation dataset pertaining this problematic PFAS
    index_combine_exist <- which(subset_doc_cleaning[,column_name_combine] == affected_codename_i)

    # Define a dataset of the cleaning documentation dataset pertaining this problematic PFAS
    subset_cleaning_i <- subset_doc_cleaning[index_combine_exist,]
    # print(subset_cleaning_i)

    # If the correction is done on the comment codenames of the PFAS, then the comment codenames of the 
    # isomers need to be known 
    if(motif == "comments")
    {
      comments_linear_and_branched <- subset_cleaning_i[,codename_to_use] %>%
        unique(.) %>%
        unlist(., use.names = FALSE)
      # print(comments_linear_and_branched)
    }

    # Determine the chemical codenames of the isomers
    measurements_linear_and_branched <- subset_cleaning_i %>%
      select(chemical_codename) %>%
      unique(.) %>%
      unlist(., use.names = FALSE)
    # print(measurements_linear_and_branched)

    # Determine the affected cycles
    affected_cycles <- subset_cleaning_i %>%
      select(SDDSRVYR) %>%
      unique(.) %>%
      unlist(., use.names = FALSE)
    # print(affected_cycles)
    
    # Determine the number of affected cycles
    num_affected_cycles <- length(affected_cycles)

    for(j in seq(num_affected_cycles))
    {
      # Determine an affected cycle
      affected_cycle_j <- affected_cycles[j]
      # print(affected_cycle_j)

      # Define a subset of the cleaning documentation dataset that pertain to this affected cycle and PFAS
      subset_affected_cycle_j <- subset_cleaning_i %>%
        filter(SDDSRVYR == affected_cycle_j)
      # print(subset_affected_cycle_j)

      # Determine the participants who have measurements in this affected cycle
      index_cycle_affected <- which(dataset_new[,codename_cycle] == affected_cycle_j)
      # print(index_cycle_affected)

      # Determine chemical codename of the isomers that are needed to be summed to calculate the PFAS
      chemical_codenames <- subset_affected_cycle_j %>%
        select(chemical_codename) %>%
        unique(.) %>%
        unlist(., use.names = FALSE)
      # print(chemical_codenames)

      # Sum the isomers to calculate the PFAS
      combined_measurements <- dataset_old[index_cycle_affected,chemical_codenames[1]] +
        dataset_old[index_cycle_affected,chemical_codenames[2]]

      # For a comment, compare the sum of the isomers to a new LOD to determine whether participants' biomarker levels 
      # are below or above the LOD
      if(motif == "comments")
      {
        # Define a new LOD for the PFAS by multiple the original LOD by 2
        new_lod <- subset_affected_cycle_j %>%
          select(LOD) %>%
          unique(.) %>%
          unlist(., use.names = FALSE)*2
        # print(new_lod)
        
        # df_combined <- data.frame(combined_measurements
        #                           , dataset_old[index_cycle_affected,chemical_codenames[1]]
        #                           , dataset_old[index_cycle_affected,chemical_codenames[2]]) %>%
        #   unique(.) %>%
        #   View(.)
        
        # Compare the sum of PFAS isomers to the new LOD to determine whether participants' biomarker levels are below
        # or above the LOD
        new_comments <- ifelse(combined_measurements >= new_lod
                               , 0
                               , 1)
        
        # Assign the comment indicators into the dataset of comments
        dataset_new[index_cycle_affected,affected_codename_i] <- new_comments
        
        # data.frame(new_comments = dataset_new[index_cycle_affected,affected_codename_i]
        #            , combined_measurements
        #            , dataset_old[index_cycle_affected,chemical_codenames[1]]
        #            , dataset_old[index_cycle_affected,chemical_codenames[2]]) %>%
        #   unique(.) %>%
        #   View(.)
        
      # For a chemical, assign the sum of isomers into the dataset of chemical biomarker measurements
      } else if(motif == "chemicals") {
        
        dataset_new[index_cycle_affected,affected_codename_i] <- combined_measurements
        
        # data.frame(new_measurements = dataset_new[index_cycle_affected,affected_codename_i]
        #            , dataset_old[index_cycle_affected,chemical_codenames[1]]
        #            , dataset_old[index_cycle_affected,chemical_codenames[2]]) %>%
        #   unique(.) %>%
        #   View(.)
        
      }


    }

    # data.frame(new = dataset_new[which(dataset_new[,codename_cycle] %in% affected_cycles)
    #                                            , affected_codename_i]
    #            , dataset_old[which(dataset_new[,codename_cycle] %in% affected_cycles)
    #                                   ,measurements_linear_and_branched]) %>%
    #   unique(.) %>%
    #   View(.)

  }

  dataset_clean <- dataset_new

  return(dataset_clean)
}