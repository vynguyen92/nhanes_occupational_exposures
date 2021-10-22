#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
####################  FUNCTION TO EXCLUDE MEASUREMENTS BASED ON DRASTIC CHANGES IN THE LOD  ###################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function harmonizes excludes the measurements from study year for chemicals that show drastic
#          changes in the LOD. We do not want our analyses to be affect by technological advances. 
#
# Inputs: dataset_unclean - dataframe of the unclean dataset
#         dataset_doc_cleaning - dataframe of the dataset containing the cleaning documentation
#         codename_use - string of the column name pertaining to the harmonized chemical or comment codenames
#         codename_cycle - string of the column name pertaining to the study years
#
# Outputs: dataset_cleaned - dataframe of the dataset with measurements in affected study year are defined as 
#                            missing.

exclude_measurements_based_changes_in_lod <- function(dataset_unclean
                                                      , dataset_doc_cleaning
                                                      , codename_use
                                                      , codename_cycle)
{
  # Define a subset to indicate which study years for which chemicals should be excluded based on 
  # drastic changes in the LOD.
  subset_to_exclude <- dataset_doc_cleaning %>%
    filter(remove_cycle_changes_in_LOD == 1)
  # View(subset_to_exclude)
  
  # Determine the codename of chemicals in which some of study years need to be excluded
  chemicals_with_exclusion <- subset_to_exclude[,codename_use] %>%
    unique(.) %>%
    unlist(., use.names = FALSE) %>%
    .[!is.na(.)] #%>%
    # .[1]
  # print(chemicals_with_exclusion)
  
  # Determine number of chemicals that have study years that need to be excluded
  num_chemicals_with_exclusion <- length(chemicals_with_exclusion)

  for(i in seq(num_chemicals_with_exclusion))
  {
    # Determine the codename of a chemical with the needed exclusion
    chemical_exclude_i <- chemicals_with_exclusion[i]
    # print(chemical_exclude_i)

    # Determine the row indices pertaining to the codename 
    index_chemical_exclude <- which(subset_to_exclude[,codename_use] == chemical_exclude_i)

    # Extract the row pertaining to the codename
    subset_doc_cleaning_i <- subset_to_exclude[index_chemical_exclude,]
    # print(subset_doc_cleaning_i)

    # Determine which study years should be excluded 
    cycles_to_exclude <- subset_doc_cleaning_i %>%
      select(SDDSRVYR) %>%
      unlist(., use.names = FALSE)
    # print(cycles_to_exclude)

    # Determine the row indices in the nhanes dataset for participants who have measurements in 
    # these affected study years
    index_cycles_to_exclude <- which(dataset_unclean[,codename_cycle] %in% cycles_to_exclude)
    # print(index_cycles_to_exclude)
    
    # For participants with measurements in the affected years, their measurements will be set to NA as 
    # to exclude these measurements from the analysis
    dataset_unclean[index_cycles_to_exclude, chemical_exclude_i] <- rep(NA
                                                                        , length(index_cycles_to_exclude))
    
    # # Check if the measurements are now NA after the exclusions
    # View(dataset_unclean[,c(chemical_exclude_i, codename_cycle)] %>%
    #        unique(.))
  }

  # Define the dataset as cleaned after measurements in the affected study years are excluded
  dataset_cleaned <- dataset_unclean
  
  return(dataset_cleaned)
}