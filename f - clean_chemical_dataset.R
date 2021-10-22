#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##################################  FUNCTION TO CLEAN THE CHEMICAL DATASET  ###################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function harmonizes clean the chemical dataset by harmonizing the chemical codenames, ensuring
#          units are consistent across the study period, excluding measurements in study year showing drastic 
#          changes in the LOD, and ensuring the label are correctly assigned with the chemical name
#
# Inputs: dataset_unclean - dataframe of the compiled dataset
#         list_document_cleaning - list of datasets dictating the cleaning documentation
#         name_dataset - string of the name of the dataset to clean
#
# Outputs: dataset_cleaned - dataframe of the cleaned chemical dataset

clean_chemical_dataset <- function(dataset_unclean
                                   , list_document_cleaning
                                   , name_dataset = "Chemicals")
{
  # Extract the documentation dataset for cleaning the chemical dataset
  dataset_document_cleaning <- list_document_cleaning[[name_dataset]]
  
  # Determine the codenames that needs to be harmonized
  corrected_codenames <- dataset_document_cleaning %>%
    filter(is.na(corrected_chemical_codename) == FALSE) %>%
    select("corrected_chemical_codename") %>%
    unique(.) %>%
    unlist(., use.names = FALSE) #%>%
  # .[1]
  
  # Used for debugging. Manually assigned to a codename that needs harmonizing
  # corrected_codenames <- "VNURXPFUA"
  # print(corrected_codenames)
  
  print("Ensure a chemical has only one codename")
  
  # For chemicals with multiple chemical codenames, harmonize the codenames
  dataset_clean <- resolve_multiple_codenames(corrected_codenames
                                              , "corrected_chemical_codename"
                                              , "chemical_codename"
                                              , dataset_document_cleaning
                                              , dataset_unclean
                                              , "study_year")

  # Extract the harmonized chemical codenames
  chem_codenames <- dataset_document_cleaning %>%
    filter(is.na(codenames_for_sum) == TRUE) %>%
    select("chemical_codename_use") %>%
    unique(.) %>%
    unlist(., use.names = FALSE)
  # print(chem_codenames)
  
  # print(setdiff(chem_codenames, colnames(dataset_clean)))
  
  # Form a cleaner chemical dataset with the participant identifier, the harmonized chemical
  # codenames, and the study years
  dataset_clean <- dataset_clean[,c("SEQN"
                                    , chem_codenames
                                    , "study_year")]

  print("Ensure units are consistent across cycles")
  
  # Ensure units are consistent across the cycles
  list_clean <- ensure_consistent_units_across_cycles(dataset_document_cleaning
                                                         , dataset_clean)
  
  dataset_clean <- list_clean[["df_clean"]]

  dataset_document_cleaning <- list_clean[["df_cleaning_doc"]]
  
  print("Sum metabolites of parent compounds")

  dataset_clean <- include_summation_of_chemicals(dataset_clean
                                                   , dataset_document_cleaning
                                                   , "chemicals")
 
  
  dataset_clean <- combine_variables_together_for_pfas(dataset_clean
                                                       , dataset_unclean
                                                       , dataset_document_cleaning
                                                       , "chemicals")
  

  print("Ensure correct chemical names")
  
  dataset_clean <- assign_names_as_labels(dataset_document_cleaning
                                          , dataset_clean
                                          , "chemical_codename_use"
                                          , "chemical_name")

  attr(dataset_clean$study_year, "label") <- "Data release cycle"

  print("Exclude measurements due to drastic changes in LOD")

  dataset_clean <- exclude_measurements_based_changes_in_lod(dataset_clean
                                                             , dataset_document_cleaning
                                                             , "chemical_codename_use"
                                                             , "study_year")
  
  
  # Ensure that the study year is numeric as a data type
  dataset_clean$SDDSRVYR <- as.numeric(dataset_clean$study_year)
  
  # Assign the name of the codename for the study year
  attr(dataset_clean$SDDSRVYR, "label") <- "Data release cycle"
  
  # Ensure that the participant ID is an integer
  dataset_clean$SEQN <- as.integer(dataset_clean$SEQN)
  
  # Assign the name of the codename for the participant ID
  attr(dataset_clean$SEQN, "label") <- "Respondent sequence number"

  return(dataset_clean)

}