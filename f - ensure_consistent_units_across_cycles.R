#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###########################  FUNCTION TO ENSURE CONSISTENT UNITS ACROSS THE CYCLES  ###########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function determines which chemicals is affected by changes in units over the study period and 
#          convert the measurements to ensure that units are consistent over times
#
# Inputs: df_doc_cleaning - data frame of the cleaning documentation for the chemicals
#         df_need_harmonizing - dataframe of the compiled dataset
#
# Outputs: list_return - list containing the data frame of the cleaned NHANES dataset and cleaned cleaning
#                        documentation data where there is one name for each chemical biomarker

ensure_consistent_units_across_cycles <- function(df_doc_cleaning
                                                  , df_need_harmonizing)
{
  # Determine the chemical codenames that changes their units over the study years 
  codenames_units_changes <- df_doc_cleaning %>%
    filter(unit_change == 1) %>%
    select(chemical_codename_use) %>%
    unique(.) %>%
    unlist(., use.names = FALSE) #%>%
    # .[1]
  # print(codenames_units_changes)
  
  # Determine number of chemicals affected by unit changes
  num_codenames_units_changes <- length(codenames_units_changes)
  
  # Extract a dataset to contain the cleaning documentation for the affect chemicals
  subset_units_changes <- df_doc_cleaning %>%
    filter(chemical_codename %in% codenames_units_changes)
  # View(subset_units_changes)

  
  for(i in seq(num_codenames_units_changes))
  {
    # Determine one of the affect chemical codenames
    codename_unit_changes_i <- codenames_units_changes[i]
    # print(codename_unit_changes_i)

    # Extract the dataset to contain the cleaning documentation for a given affected chemical
    subset_units_changes_i <- subset_units_changes %>%
      filter(chemical_codename == codename_unit_changes_i)
    # print(subset_units_changes_i)

    # Determine the chemical names pertaining to the affect chemical 
    chemical_names <- subset_units_changes_i %>%
      select("chemical_name") %>%
      unique(.) %>%
      unlist(., use.names = FALSE)
    # print(chemical_names)

    # Keep the first chemical name
    chemical_names_remain <- chemical_names[1]

    # The second chemical name needs to change
    chemical_name_change <- chemical_names[2]
    
    units <- subset_units_changes_i %>%
      select("units") %>%
      unique(.) %>%
      unlist(., use.names = FALSE)
    
    # Keep the first unit
    units_remain <- units[1]
    
    # The second unit need to change
    units_change <- units[2]

    # Determine the study years where the units changes
    cycles_to_change <- subset_units_changes_i %>%
      filter(chemical_name == chemical_name_change) %>%
      select(SDDSRVYR) %>%
      unlist(., use.names = FALSE)
    # print(cycles_to_change)

    # Determine the participants who have measurements within the affect study years
    index_cycles_to_change <- which(df_need_harmonizing$study_year %in% cycles_to_change)
    # print(index_cycles_to_change)

    # Define a conversion factor to the number of pg/mL
    pg_in_a_ng <- 1000

    # Extract the measurements from the affect study years
    measurements_to_change <- df_need_harmonizing[index_cycles_to_change,codename_unit_changes_i]

    # Convert the affect measurements into ng/mL
    df_need_harmonizing[index_cycles_to_change,codename_unit_changes_i] <- pg_in_a_ng*measurements_to_change

    # Determine the indices of the chemical names with units that need to change
    index_chemical_name_to_change <- which(df_doc_cleaning$chemical_name == chemical_name_change)
    # print(index_chemical_name_to_change)
    # Change the chemical name for the affect study year in the cleaning documentation dataset so there is one
    # name per chemical
    
    df_doc_cleaning[index_chemical_name_to_change,"chemical_name"] <- chemical_names_remain
    df_doc_cleaning[index_chemical_name_to_change,"units"] <- units_remain
  }
  
  # View(df_doc_cleaning)

  # Create a list to contained the harmonized NHANES dataset and harmonized cleaning documentation
  list_return <- list("df_clean" = df_need_harmonizing
                      , "df_cleaning_doc" = df_doc_cleaning)
  
  return(list_return)
  
}