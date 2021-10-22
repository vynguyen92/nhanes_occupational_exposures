#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
############################  FUNCTION TO COMPILE NHANES DATASET ACROSS FILES  ################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function extracts the dataset for each cycle and merged all cycle-specific datasets into one
#          merged demographic dataset. There should be a single file for a given cycle. 
#
# Inputs: dataset_directory - the working directory of the folder that contains the files of cycle-specific
#                          demographics dataset
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project is housed.
#         file_name_first_cycle - filename of the dataset measured in the first cycle
#
# Outputs: merged_nhanes_dataset - dataframe with the merged dataset with attributed labelled

compile_dataset_across_files <- function(dataset_directory
                                         , current_directory
                                         , file_name_first_cycle)
{
  # Establish the working directory to be the folder that contains the files of the cycle-specific demographics
  # datasets
  setwd(dataset_directory)
  
  # Determine the name of the first file in this working directory
  first_file_name.xpt <- file_name_first_cycle
  # Replace the ".XPT" in the file name with ""
  first_file_name <- gsub(".XPT"
                          , ""
                          , first_file_name.xpt)
  
  # Use the updated file name to extract the appropriate dataset from the nhanesA package
  # Store the demographics dataset for the first cycle into merged_nhanes_dataset, so that subsequent
  # demographics datasets can be merged with the first one
  merged_nhanes_dataset <- nhanes(first_file_name) %>%
    mutate(SDDSRVYR = rep(1, nrow(.)))
  
  # Remove labels from the demographics dataset of the 1st cycle
  merged_nhanes_dataset_labs_removed <- remove_all_labels(merged_nhanes_dataset)
  # print(str(merged_nhanes_dataset))
  
  # Determine the names of the files within this working directory
  nhanes_filenames <- list.files() 
  
  # Determine the indices that does not pertain the the demographics dataset for the first cycle
  index_other_cycles <- which(nhanes_filenames != first_file_name.xpt)
  
  # Remove the file name of the demographics dataset of the first cycle
  nhanes_filenames <- nhanes_filenames[index_other_cycles]
  
  # Determine the number of remaining files 
  num_cycles <- length(nhanes_filenames)
  
  # Initialize an empty list to contain the individual labeled datasets
  list_nhanes_cycle_datasets_labels <- list()
  
  # Store the first demographics dataset into the list
  list_nhanes_cycle_datasets_labels[[1]] <- merged_nhanes_dataset
  
  
  # For the subsequent demographics datasets, the file name will be used to extract the ith cycle demographics
  # dataset and merge them with the demographics datasets from the previous cycles to form a merged demographics
  # dataset
  for(i in seq(num_cycles))
  {
    # Extract the file name for the demographics dataest in the ith cycle
    file_i_name.xpt <- nhanes_filenames[i]

    # Replace the ".XPT" in the file name with ""
    file_i_name <- gsub(".XPT"
                        , ""
                        , file_i_name.xpt)

    # Extract the demographics dataset for the ith cycle
    cycle_i_dataset <- nhanes(file_i_name) %>%
      mutate(SDDSRVYR = rep((i + 1), nrow(.)))

    # Store the demographic dataset pertaining to the ith cycle in the empty list
    list_nhanes_cycle_datasets_labels[[i + 1]] <- cycle_i_dataset
    
    # Remove attributes from cycle-specific dataset to make the merging easier
    cycle_i_dataset_labs_removed <- remove_all_labels(cycle_i_dataset)

    # Merge the demographics dataset for the ith cycle with the previous demographic dataset
    merged_nhanes_dataset_labs_removed <- full_join(merged_nhanes_dataset_labs_removed
                                                    , cycle_i_dataset_labs_removed
                                                    , by = NULL)

    # Message to help know which cycle has been merged in
    print(paste("Merge in Cycle "
                , i + 1
                , sep = ""))

  }
  
  # Reinstate the attributes into the merged dataset of demographics
  merged_nhanes_dataset_final <- copy_labels_into_nhanes(list_nhanes_datasets = list_nhanes_cycle_datasets_labels
                                                         , merged_dataset = merged_nhanes_dataset_labs_removed)
  
  # Set the working directory back
  setwd(current_directory)
  
  # Return the merged demographics dataset
  return(merged_nhanes_dataset_final)
  
}