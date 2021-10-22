#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  FUNCTION - COMPILE THE NHANES CHEMICALS DATASET ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function extracts the individual chemical dataset for each cycle and merge into one combined 
#          chemicals dataset
#
# Inputs: dataset_directory - the working directory of the folder that contains the folders for each cycle. Each 
#                          cycle-specific folder contains the file names for each the chemical dataset
#         current_directory - the working directory of the folder where the function and main scripts of the 
#                             project is housed.
#
# Outputs: returns a dataframe with the merged chemicals dataset 

compile_chemicals_dataset <- function(dataset_directory
                                      , current_directory)
{
  # Establish the working directory to be the folder that contains the folders of the cycle-specific chemical
  # datasets
  setwd(dataset_directory)
  
  # Obtain a vector of folder names, one for each cycle
  nhanes_dataset_by_cycle <- list.files()
  # Determine the number of cycles 
  num_cycles <- length(nhanes_dataset_by_cycle)
  
  # Initialize a list to store a dataframe of biomarker measurements for each cycle
  all_nhanes_datasets_by_cycle <- list()
  
  # For each cycle in NHANES, go into a cycle-specific folder and extract the corresponding files to form a 
  # merged chemicals datasest for an ith cycle
  for(i in seq(num_cycles))
  {
    # Define the directory of the ith cycle-specific folder
    cycle_specific_nhanes_dataset_directory <- paste(dataset_directory
                                                     , nhanes_dataset_by_cycle[i]
                                                     , sep = "/")
    print(cycle_specific_nhanes_dataset_directory)
    # Establish the working directory for the ith cycle-specific folder
    setwd(cycle_specific_nhanes_dataset_directory)
    
    # Determine a vector of file names in the ith cycle-specific folder
    files_names.xpt <- list.files()
    # Determine the number of files in the ith cycle-specific folder
    num_files_in_cycle_specific_folder <- length(files_names.xpt)
    
    # Determine the name of the first file in the folder
    first_file <- files_names.xpt[1]
    # Replace the ".XPT" in the file name with ""
    first_file <- gsub(".XPT"
                       , ""
                       , first_file)
    
    # Use the updated file name to extract the appropriate dataset from the nhanesA package
    # Store the first chemical dataset for the ith cycle into cycle_specific_datasest, so that subsequent
    # chemical datasets can be merged with the first one
    cycle_specific_dataset <- nhanes(first_file)
    
    # For the 2nd file and beyond (jth) in the cycle-specific folder, the corresponding chemical dataset will 
    # be merged with the first chemical dataset by SEQN to form the cycle-specific dataset of chemical 
    # measurements 
    for(j in 2:num_files_in_cycle_specific_folder)
    {
      # Determine the name of the jth file in the folder and replace the ".XPT" in the file name with ""
      file_name_j <- gsub(".XPT"
                          , ""
                          , files_names.xpt[j])
      # Message to know which chemical dataset is being extracted
      print(file_name_j)
      
      # Store jth chemical dataset into temp_file
      temp_file <- nhanes(file_name_j)
      
      # Merge the jth chemical dataset with the previous chemicals datset by SEQN
      cycle_specific_dataset <- merge(cycle_specific_dataset
                                      , temp_file
                                      , all = TRUE
                                      , by = "SEQN")
    }
    
    # Determine the number of participants in the ith cycle
    num_participants_cycle_i <- dim(cycle_specific_dataset)[1]
    # Define the study year that each participant belongs to
    study_year <- rep(i, num_participants_cycle_i)
    
    # Call a function to resolve the duplicates
    cycle_specific_dataset <- resolve_duplicates(cycle_specific_dataset)
    
    # Append the study_year for this ith cycle with the ith cycle chemical dataset
    all_nhanes_datasets_by_cycle[[i]] <- data.frame(cycle_specific_dataset
                                                    , study_year) %>%
      unlabel(.)
  }
  
  # View(all_nhanes_datasets_by_cycle[[10]])
  
  # Rename the list containing all the cycle-specific chemicals datasets
  list_merged_nhanes_datasets <- all_nhanes_datasets_by_cycle
  
  # Determine the number of cycles or technically, determine the number of dataframes stored in this list
  num_elements_in_list <- length(lengths(list_merged_nhanes_datasets))
  
  # There will be an error in the merge if the data type of the column variables are different
  list_merged_nhanes_datasets[[2]]$URXUIO <- as.numeric(list_merged_nhanes_datasets[[2]]$URXUIO)
  list_merged_nhanes_datasets[[2]]$URXP02 <- as.numeric(list_merged_nhanes_datasets[[2]]$URXP02)
  list_merged_nhanes_datasets[[5]]$URXCRS <- as.numeric(list_merged_nhanes_datasets[[5]]$URXCRS)
  list_merged_nhanes_datasets[[6]]$URXP01 <- as.numeric(list_merged_nhanes_datasets[[6]]$URXP01)
  list_merged_nhanes_datasets[[6]]$URXP04 <- as.numeric(list_merged_nhanes_datasets[[6]]$URXP04)
  list_merged_nhanes_datasets[[7]]$URXP04 <- as.numeric(list_merged_nhanes_datasets[[7]]$URXP04)
  list_merged_nhanes_datasets[[7]]$URXNO3 <- as.numeric(list_merged_nhanes_datasets[[7]]$URXNO3)
  list_merged_nhanes_datasets[[8]]$URXNO3 <- as.numeric(list_merged_nhanes_datasets[[8]]$URXNO3)
  
  # Extract the first cycle chemicals dataset to be store into merged_nhanes_datasets so that subsequent
  # kth cycles can be merged together
  merged_nhanes_datasets <- list_merged_nhanes_datasets[[1]]

  # For subsequent cycles, the kth cycle chemical dataset will be merged with the previous merged chemical
  # dataset
  for(k in 2:num_elements_in_list)
  {
   
    # Perform the merge by the codenames
    merged_nhanes_datasets <- full_join(merged_nhanes_datasets
                                        , list_merged_nhanes_datasets[[k]]
                                        , by = NULL)
    
    # Message to relay which cycle has been merged in
    print(paste("Merge in Cycle ", k, sep = ""))
  }
  
  setwd(current_directory)
  
  # Return the merged chemical biomarker dataset
  return(merged_nhanes_datasets)
  # return(list_merged_nhanes_datasets)
  
}