resolve_duplicates <- function(dataset_specific_cycle)
{
  # Determine the indices of duplicates for the ith cycle
  index_dupl_codenames <- grep(".(x|y)", colnames(dataset_specific_cycle))
  # Determine the codename of the duplicates for the ith cycle
  dupl_codenames <- colnames(dataset_specific_cycle)[index_dupl_codenames]
  # Determine a vector of unique codenames that were duplicated
  problematic_codenames <- unique(gsub(".(x|y)", "", dupl_codenames))
  
  # For each biomarker that were duplicated, all measurements will be stored into the first duplicate
  # to retain the maximum number of measurements pertaining to this biomarker
  for(p in seq(length(problematic_codenames)))
  {
    # Start the pth duplicated biomarker
    probl_codename <- problematic_codenames[p]
    
    # Establish a pattern to find all duplicates pertaining to this pth biomarker
    probl_code_pattern <- paste("\\b"
                                , probl_codename
                                , "\\b"
                                , sep = "")
    # use the pattern to determine the indices of duplicate pertaining to this pth biomarker
    index_probl_code_dupl <- grep(probl_code_pattern
                                  , colnames(dataset_specific_cycle))
    
    # Determine the number of duplicates for this pth biomarker
    num_probl_dupl <- length(index_probl_code_dupl)
    
    # Determine the first index of all the duplicates for this pth biomarker
    index_first_probl_code_dupl <- index_probl_code_dupl[1]
    
    # For any subsequent duplicate (kth), extract all measurements and store it to the corresponding 
    # participant in the first duplicated vector 
    for(k in 2:num_probl_dupl)
    {
      # Determine the index pertaining the kth duplicated column vector for the pth biomarker
      dupl_index_k <- index_probl_code_dupl[k]
      
      # Store the column vector for this kth duplicate into a more readable variable name
      measurements_dupl_k <- dataset_specific_cycle[,dupl_index_k]
      
      # Determine the indices of participants who have measurements for this pth biomarker in the 
      # kth duplicated column vector
      index_measurements_for_dupl_k <- which(!is.na(measurements_dupl_k))
      
      # Extract the measurements and store it into the column vector pertaining to the first duplicate
      dataset_specific_cycle[index_measurements_for_dupl_k,index_first_probl_code_dupl] <- 
        measurements_dupl_k[index_measurements_for_dupl_k]
      
    }
    
    # change the codename of the column vector that now contains all measurements for the pth biomarker
    # to be without any extraneous symbol (i.e. ".x" or ".y")
    colnames(dataset_specific_cycle)[index_first_probl_code_dupl] <- probl_codename
    
    # The indices pertaining to the 2nd duplicates and beyond for the pth biomarker needs to be 
    # removed from the chemical dataset for the ith cycle
    index_remove_dupl <- index_probl_code_dupl[2:num_probl_dupl]
    # Remove the column vectors pertaining to the duplicates for the pth biomarker from the 
    # ith cycle chemical dataset
    dataset_specific_cycle <- dataset_specific_cycle[,-index_remove_dupl]
    
  }
  return(dataset_specific_cycle)
}