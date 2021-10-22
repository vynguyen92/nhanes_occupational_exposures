#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###############  FUNCTION TO COPY THE ATTRIBUTES FROM INDIVIDUAL CYCLE-SPECIFIC NHANES DATASETS ###############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function extracts the attributes from the original individiual datasets and copy these labels   
#          into a merged dataset across the cycles
#
# Inputs: list_nhanes_datasets - list of cycle-specific nhanes dataset with attributes labels
#         merged_dataset - merged dataset across the cycles with no attributes
#
# Outputs: merged_dataset - merged dataframe with all attributes labeled

copy_labels_into_nhanes <- function(list_nhanes_datasets
                                    , merged_dataset)
{
  # Determine number of individual original datasets
  num_dataset_in_list <- length(list_nhanes_datasets)
  
  for(cycle in seq(num_dataset_in_list))
  {
    
    # Extract the dataset with labeled attributes
    dataset_cycle <- list_nhanes_datasets[[cycle]]
    
    # Merge the attributes into the merged dataset
    merged_dataset <- copy_labels(df_new = merged_dataset
                                  , df_origin = dataset_cycle)
  }
  
  # Return the merged dataset labeled with attributes
  return(merged_dataset)
}