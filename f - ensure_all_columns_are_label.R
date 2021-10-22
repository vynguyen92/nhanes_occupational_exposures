#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
######################  FUNCTION TO ENSURE ALL COLUMNS ARE LABEL WITH THEIR VARIABLE NAME  ####################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function ensures that all columns have an attribute containing the variable name
#
# Inputs: dataset_merged - dataframe of the compiled dataset
#         dataset_doc_cleaning - dataframe dictating the cleaning documentation of the dataset of interest
#
# Outputs: dataset_merged - dataframe with all column labeled

ensure_all_columns_are_label <- function(dataset_merged
                                         , dataset_doc_cleaning)
{
  library("sjlabelled")
  
  # Determine codenames that do not have a label
  codenames_without_labels <- get_label(dataset_merged) %>% 
    .[. == ""] %>% 
    names(.) %>%
    unlist(., use.names = FALSE)
  # print(codenames_without_labels)
  
  # Determine the number of codenames that do not have a label
  num_codenames_without_labels <- length(codenames_without_labels)
  # print(num_codenames_without_labels)
  
  if(num_codenames_without_labels == 0)
  {
    
  } else {
    
    # For each codename without a label, assign a label (i.e. description of the variable) to the codename
    for(k in seq(num_codenames_without_labels))
    {
      # Select a codename without a label
      codename_k <- codenames_without_labels[k]
      # print(codename_k)
      
      # Define a subset with information on that variable
      subset_doc_cleaning_k <- dataset_doc_cleaning %>%
        filter(variable_codename_use == codename_k)
      
      # Determine the description of the variable
      variable_name_k <- subset_doc_cleaning_k %>%
        select(variable_description) %>%
        unique(.) %>%
        unlist(., use.names = FALSE)
      # print(variable_name_k)
      
      # Assign the description to that codename 
      attr(dataset_merged[,codename_k], "label") <- variable_name_k
    }
  }
  
  return(dataset_merged)
}