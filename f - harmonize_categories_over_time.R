#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#################################  FUNCTION TO HARMONIZE THE CATEGORIES OVER TIME  ############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function is to include a column with the harmonized codenames for ease of extraction.
#
# Inputs: name_fix_categories_of_df - string of the cleaning documentation dataset on fixing the categories
#         list_cleaning_documentation - list of the datasets on cleaning documentation
#         dataset_unclean - dataframe of the NHANES dataset with the codename harmonized (i.e. there is one
#                           codename per one variable)
#
# Outputs: dataset_clean - dataframe of the cleaning documentation with a new column of harmonized codename

harmonize_categories_over_time <- function(name_fix_categories_of_df
                                           , list_cleaning_documentation
                                           , dataset_unclean)
{
  
  # Extract the dataset on cleaning documentation for harmonizing categories over time
  dataset_fix_categories <- list_cleaning_documentation[[name_fix_categories_of_df]]
  # print(dataset_fix_categories)
  
  # Determine the codenames that need harmonizing
  new_codenames_fix <- unique(dataset_fix_categories$new_codename)#[1]
  # print(new_codenames_fix)

  # Determine the number of codenames that need harmonizing
  num_new_codenames_fix <- length(new_codenames_fix)

  for(f in seq(num_new_codenames_fix))
  {
    # Determine a given codename that requires harmonizing of its categories
    new_codename_fix_f <- new_codenames_fix[f]
    # print(new_codename_fix_f)
    
    # Extract information on the number and description of the categories of this categorical variable
    subset_category_fix <- dataset_fix_categories %>%
      filter(new_codename == new_codename_fix_f)
    # print(subset_category_fix)
    
    # Format the extraction data as long with a column for the old category numbers, the new category numbers, 
    # the new codename, and the cycle 
    long_subset_category_fix <- subset_category_fix %>%
      gather(., year, value, grep("cycle", colnames(.))) %>%
      na.omit(.) %>%
      mutate(SDDSRVYR = as.numeric(gsub("cycle_"
                         , ""
                         , year))) %>%
      select(categories_num, new_categories, new_codename, SDDSRVYR) %>%
      unique(.)
    # print(long_subset_category_fix)

    # Determine the column index with the name "categories_num"
    col_index_categories_num <- which(colnames(long_subset_category_fix) == "categories_num")

    # Change that column name to the new codename
    colnames(long_subset_category_fix)[col_index_categories_num] <- new_codename_fix_f

    # Remove the column of the new codenames that will contain the harmonized categories as it is 
    # used as the column name of the old categories
    long_subset_category_fix <- long_subset_category_fix %>%
      select(-new_codename)
    # print(long_subset_category_fix)

    # Merge the dataset of old and new catgories by the new codename and the study year to harmonize the categories. 
    # This is based on the assumption that variables with multiple codenames have been harmonized
    dataset_unclean <- dataset_unclean %>%
      left_join(.
                , long_subset_category_fix
                , by = c(new_codename_fix_f, "SDDSRVYR")) %>%
      # Drop the column with the new codename or else there will be duplicates and problems with extraction
      select(-all_of(new_codename_fix_f))
    # View(dataset_unclean[,c(new_codename_fix_f, "new_categories", "SDDSRVYR")] %>%
    #        unique(.))

    # Determine the column index labeled as "new_categories"
    col_index_new_categories <- which(colnames(dataset_unclean) == "new_categories")

    # Now replace the column name of "new_categories" with the harmonized codename
    colnames(dataset_unclean)[col_index_new_categories] <- new_codename_fix_f

    # Extract the original codenames. Use for debugging and checking.
    original_codenames <- subset_category_fix %>%
      select(codename_original) %>%
      unique(.) %>%
      unlist(., use.names = FALSE)
    
    # # Extract the dataset with the original codenames, harmonized codename, and cycle years to check the merging
    # View(dataset_unclean[,c(original_codenames,"SDDSRVYR")] %>%
    #        unique(.) )
  }

  # Define the dataset as clean
  dataset_clean <- dataset_unclean
  
  return(dataset_clean)
}