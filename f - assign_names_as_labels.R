#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
###############  FUNCTION TO ASSIGN THE NAMES AS LABELS FOR ALL VARIABLES IN A NHANES DATASET  ################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function assigns the names of the variable as a label in the NHANES dataset
#
# Inputs: df_doc_cleaning - data frame of the cleaning documentation for the chemicals
#         df_need_labels - data frame of the NHANES dataset 
#         colname_variable_codenames - string of the column name from the cleaning documentaiton that lists
#                                      the codenames
#         colname_variable_name - string of the column name from the cleaning documentaiton that lists the
#                                 name of the codenames
#
# Outputs: df_labeled - data frame of the NHANES dataset with all column labelled with the name of the variable

assign_names_as_labels <- function(df_doc_cleaning
                                   , df_need_labels
                                   , colname_variable_codenames
                                   , colname_variable_name)
{
  # Extract all the codenames
  codenames_variables <- df_doc_cleaning[,colname_variable_codenames] %>%
    unique(.) %>%
    unlist(., use.names = FALSE) #%>%
    # .[1]
  # print(codenames_variables)
  
  # Determine number of codenames
  num_chem_codename_to_use <- length(codenames_variables)

  for(k in seq(num_chem_codename_to_use))
  {
    # Extract a given codename
    codename_k <- codenames_variables[k]
    print(codename_k)

    # Determine the row indices of the the cleaning documentation dataset that pertains to a given codename 
    
    index_of_codename_k <- which(df_doc_cleaning[,colname_variable_codenames] == codename_k)
    # print(index_of_codename_k)
    
    # Determine the name of the variable
    variable_name_from_doc_cleaning <- df_doc_cleaning[index_of_codename_k
                                                                 , colname_variable_name] %>%
      unique(.) %>%
      unlist(., use.names = FALSE)
    # print(variable_name_from_doc_cleaning)
   
    # Print a message if the the codename has more than one name
    if(length(variable_name_from_doc_cleaning) > 1)
    {
      print("There are more than two names")
      # print(variable_name_from_doc_cleaning)

      # Determine whether the variable names has an &
      num_with_ampersand <- variable_name_from_doc_cleaning %>%
        grepl("\\&", .) %>%
        unique(.) %>%
        sum(.)
      # print(num_with_ampersand)

      # If the variable has an &, then use the name containing the &
      if(num_with_ampersand  == 1)
      {
        variable_name_from_doc_cleaning <- variable_name_from_doc_cleaning %>%
          .[grepl("\\&", .)]

      } else {
        # For some chemicals with two names due to having different unit names even though the units are the same,
        # use the most recent name of the chemical
        variable_name_from_doc_cleaning <- variable_name_from_doc_cleaning[2]
      }
      # print(variable_name_from_doc_cleaning)

    }
    # print(variable_name_from_doc_cleaning)
    # Assign the label of the variable in the NHANES dataset as the name of the variable
    # print(str(df_need_labels[,codename_k]))
    attr(df_need_labels[,codename_k], "label") <- variable_name_from_doc_cleaning
  }

  # # Define the NHANES dataset as labelled 
  df_labeled <- df_need_labels
  # # View(df_labeled)
  
  return(df_labeled)
}