#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#######################  FUNCTION TO FORM A DATA FRAME OF CONSTRUCTED SURVEY WEIGHTS  #########################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Purpose: This function builds a data frame of constructed survey weights for all chemical codenames. For each
#          chemical codename, the weights from different survey weight codenames are extracted for all given 
#          cycles and formed into one column vector. This column vector is named  as the concatenation of the
#          "WT_" and the chemical codename for ease of extraction.
#
# Inputs: unclean_chemical_dataset - dataframe of the unclean chemical dataset as there codename for the survey
#                                    weights
#         clean_demo_dataset - dataframe of the cleaned demographics dataset
#         list_document_cleaning - list of datasets dictating the cleaning documentation
#
# Outputs: df_survey_weights - dataframe of the constructed survey weights where the rows are the participants 
#                              and the columns are the codename for the constructed survey weights 

form_survey_weights_dataset <- function(unclean_chemical_dataset
                                        , clean_demo_dataset
                                        , list_document_cleaning)
{
  # Extract the cleaning documentation for the chemical dataset
  df_chem_doc_cleaning <- list_document_cleaning[["Chemicals"]]
  
  # Extract the cleaning documentation for the survey weights dataset
  df_chem_weights_doc_cleaning <- list_document_cleaning[["Weights"]]
  # View(df_chem_weights_doc_cleaning)
  
  codenames_survey_weights <- df_chem_doc_cleaning %>%
    pull(weight_codename) %>%
    unique(.)
  
  index_double_codenames <- grep(", ", codenames_survey_weights)
  
  problematic_double_codenames <- codenames_survey_weights[index_double_codenames] %>%
    str_split(., ",\\s") %>%
    unlist(.)
  
  codenames_survey_weights[index_double_codenames] <- NA
  
  codenames_survey_weights <- codenames_survey_weights %>%
    append(., problematic_double_codenames) %>%
    unique(.) %>%
    na.omit(.)
  
  # Form a dataset by merging the chemicals dataset with the demographic dataset
  df_weights <- full_join(clean_demo_dataset
                            , unclean_chemical_dataset
                            , by = "SEQN") %>%
    select(c("SEQN"
             , codenames_survey_weights
             # , "RIDAGEYR"
             , "SDDSRVYR"))
  # View(df_weights)
  
  # Extract the harmonized chemical codenames
  chem_codenames <- df_chem_doc_cleaning %>%
    select("chemical_codename_use") %>%
    unique(.) %>%
    unlist(., use.names = FALSE) #%>%
    # .[1:2]
  
  # Used for debugging. Manually define to one chemical codename
  # chem_codenames <- c("LBXPFOA"
  #                     , "LBXPFOS")
  # chem_codenames <- "LBXPFOS"
  
  # Determine the number of harmonized chemical codenames
  num_chem <- length(chem_codenames)
  
  # Define this dataset to contain the participant identiifers and the study year. 
  # This dataset will contain the constructed weights with the column name label as the concatenation of "WT_" 
  # and the chemical codename
  df_survey_weights <- clean_demo_dataset %>%
    select("SEQN", "SDDSRVYR")
  # print(df_survey_weights)
  
  
  for(i in seq(num_chem))
  {
    # Extract a given chemical codename
    chem_codename_i <- chem_codenames[i]
    print(chem_codename_i)
    
    # Extract the cleaning documentation for that given chemical codename
    subset_cas_chem_i <- df_chem_doc_cleaning %>%
      filter(chemical_codename_use == chem_codename_i)
    # View(subset_cas_chem_i %>% select(chemical_codename_use, SDDSRVYR, weight_codename))
    
    # Define the new column name for the survey weights for this chemical codename
    chem_weight_codename <- paste("WT"
                                  , chem_codename_i
                                  , sep = "_")
    
    # Extract the name of the survey weight for the given chemical codename
    chem_weight_name_i <- df_chem_weights_doc_cleaning %>%
      filter(variable_codename == chem_weight_codename) %>%
      select(variable_description) %>%
      unique(.) %>%
      unlist(., use.names = FALSE)
    # print(chem_weight_name_i)
    
    # Intialize an empty vector to eventually hold the survey weights for a given chemical codename
    empty_chem_weight <- rep(NA, nrow(df_weights))
    
    # Determine the cycles that have the measurements for the chemical codename
    unique_cycles <- subset_cas_chem_i %>%
      select(SDDSRVYR) %>%
      unique(.) %>%
      unlist(., use.names = FALSE) #%>%
      # .[2]
    
    # Determine the number of cycles with available measurements for the chemical codename
    num_cycles <- length(unique_cycles)
    
    for(j in seq(num_cycles))
    {
      # Extract a given cycle for the given chemical codename
      cycle_j <- unique_cycles[j]
      print(cycle_j)
      
      # Extract the cleaning documentation for the given chemical codename and a given cycle
      subset_cas_chem_i_j <- subset_cas_chem_i %>%
        filter(SDDSRVYR == cycle_j)
      # print(subset_cas_chem_i_j %>%
      #         select(chemical_codename_use
      #                , SDDSRVYR
      #                , weight_codename))
      
      # Determine the rwo indices of participants who have measurements in a given cycle
      index_cycle_j <- which(df_weights$SDDSRVYR == cycle_j)
      # print(index_cycle_j)
      
      # Determine the codename of the survey weights used to construct the new survey weights specific for a 
      # given chemical
      weight_codename_j <- subset_cas_chem_i_j$weight_codename %>%
        unlist(., use.names = FALSE)
      
      
      is_comma <- grepl(",\\s", weight_codename_j)
      
      # if(length(is_comma) > 1)
      # {
      #   print(weight_codename_j)
      #   print(cycle_j)
      #   print(is_comma)
      # }
      
      
      if(TRUE %in% is_comma)
      {
        weight_codename_j <- weight_codename_j %>%
          str_split(., ",\\s") %>%
          unlist(.)
      }
      # print(weight_codename_j)
      
      
      # Determine the number of survey weights codenames used in a given cycle
      # Note: some of the PFASs may have 2 survey weights to used in the same cycle, because one is for adults
      # and the other is for children
      num_weight_codenames_j <- length(weight_codename_j)
      # print(num_weight_codenames_j)
      
      for(k in seq(num_weight_codenames_j))
      {
        # Determine a given survey weight codename 
        weight_codename_j_k <- weight_codename_j[k]
        # print(weight_codename_j_k)
        
        # Determine the row indices for participants who have a survey weight for this survey weight codename
        index_existing_weight_k <- which(!is.na(df_weights[,weight_codename_j_k]))
        
        # Determine the row indices for participants who have a survey weight in this cycle and for this particular
        # survey weight codename
        index_existing_in_cycle_k <- intersect(index_existing_weight_k
                                               , index_cycle_j)
        
        # Assign the survey weights into the empty vector to construct the new survey weights specific for a 
        # chemical codename
        empty_chem_weight[index_existing_in_cycle_k] <- df_weights[index_existing_in_cycle_k
                                                                   , weight_codename_j_k]
        # print(num_weight_codenames_j)
        # View(df_weights[index_existing_in_cycle_k
        #                 , c(#"RIDAGEYR",
        #                   weight_codename_j_k, "SDDSRVYR")] %>%
        #        unique(.))
      }
      

    }
    
    
    index_zeros <- which(empty_chem_weight == 0)
    empty_chem_weight[index_zeros] <- NA
    # View(cbind(empty_chem_weight, df_weights[,c("SEQN", weight_codename_j,"SDDSRVYR")]) %>%
    #        unique(.) %>%
    #        filter(SDDSRVYR == cycle_j))
    
    # After constructing the survey weights for a given chemical codename across all the cycles with available
    # measurments, make a new column in the dataset of constructed survey weights
    df_survey_weights <- df_survey_weights %>%
      mutate(empty_chem_weight)

    # Determine the column index pertaining to this new column for the constructed survey weights
    index_new_column <- which(colnames(df_survey_weights) == "empty_chem_weight")
    
    # Assign the new codename for the constructed survey weights
    colnames(df_survey_weights)[index_new_column] <- chem_weight_codename
    
    # Assign the label as the new description for the constructed survey weights
    attr(df_survey_weights[,index_new_column], "label") <- chem_weight_name_i
    
  }
  
  # Ensure that the study year is numeric as a data type
  df_survey_weights$SDDSRVYR <- as.numeric(df_survey_weights$SDDSRVYR)
  
  # Assign the name of the codename for the study year
  attr(df_survey_weights$SDDSRVYR, "label") <- "Data release cycle"
  
  # Ensure that the participant ID is an integer
  df_survey_weights$SEQN <- as.integer(df_survey_weights$SEQN)
  
  # Assign the name of the codename for the participant ID
  attr(df_survey_weights$SEQN, "label") <- "Respondent sequence number"
    
  
  return(df_survey_weights)
}