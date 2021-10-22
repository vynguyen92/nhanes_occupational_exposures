heatmap_percentage_above_be <- function(dataset_merged
                                        , df_include_occupations
                                        , df_stats
                                        , vector_covariates
                                        , name_of_folder
                                        , current_directory
                                        , df_survey_weights = NULL)
{
  library("tidyverse")
  library("readxl")
  library("pvclust")
  library("gplots")
  
  # Determine all file names in the current working directory
  all_files_in_current_directory <- list.files()
  
  # Make a new folder if the folder doesn't exist
  if(name_of_folder %in% all_files_in_current_directory)
  {
    
  } else {
    dir.create(name_of_folder)
  }
  
  # Define a string for the working directory for the new folder
  new_working_directory <- paste(current_directory
                                 , name_of_folder
                                 , sep = "/")
  
  df_chem_colors <- read_excel("NHANES - Color for Chemical Family.xlsx")
  
  df_biomonitoring_equivalents <- read_excel("NHANES - Master List of Files 1c.xlsx"
                                             , sheet = "Biomonitoring Equivalents")
  # View(df_biomonitoring_equivalents)
  
  # Set the working directory to this new folder
  setwd(new_working_directory)
  
  endpoint_types <- df_biomonitoring_equivalents %>%
    filter(!is.na(biomonitoring_equivalents)) %>%
    pull(endpoint_type) %>%
    unique(.) %>%
    .[2]
  
  num_endpoint_types <- length(endpoint_types)
  # print(num_endpoint_types)
  
  for(i in seq(num_endpoint_types))
  {
    endpoint_type_i <- endpoint_types[i]
    print(endpoint_type_i)
    
    subset_biomonitoring_equivalents_i <- df_biomonitoring_equivalents %>%
      filter(endpoint_type == endpoint_type_i) %>%
      group_by(chemical_codename
               , chemical_name
               , units
               , cas_num
               , biological_medium) %>%
      summarise(biomonitoring_equivalents = min(biomonitoring_equivalents)) %>%
      ungroup(.)
    # View(subset_biomonitoring_equivalents_i)
    
    occupations_exclude <- df_include_occupations %>%
      filter(include == "no") %>%
      pull(VNSECTORCOLLARCURR)
    # print(occupations_exclude)
   
    
    vector_chemical_codenames <- df_stats %>%
      filter(include == "yes") %>%
      pull(chemical_codename) %>%
      intersect(.
                , subset_biomonitoring_equivalents_i %>%
                  pull(chemical_codename)) #%>%
      # .[1:2] %>%
      # c(., "LBXCOT")
    # print(vector_chemical_codenames)

    vector_chemical_codenames <- c("LBXBCD", "LBXVXY", "URXUAS")

    dataset_long <- dataset_merged %>%
      select("SEQN"
             , all_of(vector_covariates)
             , all_of(vector_chemical_codenames)) %>%
      filter(!(VNSECTORCOLLARCURR %in% occupations_exclude)) %>%
      gather(., chemical_codename, chem_value, all_of(vector_chemical_codenames)) %>%
      na.omit(.) %>%
      filter(chem_value != 0) %>%
      left_join(.
                , subset_biomonitoring_equivalents_i %>%
                  select("chemical_codename"
                         , "chemical_name"
                         , "units"
                         , "biological_medium"
                         , "biomonitoring_equivalents")
                , by = "chemical_codename") %>%
      na.omit(.) %>%
      mutate(boolean_above_be = chem_value > biomonitoring_equivalents)
    # print(str(dataset_long))
    
    if(is_empty(df_survey_weights) == TRUE)
    {
      analysis_type <- "unweighted"
      print(analysis_type)
      # View(dataset_long)
      
      dataset_perc_above_be_long <- dataset_long %>%
        group_by(chemical_name, VNSECTORCOLLARCURR) %>%
        summarise(num_above_be = sum(boolean_above_be)
                  , total_participants = length(boolean_above_be)
                  , perc_above_be = num_above_be/total_participants*100) %>%
        ungroup(.)
      # View(dataset_perc_above_be_long)
      
    } else {
      
      analysis_type <- "weighted"
      
      vector_wt_chem_codenames <- paste("WT_"
                                        , vector_chemical_codenames
                                        , sep = "")
      
      subset_survey_weights <- df_survey_weights %>%
        select("SEQN", all_of(vector_wt_chem_codenames)) %>%
        pivot_longer(cols = all_of(vector_wt_chem_codenames)
                     , names_to = "chemical_codename"
                     , values_to = "survey_weights") %>%
        na.omit(.) %>%
        mutate(chemical_codename = gsub("^WT_"
                                       , ""
                                       , chemical_codename))
      # print(str(subset_survey_weights))
      
      dataset_long <- dataset_long %>%
        left_join(.
                  , subset_survey_weights
                  , by = c("SEQN", "chemical_codename"))
      # print(str(dataset_long))
      
      unique_cycle_info <- dataset_long %>%
        group_by(chemical_codename) %>%
        summarise(SDDSRVYR = unique(SDDSRVYR)
                  , denominator = unique(SDDSRVYR) %>%
                    length(.)
                  ) %>%
        ungroup(.) 
      
      multiplier_info <- unique_cycle_info %>%
        group_by(chemical_codename) %>%
        do(boolean_multiplier = ifelse(c(1,2) %in% .$SDDSRVYR  
                               , TRUE
                               , FALSE))
      # View(multiplier_info)
      
      unique_cycle_info <- unique_cycle_info %>%
        full_join(.
                  , multiplier_info
                  , by = "chemical_codename") %>%
        mutate(numerator = ifelse(SDDSRVYR %in% c(1,2) 
                                  & boolean_multiplier == "c(TRUE, TRUE)"
                                  , 2
                                  , 1)) %>%
        mutate(multiplier = numerator/denominator)
      # View(unique_cycle_info)
      
      dataset_long <- dataset_long %>%
        left_join(.
                  , unique_cycle_info %>%
                    select("chemical_codename", "SDDSRVYR", "multiplier")
                  , by = c("chemical_codename", "SDDSRVYR")) %>%
        mutate(adjusted_weights = multiplier*survey_weights)
      # View(dataset_long)
     
      dataset_perc_above_be_long <- dataset_long %>%
        group_by(chemical_name, VNSECTORCOLLARCURR) %>%
        summarise(num_above_be = sum(adjusted_weights[boolean_above_be == TRUE]
                                     , na.rm = TRUE)
                  , total_participants = sum(adjusted_weights
                                             , na.rm = TRUE)
                  , perc_above_be = num_above_be/total_participants*100) %>%
        ungroup(.)
      View(dataset_perc_above_be_long)
    }
    


    dataset_perc_above_be_wide <- dataset_perc_above_be_long %>%
      select("chemical_name", "VNSECTORCOLLARCURR", "perc_above_be") %>%
      pivot_wider(names_from = VNSECTORCOLLARCURR
                  , values_from = perc_above_be)
    # View(dataset_perc_above_be_wide)

    chemical_names <- dataset_perc_above_be_wide$chemical_name
    # print(chemical_names)

    df_perc_above_be <- dataset_perc_above_be_wide %>%
      select(-chemical_name)



    matrix_perc_above_be <- df_perc_above_be %>%
      as.matrix(.)


    rownames(df_perc_above_be) <- chemical_names

    rownames(matrix_perc_above_be) <- chemical_names
    # View(matrix_perc_above_be)


    # View(matrix_perc_above_be)

    colors <- c("yellow", "orange", "red")
    colors <- colorRampPalette(colors)(99)

    sector_collars <- colnames(matrix_perc_above_be)
    # print(sector_collars)

    # Define the color block next to each collapsed groupings
    colors_for_occupation <- rep(NA
                                 , length(sector_collars))

    # Determine the index of which collapsed groupings contain blue or white collars
    index_blue <- grep("Blue"
                       , sector_collars)
    index_white <- grep("White"
                        , sector_collars)
    index_unemployed <- setdiff(1:length(sector_collars)
                                , c(index_blue
                                    , index_white))

    # The color block for blue collars will be blue while the color block for white colors will be gray
    colors_for_occupation[index_blue] <- "#00B2EE"
    colors_for_occupation[index_white] <- "white"
    colors_for_occupation[index_unemployed] <- "gray"

    num_chemicals <- dataset_long %>%
      pull(chemical_codename) %>%
      unique(.) %>%
      length(.)

    if(num_chemicals > 1)
    {
      df_coeffi_scale <- t(apply(df_perc_above_be, 1, scale))
      # View(df_coeffi_scale)

      list_pv_cluster_results <- pvclust(df_coeffi_scale
                                         , method.hclust = "average"
                                         , method.dist = "correlation"
                                         , nboot = 1500
                                         , parallel = TRUE)

      dendrogram_col <- as.dendrogram(list_pv_cluster_results$hclust)
      dendrogram_row <- TRUE
      dendrogram_both <- "both"

      #

      file_name.pdf <- paste("heatmap_perc_above_be_by_"
                             , "VNSECTORCOLLARCURR"
                             , "_pearson"
                             , "_"
                             , endpoint_type_i
                             , "_"
                             , analysis_type
                             , "_"
                             , 10
                             , ".pdf"
                             , sep = "")

      file_name.png <- paste("heatmap_perc_above_be_by_"
                             , "VNSECTORCOLLARCURR"
                             , "_pearson"
                             , "_"
                             , endpoint_type_i
                             , "_"
                             , analysis_type
                             , "_"
                             , 10
                             , ".png"
                             , sep = "")

      print(file_name.pdf)
      pdf(file = file_name.pdf
          , width = 14
          , height = 9)
      heatmap.2(x = matrix_perc_above_be
                , Rowv = dendrogram_row
                , Colv = dendrogram_col
                , dendrogram = dendrogram_both
                , col = c("white", colors)
                , breaks = c(0, 1:100)
                , trace = "none"
                , margins = c(15,19)
                , key = TRUE
                , keysize = 0.70
                , key.title = NA
                , key.xlab = "Percentage of Participants above BE"
                , cexRow = 0.9
                , cexCol = 0.9
                , srtCol = 30
                , ColSideColors = colors_for_occupation
                , na.color = "white"
                , na.rm = FALSE
      )
      dev.off()

      print(file_name.png)
      png(file = file_name.png
          , width = 14
          , height = 9
          , res = 1000
          , units = "in")
      heatmap.2(x = matrix_perc_above_be
                , Rowv = dendrogram_row
                , Colv = dendrogram_col
                , dendrogram = dendrogram_both
                , col = c("white", colors)
                , breaks = c(0, 1:100)
                , trace = "none"
                , margins = c(15,19)
                , key = TRUE
                , keysize = 0.70
                , key.title = NA
                , key.xlab = "Percentage of Participants above BE"
                , cexRow = 0.9
                , cexCol = 0.9
                , srtCol = 30
                , ColSideColors = colors_for_occupation
                , na.color = "white"
                , na.rm = FALSE
      )
      dev.off()

    } else {

    }
    
  

    
  }
  
  
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
}