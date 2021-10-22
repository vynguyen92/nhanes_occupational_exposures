boxplot_cophenetic_coefficients <- function(list_clustering
                                            , formula_regressions
                                            , name_of_folder
                                            , current_directory)
{
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
  
  # Set the working directory to this new folder
  setwd(new_working_directory)
  
  names_clustering <- names(list_clustering)
  
  inclusion_detect_freq <- stri_extract_first_regex(names_clustering
                                                    , "[0-9]+") %>%
    unique(.)
  num_inclusion_detect_freq <- length(inclusion_detect_freq)
  
  exclusion_occupations <- stri_extract_first_regex(names_clustering
                                                   , "[A-Za-z]+") %>%
    unique(.)
  num_exclusion_occupations <- length(exclusion_occupations)
  
  for(i in seq(num_inclusion_detect_freq))
  {
    inclusion_detect_freq_i <- inclusion_detect_freq[i]
    print(inclusion_detect_freq_i)
    
    names_detect_freq_i <- names_clustering %>%
      grepl(inclusion_detect_freq_i, .) %>%
      names_clustering[.]
    # print(names_detect_freq_i)
    for(j in seq(num_exclusion_occupations))
    {
      excluded_occupations_j <- exclusion_occupations[j]
      print(excluded_occupations_j)
      
      names_exclusion_occupations <- names_detect_freq_i %>%
        grepl(excluded_occupations_j, .) %>%
        names_detect_freq_i[.]
      # print(names_exclusion_occupations)
      
      for(k in seq(length(names_exclusion_occupations)))
      {
        names_exclusion_occupations_k <- names_exclusion_occupations[k]
        # print(names_exclusion_occupations_k)
        
        formula_num_k<- stri_extract_first_regex(names_exclusion_occupations_k
                                                , " [0-9]+") %>%
          gsub(" ", "", .) %>%
          as.numeric(.)
        
        formula_eqt_k <- formula_regressions[formula_num_k]
        # print(formula_eqt_k)
        
        df_cophenetics_i_j <- list_clustering[[names_exclusion_occupations_k]] %>%
          .$cophenetic_correlations %>%
          mutate(excluded_occupational_groups = excluded_occupations_j) %>%
          mutate(detection_freq_threshold = inclusion_detect_freq_i) %>%
          mutate(formula_eqt = formula_eqt_k)
        # print(df_cophenetics_i_j)
        
        if(k == 1)
        {
          df_merged_coph <- df_cophenetics_i_j
          
        } else {
          
          df_merged_coph <- df_merged_coph %>%
            full_join(.
                      , df_cophenetics_i_j
                      , by = colnames(.))
          
        }
      }
      
      boxplot_cophenetic_coeffi_i_j <- ggplot(df_merged_coph) +
        geom_boxplot(aes(x = linkage
                         , y = cophenetic_cor
                         , fill = linkage)) +
        xlab("Linkage Method") +
        ylab("Cophenetic Coefficients") +
        ylim(c(0,1)) +
        theme(legend.position = "none"
              , axis.text = element_text(size = 14)
              , axis.title = element_text(size = 16))
        
      
      plot_name.png <- paste("boxplot_cophenetic_coefficients"
                             , "_"
                             , inclusion_detect_freq_i
                             , "_"
                             , excluded_occupations_j
                             , ".png"
                             , sep = "")
      plot_name.pdf <- paste("boxplot_cophenetic_coefficients"
                             , "_"
                             , inclusion_detect_freq_i
                             , "_"
                             , excluded_occupations_j
                             , ".pdf"
                             , sep = "")
      
      # Save the panel of stairway plots as a png and pdf
      print(plot_name.png)
      ggsave(filename = plot_name.png
             , plot = boxplot_cophenetic_coeffi_i_j
             , width = 14
             , height = 9)
      print(plot_name.pdf)
      ggsave(filename = plot_name.pdf
             , plot = boxplot_cophenetic_coeffi_i_j
             , width = 14
             , height = 9)
      
    }
    
  }
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
}