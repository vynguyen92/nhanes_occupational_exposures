heatmap_detect_freq_occupation <- function(list_detect_freq
                                           , name_of_folder
                                           , current_directory
                                           , name_df_occupation
                                           , codename_occupation_group
                                           , arrange_by = "clustering"
                                           , df_statistics)
{
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
  
  # Set the working directory to this new folder
  setwd(new_working_directory)
  
  df_detect_feq_occupation <- list_detect_freq[[name_df_occupation]]
  
  # print(colnames(df_detect_feq_occupation))
  
  # print(colnames(list_detect_freq[["average"]]))

  if(arrange_by == "clustering")
  {
    df_detect_feq_occupation <- df_detect_feq_occupation %>%
      right_join(.
                , df_statistics %>%
                  filter(include == "yes") %>%
                  select("comments_codename_use", "chemical_codename", "chemical_name", "chem_family") 
                , by = "comments_codename_use")  %>%
      select(all_of(codename_occupation_group), "chemical_name", "perc_above_LOD") %>%
      na.omit(.) %>%
      spread(., all_of(codename_occupation_group), perc_above_LOD)
    
    dendrogram_option <- "both"
    rowv <- TRUE
    colv <- TRUE
    
  } else {
    # print(colnames(df_statistics))
    # df_statistics_ordered <- df_statistics %>%
    #   filter(!is.na(variance_across_groups)) %>%
    #   arrange(variance_across_groups)
    # # View(df_statistics_ordered)
    # 
    # chemical_names_ordered <- df_statistics_ordered %>%
    #   pull(chemical_name)
    # print(chemical_names_ordered)
    
    df_detect_feq_occupation <- df_detect_feq_occupation %>%
      right_join(.
                , df_statistics %>%
                  filter(include == "yes") %>%
                  select("comments_codename_use", "chemical_codename", "chemical_name", "chem_family", all_of(arrange_by))
                , by = "comments_codename_use")  %>%
      select(all_of(c(codename_occupation_group, all_of(arrange_by))), "chemical_name", "perc_above_LOD") %>%
      spread(., all_of(codename_occupation_group), perc_above_LOD) %>%
      select(-all_of(arrange_by))
    
    dendrogram_option <- "none"
    rowv <- FALSE
    colv <- FALSE
    
  }
  # View(df_detect_feq_occupation)
  
  chemical_names <- df_detect_feq_occupation$chemical_name
  # print(chemical_names)

  matrix_detect_feq <- df_detect_feq_occupation %>%
    select(-chemical_name) %>%
    as.matrix(.)
  # View(matrix_detect_feq)

  rownames(matrix_detect_feq) <- chemical_names
  # print(str(matrix_detect_feq))
  # View(matrix_detect_feq)

  file_name.pdf <- paste("heatmap_detection_frequency_by_"
                     , codename_occupation_group
                     , "_"
                     , paste(arrange_by, collapse = "_")
                     , ".pdf"
                     , sep = "")

  file_name.png <- paste("heatmap_detection_frequency_by_"
                         , codename_occupation_group
                         , "_"
                         , paste(arrange_by, collapse = "_")
                         , ".png"
                         , sep = "")

  colors <- c("white", "yellow", "orange", "red")
  colors <- colorRampPalette(colors)(100)

  print(file_name.pdf)
  pdf(file = file_name.pdf
      , width = 22
      , height = 10)
  heatmap.2(x = t(matrix_detect_feq)
            , Rowv = rowv
            , Colv = colv
            , dendrogram = dendrogram_option
            , col = colors
            , trace = "none"
            , margins = c(15,19)
            , key = TRUE
            , keysize = 0.75
            , key.title = NA
            , key.xlab = "Percentage of Participants above LOD"
            , cexRow = 0.9
            , cexCol = 0.7
            , srtCol = 45
            , na.color = "black"
            )
  dev.off()

  print(file_name.png)
  png(file = file_name.png
      , width = 22
      , height = 10
      , res = 500
      , units = "in")
  heatmap.2(x = t(matrix_detect_feq)
            , Rowv = rowv
            , Colv = colv
            , dendrogram = dendrogram_option
            , col = colors
            , trace = "none"
            , margins = c(15,19)
            , key = TRUE
            , keysize = 0.75
            , key.title = NA
            , key.xlab = "Percentage of Participants above LOD"
            , cexRow = 0.9
            , cexCol = 0.7
            , srtCol = 45
            , na.color = "black"
            )
  dev.off()
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
}