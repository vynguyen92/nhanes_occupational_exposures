heatmap_clustering_occupations <- function(list_clustering
                                           , dataset_identifiers
                                           , vector_regression_formulas
                                           , name_of_folder
                                           , current_directory
                                           , file_name_motif = ""
                                           , excluded_chemicals = NA)
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
  
  df_chem_colors <- read_excel("NHANES - Color for Chemical Family.xlsx")
  
  # Set the working directory to this new folder
  setwd(new_working_directory)
  
  if(grepl("Cluster", name_of_folder) == TRUE)
  {
    occupation_name <- "cluster"
    
  } else if(grepl("Sector-Collar", name_of_folder) == TRUE) {
    
    occupation_name <- "sector_collars"
  }
  
  names_analyses <- names(list_clustering)#[1]
  print(names_analyses)
  
  # names_analyses <- "50 - 1 - None"
  
  for(analysis_type_a in names_analyses)
  {
    print(analysis_type_a)
    
    formula_num <- analysis_type_a %>%
      gsub("^[0-9]{2} - ", "", .) %>%
      gsub(" - ([a-zA-Z]*\\s[a-zA-Z]*|[a-zA-Z]*)", "", .) %>%
      as.numeric(.)
    # print(formula_num)
  
    formula_eqt <- vector_regression_formulas[formula_num]
    # print(formula_eqt)
    
    list_clustering_a <- list_clustering[[analysis_type_a]]
    # print(list_clustering_a)
    
    names_dataset <- names(list_clustering_a)
    # print(names_dataset)
    
    linkage_types_original <- names_dataset[grepl("linkage", names_dataset)] #%>%
      # .[1]
    # print(linkage_types_original)
    
    # linkage_types_original <- linkage_types_original[1]

    dataset_coeffi <- list_clustering_a[["dataset_coefficients"]]
    # View(dataset_coeffi)
    
    dataset_asterisks <- list_clustering_a[["dataset_adj_pvalues_asterisk"]]

    # name_of_list_clustering <- deparse(substitute(list_clustering_a))
    # print(name_of_list_clustering)

    dataset_identifiers <- dataset_identifiers %>%
      select(chemical_codename, chemical_name, chem_family, chem_family_shortened) %>%
      filter(chemical_codename %in% rownames(dataset_coeffi))
    # View(dataset_identifiers)

    if(anyNA(excluded_chemicals) == FALSE)
    {
      dataset_identifiers <- dataset_identifiers %>%
        filter(!(chemical_codename %in% excluded_chemicals))

      chem_codenames <- rownames(dataset_coeffi)

      index_excluded_chemicals <- which(chem_codenames %in% excluded_chemicals)
      # print(index_excluded_chemicals)

      chem_codenames <- dataset_asterisks$chemical_codename_use
      
      index_excluded_chemicals <- which(chem_codenames %in% excluded_chemicals)
      
      if(length(index_excluded_chemicals) == 0)
      {
        dataset_coeffi <- dataset_coeffi
        dataset_asterisks  <- dataset_asterisks
      } else {
        dataset_coeffi <- dataset_coeffi[-index_excluded_chemicals,]
        dataset_asterisks  <- dataset_asterisks[-index_excluded_chemicals,]
      }
      
      # View(dataset_coeffi)
      
      
      
      
    }
    # View(dataset_coeffi)
    # View(dataset_asterisks)

    dataset_identifiers <- dataset_identifiers %>%
      arrange(chemical_codename)

    duplicates <- duplicated(dataset_identifiers$chemical_codename)

    dataset_identifiers <- dataset_identifiers[!duplicates,]
    # View(dataset_identifiers)

    if(grepl("logistic", file_name_motif) == TRUE)
    {
      matrix_coeffi <- as.matrix(dataset_coeffi)
    } else {
      matrix_coeffi <- as.matrix((10^(dataset_coeffi)-1)*100)
    }
    
    # 
    
    # View(matrix_coeffi)

    sector_collars <- colnames(matrix_coeffi)
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
    
    stats_types <- "perc_diff"

    rownames_asterisk <- dataset_asterisks$chemical_codename_use

    matrix_asterisk <- dataset_asterisks %>%
      select(-chemical_codename_use) %>%
      as.matrix(.)
    # View(matrix_asterisk)
    
    rownames_motif <- dataset_identifiers$chemical_name
    

    # print(rownames(matrix_coeffi))
    rownames(matrix_coeffi) <- rownames_motif
    rownames(matrix_asterisk) <- rownames_motif
    # print(rownames(matrix_coeffi))
    # View(matrix_asterisk)

    dataset_identifiers <- dataset_identifiers %>%
      arrange(chem_family)
    # View(dataset_identifiers)
    order_chemical_names <- dataset_identifiers$chemical_name

    matrix_coeffi <- matrix_coeffi[order_chemical_names,]
    matrix_asterisk <- matrix_asterisk[order_chemical_names,]
    # View(matrix_asterisk)
    
    
    colors_for_chemicals <- df_chem_colors %>%
      filter(chemical_name %in% order_chemical_names) %>%
      pull(chem_family_color)


    plot_types <- c("heatmap"
                    , "dendrogram"
                    )

    image_types <- c("png", "pdf")

    for(linkage_type_j in linkage_types_original)
    {
      print(linkage_type_j)
      linkage_type_updated_j <- gsub("linkage_", "", linkage_type_j)
      # print((linkage_type_updated_j))

      dendrogram_j <- as.dendrogram(list_clustering_a[[linkage_type_j]]$hclust)
      # print(str(list_clustering_a[[linkage_type_j]]))

      cophenetic_cor_j <- list_clustering_a[["cophenetic_correlations"]] %>%
        filter(linkage ==linkage_type_updated_j) %>%
        pull(cophenetic_cor) %>%
        round(., digits = 2)
      # print(cophenetic_cor_j)

      title_j <- paste("Linkage: "
                       , linkage_type_updated_j
                       , "\n"
                       , "Distance: "
                       , "Pearson Correlation"
                       , "\n"
                       , "Cophenetic Correlation: "
                       , cophenetic_cor_j
                       , "\n"
                       , "Formula: "
                       , formula_eqt
                       , sep = "")
      # print(title_j)

      for(image_type_i in image_types)
      {

        for(plot_type_k in plot_types)
        {



          if(plot_type_k == "heatmap")
          {
            file_name_i <- paste("heatmap"
                                 , "_"
                                 , stats_types
                                 , "_"
                                 , occupation_name
                                 , "_"
                                 , linkage_type_updated_j
                                 , sep = "")

          } else if(plot_type_k == "dendrogram") {
            file_name_i <- paste("dendrogram"
                                 , "_"
                                 , occupation_name
                                 , "_"
                                 , linkage_type_updated_j
                                 , sep = "")
          }

          analysis_type_a_underscore <- gsub(" - "
                                             , "_"
                                             , analysis_type_a)

          if(file_name_motif != "")
          {
            file_name_i <- paste(analysis_type_a_underscore
                                 , "_"
                                 , file_name_motif
                                 ,  "_"
                                 , file_name_i
                                 , sep = "")
          }

          if(image_type_i == "png")
          {
            file_name_i.png <- paste(file_name_i
                                     , ".png"
                                     , sep = "")
            png(file = file_name_i.png
                , width = 14
                , height = 10
                , res = 1000
                , units = "in")

          } else if(image_type_i == "pdf") {

            file_name_i.pdf <- paste(file_name_i
                                     , ".pdf"
                                     , sep = "")
            pdf(file = file_name_i.pdf
                , width = 14
                , height = 10)

          }

          if(plot_type_k == "heatmap")
          {
            par(cex.main = 0.7)
            heatmap.2(matrix_coeffi
                      , cellnote = matrix_asterisk
                      , notecol = "black"
                      , notecex = 0.6
                      , Colv = dendrogram_j
                      # , Rowv = NULL
                      , dendrogram = "both"
                      , col = bluered(100)
                      , density.info = "density"
                      , margins = c(12,12)
                      , trace = "none"
                      , cexCol = 0.7
                      , adjCol = c(1,1)
                      , srtCol = 30
                      , cexRow = 0.5
                      , ColSideColors = colors_for_occupation
                      , RowSideColors = colors_for_chemicals
                      , key = TRUE
                      , keysize = 0.5
                      , key.title = NA
                      , key.xlab = "Percent Differences"
                      , main = title_j)


            dev.off()

          } else if(plot_type_k == "dendrogram") {

            plot(list_clustering_a[[linkage_type_j]], cex = 0.8)
            dev.off()
          }

          print(file_name_i)
        }
      }
    }
  }
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
}