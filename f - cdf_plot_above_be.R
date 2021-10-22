cdf_plot_above_be <- function(dataset_merged
                         , df_include_occupations
                         , df_stats
                         , vector_covariates
                         , name_of_folder
                         , current_directory)
{
  library("readxl")
  library("tidyverse")
  library("plotly")  
  library("scales")
  library("withr")
  
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
  
  occupations_exclude <- df_include_occupations %>%
    filter(include == "no") %>%
    pull(VNSECTORCOLLARCURR)
  # print(occupations_exclude)
  
  # vector_chemical_codenames <- df_stats %>%
  #   filter(include == "yes") %>%
  #   pull(chemical_codename) #%>%
  # # .[1:2] %>%
  # # c(., "LBXCOT")
  # print(vector_chemical_codenames)
  
  vector_chemical_codenames <- "LBXBPB"
  
  dataset_long <- dataset_merged %>%
    select("SEQN"
           , all_of(vector_covariates)
           , all_of(vector_chemical_codenames)) %>%
    filter(!(VNSECTORCOLLARCURR %in% occupations_exclude)) %>%
    gather(., chemical_codename, chem_value, all_of(vector_chemical_codenames)) %>%
    na.omit(.) %>%
    filter(chem_value != 0) %>%
    left_join(.
              , df_biomonitoring_equivalents %>%
                select("chemical_codename"
                       , "chemical_name"
                       , "units"
                       , "cas_num"
                       , "biological_medium"
                       , "biomonitoring_equivalents")
              , by = "chemical_codename") %>%
    na.omit(.)
  print(str(dataset_long))
  
  dataset_long <- arrange(dataset_long
                          , chem_value)
  
  df_cdf <- dataset_long %>% 
    group_by(VNSECTORCOLLARCURR) %>%
    do(data.frame(.
                  , probability = ecdf(.$chem_value)(.$chem_value)))
  print(colnames(df_cdf))
  
  
  
  cdf_plot <- ggplot(data = df_cdf
                     ) +
    geom_line(aes(x = chem_value
                  , y = (1 - probability)
                  , color = VNSECTORCOLLARCURR
                  , group = VNSECTORCOLLARCURR
                  , text = paste(VNSECTORCOLLARCURR
                                 , "<br>"
                                 , round((1 - probability)*100
                                       , digits = 2)
                                 , "% have " 
                                 , gsub(paste(" \\("
                                              , units %>%
                                                unique(.)
                                              , "\\)"
                                              , sep = "")
                                        , ""
                                        , chemical_name %>% 
                                          unique(.)
                                        ) %>%
                                   tolower(.)
                                 , " levels exceeding "
                                 , chem_value
                                 , " "
                                 , gsub("\\(|\\)", ""
                                        , units %>% 
                                          unique(.))  
                                 , sep = "")
                  )) +
    geom_vline(xintercept = df_cdf$biomonitoring_equivalents %>%
                 unique(.)
               , color = "pink") +
    scale_x_log10() +
    scale_y_log10() +
    ylab("Probability") +
    xlab(dataset_long$chemical_name %>% unique(.)) +
    theme(legend.position = "none")

  cdf_plot <- ggplotly(cdf_plot
                       , tooltip = c("text")
                       )



  # print(((cdf_plot$x$data)))
  
  # text_y <- label_number_si(
  #   cdf_plot$x$data[[2]]$y
  #   , prefix = "Probability = "
  # )
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
  # cdf_plot %>%
  #   # style(hoverinfo = "skip", traces = 1) %>%
  #   style(hoverinfo = "skip", text = text_y, traces = 1)
  cdf_plot
}