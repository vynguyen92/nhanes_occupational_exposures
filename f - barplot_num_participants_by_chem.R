barplot_num_participants_by_chem <- function(dataset_merged
                                             , df_include_occupations
                                             , df_stats
                                             , vector_covariates
                                             , current_directory)
{
  # Determine all file names in the current working directory
  all_files_in_current_directory <- list.files()
  
  name_of_folder <- "Barplot - Number of Participants by Included Chemical"
  
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
  
  occupations_exclude <- df_include_occupations %>%
    filter(include == "no") %>%
    pull(VNSECTORCOLLARCURR)
  # print(occupations_exclude)
  
  vector_chemical_codenames <- df_stats %>%
    filter(include == "yes") %>%
    pull(chemical_codename) #%>%
    # .[1:2] %>%
    # c(., "LBXCOT")
  # print(vector_chemical_codenames)
  
  dataset_long <- dataset_merged %>%
    select("SEQN"
           , all_of(vector_covariates)
           , all_of(vector_chemical_codenames)) %>%
    filter(!(VNSECTORCOLLARCURR %in% occupations_exclude)) %>%
    gather(., chemical_codename, chem_value, all_of(vector_chemical_codenames)) %>%
    na.omit(.) %>%
    filter(chem_value != 0) %>%
    mutate(chem_value = log10(chem_value))
  # print(str(dataset_long))
 
  dataset_num_participants <- dataset_long %>%
    group_by(chemical_codename) %>%
    summarise(num_participants = length(chem_value)) %>%
    left_join(.
              , df_stats %>%
                select("chemical_codename"
                       , "chemical_name"          
                       , "chem_family"           
                       , "chem_family_shortened")
              , by = "chemical_codename")
  # View(dataset_num_participants)
  

  # Define a vector of chemical family names in a particular order
  chem_family_levels <- c("Acrylamide"
                          , "Dietary Components"
                          , "Metals"
                          , "Phthalates & Plasticizers"
                          , "Personal Care & Consumer Product Compounds"
                          , "Pesticides"
                          , "Phytoestrogens"
                          , "Polyaromatic Hydrocarbons (PAH)"
                          , "Volatile Organic Compounds (VOC)"
                          , "Smoking Related Compounds"
                          , "Per- and Polyfluoroalkyl Substances (PFAS)"
                          , "Other")

  # Ensure that the levels of the chemical family are in a defined order to ensure proper color scheme
  dataset_num_participants$chem_family <- factor(dataset_num_participants$chem_family
                                                 , levels = chem_family_levels)

  chem_family_colors <- df_chem_colors %>%
    filter(chemical_codename %in% vector_chemical_codenames) %>% 
    .[order(match(.$chem_family, chem_family_levels)),] %>%
    pull(chem_family_color) %>%
    unique(.) 
    
  # View(chem_family_colors)

  barplot_participants_by_chem <- ggplot(dataset_num_participants
                                        , aes(y = reorder(chemical_name
                                                         , num_participants)
                                              , x = num_participants
                                              , fill = chem_family
                                        )) +
    geom_bar(stat = "identity") +
    scale_fill_manual(name = "Chemical Family"
                      , values = chem_family_colors) +
    xlab("Number of Participants") +
    theme(legend.position = "top"
          , legend.text = element_text(size = 7)
          , legend.title = element_text(size = 10)
          , axis.title.y = element_blank()
          , axis.text.y = element_text(size = 6
                                       , face = "bold")
          , axis.text.x = element_text(size = 14
                                     , face = "bold")
          , axis.title.x = element_text(size = 14
                                        , face = "bold"))

  # Define the file name of the bar plots
  plot_name.png <- paste("bar_plot_"
                         , "num_participants_by_chemical.png"
                         , sep = "")
  plot_name.pdf <- paste("bar_plot_"
                         , "num_participants_by_chemical.pdf"
                         , sep = "")

  # Save the bar plots as a png and pdf
  print(plot_name.png)
  ggsave(filename = plot_name.png
         , plot = barplot_participants_by_chem
         , width = 14
         , height = 9)

  print(plot_name.pdf)
  ggsave(filename = plot_name.pdf
         , plot = barplot_participants_by_chem
         , width = 14
         , height = 9)
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
  # barplot_participants_by_chem
}