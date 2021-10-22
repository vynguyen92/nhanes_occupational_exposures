heatmap_sparsity_chemicals <- function(dataset_merged
                                       , include_sector_collar_dataset
                                       , occupation_group
                                       , include_chemical_dataset
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
  
  df_chem_colors <- read_excel("NHANES - Color for Chemical Family.xlsx") %>%
    rename(chemical_codename_use = chemical_codename)
  
  # Set the working directory to this new folder
  setwd(new_working_directory)
  
  vector_chemical_codenames <- include_chemical_dataset %>%
    filter(include == "yes") %>%
    pull(chemical_codename) #%>%
    # .[1:2]
  
  vector_sector_collar_exclude <- include_sector_collar_dataset %>%
    filter(include == "yes") %>%
    pull(all_of(occupation_group))
  
  ordered_chem_by_family <- df_chem_colors %>%
    filter(chemical_codename_use %in% vector_chemical_codenames) %>%
    arrange(chem_family) %>%
    pull(chemical_name)
  
  subset_merged <- dataset_merged %>%
    filter(!!rlang::sym(occupation_group) %in% vector_sector_collar_exclude)
  # print(dim(subset_merged))
  
  subset_merged_long <- subset_merged %>%
    select(all_of(vector_chemical_codenames)
           , !!rlang::sym(occupation_group)
           , "SEQN"
           , "RIDAGEYR"           
           , "RIAGENDR"          
           , "RIDRETH1"           
           , "INDFMPIR"          
           , "SDDSRVYR"
           , "VNSMOKING") %>%
    pivot_longer(cols = vector_chemical_codenames
                 , names_to = "chemical_codename_use"
                 , values_to = "chemical_concentration") %>%
    mutate(missing = ifelse(as.integer(!is.na(chemical_concentration)) == 0
                            , "No"
                            , "Yes"))     
  # print(colnames(subset_merged_long))
  
  stats_num_chem_by_participants <- subset_merged_long %>%
    group_by(SEQN) %>%
    summarise(num_chem = sum(as.integer(!is.na(chemical_concentration)))) %>%
    ungroup(.) 
  View(stats_num_chem_by_participants)
  
  stats_num_participants_with_num_chem_measured <- stats_num_chem_by_participants %>%
    group_by(num_chem) %>%
    summarise(num_participants = length(SEQN)) %>%
    ungroup(.)
  View(stats_num_participants_with_num_chem_measured)
  
  write.csv(stats_num_participants_with_num_chem_measured
            , "NHANES - Number of Participants with Number of Measured Chemicals 1a.csv")
  
  ordered_participants <- stats_num_chem_by_participants %>%
    arrange(num_chem) %>%
    pull(SEQN) 
  # View(ordered_participants)
  
  subset_merged_long <- subset_merged_long %>%
    left_join(.
              , df_chem_colors
              , by = "chemical_codename_use") %>%
    mutate(SEQN = factor(SEQN
                         , ordered_participants)) %>%
    mutate(chemical_name = factor(chemical_name
                                  , levels = ordered_chem_by_family))
  # print(str(subset_merged_long))
  # View(subset_merged_long)
  
  # heatmap_sparsity <- ggplot(data = subset_merged_long
  #                            , aes(x = SEQN
  #                                  , y = chemical_name
  #                                  , fill = missing)) +
  #   geom_tile() +
  #   xlab("NHANES Participants") +
  #   ylab("Chemical Biomarkers") +
  #   scale_fill_manual(name = "Measurements available"
  #                     , values = c("#DCDCDC", "red")) +
  #   theme(legend.position = "top"
  #         , axis.text.x = element_blank()
  #         , axis.text.y = element_text(size = 6)
  #         , axis.title = element_text(size = 14))
  # 
  # plot_name.png <- paste("heatmap_"
  #                        , "chemicals_participants"
  #                        , ".png"
  #                        , sep = "")
  # plot_name.pdf <- paste("heatmap_"
  #                        , "chemicals_participants"
  #                        , ".pdf"
  #                        , sep = "")
  # 
  # # Save the bar plots as a png and pdf
  # print(plot_name.png)
  # ggsave(filename = plot_name.png
  #        , plot = heatmap_sparsity
  #        , width = 14
  #        , height = 9)
  # 
  # print(plot_name.pdf)
  # ggsave(filename = plot_name.pdf
  #        , plot = heatmap_sparsity
  #        , width = 14
  #        , height = 9)
  
  setwd(current_directory)
}