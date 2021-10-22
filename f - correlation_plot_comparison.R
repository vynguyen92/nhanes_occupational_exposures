correlation_plot_comparison <- function(df_stats_include
                                        , df_stats_exclude
                                        , vector_analysis_type
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
  # View(df_chem_colors)
  
  # Set the working directory to this new folder
  setwd(new_working_directory)
  
  df_stats_include <- df_stats_include %>%
    mutate(analysis_type = vector_analysis_type[1])
  
  df_stats_exclude <- df_stats_exclude %>%
    mutate(analysis_type = vector_analysis_type[2])
  
  df_stats_merged <- df_stats_include %>%
    full_join(.
              , df_stats_exclude
              , by = colnames(.)) %>%
    left_join(.
              , df_chem_colors
              , by = "chemical_codename_use")
  # View(df_stats_merged)
  print(colnames(df_stats_merged))
  
  df_stats_merged_wide <- df_stats_merged %>%
    filter(excluded_occupational_groups == "None") %>%
    filter(!(term %in% c("(Intercept)"
                         , "INDFMPIR"
                         , "RIAGENDR_females"
                         , "RIDAGEYR"
                         , "RIDRETH1_mexican_americans"
                         , "RIDRETH1_blacks"
                         , "RIDRETH1_other_hispanics"
                         , "RIDRETH1_other_race"
                         , "SDDSRVYR"
                         , "VNSMOKING"))) %>%
    select("chemical_name"
           , "term"
           , "fold_diff"
           , "analysis_type"
           , "formula_eqt"
           , "chem_family"
           , "chem_family_shortened"
           , "chem_family_color") %>%
    pivot_wider(.
                , names_from = analysis_type
                , values_from = fold_diff) %>%
    mutate(perc_change = abs(exclude_smokers - include_smokers)/include_smokers*100) %>%
    mutate(occupation_type = case_when(grepl("Blue", term) == TRUE ~ "Blue Collars"
                                       , grepl("White", term) == TRUE  ~ "White Collars"
                                       , term %in% c("Going to school"
                                                     , "Occupation Missing"
                                                     , "Unable to work for health reasons"
                                                     , "Retired"
                                                     , "Looking for work"
                                                     , "On layoff"
                                                     , "Disabled"
                                                     , "Taking care of house or family") ~ "Unemployed")) %>%
    mutate(occupation_type = factor(occupation_type
                                    , levels = c("Blue Collars"
                                                 , "White Collars"
                                                 , "Unemployed"))) %>%
    filter(!(formula_eqt == "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR + VNSMOKING"
           & chemical_name == "Cotinine"))
  View(df_stats_merged_wide)
  
  colors_chem_family <- df_stats_merged_wide %>%
    arrange(chem_family) %>%
    pull(chem_family_color) %>%
    unique(.)
  
  correlation_plot <- ggplot(data = df_stats_merged_wide) +
    geom_point(aes(x = include_smokers
                   , y = exclude_smokers
                   , shape = occupation_type
                   , color = chem_family)
               , size = 2) +
    facet_wrap(.~formula_eqt
               , ncol = 2
               ) +
    geom_abline(slope = 1, intercept = 0) +
    xlab("Fold Difference when Including Smokers") +
    ylab("Fold Difference when Excluding Smokers") +
    scale_x_log10() +
    scale_y_log10() +
    scale_color_manual(name = "Chemical Family"
                       , values = colors_chem_family) +
    scale_shape_manual(name = "Occupation Type"
                       , values = c(66
                                    , 87
                                    , 85)) +
    theme(legend.position = "top"
          , legend.box = "vertical"
          , strip.text = element_text(size = 8))

  plot_name.png <- paste("panel_correlation_plots"
                         , ".png"
                         , sep = "")
  plot_name.pdf <- paste("panel_correlation_plots"
                         , ".pdf"
                         , sep = "")

  # Save the panel of stairway plots as a png and pdf
  print(plot_name.png)
  ggsave(filename = plot_name.png
         , plot = correlation_plot
         , width = 14
         , height = 16)
  print(plot_name.pdf)
  ggsave(filename = plot_name.pdf
         , plot = correlation_plot
         , width = 14
         , height = 16)
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
  # correlation_plot
}