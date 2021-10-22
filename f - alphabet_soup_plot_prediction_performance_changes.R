alphabet_soup_plot_prediction_performance_changes <- function(df_model_stats
                                                              , name_of_folder
                                                              , current_directory
                                                              , selected_formulas_for_plot
                                                              , selected_formulas_for_stats = NULL
                                                              , formula_numbers = NULL
                                                              , rsquared_statistic = "r.squared"
                                                              , df_additional_stats = NULL)
{
  library("readxl")
  library("tidyverse")
  
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
  
  analysis_types <- df_model_stats %>%
    pull(excluded_occupational_groups) %>%
    unique(.) #%>%
    # .[1]
  
  num_analysis_types <- length(analysis_types)
  
  for(i in seq(num_analysis_types))
  {
    analysis_type_i <- analysis_types[i]
    print(analysis_type_i)
    
    file_name_analysis_type_i <- analysis_type_i %>%
      tolower(.) %>%
      gsub(" ", "_", .)
    # print(file_name_analysis_type_i)
    
    subset_model_stats_i <- df_model_stats %>%
      filter(excluded_occupational_groups == analysis_type_i) %>%
      left_join(.
                , df_chem_colors
                , by = "chemical_codename_use")
    # View(subset_model_stats_i)
    print(colnames(subset_model_stats_i))
    
    num_model_types <- subset_model_stats_i %>%
      pull(model_type) %>%
      unique(.) %>%
      length(.)
    
    if(num_model_types > 1)
    {
      subset_model_stats_i <- subset_model_stats_i %>%
        mutate(model_formula_eqt = ifelse(model_type == "elastic net"
                                          , paste(model_type
                                                   , formula_eqt
                                                   , sep = ":      ")
                                          , paste(model_type
                                                  , formula_eqt
                                                  , sep = ":   "))
                )
    }
    # View(subset_model_stats_i)


    subset_model_stats_i <- subset_model_stats_i %>%
      filter(formula_eqt %in% selected_formulas_for_plot)
    # View(subset_model_stats_i)
    
    subset_model_wide_adj_i <- subset_model_stats_i %>%
      filter(chemical_codename_use != "LBXCOT") %>%
      filter(formula_eqt %in% selected_formulas_for_plot) %>%
      mutate(formula_eqt = ifelse(formula_eqt == "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR"
                                   , "without_smoking"
                                   , "with_smoking")) %>%
      select("chemical_codename_use"
              , all_of(rsquared_statistic)
              , "formula_eqt"
              , "chemical_name"
              , "chem_family"
              , "chem_family_shortened"
              , "chem_family_color") %>%
       pivot_wider(names_from = formula_eqt
                   , values_from = all_of(rsquared_statistic)) %>%
       mutate(variance_explained = with_smoking - without_smoking) %>%
       mutate(variance_explained = ifelse(variance_explained < 0
                                          , 0
                                          , variance_explained)) %>%
       mutate(variance_type = "adjusted for occupation, age, sex, race, PIR, and NHANES cycles") %>%
       select("chemical_codename_use"
              , "variance_explained"
              , "variance_type"
              , "chemical_name"
              , "chem_family"
              , "chem_family_shortened"
              , "chem_family_color")
    # View(subset_model_wide_adj_i)
    
    
    # subset_model_wide_adj_i <- subset_model_stats_i %>%
    #   filter(formula_eqt %in% selected_formulas_for_plot[2:3]) %>%
    #   mutate(formula_eqt = ifelse(formula_eqt == "chem_value ~ RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR + VNSMOKING"
    #                               , "without_occupation"
    #                               , "with_occupation")) %>%
    #   select("chemical_codename_use"
    #          , all_of(rsquared_statistic)
    #          , "formula_eqt"
    #          , "chemical_name"
    #          , "chem_family"
    #          , "chem_family_shortened"
    #          , "chem_family_color") %>%
    #   pivot_wider(names_from = formula_eqt
    #               , values_from = all_of(rsquared_statistic)) %>%
    #   mutate(variance_explained = with_occupation - without_occupation) %>%
    #   mutate(variance_explained = ifelse(variance_explained < 0
    #                                      , 0
    #                                      , variance_explained)) %>%
    #   mutate(variance_type = "adjusted for age, sex, race, PIR, smoking, and NHANES cycles") %>%
    #   select("chemical_codename_use"
    #          , "variance_explained"
    #          , "variance_type"
    #          , "chemical_name"
    #          , "chem_family"
    #          , "chem_family_shortened"
    #          , "chem_family_color")
    # View(subset_model_wide_adj_i)
    
    # subset_model_stats_univariate_i <- subset_model_stats_i %>%
    #   filter(formula_eqt == selected_formulas_for_plot[1]) %>%
    #   select("chemical_codename_use"
    #          , all_of(rsquared_statistic)
    #          # , "formula_eqt"
    #          , "chemical_name"
    #          , "chem_family"
    #          , "chem_family_shortened"
    #          , "chem_family_color") %>%
    #   rename(variance_explained = all_of(rsquared_statistic)) %>%
    #   mutate(variance_type = "univariate model") %>%
    #   select("chemical_codename_use"
    #          , "variance_explained"
    #          , "variance_type"
    #          , "chemical_name"
    #          , "chem_family"
    #          , "chem_family_shortened"
    #          , "chem_family_color")
    # View(subset_model_stats_univariate_i)
    
    # subset_model_merged_i <- subset_model_stats_univariate_i %>%
    #   full_join(.
    #             , subset_model_wide_adj_i
    #             , by = colnames(.)) %>%
    #   mutate(variance_type = factor(variance_type
    #                                 , levels = c("univariate model"
    #                                              , "adjusted for age, sex, race, PIR, smoking, and NHANES cycles")))
    # View(subset_model_merged_i)
    
    subset_model_merged_i <- subset_model_wide_adj_i
    # print(colnames(subset_model_merged_i))
    
    # # print(subset_model_stats_i$formula_eqt %>% unique(.))
    # 
    # subset_model_stats_i$formula_eqt <- factor(subset_model_stats_i$formula_eqt
    #                                            , levels = selected_formulas_for_plot)
    # # print(levels(subset_model_stats_i$formula_eqt))
    # 
   
    
    # if(length(selected_formulas_for_stats) == 1)
    # {
    #   df_stats_diff <- subset_model_merged_i %>%
    #     filter(variance_type == "univariate model") %>%
    #     arrange(variance_explained)
    #   # View(df_stats_diff)
    # 
    # } else {
    #   
    #    df_stats_diff <- subset_model_stats_i %>%
    #     filter(formula_eqt %in% selected_formulas_for_stats) %>%
    #     group_by(chemical_name) %>%
    #     summarise(diff = diff(r.squared)) %>%
    #     ungroup(.) %>%
    #     arrange(diff)
    # }
    df_stats_diff <- subset_model_merged_i %>%
      arrange(variance_explained)
    # View(df_stats_diff)
    
    if(is_empty(df_additional_stats) == FALSE)
    {
      subset_additional_stats <- df_additional_stats %>%
        filter(median_perc != 0)
      # View(subset_additional_stats)
      
      chemical_codename_include <- subset_additional_stats %>%
        pull(chemical_codename)
      # print(chemical_codename_include)
      
      # df_stats_diff <- df_stats_diff %>%
      #   filter(chemical_codename_use %in% chemical_codename_include)
      
      # subset_model_merged_i <- subset_model_merged_i %>%
      #   filter(chemical_codename_use %in% chemical_codename_include)
    }
    # View(subset_model_merged_i)
    # View(df_stats_diff)

    # if(num_model_types > 1)
    # {
    #   
    #   num_equations <- subset_model_stats_i %>%
    #     pull(model_formula_eqt) %>%
    #     unique(.) %>%
    #     length(.)
    #   
    #   subset_model_eqt_formula <- subset_model_stats_i %>%
    #     select(model_formula_eqt, model_type, formula_eqt) %>%
    #     unique(.) %>%
    #     arrange(formula_eqt)
    #   
    #   ordered_model_eqt_formula <- subset_model_eqt_formula %>%
    #     pull(model_formula_eqt)
    #   
    #   subset_model_stats_i$model_formula_eqt <- factor(subset_model_stats_i$model_formula_eqt
    #                                                    , levels = ordered_model_eqt_formula)
    #   # print(levels(subset_model_stats_i$model_formula_eqt))
    #   
    # } else {
      
      # num_equations <- subset_model_stats_i %>%
      #   pull(formula_eqt) %>%
      #   unique(.) %>%
      #   length(.)
    
    num_equations <- subset_model_merged_i %>%
      pull(variance_type) %>%
      unique(.) %>%
      length(.)
      
    # }
    
    if(length(formula_numbers) == 0)
    {
      formula_numbers <- seq(from = 49
                             , length.out = num_equations)
    } else {
      formula_numbers <- formula_numbers
    }



    ordered_chemical_names <- df_stats_diff %>%
      pull(chemical_name) %>%
      unique(.)

    subset_model_merged_i$chemical_name <- factor(subset_model_merged_i$chemical_name
                                                 , levels = ordered_chemical_names)

    alphabet_soup_plot_i <- ggplot(data = subset_model_merged_i
                                   , aes(x = variance_explained
                                         , y = chemical_name
                                         # , group = model_type
                                         , shape = variance_type
                                         , color = chem_family)) +
      # geom_jitter(size = 3.5
      #            , position = position_dodge(-2)) +
      geom_point(size = 3.5) +
      scale_shape_manual(name = "Variance Explained by Smoking"
                         , values = formula_numbers ) +
      scale_color_manual(name = "Chemical Family"
                         , values = subset_model_merged_i %>%
                           arrange(chem_family) %>%
                           pull(chem_family_color) %>%
                           unique(.)) +
      guides(shape = guide_legend(order = 1
                                  , nrow = num_equations
                                  )
             , color = guide_legend(order = 2)) +
      ylab("Chemical Biomarkers") +
      xlab("R2") +
      theme(legend.position = "top"
            , legend.box = "vertical"
            , legend.text = element_text(size = 9)
            , axis.title = element_text(size = 12))

    # Define file names of the png and pdf versions of the panel of stairway plots
    plot_name.png <- paste("alphabet_soup_plot_r2_changes"
                           , "_"
                           , file_name_analysis_type_i
                           , "_"
                           , rsquared_statistic
                           , ".png"
                           , sep = "")
    plot_name.pdf <- paste("alphabet_soup_plot_r2_changes"
                           , "_"
                           , file_name_analysis_type_i
                           , "_"
                           , rsquared_statistic
                           , ".pdf"
                           , sep = "")

    # Save the panel of stairway plots as a png and pdf
    print(plot_name.png)
    ggsave(filename = plot_name.png
           , plot = alphabet_soup_plot_i
           , width = 15
           , height = 20)
    print(plot_name.pdf)
    ggsave(filename = plot_name.pdf
           , plot = alphabet_soup_plot_i
           , width = 15
           , height = 20)
  }
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
}