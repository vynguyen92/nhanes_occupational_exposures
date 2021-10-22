barplot_perc_participants_by_occupation <- function(dataset_merged
                                                    , include_sector_collar_dataset
                                                    , occupation_group
                                                    , name_of_folder
                                                    , current_directory
                                                    , analysis_type)
{
  library("tidyverse")
  library("gridExtra")
  library("grid")
  library("RColorBrewer")
  
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
  
  vector_sector_collar_exclude <- include_sector_collar_dataset %>%
    filter(include == "no") %>%
    pull(all_of(occupation_group))
  # print(vector_sector_collar_exclude)

  dataset_merged <- dataset_merged %>%
    mutate(race = factor(case_when(RIDRETH1 == "_mexican_americans" ~ "Mexican Americans"
                                   , RIDRETH1 == "_other_hispanics" ~ "Other Hispanics"
                                   , RIDRETH1 == "_whites" ~ "Non-Hispanic Whites"
                                   , RIDRETH1 == "_blacks" ~ "Non-Hispanic Blacks"
                                   , RIDRETH1 == "_other_race" ~ "Other Race/Multi-Racial" )
                         , levels = rev(c("Non-Hispanic Whites"
                                          , "Non-Hispanic Blacks"
                                          , "Mexican Americans"
                                          , "Other Hispanics"
                                          , "Other Race/Multi-Racial" )))) %>%
    mutate(sex = factor(if_else(RIAGENDR == "_males", "Males", "Females"))) %>%
    mutate(age = RIDAGEYR) %>%
    mutate(age_group = factor(case_when(RIDAGEYR < 16 ~ "[0,16)"
                                        , RIDAGEYR >= 16 & RIDAGEYR <= 18 ~ "[16,18]"
                                        , RIDAGEYR > 18 & RIDAGEYR <= 28 ~ "(18,28]"
                                        , RIDAGEYR > 28 & RIDAGEYR <= 38 ~ "(28,38]"
                                        , RIDAGEYR > 38 & RIDAGEYR <= 48 ~ "(38,48]"
                                        , RIDAGEYR > 48 & RIDAGEYR <= 58 ~ "(48,58]"
                                        , RIDAGEYR > 58 & RIDAGEYR <= 68 ~ "(58,68]"
                                        , RIDAGEYR > 68 & RIDAGEYR <= 78 ~ "(68,78]"
                                       , RIDAGEYR > 78 ~ "(78,85]")
                              , levels = c("[0,16)"
                                           , "[16,18]"
                                           , "(18,28]"
                                           , "(28,38]"
                                           , "(38,48]"
                                           , "(48,58]"
                                           , "(58,68]"
                                           , "(68,78]"
                                           , "(78,85]"))) %>%
    mutate(poverty_income_ratio = factor(case_when(INDFMPIR <= 1 ~ "[0,1]"
                           , INDFMPIR > 1 & INDFMPIR <= 2 ~ "(1,2]"
                           , INDFMPIR > 2 & INDFMPIR <= 3 ~ "(2,3]"
                           , INDFMPIR > 3 & INDFMPIR <= 4 ~ "(3,4]"
                           , INDFMPIR > 4 & INDFMPIR <= 5 ~ "(4,5]")
                        , levels = c("[0,1]"
                                     , "(1,2]"
                                     , "(2,3]"
                                     , "(3,4]"
                                     , "(4,5]"))) %>%
    mutate(smoking = factor(case_when(LBXCOT <= 1 ~ "no smoking"
                               , LBXCOT > 1 & LBXCOT <= 3 ~ "secondhand smoke"
                               , LBXCOT > 3 ~ "active smoking")
                            , levels = c("no smoking"
                                         , "secondhand smoke"
                                         , "active smoking"))) %>%
    mutate(cycle = case_when(SDDSRVYR == 1 ~ "1999-2000"
                             , SDDSRVYR == 2 ~ "2001-2002"
                             , SDDSRVYR == 3 ~ "2003-2004"
                             , SDDSRVYR == 4 ~ "2005-2006"
                             , SDDSRVYR == 5 ~ "2007-2008"
                             , SDDSRVYR == 6 ~ "2009-2010"
                             , SDDSRVYR == 7 ~ "2011-2012"
                             , SDDSRVYR == 8 ~ "2013-2014")) %>%
    dplyr::select(all_of(occupation_group)
                  , "race"
                  , "age"
                  , "age_group"
                  , "sex"
                  , "poverty_income_ratio"
                  , "smoking"
                  , "cycle"
                  , "WTMECADJ"
                  , "WTINTADJ") %>%
    mutate(occupation_group = "NHANES population")


  # print(str(dataset_merged))

  dataset_merged_16_up <- dataset_merged %>%
    filter(age >= 16) %>%
    mutate(occupation_group = "NHANES 16+ population")

  subset_merged <- dataset_merged %>%
    filter(!(!!rlang::sym(occupation_group) %in% vector_sector_collar_exclude)) %>%
    mutate(occupation_group = !!rlang::sym(occupation_group))

  all_merged <- full_join(dataset_merged
                          , subset_merged
                          , by = colnames(dataset_merged)) %>%
    full_join(.
              , dataset_merged_16_up
              , by = colnames(.)) %>%
    filter(!is.na(occupation_group))

  covariates <- c("age_group"
                  , "sex"
                  , "race"
                  , "poverty_income_ratio"
                  , "smoking"
                  , "cycle"
                  )

  ref_of_covariates <- list("age_group" = c("[0,16)", "[16,18]", "(18,28]")
                            , "sex" = "Males"
                            , "race" = "Non-Hispanic Whites"
                            , "poverty_income_ratio" = "[0,1]"
                            , "smoking" = "active smoking"
                            , "cycle" = c("1999-2000", "2001-2002")
                            )

  num_covariates <- length(covariates)



  list_perc_plots <- list()

  for(i in seq(num_covariates))
  {
    covariate_i <- covariates[i]
    print(covariate_i)

    occupation_and_covariate <- c("occupation_group", covariate_i)

    if(analysis_type == "unweighted")
    {
      num_participants_by_occupation <- all_merged %>%
        select(all_of(occupation_and_covariate)) %>%
        na.omit(.) %>%
        group_by_at("occupation_group") %>%
        summarise(num_participants = n()) %>%
        ungroup(.) %>%
        data.frame(.)
      # View(num_participants_by_occupation)
      
      num_participants_by_occupation_covariate_i <- all_merged %>%
        select(all_of(occupation_and_covariate)) %>%
        na.omit(.) %>%
        group_by_at(vars(one_of(occupation_and_covariate))) %>%
        summarise(num_participants = n()) %>%
        ungroup(.) %>%
        data.frame(.)
      # View(num_participants_by_occupation_covariate_i)
      
    } else if(analysis_type == "weighted") {
      
      num_participants_by_occupation <- all_merged %>%
        select(all_of(c(occupation_and_covariate, "WTMECADJ"))) %>%
        na.omit(.) %>%
        group_by_at("occupation_group") %>%
        summarise(num_participants = sum(WTMECADJ)) %>%
        ungroup(.) %>%
        data.frame(.)
      
      num_participants_by_occupation_covariate_i <- all_merged %>%
        select(all_of(c(occupation_and_covariate, "WTMECADJ"))) %>%
        na.omit(.) %>%
        group_by_at(vars(one_of(occupation_and_covariate))) %>%
        summarise(num_participants = sum(WTMECADJ)) %>%
        ungroup(.) %>%
        data.frame(.)
      # View(num_participants_by_occupation_covariate_i)
      
    }
    
   

    percentage_df <- num_participants_by_occupation_covariate_i %>%
      left_join(.
                , num_participants_by_occupation
                , by = "occupation_group") %>%
      mutate(percentage = num_participants.x/num_participants.y*100)
    # View(percentage_df)

    ref_of_covariate_i <- ref_of_covariates[[covariate_i]]
    # print(ref_of_covariate_i)

    if(covariate_i %in% c("age_group", "cycle"))
    {
      ordered_occupation <- percentage_df %>%
        filter(!!rlang::sym(covariate_i) %in% ref_of_covariate_i) %>%
        group_by_at("occupation_group") %>%
        summarise(percentage = sum(percentage)) %>%
        ungroup(.) %>%
        arrange(percentage) %>%
        pull("occupation_group")

    } else {

      ordered_occupation <- percentage_df %>%
        filter(!!rlang::sym(covariate_i) %in% ref_of_covariate_i) %>%
        arrange(percentage) %>%
        pull("occupation_group")
    }
    # print(ordered_occupation)

    # View(num_participants_by_occupation_covariate_i)


    num_participants_by_occupation_covariate_i[,"occupation_group"] <- factor(num_participants_by_occupation_covariate_i[,"occupation_group"]
                                                                            , levels = ordered_occupation)

    covariate_name_i <- gsub("_", " ", covariate_i) %>%
      str_to_title

    num_participants_by_occupation_covariate_i <- num_participants_by_occupation_covariate_i %>%
      mutate(covariate_name = covariate_name_i)

    # View(num_participants_by_occupation_covariate_i)
    # print(ordered_occupation)
    
    color_category <- case_when(grepl("Blue", ordered_occupation) == TRUE ~ "#0096FF"
                                , grepl("White", ordered_occupation) == TRUE ~ "black"
                                , grepl("NHANES", ordered_occupation) == TRUE ~ "#B8860B"
                                , ordered_occupation %in% c("Going to school"
                                                          , "Occupation Missing"
                                                          , "Unable to work for health reasons"
                                                          , "Retired"
                                                          , "Looking for work"
                                                          , "On layoff"
                                                          , "Disabled"
                                                          , "Taking care of house or family") ~ "#A9A9A9")
    
    barplot_num_occupation_covariate <- ggplot(data = num_participants_by_occupation_covariate_i
                                         , aes(x = num_participants
                                               , y = num_participants_by_occupation_covariate_i[,"occupation_group"]
                                               , fill = !!rlang::sym(covariate_i))) +
      geom_bar(position = "fill"
               , stat = "identity") +
      facet_grid(cols = vars(covariate_name)) +
      scale_x_continuous(labels = scales::percent_format()) +
      scale_fill_brewer(name = element_blank()
                        , palette = "RdYlBu") +
      guides(fill = guide_legend(nrow = 1)) +
      theme(legend.position = "top"
            , legend.text = element_text(size = 11)
            , axis.title.y = element_blank()
            , axis.title.x = element_blank()
            , axis.text.x = element_text(size = 14)
            , axis.text.y = element_text(colour = color_category)
            , strip.text = element_text(size = 14))

    # Extract a letter to label the stairway plot
    letter_i <- LETTERS[i]

    # Define a title label of the letter and the position of that title for the physiological indicator
    title_i <- textGrob(label = letter_i
                        , x = unit(0.5, "lines")
                        , y = unit(0, "lines")
                        , hjust = 0
                        , vjust = 0
                        , gp = gpar(fontsize = 20
                                    , fontface = "bold"))

    plot_name.png <- paste("barplots_percentages_"
                           , covariate_i
                           , "_"
                           , analysis_type
                           , ".png"
                           , sep = "")
    plot_name.pdf <- paste("barplots_percentages_"
                           , covariate_i
                           , "_"
                           , analysis_type
                           , ".pdf"
                           , sep = "")

    ggsave(filename = plot_name.png
           , plot = barplot_num_occupation_covariate
           , width = 14
           , height = 9)

    ggsave(filename = plot_name.pdf
           , plot = barplot_num_occupation_covariate
           , width = 14
           , height = 9)

    list_perc_plots[[i]] <- arrangeGrob(barplot_num_occupation_covariate
                                   , top = title_i)
  }

  panel_perc_plots <- do.call("grid.arrange"
                            , c(list_perc_plots
                                , ncol = 2))

  panel_perc_plots <- arrangeGrob(panel_perc_plots
                                  , bottom = textGrob("Percentages (%)"
                                                      , gp = gpar(fontface = "bold"
                                                                  , cex = 1.5)
                                                      ))

  # Define file names of the png and pdf versions of the panel of stairway plots
  plot_name.png <- paste("barplots_percentages_demographics"
                         , "_"
                         , analysis_type
                         , ".png"
                         , sep = "")
  plot_name.pdf <- paste("barplots_percentages_demographics"
                         , "_"
                         , analysis_type
                         , ".pdf"
                         , sep = "")

  # Save the panel of stairway plots as a png and pdf
  print(plot_name.png)
  ggsave(filename = plot_name.png
         , plot = panel_perc_plots
         , width = 25
         , height = 20)
  print(plot_name.pdf)
  ggsave(filename = plot_name.pdf
         , plot = panel_perc_plots
         , width = 25
         , height = 20)
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
  # barplot_num_occupation_covariate
}