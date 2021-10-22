boxplot_chem_occupation <- function(dataset_merged
                                    , include_sector_collar_dataset
                                    , include_chemical_dataset
                                    , chemical_identifiers_dataset
                                    , occupation_group
                                    , name_of_folder
                                    , current_directory
                                    , selected_chemicals = ""
                                    , motif_panel = ""
                                    , df_regression_stats = NA
                                    , analysis_type = "unweighted"
                                    , df_survey_weights = NULL)
{
  library("psych")
  library("gridExtra")
  library("grid")
  library("cowplot")
  library("tidyverse")
  library("survey")
  library("readxl")
  
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
  
  df_biomonitoring_equivalents <- read_excel("NHANES - Master List of Files 1c.xlsx"
                                             , sheet = "Biomonitoring Equivalents")
  
  # Set the working directory to this new folder
  setwd(new_working_directory)
  
  vector_chemical_codenames <- include_chemical_dataset %>%
    filter(include == "yes") %>%
    pull(chemical_codename)
  
  vector_chemical_codenames <- c("LBXVBF"
                                 , selected_chemicals
                                 )
  
  vector_sector_collar_exclude <- include_sector_collar_dataset %>%
    filter(include == "no") %>%
    pull(all_of(occupation_group))
  
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
                                      , LBXCOT > 1 & LBXCOT <= 4 ~ "secondhand smoke"
                                      , LBXCOT > 4 ~ "active smoking")
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
    dplyr::select("SEQN"
                  , all_of(occupation_group)
                  , "VNCURRJOBOLD"
                  , "race"
                  , "age"
                  , "age_group"
                  , "sex"
                  , "poverty_income_ratio"
                  , "smoking"
                  , "cycle"
                  , "SDDSRVYR"
                  , "SDMVPSU"
                  , "SDMVSTRA"
                  , all_of(vector_chemical_codenames)) %>%
    mutate(occupation_group = "NHANES population")
  
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
    filter(!is.na(occupation_group)) %>%
    mutate(color_category = case_when(grepl("Blue", occupation_group) == TRUE ~ "Blue Collars"
                                      , grepl("White", occupation_group) == TRUE & 
                                        occupation_group != "Public Administration - White" ~ "White Collars"
                                      , occupation_group == "Public Administration - White" ~ "Reference"
                                      , grepl("NHANES", occupation_group) == TRUE ~ "NHANES population"
                                      , occupation_group %in% c("Going to school"
                                                                , "Occupation Missing"
                                                                , "Unable to work for health reasons"
                                                                , "Retired"
                                                                , "Looking for work"
                                                                , "On layoff"
                                                                , "Disabled"
                                                                , "Taking care of house or family") ~ "Unemployed"
                                      )) %>%
    mutate(color_category = factor(color_category
                                   , levels = c("Reference"
                                                , "Blue Collars"
                                                , "White Collars"
                                                , "Unemployed"
                                                , "NHANES population")))
  # print(all_merged$VNSECTORCOLLARCURR)
  # print(str(all_merged))
  
  count_selected_chemical <- 1
  
  num_chemicals <- length(vector_chemical_codenames)
  
  list_panel_plots <- list()
  
  for(i in seq(num_chemicals))
  {
    chem_codename_i <- vector_chemical_codenames[i]
    print(chem_codename_i)
    
    subset_biomonitoring_equivalents <- df_biomonitoring_equivalents %>%
      filter(chemical_codename == chem_codename_i)
    # print(subset_biomonitoring_equivalents)
    
    chemical_name_i <- chemical_identifiers_dataset %>%
      filter(chemical_codename_use == chem_codename_i) %>%
      pull(chemical_name) %>% 
      unique(.) %>%
      .[1]
    # print(chemical_name_i)
    
    subset_merged_i <- all_merged %>%
      select("SEQN"
             , occupation_group
             , "race"
             , "age"
             , "age_group"
             , "sex"
             , "poverty_income_ratio"
             , "smoking"
             , "cycle"
             , "SDDSRVYR"
             , "SDMVSTRA"
             , "SDMVPSU"
             , "color_category"
             , all_of(chem_codename_i)) #%>%
      # filter(occupation_group == "Information Services - Blue")
    # View((subset_merged_i))
    
    if(analysis_type == "unweighted")
    {
      # View(df_stats)

      df_stats <- subset_merged_i %>%
        group_by(occupation_group) %>%
        summarise(median = median(!!rlang::sym(chem_codename_i)
                                  , na.rm = TRUE)
                  , geometric_mean = geometric.mean(!!rlang::sym(chem_codename_i)
                                                    , na.rm = TRUE)) %>%
        ungroup(.) %>%
        arrange(median)
      # View(df_stats)

      ordered_occupation_group <- df_stats %>%
        pull(occupation_group)

      subset_merged_i$occupation_group <- factor(subset_merged_i$occupation_group
                                                 , levels = ordered_occupation_group)

      
      subset_merged_i <- subset_merged_i %>%
        mutate(chemical_name = rep(chemical_name_i, nrow(.))) %>%
        mutate(stats_type = "Distribution")

      boxplot_i <- ggplot() +
        geom_boxplot(data = subset_merged_i
                     , aes(x = !!rlang::sym(chem_codename_i)
                           , y = occupation_group
                           , fill = color_category)) +
        geom_point(data = df_stats
                   , aes(x = geometric_mean
                         , y = occupation_group)
                   , shape = 17
                   , color = "purple"
                   , size = 2.5) +
        scale_fill_manual(name = "Type of Occupation Status"
                          , values = c("#BE0000" #Reference
                                       , "#63B8FF" # Blue
                                       , "white"  # White
                                       , "gray"   # Unemployed
                                       , "#FFDF00" # NHANES population
                                       )) +
        scale_x_log10() +
        xlab(chemical_name_i) +
        theme(legend.position = "top"
              , axis.title.x = element_text(size = 14)
              , axis.title.y = element_blank()
              , axis.text.x = element_text(size = 12))
      
    } else if(analysis_type == "weighted") {
      
      wt_chem_codename <- paste("WT_"
                                , chem_codename_i
                                , sep = "")
      # print(wt_chem_codename)
      
      subset_survey_weights <- df_survey_weights %>%
        select("SEQN", all_of(wt_chem_codename)) 
      
      index_survey_weights <- which(colnames(subset_survey_weights) == wt_chem_codename)
      colnames(subset_survey_weights)[index_survey_weights] <- "survey_weights"
      # View(subset_survey_weights)
      
      subset_merged_i <- subset_merged_i %>%
        left_join(.
                  , subset_survey_weights 
                  , by = "SEQN") %>%
        na.omit(.)
      # View(subset_merged_i)
      
      df_num_psu_per_stratum <- subset_merged_i %>%
        select("SDMVSTRA"
               , "SDMVPSU") %>%
        unique(.) %>%
        group_by(SDMVSTRA) %>%
        summarise(num_unique_psu = length(SDMVPSU)) %>%
        ungroup(.) %>%
        filter(num_unique_psu == 1)
      
      problematic_stratum <- df_num_psu_per_stratum %>%
        pull(SDMVSTRA)
      # View(df_num_psu_per_stratum)
      
      subset_merged_i <- subset_merged_i %>%
        filter(!(SDMVSTRA %in% problematic_stratum))
      
      unique_cycle_info <- subset_merged_i %>%
        summarise(SDDSRVYR = unique(SDDSRVYR)
                  , denominator = unique(SDDSRVYR) %>%
                    length(.)) %>%
        group_by(SDDSRVYR, denominator) %>%
        do(boolean_multiplier = ifelse(.$SDDSRVYR %in% c(1,2) 
                                       , TRUE
                                       , FALSE)) %>%
        ungroup(.) %>%
        mutate(numerator = ifelse(SDDSRVYR %in% c(1,2) 
                                  & boolean_multiplier == TRUE
                                  , 2
                                  , 1)) %>%
        mutate(multiplier = numerator/denominator)
      # View(unique_cycle_info)
      
      subset_merged_i <- subset_merged_i %>%
        left_join(.
                  , unique_cycle_info %>%
                    select("SDDSRVYR", "multiplier")
                  , by = c("SDDSRVYR")) %>%
        mutate(adjusted_weights = multiplier*survey_weights) %>%
        mutate(survey_clusters = paste(SDMVPSU
                                       , SDMVSTRA))
      # View(subset_merged_i)
      
      nhanes_design <- subset_merged_i %>%
        select("SEQN"
               , "occupation_group"
               , "race"
               , "sex"
               , "age"
               , "poverty_income_ratio"
               , "smoking"
               , "cycle"
               , "adjusted_weights"
               , "SDMVPSU"
               , "SDMVSTRA"
               , "color_category"
               , all_of(chem_codename_i)) %>%
        mutate(occupation_group = factor(occupation_group
                                         , levels = unique(occupation_group))) %>%
        svydesign(ids = ~SDMVPSU
                  , strata = ~SDMVSTRA
                  , weights = ~adjusted_weights
                  , data = .
                  , nest = TRUE)
      
      df_means <- svyby(~eval(parse(text = chem_codename_i))
                        , ~occupation_group
                        , nhanes_design
                        , svymean
                        , na.rm = TRUE
                        , keep.var = FALSE) %>%
        rename(mean = "statistic")
      # View(df_means)
      
      df_quantiles <- svyby(~eval(parse(text = chem_codename_i))
                            , ~occupation_group
                            , nhanes_design
                            , svyquantile
                            , quantile = c(0, 0.25, 0.5, 0.75, 1)
                            , na.rm = TRUE
                            , keep.var = FALSE) %>%
        rename(perc_0 = "statistic1"
               , perc_25 = "statistic2"
               , perc_50 = "statistic3"
               , perc_75 = "statistic4"
               , perc_100 = "statistic5"
               ) %>%
        mutate(IQR = perc_75 - perc_25
               , perc_1.5_iqr_above = perc_75 + 1.5*IQR
               , perc_1.5_iqr_below = perc_25 - 1.5*IQR) %>%
        rowwise() %>%
        mutate(whisker_above = min(c(perc_100, perc_1.5_iqr_above))
               , whisker_below = max(c(perc_0, perc_1.5_iqr_below))
               ) %>%
        mutate(stats_type = "Distribution") %>%
        left_join(.
                  , df_means
                  , by = "occupation_group") %>%
        left_join(.
                  , subset_merged_i %>%
                    select("occupation_group"
                           , "color_category") %>%
                    unique(.)
                  , by = "occupation_group") %>%
        arrange(perc_50) 
      
      ordered_occupation_group <- df_quantiles %>%
        pull("occupation_group")
      
      df_quantiles$occupation_group <- factor(df_quantiles$occupation_group
                                              , levels = ordered_occupation_group)
      
      # View(df_quantiles)
      
      df_outliers <- subset_merged_i %>%
        left_join(.
                  , df_quantiles %>%
                    select("occupation_group"
                           , "whisker_above"
                           , "whisker_below")
                  , by = "occupation_group") %>%
        mutate(outlier = ifelse((eval(parse(text = chem_codename_i)) < whisker_below
                                | eval(parse(text = chem_codename_i)) > whisker_above) == TRUE
                                , TRUE
                                , FALSE)) %>%
        filter(outlier == TRUE)
      # View(df_outliers)
      
      boxplot_i <- ggplot() +
        geom_boxplot(data = df_quantiles
                     , aes(xmin = whisker_below
                           , xlower = perc_25
                           , xmiddle = perc_50
                           , xupper = perc_75
                           , xmax = whisker_above
                           , y = occupation_group
                           , fill = color_category)
                     , stat = "identity") +
        scale_fill_manual(name = "Type of Occupation Status"
                          , values = c("#BE0000" #Reference
                                       , "#63B8FF" # Blue
                                       , "white"  # White
                                       , "gray"   # Unemployed
                                       , "#FFDF00" # NHANES population
                          )) +
        geom_point(data = df_outliers
                   , aes(x = eval(parse(text = chem_codename_i))
                         , y = occupation_group)
                   , color = "black") +
        scale_x_log10() +
        xlab(chemical_name_i) +
        theme(legend.position = "top"
              , axis.title.x = element_text(size = 14)
              , axis.title.y = element_blank()
              , axis.text.x = element_text(size = 12))
    }
    
   
   
    
    biomonitoring_equivalent_i <- subset_biomonitoring_equivalents %>%
      pull(biomonitoring_equivalents)
    
    if(is.na(biomonitoring_equivalent_i) == FALSE)
    {
      boxplot_i <- boxplot_i +
        geom_vline(xintercept = biomonitoring_equivalent_i
                   , color = "pink"
                   , size = 1)
    } else {

    }

    dummy_boxplot <- boxplot_i +
      theme(legend.text = element_text(size = 18)
            , legend.title = element_text(size = 18
                                          , face = "bold"))

    if(motif_panel != "")
    {


      subset_stats <- df_regression_stats %>%
        filter(chemical_codename_use == chem_codename_i) %>%
        filter(formula_eqt == "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR + VNSMOKING") %>%
        filter(excluded_occupational_groups == "None") %>%
        filter(!(term %in% c("(Intercept)"
                             , "RIDAGEYR"
                             , "RIAGENDR_females"
                             , "RIDRETH1_blacks"
                             , "RIDRETH1_mexican_americans"
                             , "RIDRETH1_other_hispanics"
                             , "RIDRETH1_other_race"
                             , "INDFMPIR"
                             , "SDDSRVYR"
                             , "VNSMOKING"))) %>%
        mutate(label_p_values = paste("p-value = "
                                      , formatC(adj.p.values, format = "e", digits = 1)
                                      , sep = "")) %>%
        mutate(label_perc_diff = paste(round((10^estimate-1)*100
                                             , digits = 1)
                                       , "%"
                                       , sep ="")) %>%
        mutate(label_asterisk = case_when(adj.p.values > 0.05 ~ ""
                                          , adj.p.values > 0.01 & adj.p.values <= 0.05 ~ "*"
                                          , adj.p.values > 0.001 & adj.p.values <= 0.01 ~ "**"
                                          , adj.p.values <= 0.001 ~ "***")) %>%
        add_row(chemical_codename_use = chem_codename_i
                , term = "Public Administration - White"
                , estimate = 0
                , std.error = NA
                , statistic = NA
                , p.value = NA
                , excluded_occupational_groups = "None"
                , formula_eqt = "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR + VNSMOKING"
                , fold_diff = 1
                , adj.p.values = NA
                , label_p_values = ""
                , label_perc_diff = "reference"
                , label_asterisk = "") %>%
        add_row(chemical_codename_use = chem_codename_i
                , term = "NHANES population"
                , estimate = NA
                , std.error = NA
                , statistic = NA
                , p.value = NA
                , excluded_occupational_groups = "None"
                , formula_eqt = "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR + VNSMOKING"
                , fold_diff = NA
                , adj.p.values = NA
                , label_p_values = ""
                , label_perc_diff = "NHANES"
                , label_asterisk = "") %>%
        add_row(chemical_codename_use = chem_codename_i
                , term = "NHANES 16+ population"
                , estimate = NA
                , std.error = NA
                , statistic = NA
                , p.value = NA
                , excluded_occupational_groups = "None"
                , formula_eqt = "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR + VNSMOKING"
                , fold_diff = NA
                , adj.p.values = NA
                , label_p_values = ""
                , label_perc_diff = "NHANES"
                , label_asterisk = "")
        
      # print(colnames(subset_stats))
      # View(subset_stats)
      
      
      if(motif_panel == "text")
      {
        subset_stats <- subset_stats %>%
          mutate(stats_type = "Regression Statistics")

        # print(ordered_occupation_group)

        subset_stats$term <- factor(subset_stats$term
                                    , levels = ordered_occupation_group)

        plot_stats_labels_i <- ggplot(data = subset_stats) +
          facet_grid(cols = vars(stats_type)) +
          geom_text(data = subset_stats
                    , aes(x = 0.05
                          , y = term
                          , label = label_perc_diff)
                    , hjust = 0
                    , size = 3) +
          geom_text(data = subset_stats
                    , aes(x = 0.15
                            , y = term
                          , label = label_asterisk)
                    , hjust = 0
                    , size = 3) +
          xlab(" ") +
          xlim(c(0.025,0.175)) +
          theme(legend.position = "top"
                , axis.text.x = element_text(size = 12)
                , axis.title.x = element_text(size = 14)
                , axis.title.y = element_blank()
                , strip.text = element_text(size = 12))


      } else if(motif_panel == "point") {

        subset_stats <- subset_stats %>%
          mutate(stats_type = "Percent Differences") %>%
          mutate(color_category = case_when(grepl("Blue", term) == TRUE ~ "Blue Collars"
                                            , grepl("White", term) == TRUE &
                                              term != "Public Administration - White" ~ "White Collars"
                                            , term == "Public Administration - White" ~ "Reference"
                                            , grepl("NHANES", term) == TRUE ~ "NHANES population"
                                            , term %in% c("Going to school"
                                                                      , "Occupation Missing"
                                                                      , "Unable to work for health reasons"
                                                                      , "Retired"
                                                                      , "Looking for work"
                                                                      , "On layoff"
                                                                      , "Disabled"
                                                                      , "Taking care of house or family") ~ "Unemployed"
                                            )) %>%
          mutate(color_category = factor(color_category
                                         , levels = c("Reference"
                                                      , "Blue Collars"
                                                      , "White Collars"
                                                      , "Unemployed"
                                                      , "NHANES population"
                                                      ))) %>%
          mutate(ci_low = (10^estimate-1)*100 - 1.96*(10^std.error-1)*100
                 , ci_high = (10^estimate-1)*100 + 1.96*(10^std.error-1)*100)
        # View(subset_stats)
        # print(str(subset_stats$color_category))

        # print(ordered_occupation_group)

        # print(ordered_occupation_group)
        # print(subset_stats$term)
        subset_stats$term <- factor(subset_stats$term
                                    , levels = ordered_occupation_group)
        # View(subset_stats)

        plot_stats_labels_i <- ggplot(data = subset_stats) +
          facet_grid(cols = vars(stats_type)) +
          geom_point(aes(x = (10^estimate-1)*100
                         , y = term)) +
          geom_text(aes(x = (10^estimate-1)*100
                         , y = term
                         , label = label_asterisk)
                    , vjust = 0.2) +
          geom_errorbarh(aes(y = term
                              , xmin = ci_low
                              , xmax = ci_high)
                        , width = 0.1
                        ) +
          geom_vline(xintercept = 0) +
          xlab("%") +
          theme(axis.title.y = element_blank()
                , axis.text.x = element_text(size = 12)
                , axis.title.x = element_text(size = 14)
                , legend.position = "none"
                , strip.text = element_text(size = 12))
      }
      
      boxplot_i <- boxplot_i +
        facet_grid(cols = vars(stats_type)) +
        theme(legend.position = "none"
              , axis.text.y = element_blank()
              , axis.text.x = element_text(size = 12)
              , strip.text = element_text(size = 12))
      
      if(is.na(biomonitoring_equivalent_i) == FALSE)
      {
        # View(subset_merged_i)
        # print(colnames(subset_merged_i))
        df_above_be <- subset_merged_i %>%
          group_by(occupation_group) %>%
          drop_na(., any_of(chem_codename_i)) %>%
          summarise(num_above_threshold = sum(eval(parse(text = chem_codename_i)) >= biomonitoring_equivalent_i)
                    , total_num = length(eval(parse(text = chem_codename_i)))
                    , perc_above_threshold = num_above_threshold/total_num*100) %>%
          mutate(stats_type = "Percentage Above BE") %>%
          mutate(label_perc_above = paste(round(perc_above_threshold
                                                , digits = 2)
                                          , "%"
                                          , sep ="")) %>%
          mutate(color_category = case_when(grepl("Blue", occupation_group) == TRUE ~ "Blue Collars"
                                            , grepl("White", occupation_group) == TRUE &
                                              occupation_group != "Public Administration - White" ~ "White Collars"
                                            , occupation_group == "Public Administration - White" ~ "Reference"
                                            , grepl("NHANES", occupation_group) == TRUE ~ "NHANES population"
                                            , occupation_group %in% c("Going to school"
                                                                      , "Occupation Missing"
                                                                      , "Unable to work for health reasons"
                                                                      , "Retired"
                                                                      , "Looking for work"
                                                                      , "On layoff"
                                                                      , "Disabled"
                                                                      , "Taking care of house or family") ~ "Unemployed")) %>%
          mutate(color_category = factor(color_category
                                         , levels = c("Reference"
                                                      , "Blue Collars"
                                                      , "White Collars"
                                                      , "Unemployed"
                                                      , "NHANES population")))
        # View(df_above_be)

        plot_above_threshold <- ggplot(data = df_above_be) +
          facet_grid(cols = vars(stats_type)) +
          geom_point(data = df_above_be
                    , aes(x = perc_above_threshold
                          , y = occupation_group
                          , fill = color_category
                          , size = 1)
                    , color = "black"
                    , shape = 21) +
          geom_vline(xintercept = 0) +
          scale_fill_manual(name = "Type of Occupation Status"
                            , values = c("#BE0000" #Reference
                                         , "#63B8FF" # Blue
                                         , "white"  # White
                                         , "gray"   # Unemployed
                                         , "#FFDF00" # NHANES population
                            )) +
          xlab("%") +
          theme(legend.position = "none"
                , axis.title.y = element_blank()
                , axis.text.y = element_blank()
                , axis.text.x = element_text(size = 12)
                , strip.text = element_text(size = 12))

        panel_boxplot <- grid.arrange(grobs = list(plot_stats_labels_i
                                                   , boxplot_i
                                                   # , plot_above_threshold
                                                   )
                                      , ncol = #3
                                      , widths = c(1.25, 2
                                                   # , 0.5
                                                   ))

      } else {
        panel_boxplot <- grid.arrange(grobs = list(plot_stats_labels_i
                                                   , boxplot_i)
                                      , ncol = 2
                                      , widths = c(1.25, 2))
      }
      
      
      boxplot_i <- panel_boxplot
    }
    
    legend_for_panel <- get_legend(dummy_boxplot)

    # Define file names of the png and pdf versions of the panel of boxplots
    plot_name.png <- paste("boxplot_"
                           , occupation_group
                           , "_"
                           , chem_codename_i
                           , "_"
                           , analysis_type
                           , ".png"
                           , sep = "")
    plot_name.pdf <- paste("boxplot_"
                           , occupation_group
                           , "_"
                           , chem_codename_i
                           , "_"
                           , analysis_type
                           , ".pdf"
                           , sep = "")

    # Save the panel of boxplots as a png and pdf
    print(plot_name.png)
    ggsave(filename = plot_name.png
           , plot = boxplot_i
           , width = 14
           , height = 9)
    
    print(plot_name.pdf)
    ggsave(filename = plot_name.pdf
           , plot = boxplot_i
           , width = 14
           , height = 9)
    
    if(chem_codename_i %in% selected_chemicals)
    {

      # Extract a letter to label the stairway plot
      letter_i <- LETTERS[count_selected_chemical]
      print(letter_i)

      # Define a title label of the letter and the position of that title for the physiological indicator
      title_i <- textGrob(label = letter_i
                          , x = unit(0.5, "lines")
                          , y = unit(0, "lines")
                          , hjust = 0
                          , vjust = 0
                          , gp = gpar(fontsize = 20
                                      , fontface = "bold"))

      if(motif_panel != "")
      {
        boxplot_i <- panel_boxplot

      } else if(motif_panel == "") {
        boxplot_i <- boxplot_i +
          facet_grid(cols = vars(chemical_name)) +
          theme(legend.position = "none"
                , axis.title.x = element_blank()
                , strip.text = element_text(size = 18))
      }


      list_panel_plots[[chem_codename_i]] <- arrangeGrob(boxplot_i
                                                         , top = title_i)

      count_selected_chemical <- count_selected_chemical + 1
      # print(count_selected_chemical)
    }
    
  }
  
  panel_boxplots <- do.call("grid.arrange"
                              , c(list_panel_plots
                                  , ncol = 2))

  panel_boxplots <- arrangeGrob(panel_boxplots
                                , top = legend_for_panel
                                )

  panel_boxplots <- arrangeGrob(panel_boxplots
                                , bottom = textGrob("Chemical Concentrations"
                                                    , gp = gpar(fontface = "bold"
                                                                , cex = 2)))

  if(motif_panel == "")
  {
    plot_name.png <- paste("panel_boxplots_chemical_conc_by_occupation"
                           , "_"
                           , analysis_type
                           , ".png"
                           , sep = "")
    plot_name.pdf <- paste("panel_boxplots_chemical_conc_by_occupation"
                           , "_"
                           , analysis_type
                           , ".pdf"
                           , sep = "")

  } else {
    plot_name.png <- paste("panel_boxplots_chemical_conc_by_occupation"
                           , "_"
                           , analysis_type
                           , "_"
                           , motif_panel
                           , ".png"
                           , sep = "")
    plot_name.pdf <- paste("panel_boxplots_chemical_conc_by_occupation"
                           , "_"
                           , analysis_type
                           , "_"
                           , motif_panel
                           , ".pdf"
                           , sep = "")
  }



    # Save the panel of stairway plots as a png and pdf
    print(plot_name.png)
    ggsave(filename = plot_name.png
           , plot = panel_boxplots
           , width = 28
           , height = 23)
    print(plot_name.pdf)
    ggsave(filename = plot_name.pdf
           , plot = panel_boxplots
           , width = 28
           , height = 23)
  
  # print(names(list_panel_plots))
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
}