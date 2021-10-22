#############################################################################################################
################################  MAIN SCRIPT - FORMING NHANES MERGED DATASET  ##############################
#############################################################################################################

working_directory <- "~/Dropbox (University of Michigan)/Postdoc/Occupational Exposome/OCCUPATION/Phase 6"

setwd(working_directory)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Install and Upload Any Necessary Packages  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

install_packages_upload_libraries()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Assign Directories of NHANES Datasets  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Set the working directory that contains the individual demographic datasets
demographics_directory <- paste(working_directory
                                , "Demographics Datasets"
                                , sep = "/")

# Set the working directory that contains the individual chemical datasets
chemical_directory <- paste(working_directory
                            , "Chemical Datasets"
                            , sep = "/")

mortality_directory <- paste(working_directory
                             , "Mortality Datasets/Follow-up to 2015"
                             , sep = "/") 

occupation_directory <- paste(working_directory
                              , "Occupation Datasets"
                              , sep = "/")

questionnaire_directory <- paste(working_directory
                                 , "Questionnaire Datasets"
                                 , sep = "/")

# Set the working directory that contains the individual response datasets
response_directory <- paste(working_directory
                            , "Response Datasets"
                            , sep = "/")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Compile the NHANES Datasets  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Extract the individual demographics datasets and compile them into the unclean demographics dataset
demographics_unclean <- compile_dataset_across_files(dataset_directory = demographics_directory
                                                     , current_directory = working_directory
                                                     , file_name_first_cycle = "DEMO.XPT")

# Extract the individual chemical datasets and compile them into the unclean chemicals dataset
chemicals_unclean <- compile_chemicals_dataset(dataset_directory = chemical_directory
                                               , current_directory = working_directory)

# Extract the individual mortality datasets and compile them into the unclean mortality dataset
mortality_unclean <- compile_mortality_dataset(dataset_directory = mortality_directory
                                               , current_directory = working_directory)

# Extract the individual occupation datasets and compile them into the unclean occupation dataset
occupation_unclean <- compile_dataset_across_files(dataset_directory = occupation_directory
                                                   , current_directory = working_directory
                                                   , file_name_first_cycle = "OCQ.XPT")

# Extract the individual questionnaire datasets and compile them into the unclean questionnaire dataset
questionnaire_unclean <- compile_dataset_across_folders(dataset_directory = questionnaire_directory
                                                        , current_directory = working_directory)

setwd(working_directory)
responses_unclean <- compile_dataset_across_folders(dataset_directory = response_directory
                                                    , current_directory = working_directory)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Clean the NHANES Datasets  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Upload the cleaning documentation for all datasets
list_master_files <- upload_nhanes_master_files("NHANES - Master List of Files 1d_shiny.xlsx")

# Clean the mortality dataset
mortality_clean <- clean_mortality_dataset(mortality_unclean
                                           , list_master_files
                                           , "Mortality")

# Clean the demographics dataset
demographics_clean <- clean_dataset_based_on_codename_changes(demographics_unclean
                                                              , list_master_files
                                                              , "Demographics")

# Clean the response dataset
response_clean <- clean_dataset_based_on_codename_changes(responses_unclean
                                                          , list_master_files
                                                          , "Response")

# Clean the questionnaire dataset
questionnaire_clean <- clean_dataset_based_on_codename_changes(questionnaire_unclean
                                                               , list_master_files
                                                               , "Questionnaire")

# Clean the occupation dataset
occupation_clean <- clean_dataset_based_on_codename_changes(occupation_unclean
                                                            , list_master_files
                                                            , "Occupation")

# Form the comments datasets
comments_clean <- form_comments_dataset(chemicals_unclean
                                        , list_master_files)

# Form the survey weights dataset
survey_weights <- form_survey_weights_dataset(chemicals_unclean
                                              , demographics_clean
                                              , list_master_files)

# Clean the chemical dataset
chemicals_clean <- clean_chemical_dataset(chemicals_unclean
                                          , list_master_files)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Determine Counting Statistics  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

df_counts_by_comments_and_chemicals <- determine_num_participants_by_comments_and_chemicals(chemicals_clean
                                                                                            , comments_clean
                                                                                            , list_master_files)

df_sector_collars_include <- determine_sector_collars_include(occupation_clean
                                                              , chemicals_clean
                                                              , demographics_clean
                                                              , 50)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Calculate Detection Frequencies  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

list_num_participants_chem_sector_collar <- calculate_num_per_chem_occupation(occupation_clean
                                                                              , chemicals_clean
                                                                              , demographics_clean
                                                                              , df_sector_collars_include
                                                                              , list_master_files$Chemicals
                                                                              , 90)

list_sensitivity_detection_frequency <- list()
list_inclusion_stats <- list()

threshold_detect_frequency <- seq(from = 10, to = 50, by = 10)
threshold_detect_frequency <- threshold_detect_frequency[1]

for(detect_frequency in threshold_detect_frequency)
{
  print(detect_frequency)
  
  string_detect_freq <- as.character(detect_frequency)
  
  list_sensitivity_detection_frequency[[string_detect_freq]] <- calculate_detection_frequencies(comments_clean
                                                                , occupation_clean
                                                                , chemicals_clean
                                                                , demographics_clean
                                                                , list_master_files$Chemicals
                                                                , df_sector_collars_include
                                                                , detect_frequency)
  
  list_inclusion_stats[[string_detect_freq]] <- decide_chemicals_to_include(list_sensitivity_detection_frequency[[string_detect_freq]]
                                                                            , list_num_participants_chem_sector_collar
                                                                            , detect_frequency
                                                                            , 90)
  
  write_excel_csv(list_inclusion_stats[[string_detect_freq]]
                  , paste("NHANES - Chemical Inclusion Critiera "
                          , string_detect_freq
                          , ".csv"
                          , sep =""))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~  Heatmap of Detection Frequencies by Occupation  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd(working_directory)
heatmap_detect_freq_occupation(list_sensitivity_detection_frequency$`10`
                               , "Heatmap - Detection Frequencies by Occupation"
                               , working_directory
                               , "each_sector_collar"
                               , "VNSECTORCOLLARCURR"
                               , "clustering"
                               , list_inclusion_stats$`10`)


heatmap_detect_freq_occupation(list_sensitivity_detection_frequency$`10`
                               , "Heatmap - Detection Frequencies by Occupation"
                               , working_directory
                               , "each_sector_collar"
                               , "VNSECTORCOLLARCURR"
                               , c("variance_across_groups")
                               , list_inclusion_stats$`10`)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Form Merged NHANES Dataset  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

nhanes_merged_dataset <- merge_datasets_together(occupation_clean
                                                 , chemicals_clean
                                                 , demographics_clean
                                                 , list_inclusion_stats$`10`
                                                 , df_sector_collars_include)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Population Statistics  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

calculate_table_1_statistics(nhanes_merged_dataset
                             , df_sector_collars_include)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Chemical Statistics  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

calculate_num_per_chemical(nhanes_merged_dataset
                           , df_sector_collars_include
                           , list_inclusion_stats$`10`)

list_stats_perc_above_be_unweighted <- calculate_statistics_on_percentages_above_be(nhanes_merged_dataset
                                                                                    , df_sector_collars_include
                                                                                    , list_inclusion_stats[["10"]]
                                                                                    , c("VNSECTORCOLLARCURR"
                                                                                        , "RIDAGEYR"
                                                                                        , "RIAGENDR"
                                                                                        , "RIDRETH1"
                                                                                        , "INDFMPIR"
                                                                                        , "SDDSRVYR"
                                                                                        , "VNSMOKING"))

list_stats_perc_above_be_weighted <- calculate_statistics_on_percentages_above_be(nhanes_merged_dataset
                                                                                  , df_sector_collars_include
                                                                                  , list_inclusion_stats[["10"]]
                                                                                  , c("VNSECTORCOLLARCURR"
                                                                                      , "RIDAGEYR"
                                                                                      , "RIAGENDR"
                                                                                      , "RIDRETH1"
                                                                                      , "INDFMPIR"
                                                                                      , "SDDSRVYR"
                                                                                      , "VNSMOKING")
                                                                                  , survey_weights)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Run Regression Models  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

regression_formulas <- c("chem_value ~ VNSECTORCOLLARCURR"
                         , "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR"
                         , "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR"
                         , "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1"
                         , "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR"
                         , "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR"
                         , "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR + VNSMOKING"
                         )

df_sector_collars_regressions_coeffies <- run_stepwise_regressions(nhanes_merged_dataset
                                                                   , regression_formulas
                                                                   , list_inclusion_stats$`10`
                                                                   , df_sector_collars_include
                                                                   , "coefficients")

df_sector_collars_regressions_coeffies_adj <- adjust_multiple_comparison(df_sector_collars_regressions_coeffies)

df_sector_collars_regressions_coeffies_weighted_lm <- run_stepwise_regressions(nhanes_merged_dataset
                                                                              , regression_formulas
                                                                              , list_inclusion_stats$`10`
                                                                              , df_sector_collars_include
                                                                              , "coefficients"
                                                                              , df_survey_weights = survey_weights
                                                                              , type_of_weighted_model = "lm")

df_sector_collars_regressions_coeffies_weighted_lm_adj <- adjust_multiple_comparison(df_sector_collars_regressions_coeffies_weighted_lm)


df_sector_collars_regressions_coeffies_weighted_svyglm <- run_stepwise_regressions(nhanes_merged_dataset
                                                                                   , regression_formulas
                                                                                   , list_inclusion_stats$`10`
                                                                                   , df_sector_collars_include
                                                                                   , "coefficients"
                                                                                   , df_survey_weights = survey_weights
                                                                                   , type_of_weighted_model = "svyglm")

df_sector_collars_regressions_coeffies_weighted_svyglm_adj <- adjust_multiple_comparison(df_sector_collars_regressions_coeffies_weighted_svyglm)

df_sector_collars_regressions_stats <- run_stepwise_regressions(nhanes_merged_dataset
                                                                , regression_formulas
                                                                , list_inclusion_stats$`10`
                                                                , df_sector_collars_include
                                                                , "models")

df_sector_collars_regressions_stats_weighted <- run_stepwise_regressions(nhanes_merged_dataset
                                                                , regression_formulas
                                                                , list_inclusion_stats$`10`
                                                                , df_sector_collars_include
                                                                , "models"
                                                                , df_survey_weights = survey_weights
                                                                , type_of_weighted_model = "svyglm")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~  Run Regression Models - Variance of Chemical Exposures Additionally Explained by Occupation  ~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

regression_formulas_additional <- c("chem_value ~ RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR + VNSMOKING"
                                    , "chem_value ~ RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR + VNSMOKING + VNSECTORCOLLARCURR")

df_additional_regressions_stats <- run_stepwise_regressions(nhanes_merged_dataset
                                                            , regression_formulas_additional
                                                            , list_inclusion_stats$`10`
                                                            , df_sector_collars_include
                                                            , "models")

df_additional_regressions_stats_weighted <- run_stepwise_regressions(nhanes_merged_dataset
                                                                     , regression_formulas_additional
                                                                     , list_inclusion_stats$`10`
                                                                     , df_sector_collars_include
                                                                     , "models"
                                                                     , df_survey_weights = survey_weights
                                                                     , type_of_weighted_model = "svyglm")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~  Run Regression Models - Sensitivity Analysis to Exclude Active Smokers  ~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

df_sector_collars_regressions_coeffies_no_smokers <- run_stepwise_regressions(nhanes_merged_dataset
                                                                              , regression_formulas
                                                                              , list_inclusion_stats$`10`
                                                                              , df_sector_collars_include
                                                                              , "coefficients"
                                                                              , "LBXCOT <= 4")

df_sector_collars_regressions_coeffies_no_smokers_adj <- adjust_multiple_comparison(df_sector_collars_regressions_coeffies_no_smokers)

df_sector_collars_regressions_stats_no_smokers <- run_stepwise_regressions(nhanes_merged_dataset
                                                                           , regression_formulas
                                                                           , list_inclusion_stats$`10`
                                                                           , df_sector_collars_include
                                                                           , "models"
                                                                           , "LBXCOT <= 4")

list_clusters_no_active_smokers <- run_clustering_analyses(df_sector_collars_regressions_coeffies_no_smokers_adj
                                                           , list_inclusion_stats
                                                           , regression_formulas
                                                           , c("10", "50"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Run Clustering Analysis  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

list_clusters_with_all <- run_clustering_analyses(df_sector_collars_regressions_coeffies_adj
                                                  , list_inclusion_stats
                                                  , regression_formulas
                                                  , c("10", "50"))

list_clusters_with_no_smoking <- run_clustering_analyses(df_sector_collars_regressions_coeffies_adj
                                                             , list_inclusion_stats
                                                             , regression_formulas
                                                             , c("10", "50")
                                                             , c("LBXCOT", "URXNAL"))



df_sector_collars_regressions_coeffies_weighted_adj <- df_sector_collars_regressions_coeffies_weighted_svyglm_adj %>%
  select("chemical_codename_use"
         , "term"
         , "estimate"                    
         , "excluded_occupational_groups"
         , "formula_eqt"                  
         , "fold_diff") %>%
  full_join(.
            , df_sector_collars_regressions_coeffies_weighted_lm_adj %>%
              select("chemical_codename_use"       
                     , "term"   
                     , "std.error"                  
                     , "statistic"                   
                     , "p.value"                  
                     , "excluded_occupational_groups" 
                     , "formula_eqt"                   
                     , "adj.p.values")
            , by = c("chemical_codename_use"       
                     , "term"
                     , "excluded_occupational_groups"
                     , "formula_eqt"))

list_clusters_with_all_weighted <- run_clustering_analyses(df_sector_collars_regressions_coeffies_weighted_adj
                                                           , list_inclusion_stats
                                                           , "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR + VNSMOKING"
                                                           , c("10"))

list_clusters_with_no_smoking_weighted <- run_clustering_analyses(df_sector_collars_regressions_coeffies_weighted_adj
                                                         , list_inclusion_stats
                                                         , "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR + VNSMOKING"
                                                         , c("10")
                                                         , c("LBXCOT", "URXNAL"))

list_clusters_logistic_with_no_smoking <- run_clustering_analyses(df_lgoistics_regressions_coeffies_adj
                                                                  , list_inclusion_stats
                                                                  , regression_formulas
                                                                  , c("50")
                                                                  # , c("URXMEP", "LBXVST", "LBXVTO")
                                                                  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Barplot of Detection Frequency by Chemical  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

barplot_detection_frequency_chem(df_detection_frequencies_updated
                                 , working_directory)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Barplot of Number of Participants by Chemical  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd(working_directory)
barplot_num_participants_by_chem(nhanes_merged_dataset
                                 , df_sector_collars_include
                                 , list_inclusion_stats$`50`
                                 , c("VNSECTORCOLLARCURR"
                                     , "RIDAGEYR"
                                     , "RIAGENDR"
                                     , "RIDRETH1"
                                     , "INDFMPIR"
                                     , "SDDSRVYR"
                                     , "VNSMOKING")
                                 , working_directory)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~  Barplot of Demographic Attribution in Sector-Collar Combinations  ~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


barplot_perc_participants_by_occupation(nhanes_merged_dataset
                                        , df_sector_collars_include
                                        , "VNSECTORCOLLARCURR"
                                        , "Barplot - Number of Participants by Sector-Collar"
                                        , working_directory
                                        , "unweighted")

setwd(working_directory)
barplot_perc_participants_by_occupation(nhanes_merged_dataset
                                        , df_sector_collars_include
                                        , "VNSECTORCOLLARCURR"
                                        , "Barplot - Number of Participants by Sector-Collar"
                                        , working_directory
                                        , "weighted")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Heatmap of Sparsity of Chemical Dataset  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd("/Volumes/Data/Occupational Exposome/OCCUPATION/Phase 6")
heatmap_sparsity_chemicals(nhanes_merged_dataset
                           , df_sector_collars_include
                           , "VNSECTORCOLLARCURR"
                           , list_inclusion_stats$`10`
                           , "Heatmap - Sparsity of Chemical Biomarkers Dataset"
                           , working_directory)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Boxplot of Cophenetic Coefficients  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


boxplot_cophenetic_coefficients(list_clusters_with_all
                                , regression_formulas
                                , "Boxplot - Cophenetic Coefficients"
                                , working_directory)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~  Boxplot of Chemical Biomarker by Occupational Group  ~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



chem_codenames_selected <- c("LBXBPB"
                             , "LBXVXY"
                             , "LBXCOT"
                             , "URX24D"
                             , "LBXGLY"
                             , "VNSUMDEHP")

setwd(working_directory)
boxplot_chem_occupation(nhanes_merged_dataset
                        , df_sector_collars_include
                        , list_inclusion_stats$`10`
                        , list_master_files$Chemicals
                        , "VNSECTORCOLLARCURR"
                        , "Boxplot - Weighted Chemical Distribution by Occupations"
                        , working_directory
                        , chem_codenames_selected
                        , motif_panel = "point"
                        , df_regression_stats = df_sector_collars_regressions_coeffies_weighted_adj
                        , analysis_type = "weighted"
                        , df_survey_weights = survey_weights)

boxplot_chem_occupation(nhanes_merged_dataset
                        , df_sector_collars_include
                        , list_inclusion_stats$`10`
                        , list_master_files$Chemicals
                        , "VNSECTORCOLLARCURR"
                        , "Boxplot - Chemical Distribution by Sector-Collars"
                        , working_directory
                        , chem_codenames_selected)


boxplot_chem_occupation(nhanes_merged_dataset
                        , df_sector_collars_include
                        , list_inclusion_stats$`10`
                        , list_master_files$Chemicals
                        , "VNSECTORCOLLARCURR"
                        , "Boxplot - Chemical Distribution by Sector-Collars Statistics Text"
                        , working_directory
                        , chem_codenames_selected
                        , "text"
                        , df_sector_collars_regressions_coeffies_adj)


boxplot_chem_occupation(nhanes_merged_dataset
                        , df_sector_collars_include
                        , list_inclusion_stats$`10`
                        , list_master_files$Chemicals
                        , "VNSECTORCOLLARCURR"
                        , "Boxplot - Chemical Distribution by Sector-Collars Statistics Points"
                        , working_directory
                        , chem_codenames_selected
                        , "point"
                        , df_sector_collars_regressions_coeffies_adj)

boxplot_chem_occupation(nhanes_merged_dataset
                        , df_sector_collars_include
                        , list_inclusion_stats$`10`
                        , list_master_files$Chemicals
                        , "VNSECTORCOLLARCURR"
                        , "Boxplot - Chemical Distribution by Sector-Collars Statistics Points"
                        , working_directory
                        , chem_codenames_selected
                        , "point"
                        , df_sector_collars_regressions_coeffies_adj)


boxplot_chem_occupation(nhanes_merged_dataset
                        , df_sector_collars_include
                        , list_inclusion_stats$`10`
                        , list_master_files$Chemicals
                        , "VNSECTORCOLLARCURR"
                        , "Boxplot Weighted - Chemical Distribution by Sector-Collars Statistics Points"
                        , working_directory
                        , chem_codenames_selected
                        , "point"
                        , df_sector_collars_regressions_coeffies_weighted_adj)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Alphabet Soup Plot of Prediction Performance  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


alphabet_soup_plot_prediction_performance_changes(df_sector_collars_regressions_stats
                                                  , "Alphabet Soup Plot - Prediction Performance"
                                                  , working_directory
                                                  , c("chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR"            
                                                      , "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR + VNSMOKING"))


alphabet_soup_plot_prediction_performance_changes(df_sector_collars_regressions_stats %>%
                                                    full_join(.
                                                              , df_additional_regressions_stats
                                                              , by = colnames(.))
                                                  , "Alphabet Soup Plot - Prediction Performance Univariate & Multivariable"
                                                  , working_directory
                                                  , c("chem_value ~ VNSECTORCOLLARCURR"
                                                      , regression_formulas_additional)
                                                  , "chem_value ~ VNSECTORCOLLARCURR"
                                                  , c(49, 54, 55))

setwd("~/Dropbox (University of Michigan)/Postdoc/Occupational Exposome/OCCUPATION/Phase 6")
alphabet_soup_plot_prediction_performance_changes(df_sector_collars_regressions_stats_weighted %>%
                                                    full_join(.
                                                              , df_additional_regressions_stats_weighted
                                                              , by = colnames(.))
                                                  , "Alphabet Soup Plot - Weighted Prediction Performance Univariate & Multivariable"
                                                  , working_directory
                                                  , selected_formulas_for_plot = c("chem_value ~ VNSECTORCOLLARCURR"
                                                                                   , regression_formulas_additional)
                                                  , selected_formulas_for_stats = regression_formulas_additional)


setwd("~/Dropbox (University of Michigan)/Postdoc/Occupational Exposome/OCCUPATION/Phase 6")
alphabet_soup_plot_prediction_performance_changes(df_sector_collars_regressions_stats_weighted
                                                  , "Alphabet Soup Plot - Weighted Smoking Contribution"
                                                  , working_directory
                                                  , selected_formulas_for_plot = c("chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR"            
                                                                                   , "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR + VNSMOKING")
                                                  )


alphabet_soup_plot_prediction_performance_changes(df_sector_collars_regressions_stats
                                                  , "Alphabet Soup Plot - Prediction Performance Univariate"
                                                  , working_directory
                                                  , "chem_value ~ VNSECTORCOLLARCURR")



alphabet_soup_plot_prediction_performance_changes(df_additional_regressions_stats
                                                  , "Alphabet Soup Plot - Prediction Performance Additional"
                                                  , working_directory
                                                  , regression_formulas_additional)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~  Heatmap of Clustering Occupations by Chemical Profiles   ~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

heatmap_clustering_occupations(list_clusters_with_all
                               , list_inclusion_stats[["10"]]
                               , regression_formulas
                               , "Heatmaps - Sector-Collar All Chemicals"
                               , working_directory
                               , file_name_motif = "all_chemicals")


heatmap_clustering_occupations(list_clusters_with_all
                               , list_inclusion_stats[["10"]]
                               , regression_formulas
                               , "Heatmaps - Sector-Collar No Smoking"
                               , working_directory
                               , file_name_motif = "no_smoking"
                               , excluded_chemicals = c("URXNAL"
                                                        , "LBXCOT"))


heatmap_clustering_occupations(list_clusters_with_no_smoking_weighted
                               , list_inclusion_stats[["10"]]
                               , "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR + VNSMOKING"
                               , "Heatmaps Weighted Combined svylgm and lm - Sector-Collar No Smoking"
                               , working_directory
                               , file_name_motif = "no_smoking"
                               , excluded_chemicals = c("URXNAL"
                                                        , "LBXCOT"))

setwd(working_directory)
heatmap_clustering_occupations(list_clusters_with_all_weighted
                               , list_inclusion_stats[["10"]]
                               , "chem_value ~ VNSECTORCOLLARCURR + RIDAGEYR + RIAGENDR + RIDRETH1 + INDFMPIR + SDDSRVYR + VNSMOKING"
                               , "Heatmaps Weighted Combined svylgm and lm - Sector-Collar All Chemicals No Smoking"
                               , working_directory
                               , file_name_motif = "all_chemicals"
                               , excluded_chemicals = c("URXNAL"
                                                        , "LBXCOT"))


heatmap_clustering_occupations(list_clusters_with_no_smoking
                               , list_inclusion_stats[["10"]]
                               , regression_formulas
                               , "Heatmaps - Sector-Collar No Smoking in Clustering"
                               , working_directory
                               , file_name_motif = "no_smoking_in_clustering")


heatmap_clustering_occupations(list_clusters_no_active_smokers
                               , list_inclusion_stats[["10"]]
                               , regression_formulas
                               , "Heatmaps - Sector-Collar No Active Smokers"
                               , working_directory
                               , file_name_motif = "no_active_smokers")


heatmap_clustering_occupations(list_clusters_no_active_smokers
                               , list_inclusion_stats[["10"]]
                               , regression_formulas
                               , "Heatmaps - Sector-Collar No Active Smokers No Smoking"
                               , working_directory
                               , file_name_motif = "no_active_smokers_no_smoking"
                               , excluded_chemicals = c("URXNAL"
                                                        , "LBXCOT"))

setwd("/Volumes/Data/Occupational Exposome/OCCUPATION/Phase 6")
heatmap_clustering_occupations(list_clusters_logistic_with_no_smoking
                               , list_inclusion_stats[["10"]]
                               , regression_formulas
                               , "Heatmaps - Sector-Collar Logistic"
                               , working_directory
                               , file_name_motif = "logistic")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~  CDF Plot of Prevalence above Biomonitoring Equivalents for Blood Lead  ~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


cdf_plot_above_be(nhanes_merged_dataset
             , df_sector_collars_include
             , list_inclusion_stats[["50"]]
             , c("VNSECTORCOLLARCURR"
                 , "RIDAGEYR"
                 , "RIAGENDR"
                 , "RIDRETH1"
                 , "INDFMPIR"
                 , "SDDSRVYR"
                 , "VNSMOKING")
             , "CDF Plots - Percentage above Biomonitoring Equivalents"
             , working_directory)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~  Heatmap of Percentage of Workers above Biomonitoring Equivalents   ~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd("/Volumes/Data/Occupational Exposome/OCCUPATION/Phase 6")
heatmap_percentage_above_be(nhanes_merged_dataset
                            , df_sector_collars_include
                            , list_inclusion_stats[["10"]]
                            , c("VNSECTORCOLLARCURR"
                                , "RIDAGEYR"
                                , "RIAGENDR"
                                , "RIDRETH1"
                                , "INDFMPIR"
                                , "SDDSRVYR"
                                , "VNSMOKING")
                            , "Heatmaps - Percentage above Biomonitoring Equivalents"
                            , working_directory)


heatmap_percentage_above_be(nhanes_merged_dataset
                            , df_sector_collars_include
                            , list_inclusion_stats[["10"]]
                            , c("VNSECTORCOLLARCURR"
                                , "RIDAGEYR"
                                , "RIAGENDR"
                                , "RIDRETH1"
                                , "INDFMPIR"
                                , "SDDSRVYR"
                                , "VNSMOKING")
                            , "Heatmaps - Percentage above Biomonitoring Equivalents"
                            , working_directory
                            , survey_weights)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~  Correlation Plot Comparing With and Without Smokers  ~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


correlation_plot_comparison(df_sector_collars_regressions_coeffies_adj
                            , df_sector_collars_regressions_coeffies_no_smokers_adj
                            , c("include_smokers"
                                , "exclude_smokers")
                            , "Correlation Plot - With and Without Smokers"
                            , working_directory)
