calculate_table_1_statistics <- function(merged_dataset
                                         , include_sector_collar_dataset)
{
  library("tidyverse")
  library("survey")
  
  vector_sector_collar_exclude <- include_sector_collar_dataset %>%
    filter(include == "yes") %>%
    pull(VNSECTORCOLLARCURR) 
  
  merged_subset <- merged_dataset %>%
    filter(VNSECTORCOLLARCURR %in% vector_sector_collar_exclude)
  print(dim(merged_subset))
  
  num_participants <- merged_subset %>%
    nrow(.)
  print(paste("Number of participants:", num_participants))
  
  df_num_cycles <- merged_subset %>%
    group_by(SDDSRVYR) %>%
    summarise(num = n()
              , perc = num/num_participants*100) %>%
    ungroup(.)
  print(df_num_cycles)
  
  df_num_sex <- merged_subset %>%
    group_by(RIAGENDR) %>%
    summarise(num = n()
              , perc = num/num_participants*100) %>%
    ungroup(.)
  print(df_num_sex)
  
  df_num_race <- merged_subset %>%
    group_by(RIDRETH1) %>%
    summarise(num = n()
              , perc = num/num_participants*100) %>%
    ungroup(.)
  print(df_num_race)
  
  df_num_age <- merged_subset %>%
    summarise(num = n()
              , min = min(RIDAGEYR)
              , perc = num/num_participants*100
              , perc_5th = quantile(RIDAGEYR, probs = 0.05)
              , perc_10th = quantile(RIDAGEYR, probs = 0.10)
              , median = quantile(RIDAGEYR, probs = 0.50)
              , mean = mean(RIDAGEYR) %>%
                round(., digits = 1)
              , sd = sd(RIDAGEYR)  %>%
                round(., digits = 1)
              , perc_90th = quantile(RIDAGEYR, probs = 0.90)
              , perc_95th = quantile(RIDAGEYR, probs = 0.95)
              , max = max(RIDAGEYR))
  print(df_num_age)
  
  df_num_pir <- merged_subset %>%
    filter(!is.na(INDFMPIR)) %>%
    summarise(num = n()
              , perc = num/num_participants*100
              , min = min(INDFMPIR)
              , perc_5th = quantile(INDFMPIR, probs = 0.05)
              , perc_10th = quantile(INDFMPIR, probs = 0.10)
              , median = quantile(INDFMPIR, probs = 0.50)
              , mean = mean(INDFMPIR) %>%
                round(., digits = 1)
              , sd = sd(INDFMPIR)  %>%
                round(., digits = 1)
              , perc_90th = quantile(INDFMPIR, probs = 0.90)
              , perc_95th = quantile(INDFMPIR, probs = 0.95)
              , max = max(INDFMPIR))
  print(df_num_pir)
  
  df_num_smoking <- merged_subset %>%
    filter(!is.na(VNSMOKING)) %>%
    summarise(num = n()
              , perc = num/num_participants*100
              , min = min(VNSMOKING)
              , perc_5th = quantile(VNSMOKING, probs = 0.05)
              , perc_10th = quantile(VNSMOKING, probs = 0.10)
              , median = quantile(VNSMOKING, probs = 0.50)
              , mean = mean(VNSMOKING) %>%
                round(., digits = 1)
              , sd = sd(VNSMOKING)  %>%
                round(., digits = 1)
              , perc_90th = quantile(VNSMOKING, probs = 0.90)
              , perc_95th = quantile(VNSMOKING, probs = 0.95)
              , max = max(VNSMOKING))
  print(df_num_smoking)
  
  nhanes_design <- merged_subset %>%
    select("SEQN"
           , "VNSECTORCOLLARCURR"
           , "RIDRETH1"
           , "RIAGENDR"
           , "RIDAGEYR"
           , "INDFMPIR"
           , "VNSMOKING"
           , "SDDSRVYR"
           , "WTMECADJ"
           , "SDMVPSU"
           , "SDMVSTRA") %>%
    svydesign(ids = ~SDMVPSU
              , strata = ~SDMVSTRA
              , weights = ~WTMECADJ
              , data = .
              , nest = TRUE)
  # print(str(nhanes_design))
  
  df_num_race_weighted <- svytable(~RIDRETH1
                                   , nhanes_design)
  # print(df_num_race_weighted)
  
  df_perc_race_weighted <- df_num_race_weighted/sum(df_num_race_weighted)*100 %>%
    round(., digits = 2)
  print(df_perc_race_weighted)
  
  df_num_sex_weighted <- svytable(~RIAGENDR
                                   , nhanes_design)
  # print(df_num_race_weighted)
  
  df_perc_sex_weighted <- df_num_sex_weighted/sum(df_num_sex_weighted)*100 %>%
    round(., digits = 2)
  print(df_perc_sex_weighted)
  
  df_num_cycle_weighted <- svytable(~SDDSRVYR
                                  , nhanes_design)
  # print(df_num_race_weighted)
  
  df_perc_cycle_weighted <- df_num_cycle_weighted/sum(df_num_cycle_weighted)*100 %>%
    round(., digits = 2)
  print(df_perc_cycle_weighted)
  
  
  # df_num_age_weighted <- svyquantile(x = ~RIDAGEYR
  #                                    , design = nhanes_design
  #                                    , quantiles = c(0, 0.05, 0.1, 0.5, 0.9, 0.99, 1)
  #                                    , na.rm = TRUE)
  # print(df_num_age_weighted)
  # 
  # df_mean_age_weighted <- svymean(x = merged_subset %>%
  #                                   pull(RIDAGEYR)
  #                                 , design = nhanes_design)
  # print(df_mean_age_weighted)
  # # 
  # # print(unique(merged_subset$INDFMPIR))
  # 
  # df_num_pir_weighted <- svyquantile(x = ~INDFMPIR
  #                                    , design = nhanes_design
  #                                    , quantiles = c(0, 0.05, 0.1, 0.5, 0.9, 0.99, 1)
  #                                    , na.rm = TRUE)
  # print(df_num_pir_weighted)
  # 
  # df_mean_pir_weighted <- svymean(x = merged_subset %>%
  #                                   pull(INDFMPIR)
  #                                 , design = nhanes_design
  #                                 , na.rm = TRUE)
  # print(df_mean_pir_weighted)
  # 
  # df_num_smoking_weighted <- svyquantile(x = ~VNSMOKING
  #                                    , design = nhanes_design
  #                                    , quantiles = c(0, 0.05, 0.1, 0.5, 0.9, 0.99, 1)
  #                                    , na.rm = TRUE)
  # print(df_num_smoking_weighted)
  # 
  # df_mean_smoking_weighted <- svymean(x = merged_subset %>%
  #                                   pull(VNSMOKING)
  #                                 , design = nhanes_design
  #                                 , na.rm = TRUE)
  # print(df_mean_smoking_weighted)
  
}