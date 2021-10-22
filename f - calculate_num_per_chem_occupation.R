calculate_num_per_chem_occupation <- function(occupation_dataset
                                              , chemicals_dataset
                                              , demographics_dataset
                                              , sector_collar_dataset
                                              , dataset_document_cleaning
                                              , threshold_sample_size)
{
  sector_collars_include <- sector_collar_dataset %>%
    filter(include == "yes") %>%
    select(VNSECTORCOLLARCURR) %>%
    unlist(., use.names = FALSE)
  
  occupation_dataset <- occupation_dataset %>%
    select("SEQN", "VNSECTORCOLLARCURR", "SDDSRVYR") %>%
    mutate(VNSECTORCOLLARCURR = ifelse(is.na(VNSECTORCOLLARCURR)
                                       , "Occupation Missing"
                                       , VNSECTORCOLLARCURR)) %>%
    filter(VNSECTORCOLLARCURR %in% sector_collars_include) %>%
    full_join(.
              , demographics_dataset[,c("SEQN", "RIDAGEYR")]
              , by = "SEQN") %>%
    filter(RIDAGEYR >= 16) %>%
    filter(SDDSRVYR <= 8)
  print(dim(occupation_dataset))
  
  chemical_codenames <- chemicals_dataset %>%
    colnames(.) %>%
    .[which(!(. %in% c("SEQN", "SDDSRVYR", "study_year")))] #%>%
    # .[1:5]
  # print(chemical_codenames)

  merged_dataset <- occupation_dataset %>%
    select("SEQN", "VNSECTORCOLLARCURR", "RIDAGEYR") %>% 
    left_join(.
              , chemicals_dataset
              , by = "SEQN")
  print(dim(merged_dataset))

  merged_long <- merged_dataset %>%
    select("SEQN", "VNSECTORCOLLARCURR", "RIDAGEYR", "SDDSRVYR", all_of(chemical_codenames)) %>%
    gather(., chemical_codename_use, value, all_of(chemical_codenames)) %>%
    na.omit(.)
  # View((merged_long))
  
  df_num_per_chem_group <- merged_long %>%
    group_by(VNSECTORCOLLARCURR,chemical_codename_use) %>%
    summarise(num = n()) %>%
    ungroup(.) %>%
    left_join(.
              , dataset_document_cleaning[,c("comments_codename_use"
                                             , "chemical_codename_use"
                                             , "chemical_name"
                                             , "units"
                                             , "chem_family"
                                             , "chem_family_shortened")] %>%
                unique(.)
              , by = "chemical_codename_use") 
  
  # View(df_num_per_chem_group)

  df_median_size_per_chem <- df_num_per_chem_group %>%
    group_by(chemical_codename_use) %>%
    summarise(median = median(num)) %>%
    ungroup(.) %>%
    mutate(include = ifelse(median >= threshold_sample_size
                            , "yes"
                            , "no")) %>%
    left_join(.
              , dataset_document_cleaning[,c("comments_codename_use"
                                             , "chemical_codename_use"
                                             , "chemical_name"
                                             , "units"
                                             , "chem_family"
                                             , "chem_family_shortened")] %>%
                unique(.)
              , by = "chemical_codename_use") 
  # View(df_median_size_per_chem)

  list_num_per_chem_and_group <- list("num_per_chem_and_group" = df_num_per_chem_group
                                      , "median_per_chem" = df_median_size_per_chem)

  return(list_num_per_chem_and_group)
  
}