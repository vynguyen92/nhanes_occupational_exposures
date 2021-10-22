decide_chemicals_to_include <- function(list_detect_freq
                                        , list_sample_size
                                        , threshold_detect_freq
                                        , threshold_sample_size)
{
  df_inclusion <- list_detect_freq[["average"]] %>%
    rename("include_freq" = "include") %>%
    select("chemical_codename"
           , "num_above_LOD"
           , "num_below_LOD"
           , "num_exceeding_LOD"
           , "total_participants"     
           , "perc_above_LOD"
           , "perc_below_LOD"
           , "perc_exceeding_LOD"
           , "num_sector_collars"  
           , "variance_across_groups"
           , "mean_across_groups"
           , "include_freq") %>%
    full_join(.
              , list_sample_size[["median_per_chem"]] %>%
                rename("chemical_codename" = "chemical_codename_use") %>%
                rename("include_sample_size" = "include")
              , by = "chemical_codename") %>%
    mutate(include = case_when(include_freq == "yes" & include_sample_size == "yes" ~ "yes"
                               , include_freq == "no" & include_sample_size == "yes" ~ "no"
                               , include_freq == "yes" & include_sample_size == "no" ~ "no"
                               , include_freq == "no" & include_sample_size == "no" ~ "no"
                               , is.na(include_freq) == TRUE & include_sample_size == "no" ~ "no"
                               , is.na(include_freq) == TRUE & include_sample_size == "yes" ~ "yes")) %>%
    mutate(chemical_name = mgsub(string = chemical_name
                                 , pattern = paste(" \\", units,"$", sep = "")
                                 , replacement = rep("", nrow(.)))) %>%
    select(-units) %>%
    unique(.) %>%
    mutate(reason_detect_freq = case_when(perc_above_LOD >= threshold_detect_freq ~ ""
                                          , perc_above_LOD < threshold_detect_freq ~ paste("detection frequency <", threshold_detect_freq)
                                          , is.na(perc_above_LOD) == TRUE ~ "")) %>%
    mutate(reason_sample_size = ifelse(median >= threshold_sample_size
                                       , ""
                                       , paste("sample size <", threshold_sample_size))) %>%
    mutate(reason_lipid_adjust = case_when(grepl("Lipid Adj$|lipid adj$", chemical_name) == TRUE ~ ""
                                           , grepl("Lipid Adj$|lipid adj$", chemical_name) == FALSE & 
                                             chem_family_shortened %in% c("PCBs", "Dioxins", "Furans", "BFRs") ~ "NOT adjusted for blood lipids"
                                           , grepl("Lipid Adj$|lipid adj$", chemical_name) == FALSE & 
                                             !(chem_family_shortened %in% c("PCBs", "Dioxins", "Furans", "BFRs")) ~ ""))
  
  pesticides_lipid_adjusted <- df_inclusion %>%
    filter(chem_family_shortened == "Pesticides") %>%
    filter(reason_lipid_adjust == "adjusted for blood lipids") %>%
    pull(chemical_name) %>%
    gsub("\\sLipid Adj$", "", .)
  
  index_codename_non_adjusted_pesticides <- which(df_inclusion$chemical_name %in% pesticides_lipid_adjusted)
  
  num_codenames_non_lipid_adjusted <- length(index_codename_non_adjusted_pesticides)
  
  df_inclusion[index_codename_non_adjusted_pesticides, "reason_lipid_adjust"] <- rep("NOT adjusted for blood lipids"
                                                                                     , num_codenames_non_lipid_adjusted)
  
  df_inclusion <- df_inclusion %>%
    mutate(include = ifelse(chem_family == "Creatinine"
                            , "no"
                            , include)) %>%
    mutate(reason_no_exposures = ifelse(chem_family == "Creatinine"
                                        , "Not exposure biomarker"
                                        , "")) %>%
    mutate(reason_to_exclude = paste(reason_detect_freq
                                     , reason_sample_size
                                     , reason_lipid_adjust
                                     , reason_no_exposures
                                     , sep = ", ")) %>%
    mutate(reason_to_exclude = gsub(", , , |^, |, , $|, $", "", reason_to_exclude))
  
  df_inclusion[df_inclusion == ""] <- NA
  
  df_inclusion <- df_inclusion %>%
    select(-c(reason_detect_freq
              , reason_sample_size
              , reason_no_exposures
              , reason_lipid_adjust))
  
  # View(df_inclusion)
  
  return(df_inclusion)
}