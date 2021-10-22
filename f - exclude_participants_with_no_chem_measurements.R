exclude_participants_with_no_chem_measurements <- function(dataset_merged
                                                           , vector_chem_codenames)
{
  merged_long <- dataset_merged %>%
    select("SEQN", "VNSECTORCOLLARCURR", all_of(vector_chem_codenames)) %>%
    gather(., chemical_codename_use, value, all_of(vector_chem_codenames)) %>%
    na.omit(.)
  
  df_counting_stats <- merged_long %>%
    group_by(SEQN) %>%
    summarise(num = n()) %>%
    ungroup(.)
  
  View(df_counting_stats)
}