determine_sector_collars_include <- function(occupation_dataset
                                             , chemicals_dataset
                                             , demographics_dataset
                                             , threshold)
{
  occupation_dataset <- occupation_dataset %>%
    select("SEQN", "VNSECTORCOLLARCURR", "SDDSRVYR") %>%
    full_join(.
              , demographics_dataset[,c("SEQN", "RIDAGEYR", "RIAGENDR", "RIDRETH1", "INDFMPIR")]
              , by = "SEQN") %>%
    full_join(.
              , chemicals_dataset %>% select(-SDDSRVYR)
              , by = "SEQN") %>%
    filter(RIDAGEYR >= 16) %>%
    filter(SDDSRVYR <= 8) %>%
    mutate(VNSECTORCOLLARCURR = ifelse(is.na(VNSECTORCOLLARCURR)
                                       , "Occupation Missing"
                                       , VNSECTORCOLLARCURR))
  print(dim(occupation_dataset))
 
  
  df_count <- occupation_dataset %>%
    group_by(VNSECTORCOLLARCURR) %>%
    summarise(num_participants = n()) %>%
    mutate(include = ifelse(is.na(VNSECTORCOLLARCURR) == FALSE & num_participants > threshold
                            , "yes"
                            , "no"))
  # View(df_count)
  
  included_occupations <- df_count %>%
    filter(include == "yes") %>%
    pull(VNSECTORCOLLARCURR)
  
  occupation_dataset <- occupation_dataset %>%
    filter(VNSECTORCOLLARCURR %in% included_occupations)
  print(dim(occupation_dataset))

  return(df_count)
}