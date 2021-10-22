merge_datasets_together <- function(occupational_dataset
                                    , chemical_dataset
                                    , demographic_dataset
                                    , df_stats
                                    , include_sector_collar_dataset)
{
  # View(include_sector_collar_dataset)
  
  vector_sector_collar_exclude <- include_sector_collar_dataset %>%
    filter(include == "no") %>%
    pull(VNSECTORCOLLARCURR) 
  # print(vector_sector_collar_include)
  
  occupational_dataset <- occupational_dataset %>%
    select("SEQN"
           , "VNINDUSTRYABBREV"
           , "VNINDUSTRYOLD"
           , "VNINDUSTRY"
           , "VNCURRJOBABBREV"
           , "VNBWCURRJOB"
           , "VNCURRJOBOLD"
           , "VNCURRJOB"
           , "VNSECTORCOLLARCURR") %>%
    mutate(VNBLUETOGCURR = ifelse(grepl("Blue", VNSECTORCOLLARCURR) == TRUE
                                  , "Blue"
                                  , VNSECTORCOLLARCURR))

  demographic_dataset <- demographic_dataset %>%
    select("SEQN"
           , "RIDAGEYR"
           , "RIAGENDR"
           , "RIDRETH1"
           , "INDFMPIR"
           , "SDDSRVYR"
           , "WTMEC2YR"
           , "WTMEC4YR"
           , "WTINT2YR"
           , "WTINT4YR"
           , "SDMVPSU"
           , "SDMVSTRA") %>%
    mutate(WTMECADJ = ifelse(SDDSRVYR %in% c(1,2)
                             , WTMEC4YR*(2/8)
                             , WTMEC2YR*(1/8))) %>%
    mutate(WTINTADJ = ifelse(SDDSRVYR %in% c(1,2)
                             , WTINT4YR*(2/8)
                             , WTINT2YR*(1/8))) %>%
    mutate(RIDRETH1 = case_when(RIDRETH1 == 1 ~ "_mexican_americans"
                                , RIDRETH1 == 2 ~ "_other_hispanics"
                                , RIDRETH1 == 3 ~ "_whites"
                                , RIDRETH1 == 4 ~ "_blacks"
                                , RIDRETH1 == 5 ~ "_other_race")) %>%
    mutate(RIDRETH1 = relevel(factor(RIDRETH1)
                              , ref = "_whites")) %>%
    mutate(RIAGENDR = ifelse(RIAGENDR == 1
                             , "_males"
                             , "_females")) %>%
    mutate(RIAGENDR = relevel(factor(RIAGENDR)
                              , ref = "_males"))
 

  # View(df_stats)

  vector_chem_codenames_include <- df_stats %>%
    filter(include == "yes") %>%
    select(chemical_codename) %>%
    unlist(., use.names = FALSE)
  # print(vector_chem_codenames_include)

  # print(vector_chem_codenames_include %>% length(.))
  
  chemical_dataset <- chemical_dataset %>%
    select("SEQN", all_of(vector_chem_codenames_include))

  merged_dataset <- occupational_dataset %>%
    full_join(.
              , demographic_dataset
              , by = "SEQN") %>%
    full_join(.
              , chemical_dataset 
              , by = "SEQN") %>%
    mutate(VNSMOKING = LBXCOT) %>%
    # filter(RIDAGEYR >= 16) %>%
    filter(SDDSRVYR <= 8) %>%
    mutate(VNSECTORCOLLARCURR = if_else(is.na(VNSECTORCOLLARCURR) & RIDAGEYR >= 16
                                       , "Occupation Missing"
                                       , VNSECTORCOLLARCURR)) 
  
  return(merged_dataset)
}