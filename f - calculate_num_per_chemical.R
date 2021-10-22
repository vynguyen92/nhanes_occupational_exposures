calculate_num_per_chemical <- function(merged_dataset
                                       , include_sector_collar_dataset
                                       , include_chemical_dataset)
{
  vector_sector_collar_exclude <- include_sector_collar_dataset %>%
    filter(include == "yes") %>%
    pull(VNSECTORCOLLARCURR) 
  
  vector_chemicals_include <- include_chemical_dataset %>%
    filter(include == "yes") %>%
    pull(chemical_codename)
  
  # print(colnames(merged_dataset))
  
  merged_subset <- merged_dataset %>%
    filter(VNSECTORCOLLARCURR %in% vector_sector_collar_exclude) %>%
    filter(RIDAGEYR >= 16) %>%
    filter(SDDSRVYR <= 8) %>%
    select("SEQN"
           , all_of(vector_chemicals_include)
           , "VNSECTORCOLLARCURR"
           , "RIDAGEYR"
           , "RIAGENDR"
           , "RIDRETH1"
           , "INDFMPIR"
           , "SDDSRVYR"
           , "VNSMOKING")
  # print(dim(merged_subset))
  
  merged_subset_long <- merged_subset %>%
    pivot_longer(cols = all_of(vector_chemicals_include)
                 , names_to = "chemical_codename"
                 , values_to = "concentration") %>%
    na.omit(.)
  # print(str(merged_subset_long))
  
  df_num_participants_per_chemical <- merged_subset_long %>%
    group_by(chemical_codename) %>%
    summarise(num = n()) %>%
    left_join(.
              , include_chemical_dataset %>%
                select("chemical_codename"
                       , "chemical_name"          
                       , "chem_family"            
                       , "chem_family_shortened")
              , by = "chemical_codename")
  # View(df_num_participants_per_chemical)
  
  write_excel_csv(df_num_participants_per_chemical
                  , "NHANES - Number of Participants per Chemical 1a.csv")
  
}