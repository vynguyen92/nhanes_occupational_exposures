determine_num_participants_by_comments_and_chemicals <- function(chemicals_dataset
                                                                 , comments_dataset
                                                                 , doc_cleaning_list)
{
  
  type_codenames <- c("chemical_codename_use", "comments_codename_use") #%>%
    # .[2]
  num_types <- length(type_codenames)
  
  for(i in seq(num_types))
  {
    type_codename_i <- type_codenames[i]
    # print(type_codename_i)
    
    codenames <-  doc_cleaning_list$Chemicals %>%
      select(all_of(type_codename_i)) %>%
      unique(.) %>%
      unlist(., use.names = FALSE) %>%
      .[!is.na(.)] #%>%
      # .[1:10]
    # print(codenames)
    
    if(type_codename_i == "chemical_codename_use")
    {
      dataset_interest <- chemicals_dataset
      
    } else if(type_codename_i == "comments_codename_use") {
      
      dataset_interest <- comments_dataset
    }
    
    long_dataset <- dataset_interest %>%
      select("SEQN", all_of(codenames)) %>%
      gather(., type_codename_i, value, all_of(codenames)) %>%
      na.omit(.)
    # print(str(long_dataset))

    df_counts <- long_dataset %>%
      group_by(type_codename_i) %>%
      summarise(num = n()) %>%
      ungroup(.)
    # print(df_counts)

    index_rename <- which(colnames(df_counts) == "type_codename_i")

    colnames(df_counts)[index_rename] <- type_codename_i
    # print(df_counts)

    if(type_codename_i == "comments_codename_use")
    {
      df_counts <- df_counts %>%
        left_join(.
                  , doc_cleaning_list$Chemicals %>%
                    select(comments_codename_use, chemical_codename_use, chemical_name) %>%
                    unique(.)
                  , by = "comments_codename_use")
    }
    # View(df_counts)

    if(i == 1)
    {
      df_counts_merged <- df_counts
    } else {
      df_counts_merged <- full_join(df_counts
                                    , df_counts_merged
                                    , by = "chemical_codename_use")
    }
  }
 
  
  df_counts_merged <- df_counts_merged %>%
    mutate(diff = abs(num.x - num.y))
  # View(df_counts_merged)
  
  
  return(df_counts_merged)
}