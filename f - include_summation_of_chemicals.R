include_summation_of_chemicals <- function(dataset_new
                                           , df_doc_cleaning
                                           , motif
                                           , dataset_old = NA)
{
  if(motif == "comments")
  {
    codename_to_use <- "comments_codename_use"
    dataset_use <- dataset_old
    
  } else if(motif == "chemicals") {
    codename_to_use <- "chemical_codename_use"
    dataset_use <- dataset_new
  }
  
  subset_doc_cleaning <- df_doc_cleaning %>%
    filter(is.na(codenames_for_sum) == FALSE)
  
  # View(subset_doc_cleaning)
  # print(colnames(dataset_use))
  
  affected_codenames <- subset_doc_cleaning %>%
    pull("chemical_codename_use") %>%
    unique(.) 
  # print(affected_codenames)
  
  num_affected_codenames <- length(affected_codenames)
  
  for(i in seq(num_affected_codenames))
  {
    affected_codename_i <- affected_codenames[i]
    # print(affected_codename_i)
    
    index_combine_exist <- which(subset_doc_cleaning[,"chemical_codename_use"] == affected_codename_i)
    
    subset_cleaning_i <- subset_doc_cleaning[index_combine_exist, ]
    # print(subset_cleaning_i)
    
    codenames_need_for_sum <- subset_cleaning_i %>%
      pull("codenames_for_sum") %>%
      unique(.) %>%
      strsplit(., split = ",") %>%
      unlist(.)
    # print(codenames_need_for_sum)
    
    df_summation <- dataset_use %>%
      select("SEQN"
             , all_of(codenames_need_for_sum)) 
    # View(df_summation)
    
    summation <- dataset_use %>%
      select(all_of(codenames_need_for_sum)) %>%
      rowSums(.)
    # View(summation)
    
    df_summation <- df_summation %>%
      mutate(sum = summation)
    # View(df_summation %>%
    #        select(all_of(codenames_need_for_sum), "sum") %>% unique(.))
    
    dataset_use <- dataset_use %>%
      left_join(.
                , df_summation %>%
                  select("SEQN"
                         , "sum")
                , by = "SEQN") 
    
    
    index_sum <- which(colnames(dataset_use) == "sum")
    colnames(dataset_use)[index_sum] <- affected_codename_i
    # print(colnames(dataset_use))
    # print(which(colnames(dataset_use) == affected_codename_i))
    # View(dataset_use)
    
    if(motif == "comments")
    {
      # print(colnames(subset_cleaning_i))
      comment_codename_use_i <- subset_cleaning_i %>%
        pull("comments_codename_use") %>%
        unique(.)
      # print(comment_codename_use_i)

      affected_cycles <- subset_cleaning_i %>%
        pull(SDDSRVYR) %>%
        unique(.)
      # print(affected_cycles)

      num_affected_cycles <- length(affected_cycles)

      for(j in seq(num_affected_cycles))
      {
        affected_cycle_j <- affected_cycles[j]
        # print(affected_cycle_j)

        subset_affected_cycle_j <- subset_cleaning_i %>%
          filter(SDDSRVYR == affected_cycle_j)
        # print(subset_affected_cycle_j)

        index_cycle_affected <- which(dataset_use[,"study_year"] == affected_cycle_j)
        # print(index_cycle_affected)

        new_lod <- subset_affected_cycle_j %>%
          pull(LOD) %>%
          unique(.)
        # print(new_lod)


        new_comments <- ifelse(dataset_use[index_cycle_affected,affected_codename_i] >= new_lod
                               , 0
                               , 1)
        # print(new_comments)


        dataset_new[index_cycle_affected, comment_codename_use_i] <- new_comments

      }
      # print(colnames(dataset_new))
      
      # dataset_new %>%
      #   select("SEQN"
      #          , all_of(comment_codename_use_i)) %>%
      #   left_join(.
      #             , dataset_use %>%
      #               select("SEQN"
      #                      , all_of(affected_codename_i)
      #                      , "study_year")
      #             , by = "SEQN") %>%
      #   select(all_of(comment_codename_use_i)
      #          , all_of(affected_codename_i)
      #          , "study_year") %>%
      #   unique(.) %>%
      #   filter(study_year == 9) %>%
      #   View(.)
    }
    
  }
  # print(colnames(dataset_new))  
  return(dataset_new)
}