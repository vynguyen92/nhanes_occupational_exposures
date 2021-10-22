ensure_reasonable_comments <- function(dataset_new
                                       , dataset_old
                                       , df_doc_cleaning)
{
  not_comments_codename <- c("SEQN", "SDDSRVYR")
  
  codenames <- dataset_new %>%
    select(-one_of(not_comments_codename)) %>%
    colnames(.)
  # print(codenames)
  
  # codenames <- c("URDMHPLC", "LBDMFOSL")
  num_comments <- length(codenames)
  
  for(i in seq(num_comments))
  {
    comment_codename_i <- codenames[i]
    # print(comment_codename_i)
    
    unique_comments <- dataset_new[,comment_codename_i] %>%
      unique(.) %>%
      unlist(., use.names = FALSE) %>%
      .[!is.na(.)]
    # print(unique_comments)
    
    exist_comment_unreasonable <- which(unique_comments > 2) %>%
      length(.)
    # print(exist_comment_unreasonable)
    
    if(exist_comment_unreasonable > 0)
    {
      comment_unreasonable <- which(unique_comments > 2) %>%
        unique_comments[.]
      # print(comment_unreasonable)
      
      subset_doct_cleaning <- df_doc_cleaning %>%
        filter(comments_codename_use == comment_codename_i)
      
      chemical_codename <- subset_doct_cleaning %>%
        select(chemical_codename_use) %>%
        unique(.) %>%
        unlist(., use.names = FALSE)
      # print(chemical_codename)
      
      index_participant_affected <- which(dataset_new[,comment_codename_i] == comment_unreasonable)
      # print(index_participant_affected)
      
      cycle_affected <- dataset_new[index_participant_affected,"SDDSRVYR"] %>%
        unique(.)
      # print(cycle_affected)
      
      lod <- subset_doct_cleaning %>%
        filter(SDDSRVYR == cycle_affected) %>%
        select(LOD) %>%
        unique(.) %>%
        unlist(., use.names = FALSE)
      # print(lod)
      
      measurements <- dataset_old[index_participant_affected, chemical_codename]
      # print(measurements)
      
      dataset_new[index_participant_affected, comment_codename_i] <- ifelse(measurements > lod
                                                                            , 1
                                                                            , 0)
        
    }

  }
  return(dataset_new)
}