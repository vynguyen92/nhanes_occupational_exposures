adjust_multiple_comparison <- function(df_coefficients)
{
  types_of_analysis <- df_coefficients %>%
    pull(excluded_occupational_groups) %>%
    unique(.)
  # print(types_of_analysis)
  
  num_analyses <- length(types_of_analysis)
  
  for(i in seq(num_analyses))
  {
    type_of_analysis_i <- types_of_analysis[i]
    print(type_of_analysis_i)
    
    subset_coefficients_i <- df_coefficients %>%
      filter(excluded_occupational_groups == type_of_analysis_i) 
    
    vector_pvalues_i <- subset_coefficients_i %>%
      pull(p.value)
    
    adjusted_pvalues <- vector_pvalues_i %>%
      p.adjust(.
               , method = "BH")
    
    subset_coefficients_i <- subset_coefficients_i %>%
      mutate(adj.p.values = adjusted_pvalues)
    
    if(i == 1)
    {
      df_coefficients_updated <- subset_coefficients_i
    } else {
      df_coefficients_updated <- df_coefficients_updated %>%
        full_join(.
                  , subset_coefficients_i
                  , by = colnames(.))
    }
  }
  
  return(df_coefficients_updated)
}