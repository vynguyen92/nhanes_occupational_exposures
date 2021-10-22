run_clustering_analyses <- function(df_regressions_coefficients
                                    , list_chemical_include
                                    , string_formulas
                                    , vector_string_detect_freq
                                    , excluded_chemicals = NA)
{
  
  excluded_occupation_groups <- df_regressions_coefficients %>%
    pull(excluded_occupational_groups) %>%
    unique(.) #%>%
    # .[1]
  # print(excluded_occupation_groups)
  
  num_excluded_occupation_groups <- length(excluded_occupation_groups)
  
  num_sensitivity_on_detect_freq <- length(vector_string_detect_freq)
  
  string_formulas <- string_formulas#[1]
  
  num_formula <- length(string_formulas)
  
  list_clustering_analyses <- list()
  
  for(i in seq(num_sensitivity_on_detect_freq))
  {
    detect_freq_i <- vector_string_detect_freq[i]
    # print(detect_freq_i)

    included_chemicals_i <- list_chemical_include[[detect_freq_i]] %>%
      filter(include == "yes") %>%
      pull(chemical_codename)
    # print(included_chemicals_i)

    subset_regression_coeffies_i <- df_regressions_coefficients %>%
      filter(chemical_codename_use %in% included_chemicals_i) %>%
      filter(!(chemical_codename_use %in% excluded_chemicals))
    
    # View(subset_regression_coeffies_i)

    for(j in seq(num_formula))
    {
      formula_j <- string_formulas[j]
      print(formula_j)

      for(k in seq(num_excluded_occupation_groups))
      {
        excluded_occup_group_k <- excluded_occupation_groups[k]

        name_clustering_results <- paste(detect_freq_i
                                         , j
                                         , excluded_occup_group_k
                                         , sep = " - ")

        print(name_clustering_results)

        list_clustering_analyses[[name_clustering_results]] <- cluster_occupations(subset_regression_coeffies_i
                                                                                   , formula_j
                                                                                   , excluded_occup_group_k)

      }
    }
  }
  
  return(list_clustering_analyses)
}