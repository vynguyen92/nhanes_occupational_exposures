cluster_occupations <- function(df_regressions
                                , formula_interest
                                , excluded_group_interest
                                , excluded_chemicals = NA)
{
  # print(colnames(df_regressions))
  library(pvclust)
  
  subset_regressions <- df_regressions %>%
    filter(formula_eqt == formula_interest) %>%
    filter(excluded_occupational_groups == excluded_group_interest) %>%
    filter(!(term %in% c("(Intercept)"
                         , "INDFMPIR"
                         , "RIAGENDR_females"
                         , "RIDAGEYR"
                         , "RIDRETH1_mexican_americans"
                         , "RIDRETH1_blacks"
                         , "RIDRETH1_other_hispanics"
                         , "RIDRETH1_other_race"
                         , "SDDSRVYR"
                         , "VNSMOKING")))
  # View(subset_regressions)
  
  df_coeffi <- subset_regressions %>%
    select("chemical_codename_use","term", "estimate") %>%
    na.omit(.) %>%
    spread(., term, estimate)
  # View(df_coeffi)
  
  df_p_values <- subset_regressions %>%
    select("chemical_codename_use","term", "adj.p.values") %>%
    spread(., term, adj.p.values)
  # View(df_p_values)
  
  df_p_values_asterisks <- subset_regressions %>%
    select("chemical_codename_use","term", "adj.p.values") %>%
    mutate(asterisks = case_when(adj.p.values > 0.05 ~ ""
                                 , adj.p.values > 0.01 & adj.p.values <= 0.05 ~ "*"
                                 , adj.p.values > 0.001 & adj.p.values <= 0.01 ~ "**"
                                 , adj.p.values <= 0.001 ~ "***")) %>%
    select("chemical_codename_use","term", "asterisks") %>%
    spread(., term, asterisks)
  # View(df_p_values_asterisks)
  # View(df_coeffi)
  
  if(anyNA(excluded_chemicals) == FALSE)
  {
    chem_codenames <- df_coeffi[,"chemical_codename_use"] %>%
      unlist(., use.names = FALSE)

    index_excluded_chemicals <- which(chem_codenames %in% excluded_chemicals)

    df_coeffi <- df_coeffi[-index_excluded_chemicals,]
  }

  codenames <- df_coeffi %>%
    pull(chemical_codename_use) 

  df_coeffi[,1] <- NULL
  # View(df_coeffi)

  # print(scale(unlist(df_coeffi[1,], use.names = FALSE)))
  sector_collars <- colnames(df_coeffi)
  # print(sector_collars)

  rownames(df_coeffi) <- codenames
  # View(df_coeffi)

  df_coeffi_scale <- t(apply(df_coeffi, 1, scale))
  colnames(df_coeffi_scale) <- sector_collars
  # View(df_coeffi_scale)

  linkages <- c("average"
                , "single"
                , "complete"
                , "mcquitty"
                )
  num_linkages <- length(linkages)

  distance_matrix_correlation <- as.dist(1-cor(df_coeffi_scale, method = "pearson"))
  # View(distance_matrix_correlation)

  # Check if the matrix is singular
  # print(is.singular.matrix((1-cor(df_coeffi_scale, method = "pearson"))))

  list_pv_cluster_results <- list()

  for(i in seq(num_linkages))
  {
    linkage_i <- linkages[i]
    print(linkage_i)

    linkage_i_name <- paste("linkage"
                            , linkage_i
                            , sep = "_")

    set.seed(225)

    list_pv_cluster_results[[linkage_i_name]] <- pvclust(df_coeffi_scale
                                                         , method.hclust = linkage_i
                                                         , method.dist = "correlation"
                                                         , nboot = 1500
                                                         , parallel = TRUE)

    distance_matrix_pv_clust <- cophenetic(list_pv_cluster_results[[linkage_i_name]]$hclust)

    
    cophenetic_cor <- cor(distance_matrix_correlation, distance_matrix_pv_clust)
    print(cophenetic_cor)

    df_temp <- data.frame(linkage = linkage_i
                          , cophenetic_cor = cophenetic_cor
                          , stringsAsFactors = FALSE)

    if(i == 1)
    {
      df_cor <- df_temp
    } else {
      df_cor <- df_cor %>%
        full_join(.
                  , df_temp
                  , by = colnames(.))
    }
  }

  list_pv_cluster_results[["cophenetic_correlations"]] <- df_cor

  list_pv_cluster_results[["dataset_coefficients"]] <- df_coeffi
  
  list_pv_cluster_results[["dataset_adj_pvalues"]] <- df_p_values
  
  list_pv_cluster_results[["dataset_adj_pvalues_asterisk"]] <- df_p_values_asterisks

  return(list_pv_cluster_results)
}