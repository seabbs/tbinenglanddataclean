#' Function to Estimate Age Adjusted Incidence
#' @inheritParams form_df_epiR_inc_rate
#' @param pop A dataframe of demographis.
#' @param cases A dataframe of cases.
#' @param rate_pop The incidence rate scale to use.
#'
#' @return A dataframe containing age stratified incidence.
#' @export
#' @import magrittr
#' @import dplyr
#' @importFrom epiR epi.directadj
#' @importFrom tibble as_data_frame
#' @examples
#'
est_inc_crude_adj = function(filter, tar = pop_age, cases = cases_age, rate_pop = 100000){
  ## format cases
  cases_mat <- cases %>% form_df_epiR_inc_rate(filter)

  ## Format populations
  pop_mat <- pop %>% form_df_epiR_inc_rate(filter)

  meta_pop <- pop_mat %>% colSums %>% as.matrix %>% t

  tmp <- epi.directadj(obs = cases_mat, tar = pop_mat, std = meta_pop, units = rate_pop)

  ##format crude rates
  tmp$crude %>%
    as_data_frame %>%
    mutate(CoB = rep(filter, nrow(tmp$crude))) -> tmp$crude

  ##format crude total rates
  tmp$crude.strata %>%
    as_data_frame %>%
    mutate(CoB = rep(filter, nrow(tmp$crude.strata)),
           cov = rep('All cases (crude)', nrow(tmp$crude.strata))) -> tmp$crude.strata

  ##format adjusted total rates

  tmp$adj.strata %>%
    as_data_frame %>%
    mutate(CoB = rep(filter, nrow(tmp$crude.strata)),
           cov = rep('All cases (adj)', nrow(tmp$crude.strata))) -> tmp$adj.strata


  tmp <- bind_rows(tmp$crude, tmp$crude.strata, tmp$adj.strata) %>%
    rename(Year = strata, Incidence = est, Inc_LCI = lower, Inc_UCI = upper)

  return(tmp)
}
