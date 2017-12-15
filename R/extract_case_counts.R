
#' Extract Case Counts from the ETS Data
#'
#' @description Case counts stratified by the required variables. In addition this function adds additional grouping
#' variables for age so that the ETS data is inline with other demographic datasets.
#' @param df A dataframe in the current ETS format .
#' @param strat_var Vaiables to stratify case counts by.
#'
#' @return Case counts stratified by the required variables
#' @export
#' @import dplyr
#' @import magrittr
#' @examples
#'
extract_case_counts = function(df,
                               strat_var =  c('age', 'year', 'ukborn')) {
case_year_age <-  df %>%
    mutate(age = replace(age, age >= 90, 90) %>% as.character) %>%
    mutate(age = replace(age, age %in% '90', '90+')) %>%
    mutate(age = factor(age, levels = c(as.character(0:89), '90+'))) %>%
    group_by(.dots = strat_var) %>%
    tally() %>% 
    ungroup() %>%
    rename(Year = year, Cases = n, CoB = ukborn) %>%
    rename(Age = age) %>%
    mutate(`Age group (condensed)` = Age %>%  as.character %>% replace(Age %in% '90+', '90') %>%
             as.numeric %>%
             cut(breaks = c(0, 15, 65, 91), right = FALSE, ordered_result = TRUE, labels = c('0-14', '15-64', '65+'))) %>%
    mutate(`Age group` = Age %>%  as.character %>% replace(Age %in% '90+', '90') %>%
             as.numeric %>%
             cut(breaks = c(seq(0,90,5), 91), right = FALSE, ordered_result = TRUE, labels = c(paste(seq(0,85, 5), seq(4,89,5), sep = '-'), '90+')))
  return(case_year_age)
}
