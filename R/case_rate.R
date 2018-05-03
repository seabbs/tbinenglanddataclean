#' Function to Estimate the Case Rate for a Given Variable, stratified by given variables, dependant on age, year, agegrp2, and uk birth status. Missing data for the rate variable is dropped
#'
#' @description  Function to estimate the case rate for a given variable, stratified by given variables,
#'  dependant on age, year, agegrp2, and uk birth status. Missing data for the rate variable is dropped.
#' @param df A dataframe in the format of the ETS
#' @param rate_for A character string of the variable to calculate the rate for.
#' @param strat_by A character string of the variables to stratfy by.
#' @param age_split A character strig og the age split to stratify case counts by.
#' @param CoB_split A logical indicating if Country of birth should be used to split the case counts.
#' @param Year_strat A logical indicating if year should be stratified over.
#'
#' @return Returns a dataframe of case rates stratified by multiple variables with confidence intervals.
#' @export
#' @importFrom dplyr filter group_by ungroup select mutate summarise rowwise funs
#' @import magrittr
#' @examples
#'
case_rate = function(df, rate_for, strat_by = NULL, age_split = NULL,
                     CoB_split = TRUE, Year_strat = TRUE) {
  CaseCountVar <- c('age', 'year', 'ukborn')
  GroupByCase <- c()
  GroupByTotCase <- c()

  if (!is.null(strat_by)) {
    CaseCountVar <- c(CaseCountVar, strat_by)
    GroupByCase <- c(GroupByCase, strat_by)
    GroupByTotCase <- c(GroupByTotCase, strat_by)
  }

  if (!is.null(rate_for)) {

    CaseCountVar <- c(CaseCountVar, rate_for)
    GroupByCase <- c(GroupByCase, rate_for)
  }

  if (!is.null(age_split)) {
    GroupByCase <- c(GroupByCase, age_split)
    GroupByTotCase <- c(GroupByTotCase, age_split)
  }

  if (CoB_split) {
    GroupByCase <- c(GroupByCase, 'CoB')
    GroupByTotCase <- c(GroupByTotCase, 'CoB')
  }

  if (Year_strat) {
    GroupByCase <- c(GroupByCase, 'Year')
    GroupByTotCase <- c(GroupByTotCase, 'Year')
  }


  df <- df %>%
    filter(.vars = rate_for, .funs = funs(!is.na(.)))  %>%
    extract_case_counts(strat_var = CaseCountVar) %>%
    group_by(.dots = GroupByCase) %>%
    summarise(Cases = sum(Cases, na.rm = TRUE)) %>%
    ungroup %>%
    group_by(.dots = GroupByTotCase) %>%
    mutate(`Total cases` = sum(Cases, na.rm = TRUE)) %>%
    ungroup() %>%
    rowwise %>%
    mutate(`Case rate` = prop.test(Cases, `Total cases`)$estimate * 100,
           LowRate = prop.test(Cases, `Total cases`)$conf.int[1] * 100,
           HiRate = prop.test(Cases, `Total cases`)$conf.int[2] * 100)

  return(df)
}
