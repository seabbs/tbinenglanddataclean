#' Function to Split by Country of Birth and Format for epiR
#'
#' @param df A dataframe.
#' @param filter Country of Birth filter to use.
#'
#' @return Returns a dataframe filtered by Country of Birth
#' @export
#' @importFrom dplyr filter select
#' @import magrittr
#' @examples
#'
form_df_epiR_inc_rate = function(df, filter)
{
  df %>%
    filter(CoB %in% filter) -> df

  df_mat <- df %>%
    select(-CoB, -Year) %>%
    as.matrix

  rownames(df_mat) <- df$Year
  return(df_mat)
}
