#'A Function to Plot Differences Between LFS and ONS Data
#'
#' @param df A Dataframe as produced by \code{\link[tbinenglanddataclean]{combine_ons_with_lfs}} of
#' population demographics
#' @param theme_set The ggpplot theme to be used defaults to minimal.
#' @return A ggplot summarising the differences between both ONS and LFS datasets
#' @export
#' @import ggplot2
#' @examples
#'
plot_pop_age_compare_ons_lfs = function(df, theme_set = NULL)
{
  if (is.null(theme_set)) {
    theme_set <- theme_minimal()
  }
  df %>%
    filter(Year %% 2 ==0) %>%
    mutate(Year = Year %>% factor(levels=c(2000:2016))) %>%
    group_by(CoB, Year, `Age group`) %>%
    summarise(Population = sum(Population)) %>%
    spread(key = CoB, value = Population) %>%
    mutate(`Percentage difference` = 100*(Total - `Total (LFS)`)/Total) %>%
    mutate(`Age group` = factor(`Age group`, levels = rev(levels(`Age group`)))) %>%
    ggplot(aes(x = `Age group` , y = `Percentage difference`, fill = Year, colour = Year)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    theme_set +
    theme(axis.text.x = element_text(angle = 45)) +
    coord_flip() +
    labs(caption = "Comparision of ONS and LFS population estimates ") -> p
  return(suppressWarnings(p))
}
