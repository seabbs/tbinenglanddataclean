#' Plot Historic Proportions of Pulmonary/Extra-Pulmonary TB
#'
#' @param df A dataframe, (defaults to using \code{\link[tbinenglanddataclean]{tb_not_ew}})
#' of historic TB notifications
#' @param return A logical indiciting if the plot should be printed or returned (defaults to \code{FALSE})
#' @return A ggplot2 plot of TB notifications over time, with a secondary zoomed plot from a specified date.
#' @export
#' @inheritParams plot_historic_tb_ew
#' @import magrittr
#' @import ggplot2
#' @import viridis
#' @importFrom dplyr mutate group_by ungroup
#' @importFrom tidyr gather
#' @importFrom scales percent
#' @examples
#'
#' plot_historic_prop_pul_tb()
plot_historic_prop_pul_tb <- function(df = NULL,
                                plot_theme = NULL,
                                colour_scale = NULL,
                                return = FALSE) {
  
  if (is.null(df)) {
    df <- tbinenglanddataclean::tb_not_ew
    }
  
  if (is.null(colour_scale)) {
    colour_scale <- scale_fill_viridis(discrete = TRUE)
  }
  
  if (is.null(plot_theme)) {
    plot_theme <- theme_minimal()
  }
  
  p <- df %>%
    mutate(Respiratory = respiratory,
           `Non-respiratory` = non_respiratory,
           Pulmonary = pulmonary,
           `Extra-Pulmonary` = extra_pulmonary,
           Year = year) %>%
    gather(key = "TB type", value = "Notifications", Respiratory,`Non-respiratory`, Pulmonary, `Extra-Pulmonary`) %>%
    group_by(year) %>% 
    mutate(total = sum(Notifications, na.rm = TRUE)) %>% 
    mutate(Proportion = Notifications/total) %>%
    ungroup %>% 
    ggplot(aes(x = Year, y = Proportion, fill = `TB type`), na.rm = TRUE) +
    geom_bar(stat = "identity", na.rm = TRUE) +
    plot_theme +
    theme(legend.position = "bottom") +
    scale_y_continuous(labels = scales::percent) +
    colour_scale +
    guides(fill = guide_legend(title = "TB type"))

  if (return) {
    return(p)
  }else{
    p
  }
}



