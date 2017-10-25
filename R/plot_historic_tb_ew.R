#' Plot Historic TB Notifications in England and Wales
#'
#' @param df A dataframe, (defaults to using \code{\link[tbinenglanddataclean]{tb_not_ew}})
#' of historic TB notifications
#' @param zoom_date Numeric, the year to filter notifications from for the second zoomed plot.
#' @param zoom_nots Numeric, the number of notifications to zoom the y axis on.
#' @param plot_theme The ggplot2 theme to use, defaults to \code{\link[ggplot2]{theme_minimal}}.
#' @param colour_scale The colour scale to plot with, defaults to \code{\link[ggplot2]{scale_colour_viridis_d}}
#' @param return A logical indiciting if the plot should be printed or returned (defaults to \code{FALSE})
#' @return A ggplot2 plot of TB notifications over time, with a secondary zoomed plot from a specified date.
#' @export
#' @import ggplot2
#' @importFrom dplyr mutate
#' @importFrom tidyr gather
#' @importFrom ggforce facet_zoom
#' @examples
#'
#' plot_historic_tb_ew()
plot_historic_tb_ew <- function(df = tb_not_ew,
                                zoom_date = 1975,
                                zoom_nots = 10000,
                                plot_theme = theme_minimal,
                                colour_scale = scale_color_viridis_d,
                                return = FALSE) {
  p <- df %>%
    mutate(Pulmonary = pulmonary,
           `Extra-Pulmonary` = extra_pulmonary,
           Year = year) %>%
    gather(key = "TB Type", value = "Notifications", Pulmonary, `Extra-Pulmonary`) %>%
    ggplot(aes(x = Year, y = Notifications, col = `TB Type`)) +
    geom_point() +
    geom_line() +
    plot_theme() +
    ggforce::facet_zoom(x = Year > zoom_date, y = Notifications <= zoom_nots, zoom.size = 1, horizontal = FALSE) +
    theme(legend.position = "bottom") +
    colour_scale()

  if (return) {
    return(p)
  }else{
    p
  }
}
