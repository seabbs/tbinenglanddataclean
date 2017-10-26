#' Plot Historic TB Notifications in England and Wales
#'
#' @param df A dataframe, (defaults to using \code{\link[tbinenglanddataclean]{tb_not_ew}})
#' of historic TB notifications
#' @param zoom_date Numeric, the year to filter notifications from for the second zoomed plot.
#' @param plot_theme The ggplot2 theme to use, defaults to \code{\link[ggplot2]{theme_minimal}}.
#' @param colour_scale The colour scale to plot with, defaults to \code{\link[ggplot2]{scale_colour_viridis_d}}
#' @param return A logical indiciting if the plot should be printed or returned (defaults to \code{FALSE})
#' @return A ggplot2 plot of TB notifications over time, with a secondary zoomed plot from a specified date.
#' @export
#' @import ggplot2
#' @import magrittr
#' @importFrom dplyr mutate bind_rows
#' @importFrom tidyr gather
#' @examples
#' plot_historic_tb_ew()
plot_historic_tb_ew <- function(df = tb_not_ew,
                                zoom_date = 1975,
                                plot_theme = NULL,
                                colour_scale = NULL,
                                return = FALSE) {
  if (!is.null(zoom_date)) {
    df_zoom <- df %>%
      filter(year >= zoom_date) %>%
      mutate(zoom = "")
    df <- df %>%
      mutate(zoom = " ")

    df <- df %>%
      bind_rows(df_zoom) %>%
      mutate(zoom = factor(zoom, levels = c(" ", "")))
  }
  
  if (is.null(colour_scale)) {
    colour_scale <- ggplot2::scale_colour_viridis_d
  }
  
  if (is.null(plot_theme)) {
    plot_theme <- ggplot2::theme_minimal
  }
  
  p <- df %>%
    mutate(Pulmonary = pulmonary,
           `Extra-Pulmonary` = extra_pulmonary,
           Year = year) %>%
    gather(key = "TB Type", value = "Notifications", Pulmonary, `Extra-Pulmonary`) %>%
    ggplot(aes(x = Year, y = Notifications, col = `TB Type`)) +
    geom_point() +
    geom_line() +
    plot_theme() +
    colour_scale() +
    theme(legend.position = "bottom")

  if (!is.null(zoom_date)) {
    p <- p + facet_wrap(~zoom, scales = "free", ncol = 1)
  }
  if (return) {
    return(p)
  }else{
    p
  }
}
