
#' @export
bro_gg_barscatter <- function(df, x, y, title = NULL, bar_color = "#EA883D") {
  df %>%
    ggplot(aes({{x}}, {{y}})) +
    stat_summary(fun.y = mean, geom = "bar", width = 0.4, fill = bar_color) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
    geom_point(position = position_jitter(width = 0.1, height = 0)) +
    ggtitle(title)
}

#' @export
bro_gg_lollipop <- function(df, x, y, point_color = "#EA883D") {
  df %>%
    ggplot(aes({{x}}, {{y}})) +
    geom_segment(aes(x = {{x}}, y = 0, xend = {{x}}, yend = {{y}}), color = "grey50") +
    geom_point(color = point_color)
}
