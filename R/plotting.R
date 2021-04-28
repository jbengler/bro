
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

#' @export
bro_geom_barscatter <- function(dodge = FALSE, colors = NA, fill_alpha = 0.5, jitter_width = 0.1) {
  out <-
    if (dodge) {
    list(
    stat_summary(fun = mean, geom = "bar", alpha = fill_alpha, color = NA, width = 0.6, position = position_dodge(width = 0.8)),
    stat_summary(fun.data = mean_se, geom = "errorbar", size = 0.25, width = 0.4, position = position_dodge(width = 0.8)),
    geom_point(size = 0.5, position = position_jitterdodge(jitter.width = jitter_width, jitter.height = 0, dodge.width = 0.8)),
    scale_y_continuous(expand = expansion(mult = c(0, .05)))
  )} else {
    list(
    stat_summary(fun = mean, geom = "bar", alpha = fill_alpha, color = NA, width = 0.6),
    stat_summary(fun.data = mean_se, geom = "errorbar", size = 0.25, width = 0.4),
    geom_point(size = 0.5, position = position_jitter(width = jitter_width, height = 0)),
    scale_y_continuous(expand = expansion(mult = c(0, .05)))
    )}

  if (!is.na(colors)) {
    out <- c(
      out,
      list(
        scale_fill_manual(values = colors),
        scale_color_manual(values = colors)
      )
    )
  }
  out
  }

# bro_geom_ribbon
#
# library(tidyverse)
# library(bro)
# library(patchwork)
#
# ggplot(mtcars, aes(factor(gear), mpg)) +
#   bro_geom_barscatter()
#
# ggplot(mtcars, aes(factor(gear), mpg, fill = factor(cyl))) +
#   bro_geom_barscatter()
#
# ggplot(mtcars, aes(factor(gear), mpg, fill = factor(cyl))) +
#   bro_geom_barscatter(dodge = TRUE)
#
# ggplot(mtcars, aes(factor(gear), mpg, fill = factor(cyl), color = factor(cyl))) +
#   bro_geom_barscatter(dodge = TRUE, colors = bro_pals$color_circle_vivid %>% unname()) +
#   bro_theme_nature() +
#   plot_layout(widths = unit(30, "mm"), heights = unit(30, "mm"))
#
#
